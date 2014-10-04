source("utils.R")
source("plotting.R")

shinyServer(
  function(input, output) {
    
    # Reactive object containing log file data (NULL pre-upload)
    #
    # needs error messages
    data <- reactive({
      input$refresh
      if(is.null(input$logfile) | is.null(categories()$regexes)) {
        NULL
      } else {
        tryCatch({
          parseLogfile(logfile.path  = input$logfile$datapath, 
                       regexes = categories()$regexes, 
                       perl = input$perl)
          }, 
          error = function(cond) NULL, 
          warning = function(cond) NULL
        )
      }
    })
    
    # Reactive object containing user-input tag names / regexes and 
    # corresponding descriptive names
    #
    # needs error messages
    categories <- reactive({
      input$refresh
      tryCatch({
        regexes <- 
          eval(parse(text = paste("c(", isolate(input$regexes), ")", 
                                  sep = "")))
        catnames <- 
          eval(parse(text = paste("c(", isolate(input$catnames), ")", 
                                  sep = "")))
        list(regexes = regexes, catnames = catnames)
        }, 
        error = function(cond) NULL, 
        warning = function(cond) NULL
      )
    })
    
    
    # Reactive date range UI (default: log file span)
    output$daterangeUI <- renderUI({
      input$refresh
      if (is.null(data())) {
        NULL
      } else {
        timestamps <- data()$timestamps
        dateRangeInput("daterange", 
          label = "Time frame to display:", 
          start = as.Date(as.POSIXct(min(timestamps), 
                                     origin = "1970-01-01")), 
          end = as.Date(as.POSIXct(max(timestamps), 
                                   origin = "1970-01-01")))
          # input$daterange is NULL if daterangeUI is NULL
      }
    })
    
    # Main category
    output$catUI <- renderUI({
      input$refresh
      selectInput("cat", 
                  label = "Pick a category to look at:", 
                  choices = categories()$catnames, 
                  selected = categories()$catnames[1])
    })
    
    # Secondary category (matrix, weekday displays)
    output$cat2UI <- renderUI({
      input$refresh
      selectInput("cat2", 
                  label = "", 
                  choices = categories()$catnames, 
                  selected = categories()$catnames[2])
    })
    
    # Category for x-axis of scatterplot
    output$catxUI <- renderUI({
      input$refresh
      selectInput("catx", 
                  label = "Independent variable:", 
                  choices = categories()$catnames, 
                  selected = categories()$catnames[2])
    })

    # Conditioning variable (trellis scatterplot)
    output$catcUI <- renderUI({
      input$refresh
      selectInput("catc", 
                  label = "", 
                  choices = categories()$catnames, 
                  selected = categories()$catnames[3])
    })

    # Color coded legend (matrix, weekday if input$addcat is TRUE)
    output$legend <- renderUI({
      input$refresh
      helpText(
        strong("Color coding:"), 
        div(input$cat, style = "color:#0080FF"), 
        div(input$cat2, style = "color:#F08080")) # lightcoral
    })
    
    
    # Boolean vector indicating for every ping whether it is in the
    # range of input$daterange
    timeframe <- reactive({
      if (is.null(data())) {
        NULL
      } else {
        timestamps <- data()$timestamps
        if (!is.null(input$daterange)) {
          starttime <- paste(input$daterange[1], "00:00:00")
          starttime <- as.numeric(as.POSIXct(starttime))
          endtime <- paste(as.Date(input$daterange[2] + 1), "00:00:00")
          endtime <- as.numeric(as.POSIXct(endtime))
          timeframe <- timestamps >= starttime & timestamps < endtime
        } else {
          timeframe <- rep(TRUE, length(timestamps))
        }
        timeframe
      }
    })
    
    # Computes date axis from timeframe()
    date.axis <- reactive({
      if (is.null(data())) {
        NULL
      } else {
        timestamps <- data()$timestamps
        at <- seq(min(timestamps[timeframe()]), 
                   max(timestamps[timeframe()]), 
                   length = 8)
        lab <- as.Date(as.POSIXct(at, origin = "1970-01-01"))
        data.frame(at = at, lab = lab)
      }
    })
    
    # Computes 0 - 24 time of day axis from input$midnight
    #
    # only works properly for input$midnight in 0:6
    timeofday.axis <- reactive({
      mn <- as.numeric(input$midnight)
      if (mn == 0) {
        lab <- seq(0, 24, by = 6)
        at <- seq(0, 24, by = 6)
      } else {
        x <- c(mn:24, 1:mn)
        x <- x[2:(length(x) - 1)]
        x <- x[x %% 6 == 0]
        lab <- c(mn, x, mn)
        at <- lab - mn
        at[length(at)] <- 24
      }
      data.frame(at = at, lab = lab)
    })
    
    # For every timestamp, computes timestamp of most recent past 
    # custom midnight, hours since past custom midnight, and weekday
    # (custom midnight adjusted)
    coords <- reactive({
      if (is.null(data())) {
        NULL
      } else {
        timestamps <- data()$timestamps
        splitTimestamp(timestamps, midnight = as.numeric(input$midnight), 
                       tz = input$timezone)
      }
    })
    
    
    output$hist <- renderPlot({
      if (is.null(data()) | 
          any(c(input$cat) == "") | 
          !all(c(input$cat) %in% categories()$catnames)) {
        NULL
      } else {
        timestamps <- data()$timestamps
        matches <- data()$matches
        n.cat <- which(input$cat == categories()$catnames)[1]
        times.all <- timestamps[matches[,n.cat]]
        times.sub <- timestamps[timeframe()][matches[timeframe(), n.cat]]
        par(mar = c(5.1, 4.1, 4.1, 2.6))
        par(las = 1)
        histPlot(catname = input$cat, 
                 times.all = times.all, 
                 times.sub = times.sub, 
                 ping.interval = input$ping.interval, 
                 n.bins = input$n.bins.hist, 
                 units = switch(input$units.hist, 
                                 "Hours per week" = 7, 
                                 "Hours per day" = 1), 
                 xaxs = date.axis(), 
                 bandwidth = input$bandwidth.hist)
      }
    })
    
    output$matrix <- renderPlot({
      if (is.null(data()) | is.null(coords()) | input$cat == "" | 
          !(input$cat %in% categories()$catnames) | 
          (input$addcat & (input$cat2 == "" | 
                           !(input$cat2 %in% categories()$catnames)))) {
        NULL
      } else {
        matches <- data()$matches
        n.cat <- which(input$cat == categories()$catnames)[1]
        coords <- coords()[timeframe() & matches[,n.cat],]
        par(mar = c(5.1, 4.1, 4.1, 2.6))
        par(las = 1)
        matrixPlot(dates = coords$prev.midnight, 
                   timesofday = coords$timeofday, 
                   xaxs = date.axis(), 
                   yaxs = timeofday.axis())
        if (input$addcat) {
          n.cat2 <- which(input$cat2 == categories()$catnames)[1]
          coords2 <- coords()[timeframe() & matches[,n.cat2],]
          matrixPlot(dates = coords2$prev.midnight, 
                     timesofday = coords2$timeofday, 
                     add = TRUE)
          title(paste(input$cat, "and", tolower(input$cat2), 
                      "(by date and time of day)"))
        } else {
          title(paste(input$cat, "(by date and time of day)"))
        }
      }
    })
    
    output$timeofday <- renderPlot({
      if (is.null(data()) | is.null(coords()) | 
          any(c(input$cat) == "") | 
          !all(c(input$cat) %in% categories()$catnames)) {
        NULL
      } else {
        matches <- data()$matches
        n.cat <- which(input$cat == categories()$catnames)[1]
        wdays <- switch(input$weekend,
                  "Weekdays only" = 1:5, 
                  "Weekends only" = 6:7, 
                  "All days" = 1:7)
        timesofday <- coords()$timeofday[timeframe() & matches[,n.cat] & 
                                         coords()$wday %in% wdays]
        timeofdayPlot(catname = input$cat, 
                    timesofday = timesofday, 
                    xaxs = timeofday.axis(), 
                    bandwidth = input$bandwidth.tod)
      }
    })
    
    output$week <- renderPlot({
      if (is.null(data()) | is.null(coords()) | input$cat == "" | 
          !(input$cat %in% categories()$catnames) | 
          (input$addcat & (input$cat2 == "" | 
                           !(input$cat2 %in% categories()$catnames)))) {
        NULL
      } else {
        matches <- data()$matches
        n.cat <- which(input$cat == categories()$catnames)[1]
        coords <- coords()[timeframe() & matches[,n.cat],]
        par(mar = c(5.1, 4.1, 4.1, 2.6))
        par(las = 1)
        weekPlot(dates = coords$prev.midnight, 
                 timesofday = coords$timeofday, 
                 wdays = coords$wday, 
                 chron = input$ordered.week,
                 yaxs = timeofday.axis())
        if (input$addcat) {
          n.cat2 <- which(input$cat2 == categories()$catnames)[1]
          coords2 <- coords()[timeframe() & matches[,n.cat2],]
          weekPlot(dates = coords2$prev.midnight, 
                   timesofday = coords2$timeofday, 
                   wdays = coords2$wday, 
                   chron = input$ordered.week, 
                   add = TRUE)
          title(paste(input$cat, "and", tolower(input$cat2), 
                      "(by weekday and time of day)"))
        } else {
          title(paste(input$cat, "(by weekday and time of day)"))
        }
      }
    })
    
    # Note antialiasing doesn't work in the deployed version
    output$scatter <- renderPlot({
      if (is.null(data()) | is.null(coords()) | 
          any(c(input$cat, input$catx) == "") | 
          !all(c(input$cat, input$catx) %in% categories()$catnames) | 
          (input$trellis & (input$catc == "" |
                            !(input$catc %in% categories()$catnames)))) {
        NULL
      } else {
        matches <- data()$matches
        n.cat <- which(input$cat == categories()$catnames)[1]
        n.catx <- which(input$catx == categories()$catnames)[1]
        
        # this is still very repetitive
        dates <- coords()$prev.midnight[timeframe()]
        y <- countPings(dates = dates,  
                        subset = matches[timeframe(), n.cat])
        x <- countPings(dates = dates, 
                        subset = matches[timeframe(), n.catx])
        y <- y * input$ping.interval / 60
        x <- x * input$ping.interval / 60
        
        if(!input$trellis) {
          scatterPlot(x, y, z = NULL, 
                      names = c(input$cat, input$catx), 
                      jitter = input$jitter, 
                      trellis = input$trellis)
        } else {
          n.catc <- which(input$catc == categories()$catnames)[1]
          z <- countPings(dates = dates, 
                          subset = matches[timeframe(), n.catc])
          z <- z * input$ping.interval / 60
          scatterPlot(x, y, z, 
                      names = c(input$cat, input$catx, input$catc), 
                      jitter = input$jitter, 
                      trellis = input$trellis)
        }
      }
    })
  }
)
