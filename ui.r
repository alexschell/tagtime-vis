shinyUI(fluidPage(
  titlePanel("TagTime Visualizer"), 
  
  sidebarLayout(
    sidebarPanel(
      p("Display time use patterns from your ", 
        a("TagTime", href = "http://messymatters.com/tagtime/"), 
        " log file. (See the ", a("GitHub repo", 
          href = "http://github.com/alexschell/tagtime-vis"), 
        " for the code.)"), 
      
      conditionalPanel(
        condition = "input.tabID == 0",
        fileInput("logfile", 
                  label = "To get started, upload your TagTime .log file:"), 
        strong("Options"), 
        numericInput("ping.interval", 
                     label = "Average interval between pings (minutes)", 
                     value = 45,
                     min = 1, 
                     max = 300, 
                     step = 1),
        checkboxInput("perl", 
                      label = "Use PERL syntax for regular expressions", 
                      value = FALSE)
      ), 

      conditionalPanel(
        condition = "input.tabID != 0", 
        uiOutput("catUI"), 
        
        uiOutput("daterangeUI"), 
        
        conditionalPanel(
          condition = "input.tabID == 2 | input.tabID == 5", 
          checkboxInput("addcat", 
                        label = "Add another category to plot", 
                        value = FALSE)
        ), 
        
        conditionalPanel(
          condition = "(input.tabID == 2 | input.tabID == 5) & input.addcat", 
          uiOutput("cat2UI")
        ), 
      
        br(), 
        strong("Graphing options:"), 
        br(), 
        
        conditionalPanel(
          condition = "input.tabID == 1", 
          selectInput("n.bins.hist", 
                      label = "Number of bins in the histogram", 
                      choices = c(10, 15, 20, 30, 50), 
                      selected = 15), 
          sliderInput("bandwidth.hist", 
                      label = "Density estimate smoothness", 
                      min = 0.3, 
                      max = 1, 
                      value = 0.85), 
          radioButtons("units.hist", 
                       label = "Units to display:", 
                       choices = c("Hours per week", "Hours per day"), 
                       selected = "Hours per week")
        ), 
        
        conditionalPanel(
          condition = "input.tabID == 3", 
          uiOutput("catxUI"), 
          checkboxInput("jitter", 
                        label = "Add jittering", 
                        value = TRUE), 
          checkboxInput("trellis", 
                        label = "Condition on a third variable", 
                        value = FALSE)
        ), 
        
        conditionalPanel(
          condition = "input.tabID == 3 & input.trellis", 
          uiOutput("catcUI")
        ), 
        
        conditionalPanel(
          condition = "input.tabID == 4", 
          radioButtons("weekend", 
                       label = "", 
                       choices = c("Weekdays only", 
                                   "Weekends only", 
                                   "All days"), 
                       selected = "All days"), 
          sliderInput("bandwidth.tod", 
                      label = "Density estimate smoothness",
                      min = 0.3, 
                      max = 0.6, 
                      value = 0.45)
        ), 
        
        conditionalPanel(
          condition = "input.tabID == 5", 
          checkboxInput("ordered.week", 
                       label = "Order chronologically within bins", 
                       value = FALSE)
        ), 
        
        conditionalPanel(
          condition = "input.tabID == 2 | input.tabID == 4 | input.tabID == 5", 
          selectInput("midnight", 
                      label = "Custom midnight", 
                      choices = 0:6, 
                      selected = 0)
        )
      ) 
    ), 
  
    mainPanel(
      tabsetPanel(type = "tabs", 
        tabPanel("Main", 
          textOutput("testing"), 
          textInput("regexes", 
                    label = paste("Enter tags for some of your activities,", 
                                  "in quotes and separated by commas:"), 
                    value = ""), 
          textInput("catnames", 
                    label = paste("Add descriptive names for the tags,", 
                                  "again in quotes and separated by commas:"), 
                    value = ""), 
          p("Example input:", 
            span(code('"work", "slp", "prod3"')), 
            span(code('"Work", "Sleep", "Productivity"'))), 
          actionButton("refresh", "Submit"), 
          br(), 
          textInput("timezone", 
                    label = paste("Enter your timezone. If you make an ", 
                                  "error here, UTC time is assumed."), 
                    value = "EST5EDT"), 
          br(), 
          p("Regular expressions are encouraged.", 
            "Here are some examples:"), 
          p(span(code('"work"')), "matches ", 
            strong("work"), "and ", strong("working")), 
          p(span(code('"work "')), "matches ", 
            strong("work"), "but not ", strong("working")), 
          p(span(code('"slp|nap"')), "matches ", 
            strong("slp"), "and ", strong("nap")),
          p(span(code('"prod[0,1]"')), "matches ", 
            strong("prod0"), "and ", strong("prod1")),
          value = 0), 
        tabPanel("Trend", plotOutput("hist"), value = 1), 
        tabPanel("Matrix", plotOutput("matrix"), value = 2), 
        tabPanel("Time of day", plotOutput("timeofday"), value = 4), 
        tabPanel("Weekdays", plotOutput("week"), value = 5), 
        tabPanel("Scatterplot", plotOutput("scatter"), value = 3), 
        id = "tabID"), 
      conditionalPanel(
        condition = "(input.tabID == 2 | input.tabID == 5) & input.addcat", 
        uiOutput("legend")
      )
    )
  )
))
