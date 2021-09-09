source('libraries.R')
source("LoadDatabyDate.R")
source("getData.R")
source("MakeCountyMap.R")
source('estFatalAxis.R')
source("contourMap.R")
source("dailyPlot.R")
source("immuStatePlot.R")
source("MakeMap.R")
## interface
ui <- fluidPage(
  titlePanel("Population Level Immunity"),
  fluidRow(
    sidebarLayout(sidebarPanel(
      wellPanel(tags$small(
        radioButtons("vaccinatedType", "Select Vaccination Type",
                     c("At Least One Dose", "Fully Vaccinated"), selected = "At Least One Dose", inline = T),
        radioButtons("ascertainment_bias", "Select Ascertainment Bias",
                     c(3,4,5), selected = 4, inline = T),
        radioButtons("Event Size", "Select event size", c(30, 50, 100), selected = 30, inline = T)
        ),
        br(), br(),
        # Download Buttons
        downloadButton("downloadAll", "Download All Data", icon = shiny::icon("download")),
        downloadButton("downloadPartial", "Download Your Selected Data", icon = shiny::icon("download"))
        ###
      )),
      wellPanel(
        tags$small(paste0(
          "Note: The data includes the territories of the United States (excluding American Samoa).
          Due to the big data, it will take a few seconds to load plots for ALL states at once."
        ))),
      wellPanel(
        tags$small(
          strong("Data Sources:"), tags$br(),
          "- ", tags$a("COVID-19 case data collated by The New York Times, based on reports from state and local health agencies.", 
                       href = 'https://github.com/nytimes/covid-19-data'), tags$br(),
          "- ", tags$a("Vaccination data collated by Our World in Data from United States Centers for Disease Control and Prevention.",
                       href = 'https://github.com/owid/covid-19-data/tree/master/public/data/vaccinations'), tags$br(),
          "- Population data from United States Census Bureau.", tags$br(),
          strong("Codes:"), tags$br(),
          " - ",tags$a(href = 'https://github.com/quannguyenminh103/Covid19-Population-Level-Immunity', "Covid19-Population-Level-Immunity")
        ))
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  # tabPanel("Daily Immunity Level Plot", br(), 
                  #          div(style = "margin-bottom:-em",
                  #              fluidRow(plotlyOutput("plot2", width = 800, height = 700))),
                  #          div(style = "margin-left: -8px; margin-top:-17em; margin-bottom:-5em",
                  #              plotOutput("taxis1",width = 825))
                  # ),
                  tabPanel("Infected Frac vs Cumulative Deaths/Population Plot", br(), 
                           div(style = "margin-bottom:-em",
                               fluidRow(plotlyOutput("plot1", width = 800, height = 700))),
                           div(style = "margin-left: -8px; margin-top:-17em; margin-bottom:-5em",
                               plotOutput("taxis2",width = 825))
                  ),
                  tabPanel("Population Immunity Level vs Risk Plot", br(), 
                           div(style = "margin-bottom:-em",
                               fluidRow(plotlyOutput("plot1", width = 800, height = 700))),
                           div(style = "margin-left: -8px; margin-top:-17em; margin-bottom:-5em",
                               plotOutput("taxis2",width = 825))
                  ),
                  tabPanel("Vaccinated Rate vs Risk Plot", br(),
                           div(style = "margin-bottom:-em",
                               fluidRow(plotlyOutput("plot1", width = 800, height = 700))),
                           div(style = "margin-left: -8px; margin-top:-17em; margin-bottom:-5em",
                               plotOutput("taxis2",width = 825))
                  ),
                  
                  #tabPanel("State-Level Map", leafletOutput("map1", width = 800, height = 700)),
                  #tabPanel("County-Level Map", leafletOutput("map2", width = 800, height = 700))),
      br(),
      fluidRow(column(10, wellPanel(tags$small(
        "Developers: ", tags$a(href = "https://www.linkedin.com/in/king-nguyen-103/", 'Quan M Nguyen'), ",", tags$a(href = 'http://sjbeckett.github.io/', 'Stephen J Beckett'), ", and ",
        tags$a(href = 'https://weitzgroup.biosci.gatech.edu/', "Joshua S Weitz."), "For more information contact Dr. Beckett (stephen.beckett@biology.gatech.edu) and
              Dr. Weitz (jsweitz@gatech.edu). This dashboard is powered and hosted by ", tags$a(href = "https://www.rstudio.com/", "RStudio."), "We acknowledge additional code contributions from ",
        tags$a(href = 'http://nickstrayer.me/', "Nick Strayer.")
      ), align = 'center')))
    )
    )
  ))

## back-end core
server <- function(input, output, session) {
  # Trigger updating of the checkbox groups whenever the select all or unselect
  # all buttons are pressed
  
  ## Current Immunity Level Plot with caching 
  output$plot1 <- renderPlotly({
    immuStatePlot(input$state, input$vaccinatedType, as.numeric(input$ascertainment_bias))
  }) %>% bindCache(input$state, input$vaccinatedType, input$ascertainment_bias)
  
  ## Daily Immunity Level Plot
  output$plot2 <- renderPlotly({
    dailyPlot(input$state, input$vaccinatedType, as.numeric(input$ascertainment_bias))
  }) %>% bindCache(input$state, input$vaccinatedType, input$ascertainment_bias)
  
  ## Map
  # state map
  output$map1 <- renderLeaflet({
    MakeMap(input$vaccinatedType, input$ascertainment_bias)
  }) %>% bindCache(input$vaccinatedType, input$ascertainment_bias)
  # county map
  output$map2 <- renderLeaflet({
    MakeCountyMap(input$vaccinatedType, input$ascertainment_bias)
  }) %>% bindCache(input$vaccinatedType, input$ascertainment_bias) 
  # ## Triangular bar
  output$taxis1 <- renderPlot(estFatalAxis()) %>% bindCache()
  output$taxis2 <- renderPlot(estFatalAxis()) %>% bindCache()
  
  ## download buttons
  output$downloadAll <-  downloadHandler(
    filename = function() {
      paste("AllData-", DATA$Date[1], ".csv", sep="")
    },
    content = function(file) {
      write.csv(DATA, file, row.names = F)
    }
  )
  output$downloadPartial <-  downloadHandler(
    filename = function() {
      paste("SelectedData-", DATA$Date[1], ".csv", sep="")
    },
    content = function(file) {
      write.csv(DATA %>% filter(State == input$state, ABias == input$ascertainment_bias,
                                VaccType == input$vaccinatedType), file, row.names = F)
    }
  ) %>% bindEvent(input$state, input$vaccinatedType, input$ascertainment_bias)
  
}
shinyApp(ui,server)
