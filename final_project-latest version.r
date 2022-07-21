library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(ggthemes)

cardata=read.csv("ElectricCarData_Clean.csv")
oil = read.csv("oilprice.csv")
oil$date = as.Date(oil$date, format = "%m/%d/%Y")



ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Why Electric Cars"),
  dashboardSidebar(sidebarMenu(
    menuItem("Intro", tabName = "page0", icon = icon("info")),
    menuItem("Oil-Price",tabName = "page1",icon = icon("line-chart")),
    menuItem("Market Trend", tabName = "page2", icon = icon("area-chart")),
    menuItem("Distribution Map", tabName = "page3", icon = icon("map-o")),
    menuItem("Vehicle Information", tabName = "page4", icon = icon("car")),
    menuItem("EV_Range", tabName = "page5", icon = icon("car-battery"))
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "page0", fluidPage(title = NULL,
                                         fluidRow(box(title = "Electric Car: The Future of Transportation", status = "primary", 
                                                      solidHeader = TRUE,  width = 14,
                                                      tags$blockquote(tags$h5("Due to the Russia-Ukraine conflict, the oil price is skyrocketing. In 2022, the oil price is already at the highest of the recent decade. Due to current circumstances and the need for oil-alternative fuel, the demand for electric vehicles is at an all-time high. In our project, we made a case for the need for electric vehicles by showcasing the recent trend in oil prices. Furthermore, we will provide assistance for people considering buying electric cars by giving the current market trend of EV makers, the distribution of charging stations in the U.S, and details of each model of the electric car.")),
                                                      tags$b("Following insights will be provided by this webside:"),
                                                      br(),
                                                      tags$li("Crude oil price from 1983 to present"),
                                                      tags$li("What is the trend of market share in the EV sector? "),
                                                      tags$li("How are charging stations distributed across the U.S?"),
                                                      tags$li("The detailed information of each model of EV"),
                                                      tags$li("What's the market price of EV?")
                                                      
                                         )),
                                         
                                         fluidRow(box(title = "About the Reference", status = "info",solidHeader = TRUE,
                                                      width = 12, tags$h5("Thanks for the dataset from Kaggle & YAA:"),
                                                      tags$li(tags$a(href="https://www.kaggle.com/datasets/sc231997/crude-oil-price","Crude Oil Price")),
                                                      tags$li(tags$a(href="https://joinyaa.com/guides/electric-vehicle-market-share-and-sales/#:~:text=2021%20Electric%20Vehicle%20Market%20Share%20and%20Sales,-As%202021%20came&text=By%20year's%20end%2C%20plug%2Din,%2C%20that%20figure%20reached%204.5%25.","Electric Vehicle Sales and Market Share")),
                                                      tags$li(tags$a(href="https://www.kaggle.com/datasets/prasertk/electric-vehicle-charging-stations-in-usa","Electric Vehicle Charging Stations")),
                                                      tags$li(tags$a(href="https://www.kaggle.com/datasets/geoffnel/evs-one-electric-vehicle-dataset","EVs - One Electric Vehicle Dataset - Smaller"))
                                         )))),
    tabItem(
      tabName = "page1",
      
      titlePanel("Crude_Oil_Price"),
      sliderInput(
        "year",
        "Year:",
        min = 1983,
        max = 2022,
        value = c(1983, 2022),
        sep = ""
      ),
      plotlyOutput("plot1"),verbatimTextOutput("range")
    ),
    tabItem(
      tabName = "page2",
      h2("Market Distribution of EV Makers"),
      plotOutput("plot2")
    ),
    tabItem(tabName = "page3",
            h2("Electric Vehicle Charging Station Distribution"),
            leafletOutput("myMap", width = "100%")),
    tabItem(
      tabName = "page4",
      
      
      
      # Application title
      titlePanel("Electric Vehicles Information"),
      
      fluidRow(
        column(2,
               selectInput("Brand",
                           "Brand:",
                           c(
                             "All",
                             unique(as.character(cardata$Brand))
                           ))),
        column(2,
               selectInput("Model",
                           "Model:",
                           c(
                             "All",
                             unique(as.character(cardata$Model))
                           ))),
        column(2,
               selectInput(
                 "BodyStyle",
                 "BodyStyle:",
                 c("All",
                   unique(as.character(cardata$BodyStyle)))
               )),
        column(2,
               selectInput("Segment",
                           "Segment:",
                           c(
                             "All",
                             unique(as.character(cardata$Segment))
                           ))),
        column(2,
               selectInput("Seats",
                           "Seats:",
                           c(
                             "All",
                             unique(as.character(cardata$Seats))
                           )))
      ),
      # Create a new row for the table.
      DT::dataTableOutput("table")
      
      
      
    ),
    tabItem(tabName = "page5",
            h2("EV Travel Range"),
            plotOutput("EV_Range",height = 700))
  ))
)


server <- function(input, output, session) {
  us_data = read_csv("ev_stations.csv")
  oil = read.csv("oilprice.csv")
  oil$date = as.Date(oil$date, format = "%m/%d/%Y")
  marketdata <- read_csv("evmarketshare.csv")
  marketdata <- pivot_longer(marketdata,
                             c("2021 q1","2021 q2","2021 q3","2021 q4","2022 q1","2022 q2"),
                             names_to = "time", values_to = "sales")
  marketdata$sales <- as.numeric(marketdata$sales)
  
  output$plot1 = renderPlotly({
    minyear = input$year[1]
    maxyear = input$year[2]
    
    Q = oil %>%
      filter(as.numeric(format(oil$date, '%Y')) >= minyear &
               as.numeric(format(oil$date, '%Y')) <= maxyear) %>%
      ggplot(aes(x = date, y = price)) + geom_point() + geom_line() + geom_smooth()+
      ylim(0, 150) +
      labs(subtitle = "",x = "Time",y = "Price in ($)")+theme_clean()
    
    return(Q)
    
  })
    
  output$range = renderPrint({
    minyear = input$year[1]
    maxyear = input$year[2]
    
    QQ = oil %>%
      filter(as.numeric(format(oil$date, '%Y')) >= minyear &
               as.numeric(format(oil$date, '%Y')) <= maxyear)
    summary(QQ)
  })
  
  output$plot2 = renderPlot({
    ggplot(marketdata, aes(x=time, y=sales, fill=automaker)) + 
      geom_col(position = "stack", color="black")+ 
      ggtitle("market trend")
    
  })
  
  output$myMap = renderLeaflet({
    view_data = us_data %>%
      group_by(lng = round(Longitude, 3), lat = round(Latitude, 3)) %>%
      summarise(N = n()) %>%
      mutate(
        latL = lat - 0.05,
        latH = lat + 0.05,
        lngL = lng - 0.05,
        lngH = lng + 0.05
      )
    
    m = view_data %>% leaflet() %>% addTiles() %>%
      
      setView(-98.58, 38.01, zoom = 4) %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addLayersControl(baseGroups = c("Toner", "OSM"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addRectangles(
        lng1 =  ~ lngL,
        lat1 =  ~ latL,
        lng2 =  ~ lngH,
        lat2 =  ~ latH,
        fillOpacity = ~ N / 10,
        opacity = 0,
        fillColor = "red",
        label = ~ N
      )
    
  })
  
  observe({
    data = cardata
    if (input$Brand != "All") {
    data = cardata %>%
      filter(Brand == input$Brand)
    }
    
    Model_choices = data %>%
      distinct(Model) %>%
      pull(Model)
    
    Model_choices = c("All", Model_choices)
    
    updateSelectInput(session = session, inputId = "Model",choices = Model_choices)
  })
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- cardata
    if (input$Brand != "All") {
      data <- data[data$Brand == input$Brand,]
    }
    if (input$Model != "All") {
      data <- data[data$Model == input$Model,]
    }
    if (input$BodyStyle != "All") {
      data <- data[data$BodyStyle == input$BodyStyle,]
    }
    if (input$Segment != "All") {
      data <- data[data$Segment == input$Segment,]
    }
    if (input$Seats != "All") {
      data <- data[data$Seats == input$Seats,]
    }
    data
  }))
  
  output$EV_Range = renderPlot({
  range1  = ggplot(data = range, aes(y=reorder(range$Model, Range_Km), x=range$Range_Km, color = Brand, fill = Brand)) + 
      geom_bar(stat = "identity",width = 0.5) + labs(title = "", x = "Model", y = "Range in (KM)")
  
  range1
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)
