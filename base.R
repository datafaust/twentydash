#twentytwenty dash---------------------------------------------------------------------------------------------------------------------

#authors:

#legacy code remains for extra features in future builds

#install and load require libraries
if(!require("shinydashboard")) {
  install.packages("shinydashboard", dependencies = T)
}
if(!require("shiny")) {
  install.packages("shiny", dependencies = T)
}
if(!require("shinydashboard")) {
  install.packages("shinydashboard", dependencies = T)
}
if(!require("circlize")) {
  install.packages("circlize", dependencies = T)
}
if(!require("ggplot2")) {
  install.packages("ggplot2", dependencies=T)
}
if(!require("plotly")) {
  install.packages("plotly", dependencies = T)
}
if(!require("scales")) {
  install.packages("scales", dependencies = T)
}
if(!require("DT")) {
  install.packages("DT", dependencies = T)
}
if(!require("stringr")) {
  install.packages("stringr", dependencies = T)
}
if(!require("Hmisc")) {
  install.packages("Hmisc", dependencies = T)
}
if(!require("zoo")) {
  install.packages("zoo", dependencies = T)
}
if(!require("RODBC")) {
  install.packages("RODBC", dependencies = T)
}
if(!require("RPostgreSQL")) {
  install.packages("RPostgreSQL", dependencies = T)
}
if(!require("rgdal")) {
  install.packages("rgdal", dependencies = T)
}
if(!require("plyr")) {
  install.packages("plyr", dependencies = T)
}
if(!require("data.table")) {
  install.packages("data.table", dependencies = T)
}



#source data-----------------------------------------------------------------------------------------------------------------------------
#zones = readOGR("taxi_zones_sp.shp", layer = "taxi_zones_sp")

yellow = fread("http://www.nyc.gov/html/tlc/downloads/csv/data_reports_monthly_indicators_yellow.csv")
green = fread("http://www.nyc.gov/html/tlc/downloads/csv/data_reports_monthly_indicators_shl.csv")

#filter and clean bind
yellow = yellow[,1:12, with = F]
green = green[,1:12, with = F]

green$type = "shl"
yellow$type = "medallion"

names(yellow) = names(green) 
master_indicators = rbind(yellow, green)
master_indicators = as.data.frame(master_indicators)

titlez = c("month_year",    "trips_per_day" ,  "farebox_per_day"  ,             
           "unique_drivers",   "unique_vehicles","vehicles_per_day",  "avg_days_vehicles_on_road",       
           "avg_hours_per_day_per_vehicle"  ,  "days_per_driver" ,    "avg_hours_per_day_per_driver" ,"avg_minutes_per_trip",            
           "perc_trips_paid_with_credit_card", "type"  )

names(master_indicators) = titlez

#trim punctuation and transform
master_indicators$trips_per_day = as.numeric(gsub('[[:punct:]]', '', master_indicators$trips_per_day))
master_indicators$farebox_per_day = as.numeric(gsub('[[:punct:]]', '', master_indicators$farebox_per_day))
master_indicators$unique_drivers = as.numeric(gsub('[[:punct:]]', '', master_indicators$unique_drivers))
master_indicators$unique_vehicles = as.numeric(gsub('[[:punct:]]', '', master_indicators$unique_vehicles))
master_indicators$vehicles_per_day = as.numeric(gsub('[[:punct:]]', '', master_indicators$vehicles_per_day))
master_indicators$perc_trips_paid_with_credit_card = gsub('[[:punct:]]', '', master_indicators$perc_trips_paid_with_credit_card)
master_indicators$perc_trips_paid_with_credit_card = as.numeric(paste0(".",master_indicators$perc_trips_paid_with_credit_card))

month_year1= as.yearmon(master_indicators$month_year)
master_indicators$days= monthDays(as.Date(month_year1))
master_indicators$trips_per_month = master_indicators$trips_per_day*master_indicators$days
master_indicators$month_date = as.Date(paste(master_indicators$month_year,"-28",sep=""))
master_indicators$farebox_per_month = master_indicators$farebox_per_day * master_indicators$days
master_indicators$farebox_per_month = master_indicators$farebox_per_month
master_indicators$week = strftime(master_indicators$month_date, format="%W")
master_indicators$week = as.factor(master_indicators$week)
master_indicators$trips_per_week = master_indicators$trips_per_day * 7
master_indicators$year = format(as.yearmon(master_indicators$month_year), "%Y")
master_indicators$farebox_per_week = master_indicators$farebox_per_day * 7
master_indicators$type = as.factor(master_indicators$type)

#copy for compatibility with old code
supra_monthly = master_indicators
supra_monthly$days = NULL



#ui.R-----------------------------------------------------------------------------------------------------------------------------------------
#header
ui = dashboardPage(skin = "yellow",
                   dashboardHeader(title = "Twentytwenty"
                                   
                                   # , dropdownMenu(type = "notifications",
                                   #                                       messageItem(
                                   #                                         from = "",
                                   #                                         message = "")),
                                   # #this message item is a place holder               
                                   # dropdownMenu(type = "messages",
                                   #              messageItem(
                                   #                from = "Sales Dept",
                                   #                message = "Sales are steady this month.")),
                                   # #this notifications item will inform the user of when data updates are complete
                                   # dropdownMenu(type = "notifications",
                                   #              notificationItem(text = "Data Update n%",
                                   #                               icon("fa fa-hourglass-start"),
                                   #                               status = "success"))
                   ),
                   
                   #this element describes the sidebar which links to other pages the user can click
                   #for the time includes: (widgets, charts, projections, and maps)
                   dashboardSidebar(
                     sidebarMenu(
                       menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                       #menuItem("Major Indicators", tabName = "major_indicators", icon = icon("fa fa-bar-chart"),
                       #        menuSubItem("FHV Indicators", tabName = "fhv_indicators", href = NULL, newtab = TRUE,
                       #                   icon = shiny::icon("angle-double-right"), selected = NULL)
                       #menuSubItem("SHL Indicators", tabName = "shl_indicators", href = NULL, newtab = TRUE,
                       #           icon = shiny::icon("angle-double-right"), selected = NULL),
                       #menuSubItem("Driver Education", tabName = "driver_education", href = NULL, newtab = TRUE,
                       #            icon = shiny::icon("angle-double-right"), selected = NULL)
                       # menuSubItem("EHAIL Metrics", tabName = "ehail", href = NULL, newtab = TRUE,
                       #             icon = shiny::icon("angle-double-right"), selected = NULL),
                       # menuSubItem("Borough Statistics", tabName = "borough_stats", href = NULL, newtab = TRUE,
                       #             icon = shiny::icon("angle-double-right"), selected = NULL),
                       # menuSubItem("Industry Insights Tool", tabName = "insighttool", href = NULL, newtab = TRUE,
                       #             icon = shiny::icon("angle-double-right"), selected = NULL)
                       #),
                       #menuItem("Query Wizard", tabName = "wizard", icon =icon("fa fa-magic")),
                       # menuItem("Trip Volume Analysis", tabName = "projections", icon = icon("fa fa-line-chart"),
                       #          menuSubItem("Trip Volume at a glance", tabName = "trip_volume", href = NULL, newtab = TRUE,
                       #                      icon = shiny::icon("angle-double-right"), selected = NULL),
                       #          menuSubItem("Trip Volume Trends", tabName = "trip_analyzer", href = NULL, newtab = TRUE,
                       #                      icon = shiny::icon("angle-double-right"), selected = NULL)),
                       # menuItem("Maps", tabName = "maps", icon = icon("fa fa-map"),
                       #          menuSubItem("Service", tabName = "service", href = NULL, newtab = TRUE,
                       #                      icon = shiny::icon("angle-double-right"), selected = NULL),
                       #          menuSubItem("Breadcrumb Research", tabName = "bc", href = NULL, newtab = TRUE,
                       #                      icon = shiny::icon("angle-double-right"), selected = NULL)
                       # ),
                       #the source code item below will share the dashboards source code for later use
                       menuItem("Data Bank", tabName = "databank", icon =icon("fa fa-university"))
                       # menuItem("Source code", icon = icon("file-code-o"), 
                       #          href = "https://github.com/rstudio/shinydashboard/"),
                       #   sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                       #                     label = "Search...")
                     )
                   ),
                   ## Body content
                   dashboardBody(
                     tabItems(
                       #First tab content
                       #this will hold the primary layout of the dashboard
                       #s
                       tabItem(tabName = "dashboard",
                               
                               
                               fluidRow(
                                 # Dynamic infoBoxes
                                 valueBoxOutput("shltripbox", width = 6),
                                 valueBoxOutput("progressBox", width = 6)#,
                                 #valueBoxOutput("ubers_etc")
                                 ,
                                 box(textOutput("textbox2")),
                                 box(textOutput("textbox3"))
                               ),
                               fluidRow(
                                 box(background = "green", dateRangeInput("monthdate", label = h3("Choose a Date Range"),
                                                                          start = '2014-01-01',
                                                                          end = as.Date(Sys.time()))),
                                 box(background="green", selectInput(inputId = "dimension", label = strong("Choose Metric"),
                                                                     choices = c('Trips, Drivers & Vehicles'='1', 'Time & Money' = '2'), 
                                                                     multiple = FALSE, selectize = TRUE)),
                                 box(textOutput("textbox"))
                               ),
                               
                               fluidRow(
                                 
                                 box(plotlyOutput(outputId = 'trips_per_day'), width = 6),
                                 box(plotlyOutput(outputId = 'trips_year'), width = 6)),
                               
                               
                               fluidRow(
                                 
                                 box(plotlyOutput(outputId = 'trips_per_month')),  
                                 
                                 box(plotlyOutput(outputId = 'medallions_per_month'))
                               ),
                               #variable switch box
                               fluidRow(
                                 sidebarLayout(
                                   sidebarPanel(
                                     dateRangeInput("monthlydate", label = h3("Date Range"),start = '2016-01-01',
                                                    end = as.Date(Sys.time())),
                                     selectInput("element_id1_m", "Select Your Variable for x-axis", c("month_date", 
                                                                                                       "week",
                                                                                                       "year"), selected = "month_date"),
                                     selectInput("element_id2_m", "Select Your Variable for y-axis", c("trips_per_day",
                                                                                                       "farebox_per_day",
                                                                                                       "unique_drivers",
                                                                                                       "unique_vehicles",
                                                                                                       "avg_minutes_per_trip",
                                                                                                       "avg_days_vehicles_on_road", 
                                                                                                       "avg_hours_per_day_per_vehicle",
                                                                                                       "days_per_driver",
                                                                                                       "avg_hours_per_day_per_driver", 
                                                                                                       "perc_trips_paid_with_credit_card"), selected = "trips"),
                                     selectInput("element_id3_m", "Select Your Grouping Variable", c("type"), selected = 'type')),
                                   mainPanel(h3("Outputs"),
                                             textOutput("id1_m"),
                                             textOutput("id2_m"),
                                             textOutput("id3_m"),
                                             plotlyOutput("plt_m"),
                                             plotlyOutput("plt2_m"))
                                   
                                   
                                 ))
                               
                               
                               
                       ),
                       
                       #widgets
                       tabItem(tabName = "widgets",
                               h2("Widgets tab content")),
                       
                       # FHV indicators
                       tabItem(tabName = "fhv_indicators",
                               h2("FHV Indicators",
                                  fluidRow(
                                    box(plotlyOutput(outputId = 'fhvtrips_per_day'), width = 6),
                                    box(plotlyOutput(outputId = 'fhvunique_drivers'), width = 6)),
                                  fluidRow(
                                    sidebarLayout(
                                      sidebarPanel(
                                        dateRangeInput("fhv_date", label = h3("Date Range"),start = as.Date(Sys.time()) - 180,
                                                       end = as.Date(Sys.time())),
                                        selectInput("element_id1_fhv", "Select Your Variable for x-axis", c("month_date",
                                                                                                            "week",
                                                                                                            "year",
                                                                                                            "type"), selected = "month_date"),
                                        selectInput("element_id2_fhv", "Select Your Variable for y-axis", c("trips_per_day", 
                                                                                                            "farebox_per_day",
                                                                                                            "unique_drivers",
                                                                                                            "unique_vehicles", 
                                                                                                            "vehicles_per_day",
                                                                                                            "avg_days_vehicles_on_road", 
                                                                                                            "avg_hours_per_day_per_vehicle",
                                                                                                            "days_per_driver",
                                                                                                            "avg_hours_per_day_per_driver"), selected = "unique_vehicles"),
                                        selectInput("element_id3_fhv", "Select Your Grouping Variable", c("type"), selected = 'type')),
                                      mainPanel(h3("Outputs"),
                                                textOutput("id1_fhv"),
                                                textOutput("id2_fhv"),
                                                textOutput("id3_fhv"),
                                                plotlyOutput("plt_fhv"),
                                                plotlyOutput("plt2_fhv"))
                                      
                                      
                                    ))
                               )),
                       
                       # driver_education        
                       tabItem(tabName = "driver_education",
                               h2("Driver Education Metrics",
                                  fluidRow(
                                    # Dynamic infoBoxes
                                    valueBoxOutput("fhvtesters"),
                                    valueBoxOutput("medalliontesters"),
                                    valueBoxOutput("alltesters")),
                                  fluidRow(
                                    box(background= "green", dateRangeInput("educationdate", label = h3("Date Range"),start = as.Date(Sys.time()) - 120,
                                                                            end = as.Date(Sys.time())), width = 6)),
                                  fluidRow(
                                    box(plotOutput(outputId = 'compliancer', height = 600, width = 1000), width = 8),
                                    box(plotlyOutput(outputId = 'timetocompliance', height = 600, width = 1000), width = 8),
                                    box(plotlyOutput(outputId = 'initialpr', height = 700, width = 1000), width = 8))
                               )),
                       tabItem(tabName = "ehail",
                               h2("EHAIL Metrics",
                                  fluidRow(
                                    box(title = "NUMBER OF REQUESTS", background = "green", solidHeader = TRUE,
                                        plotlyOutput(outputId= 'ehail1')),
                                    box(title = "TRIPS COMPLETED", background = "green", solidHeader = TRUE,
                                        plotOutput(outputId= 'ehail2'))),
                                  fluidRow( 
                                    box(background= "green", dateRangeInput("ehaildates", label = h3("EHAIL Date Range"),start = '2016-06-01',
                                                                            end = as.Date(Sys.time())), width = 12))
                                  ,fluidRow(box(dataTableOutput('ehaildatatable'), downloadButton('downloadDataehail', 'Download'), width = 12))
                               )),
                       tabItem(tabName = "borough_stats",
                               h2("Borough Statistics",
                                  fluidRow(
                                    box(title = "NUMBER OF REQUESTS", background = "green", solidHeader = TRUE,
                                        plotlyOutput(outputId= 'borough1')),
                                    box(title = "TRIPS COMPLETED", background = "green", solidHeader = TRUE,
                                        plotOutput(outputId= 'borough2'))),
                                  fluidRow( 
                                    box(background= "green", dateRangeInput("boroughdates", label = h3("BOROUGH Date Range"),start = '2016-06-01',
                                                                            end = as.Date(Sys.time())), width = 12))
                                  ,fluidRow(box(dataTableOutput('boroughdatatable'), downloadButton('downloadDataborough', 'Download'), width = 12))
                               )),
                       tabItem(tabName = "bc",
                               h2("EHAIL Metrics",
                                  fluidRow(
                                    box(title = "NUMBER OF REQUESTS", background = "green", solidHeader = TRUE,
                                        plotOutput(outputId= 'bcplot'))
                                  ))),
                       #insights tool
                       tabItem(tabName = "insighttool",
                               h2("Insights Tool",
                                  fluidRow(
                                    sidebarLayout(
                                      sidebarPanel(
                                        dateRangeInput("insightdate", label = h3("Date Range"),start = '2016-01-01',
                                                       end = '2016-06-01'),
                                        selectInput("element_id1", "Select Your Variable for x-axis", c("date", 
                                                                                                        "weekday",
                                                                                                        "month"), selected = "date"),
                                        selectInput("element_id2", "Select Your Variable for y-axis", c("trips",
                                                                                                        "farebox",
                                                                                                        "uniq_drivers",
                                                                                                        "uniq_medallions",
                                                                                                        "total_trip_hours",
                                                                                                        "trip_miles",
                                                                                                        "cc_trips",
                                                                                                        "jfk_trips",
                                                                                                        "lga_trips",
                                                                                                        "cc_fares",
                                                                                                        "type",
                                                                                                        "hours_per_trip",
                                                                                                        "miles_per_trip",
                                                                                                        "velocity_per_trip"), selected = "trips"),
                                        selectInput("element_id3", "Select Your Grouping Variable", c("type"), selected = 'type')),
                                      mainPanel(h3("Outputs"),
                                                textOutput("id1"),
                                                textOutput("id2"),
                                                textOutput("id3"),
                                                plotlyOutput("plt2"))
                                      
                                      
                                    )),
                                  fluidRow(
                                    # main plot
                                    box(plotlyOutput(outputId = 'insightplot3'), width = 6),
                                    box(plotlyOutput(outputId = 'insightplot4'), width = 6)),
                                  fluidRow(
                                    # main plot
                                    box(plotlyOutput(outputId = 'insightplot5'), width = 6),
                                    box(plotlyOutput(outputId = 'insightplot6'), width = 6))
                               )),
                       
                       
                       # WIZARD TAB
                       tabItem(tabName = "wizard",
                               h2("Query Wizard",
                                  fluidPage(
                                    
                                    titlePanel("TPEP Query (BETA: 2015 AND UP ONLY)"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        
                                        numericInput("hack", label = h3("Enter Hack Number"), value = NULL),
                                        
                                        numericInput("medallion", label = h3("Enter Medallion"), value = NULL),
                                        
                                        dateInput("wizarddate", label = h3("Enter date as far as you want records"), value = NULL),
                                        
                                        selectInput("puboroughs", "filter by pickup borough location", c('All' = 'all',
                                                                                                         'Bronx' = 'bronx',
                                                                                                         'Queens' = 'queens',
                                                                                                         'Manhattan' = 'manhattan',
                                                                                                         'Brooklyn' = 'brooklyn',
                                                                                                         'staten island' = 'staten island'), selected = 'all'),
                                        
                                        selectInput("doboroughs", "filter by dropoff borough location", c('All' = 'all',
                                                                                                          'Bronx' = 'bronx',
                                                                                                          'Queens' = 'queens',
                                                                                                          'Manhattan' = 'manhattan',
                                                                                                          'Brooklyn' = 'brooklyn',
                                                                                                          'staten island' = 'staten island'), selected = 'all'),
                                        checkboxGroupInput("variable", "Variables to show:",
                                                           c("tpep_pickup_datetime" = "tpep_pickup_datetime",
                                                             "medallion" = "medallion",
                                                             "Hack_number" = "Hack_number",
                                                             "tpep_dropoff_datetime" = "tpep_dropoff_datetime",
                                                             "VendorID" = "VendorID",
                                                             "RateCodeID" = "RateCodeID",
                                                             "pickup_longitude" = "pickup_longitude",
                                                             "pickup_latitude" = "pickup_latitude",
                                                             "dropoff_longitude" = "dropoff_longitude",
                                                             "dropoff_latitude" = "dropoff_latitude",
                                                             "trip_time_in_secs" = "trip_time_in_secs",
                                                             "trip_distance" = "trip_distance",
                                                             "fare_amount" = "fare_amount",
                                                             "extra" = "extra",
                                                             "mta_tax" = "mta_tax",
                                                             "tip_amount" = "tip_amount",  
                                                             "total_amount" = "total_amount",
                                                             "payment_type" = "payment_type"), selected = c("tpep_pickup_datetime" = "tpep_pickup_datetime",
                                                                                                            "medallion" = "medallion",
                                                                                                            "Hack_number" = "Hack_number",
                                                                                                            "tpep_dropoff_datetime" = "tpep_dropoff_datetime",
                                                                                                            "VendorID" = "VendorID",
                                                                                                            "RateCodeID" = "RateCodeID",
                                                                                                            "pickup_longitude" = "pickup_longitude",
                                                                                                            "pickup_latitude" = "pickup_latitude",
                                                                                                            "dropoff_longitude" = "dropoff_longitude",
                                                                                                            "dropoff_latitude" = "dropoff_latitude",
                                                                                                            "trip_time_in_secs" = "trip_time_in_secs",
                                                                                                            "trip_distance" = "trip_distance",
                                                                                                            "fare_amount" = "fare_amount",
                                                                                                            "extra" = "extra",
                                                                                                            "mta_tax" = "mta_tax",
                                                                                                            "tip_amount" = "tip_amount",  
                                                                                                            "total_amount" = "total_amount",
                                                                                                            "payment_type" = "payment_type")),
                                        
                                        
                                        actionButton("clicked", "submit query"),
                                        
                                        downloadButton('downloadDatawizard', 'Download trip records')
                                        
                                      ),
                                      
                                      mainPanel(
                                        h3("outputs"),
                                        tabPanel('Monthly pikcups and dropoffs', DT::dataTableOutput('mytableuno'))
                                      )
                                    ))
                               )),
                       
                       
                       # Fourth Tab content
                       tabItem(tabName = "trip_analyzer",
                               h2("Trip Volume Analyzer",
                                  fluidRow(
                                    box(title = "Route Volume for Taxi Vehicles", background = "green", solidHeader = TRUE,
                                        plotOutput(outputId= 'plot2', hover = "plot_hover", height = 850, width = 850), width = 6),
                                    fluidRow( 
                                      box(background= "green", dateRangeInput("dates", label = h3("Date Range"),start = '2016-06-01',
                                                                              end = '2016-06-05'),
                                          radioButtons("type", label = h3("Taxi Type"), choices = list("Yellowcabs" = 1, "Greencabs" = 2,
                                                                                                       "All" = 3)),width = 4),
                                      
                                      box(
                                        title = "Controls",
                                        sliderInput("slider", "Number of Routes:", 1, 50, 5), width = 4),
                                      box(background="yellow", selectInput(inputId = "list", label = strong("Choose Route Type"),
                                                                           choices = c('Boroughs'='1','Taxi Zones'='2',
                                                                                       'Service Zones' = '3'), 
                                                                           
                                                                           multiple = FALSE, selectize = TRUE, selected = 2), width = 4
                                          
                                      ),
                                      box(dataTableOutput('datatable'), downloadButton('downloadData', 'Download'))))
                               )),
                       
                       
                       #mapping pickups and dropoffs
                       tabItem(tabName = "service",
                               h2("maps tab content",
                                  fluidRow(
                                    box(background= "maroon", dateInput("mapdate", label = h3("Date Input"), startview = "month", 
                                                                        format = "yyyy-mm", value = "2015-01"), width = 6)),
                                  fluidRow(
                                    box(plotlyOutput(outputId = 'pudos1')),
                                    box(plotlyOutput(outputId = 'pudos2'))))),
                       #databank
                       tabItem(tabName = "databank", 
                               (fluidPage(
                                 title = 'Table Access',
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     conditionalPanel(
                                       'input.dataset === "supra_monthly"',
                                       checkboxGroupInput('show_vars', 'Columns in Data Set to show:',
                                                          names(supra_monthly), selected = names(supra_monthly))
                                     ),
                                     # conditionalPanel(
                                     #   'input.dataset === "education"',
                                     #   helpText('Display 5 records by default.')
                                     # ),
                                     # conditionalPanel(
                                     #   'input.dataset === "daily_master_indicators"',
                                     #   helpText('Display 5 records by default.')
                                     # ),
                                     downloadButton('downloadData1', 'Download Data Set')
                                   ),
                                   mainPanel(
                                     tabsetPanel(
                                       id = 'dataset',
                                       tabPanel('supra_monthly', DT::dataTableOutput('mytable1'))#,
                                       #tabPanel('Daily Indicators', DT::dataTableOutput('mytable1.5')),
                                       #tabPanel('education', DT::dataTableOutput('mytable2'))
                                     )
                                   )
                                 )
                               )
                               )))
                   )
)



#server.R  -------------------------------------------------------------------------------------------------
#legagcy backend still in place

server = function(input, output) {
  #graph trips per day?
  output$trips_per_day = renderPlotly({
    start_date1 <- input$monthdate[1]
    end_date1 <- input$monthdate[2]
    # td = subset(master_indicators, (month_date >= start_date1 & month_date <= end_date1), 
    #             c('trips_per_day', 'month_date', 'type'))
    # 
    td = subset(supra_monthly, 
                (type == 'medallion' | type == 'shl') & 
                  (month_date >= start_date1 & month_date <= end_date1), c('trips_per_day', 'month_date', 'type'))
    print(td)
    # trips = ggplot(td, aes(x = month_date, y = trips_per_day, fill = type))+
    #   geom_bar(stat = "identity")+
    #   labs(x = "Month & Year", y = "Avg No. Trips per Driver each day", 
    #        title = "Average Trips per day Over Time")+
    #   scale_y_continuous(name="trips", labels = comma)+
    #   scale_fill_manual(values=c("yellow", "#66FF66"))
    # trips = trips + theme(panel.background = element_rect(fill = 'black'),
    #                       panel.grid.major = element_blank(),
    #                       panel.grid.minor = element_blank(),
    #                       axis.title.x = element_text(size = 13, colour = 'black'),
    #                       axis.title.y = element_text(size = 13, colour = 'black'),
    #                       axis.title = element_text(size = 18, colour = 'black'),
    #                       axis.text.x  = element_text(vjust=.5, size=13, angle = 90))
    # ggplotly(trips)
    trips = plot_ly(td, x = ~month_date, y = ~trips_per_day, type = 'bar', split = ~type)
    trips <- layout(trips,              # all of layout's properties: /r/reference/#layout
                    title = "Average Trips per Day each Month", # layout's title: /r/reference/#layout-title
                    xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                      title = "Month & Year",     # xaxis's title: /r/reference/#layout-xaxis-title
                      showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                    ),
                    yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                      title = "Trips Per Day"      # yaxis's title: /r/reference/#layout-yaxis-title
                    ))
    
    if (input$dimension == '2') {
      start_date1 <- input$monthdate[1]
      end_date1 <- input$monthdate[2]
      # td = subset(master_indicators, (month_date >= start_date1 & month_date <= end_date1), 
      #             c('farebox_per_day', 'month_date', 'type'))
      td = subset(supra_monthly, 
                  (type == 'medallion' | type == 'shl') & 
                    (month_date >= start_date1 & month_date <= end_date1), 
                  c('farebox_per_day', 'month_date', 'type'))
      
      # trips = ggplot(td, aes(x = month_date, y = farebox_per_day, fill = type))+
      #   geom_bar(stat = "identity")+
      #   labs(x = "Month & Year", y = "Avg No. Trips per Driver each day", 
      #        title = "Farbeox per day Over Time")+
      #   scale_y_continuous(name="trips", labels = comma)+
      #   scale_fill_manual(values=c("yellow", "#66FF66"))
      # trips = trips + theme(panel.background = element_rect(fill = 'black'),
      #                       panel.grid.major = element_blank(),
      #                       panel.grid.minor = element_blank(),
      #                       axis.title.x = element_text(size = 13, colour = 'black'),
      #                       axis.title.y = element_text(size = 13, colour = 'black'),
      #                       axis.title = element_text(size = 18, colour = 'black'),
      #                       axis.text.x  = element_text(vjust=.5, size=13, angle = 90))
      trips = plot_ly(td, x = ~month_date, y = ~farebox_per_day, type = 'bar', split = ~type) 
      trips <- layout(trips,              # all of layout's properties: /r/reference/#layout
                      title = "Average Farebox Per Day each Month", # layout's title: /r/reference/#layout-title
                      xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                        title = "Month & Year",     # xaxis's title: /r/reference/#layout-xaxis-title
                        showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                      ),
                      yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                        title = "Farebox Per Day"      # yaxis's title: /r/reference/#layout-yaxis-title
                      ))
    }
    trips
  })
  #graph Yearly trip volume
  output$trips_year = renderPlotly({
    ###########################
    #start process
    start_date1 <- input$monthdate[1]
    end_date1 <- input$monthdate[2]
    # td = subset(master_indicators, (month_date >= start_date1 & month_date <= end_date1), 
    #             c('trips_per_month','month_date','type', 'year','trips_per_week'))
    
    td = subset(supra_monthly, 
                (type == 'medallion' | type == 'shl') & 
                  (month_date >= start_date1 & month_date <= end_date1), 
                c('trips_per_month','trips_per_day','month_date','type', 'year','trips_per_week'))
    
    
    nd = aggregate(trips_per_day ~ year + type, data = td, FUN = sum)
    # uniks = ggplot(data=nd,
    #                aes(x=year, y= trips_per_day, fill=type)) +
    #   geom_bar(stat = 'identity')+
    #   labs(x = "Year", y = "Yearly Trip Totals", 
    #        title = "Yearly Trip Volume")+
    #   scale_fill_manual(values=c("yellow", "#66FF66"))+
    #   scale_y_continuous(name="trips", labels = comma)+
    #   theme(panel.background = element_rect(fill = 'black'),
    #         panel.grid.major = element_blank(),
    #         panel.grid.minor = element_blank(),
    #         axis.title.x = element_text(size = 13, colour = 'black'),
    #         axis.title.y = element_text(size = 13, colour = 'black'),
    #         axis.title = element_text(size = 18, colour = 'black'),
    #         axis.text.x  = element_text(vjust=.5, size=13, angle = 90))
    # ggplotly(uniks)
    uniks = plot_ly(nd, x = ~year, y = ~trips_per_day, split = ~type, type = 'bar')
    uniks <- layout(uniks,              # all of layout's properties: /r/reference/#layout
                    title = "*Average Trips Per Year", # layout's title: /r/reference/#layout-title
                    xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                      title = "Month & Year",     # xaxis's title: /r/reference/#layout-xaxis-title
                      showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                    ),
                    yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                      title = "Trips"      # yaxis's title: /r/reference/#layout-yaxis-title
                    ))
    
    ######alternate money and time dimension
    if (input$dimension == '2') {
      start_date1 <- input$monthdate[1]
      end_date1 <- input$monthdate[2]
      td =  subset(supra_monthly, 
                   (type == 'medallion' | type == 'shl') & 
                     (month_date >= start_date1 & month_date <= end_date1), 
                   c('farebox_per_day','month_date','type', 'year','trips_per_week'))
      
      nd = aggregate(farebox_per_day ~ year + type, data = td, FUN = sum)
      # uniks = ggplot(data=nd,
      #                aes(x=year, y= farebox_per_day, fill=type)) +
      #   geom_bar(stat = 'identity')+
      #   labs(x = "Year", y = "Yearly Trip Totals", 
      #        title = "Yearly Farebox Totals")+
      #   scale_fill_manual(values=c("yellow", "#66FF66"))+
      #   scale_y_continuous(name="trips", labels = comma)+
      #   theme(panel.background = element_rect(fill = 'black'),
      #         panel.grid.major = element_blank(),
      #         panel.grid.minor = element_blank(),
      #         axis.title.x = element_text(size = 13, colour = 'black'),
      #         axis.title.y = element_text(size = 13, colour = 'black'),
      #         axis.title = element_text(size = 18, colour = 'black'),
      #         axis.text.x  = element_text(vjust=.5, size=13, angle = 90))
      uniks = plot_ly(nd, x = ~year, y = ~farebox_per_day, split = ~type, type = 'bar')
      uniks = layout(uniks,              # all of layout's properties: /r/reference/#layout
                     title = "*Average Farebox Per Year", # layout's title: /r/reference/#layout-title
                     xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                       title = "Month & Year",     # xaxis's title: /r/reference/#layout-xaxis-title
                       showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                     ),
                     yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                       title = "Farebox"      # yaxis's title: /r/reference/#layout-yaxis-title
                     ))
    }
    uniks
  })
  #graph total trips each month
  output$trips_per_month = renderPlotly({
    ###########################
    #start process
    start_date1 <- input$monthdate[1]
    end_date1 <- input$monthdate[2]
    # td = subset(master_indicators, (month_date >= start_date1 & month_date <= end_date1), 
    #             c('trips_per_month', 'month_date', 'type'))
    td =  subset(supra_monthly, 
                 (type == 'medallion' | type == 'shl') & 
                   (month_date >= start_date1 & month_date <= end_date1), 
                 c('trips_per_month','month_date','type', 'year'))
    uniks <- plot_ly(td, 
                     x = ~month_date, y = ~trips_per_month, type = "bar", split = ~type)
    
    uniks <- layout(uniks,              # all of layout's properties: /r/reference/#layout
                    title = "*Trips Per Month Over Time", # layout's title: /r/reference/#layout-title
                    xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                      title = "Month & Year",     # xaxis's title: /r/reference/#layout-xaxis-title
                      showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                    ),
                    yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                      title = "Trips Per Month"      # yaxis's title: /r/reference/#layout-yaxis-title
                    ))
    uniks                
    
    
    #####alternate money dimension
    if (input$dimension == '2') {
      start_date1 <- input$monthdate[1]
      end_date1 <- input$monthdate[2]
      # td = subset(master_indicators, (month_date >= start_date1 & month_date <= end_date1), 
      #             c('farebox_per_month','month_date','type'))
      td =  subset(supra_monthly, 
                   (type == 'medallion' | type == 'shl') & 
                     (month_date >= start_date1 & month_date <= end_date1), 
                   c('farebox_per_month','month_date','type', 'year'))
      
      uniks <- plot_ly(td, 
                       x = ~month_date, y = ~farebox_per_month, type = "bar", split = ~type)
      
      uniks <- layout(uniks,              # all of layout's properties: /r/reference/#layout
                      title = "*Farebox Per Month Over Time", # layout's title: /r/reference/#layout-title
                      xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                        title = "Month & Year",     # xaxis's title: /r/reference/#layout-xaxis-title
                        showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                      ),
                      yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                        title = "Farebox Per Month"      # yaxis's title: /r/reference/#layout-yaxis-title
                      ))
    }
    uniks     
  })
  
  #graph medallions each month
  output$medallions_per_month = renderPlotly({
    
    ###########################
    #start process
    start_date1 <- input$monthdate[1]
    end_date1 <- input$monthdate[2]
    # td = subset(master_indicators, (month_date >= start_date1 & month_date <= end_date1), 
    #             c('uniq_med', 'month_date', 'type'))
    td =  subset(supra_monthly, 
                 (type == 'medallion' | type == 'shl') & 
                   (month_date >= start_date1 & month_date <= end_date1), 
                 c('unique_vehicles','month_date','type'))
    uniks <- plot_ly(td, 
                     x = ~month_date, y = ~unique_vehicles, type = "bar", split = ~type)
    uniks <- layout(uniks,              # all of layout's properties: /r/reference/#layout
                    title = "Unique Vehicles Per Month Over Time", # layout's title: /r/reference/#layout-title
                    xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                      title = "Month & Year",     # xaxis's title: /r/reference/#layout-xaxis-title
                      showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                    ),
                    yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                      title = "Unique Vehicles"      # yaxis's title: /r/reference/#layout-yaxis-title
                    ))
    uniks                
    
    
    #alternate dimension looking at 
    if (input$dimension == '2') {
      start_date1 <- input$monthdate[1]
      end_date1 <- input$monthdate[2]
      # td = subset(master_indicators, (month_date >= start_date1 & month_date <= end_date1), 
      #             c('medallions_per_day','month_date','type'))
      td =  subset(supra_monthly, 
                   (type == 'medallion' | type == 'shl') & 
                     (month_date >= start_date1 & month_date <= end_date1), 
                   c('vehicles_per_day','month_date','type'))
      uniks <- plot_ly(td, 
                       x = ~month_date, y = ~vehicles_per_day, type = "bar", split = ~type)
      uniks <- layout(uniks,              # all of layout's properties: /r/reference/#layout
                      title = "Vehicles Per Day Over Time", # layout's title: /r/reference/#layout-title
                      xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                        title = "Month & Year",     # xaxis's title: /r/reference/#layout-xaxis-title
                        showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                      ),
                      yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                        title = "Vehicless Per Day Per Month"      # yaxis's title: /r/reference/#layout-yaxis-title
                      ))
    }
    uniks     
  })
  ####experimental additions####experimental additions####experimental additions
  
  output$id1_m <- renderText({
    sprintf("You have selected %s on the x-axis", input$element_id1_m)
  })
  
  output$id2_m <- renderText({
    sprintf("You have selected %s on the y-axis", input$element_id2_m)
  })
  output$id3_m <- renderText({
    sprintf("You have selected %s as your spliting variable", input$element_id3_m)
  })
  output$plt_m <- renderPlotly({
    ###########################
    #start process
    start_date <- input$monthlydate[1]
    end_date <- input$monthlydate[2]
    # td = subset(master_indicators, (month_date >= start_date & month_date <= end_date),
    #             c(
    #               "month_date", 
    #               "week",
    #               "year",
    #               "avg_days_medallions_on_road", 
    #               "avg_hours_per_day_per_medallion",
    #               "days_per_driver",
    #               "avg_hours_per_day_per_driver_med", 
    #               "perc_trips_paid_with_credit_card",
    #               "type"
    #             ))
    
    td =  subset(supra_monthly, 
                 (type == 'medallion' | type == 'shl') & 
                   (month_date >= start_date & month_date <= end_date), 
                 c("month_date",
                   "trips_per_day",
                   "farebox_per_day",
                   "unique_drivers",
                   "unique_vehicles",
                   "avg_minutes_per_trip",
                   "week",
                   "year",
                   "avg_days_vehicles_on_road", 
                   "avg_hours_per_day_per_vehicle",
                   "days_per_driver",
                   "avg_hours_per_day_per_driver", 
                   "perc_trips_paid_with_credit_card",
                   "type"))
    
    # ggplotly(ggplot(td, aes_string(x = input$element_id1_m, y = input$element_id2_m, fill = input$element_id3_m)) + 
    #            geom_bar(stat = 'identity') + 
    #            ggtitle("Trends in Monthly TLC Metrics")+
    #            scale_y_continuous(labels = comma)+
    #            theme(panel.background = element_rect(fill = 'black'),
    #                  panel.grid.major = element_blank(),
    #                  panel.grid.minor = element_blank(),
    #                  axis.title.x = element_text(size = 13, colour = 'black'),
    #                  axis.title.y = element_text(size = 13, colour = 'black'),
    #                  axis.title = element_text(size = 18, colour = 'black'),
    #                  axis.text.x  = element_text(vjust=.5, size=13, angle = 90)))
    boots = plot_ly(x = td[,input$element_id1_m], y = td[,input$element_id2_m], type = "bar", 
                    data = td, split = td[,input$element_id3_m]) 
    boots = layout(boots,              # all of layout's properties: /r/reference/#layout
                   title = "Monthly Industry Trends Over Time", # layout's title: /r/reference/#layout-title
                   xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                     title = input$element_id1_m,     # xaxis's title: /r/reference/#layout-xaxis-title
                     showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                   ),
                   yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                     title = input$element_id2_m      # yaxis's title: /r/reference/#layout-yaxis-title
                   ))
    
  })
  
  # output$plt2_m <- renderPlotly({
  #   ###########################
  #   #start process
  #   start_date <- input$monthlydate[1]
  #   end_date <- input$monthlydate[2]
  #   td = subset(master_indicators, (month_date >= start_date & month_date <= end_date),
  #               c( "month_date", 
  #                  "week",
  #                  "year",
  #                  "avg_days_medallions_on_road", 
  #                  "avg_hours_per_day_per_medallion",
  #                  "days_per_driver",
  #                  "avg_hours_per_day_per_driver_med", 
  #                  "perc_trips_paid_with_credit_card",
  #                  "type"))
  #   plot_ly(x = td[,input$element_id1_m], y = td[,input$element_id2_m], 
  #           type = "line", data = td, split = td[,input$element_id3_m]) 
  # })
  # 
  # 
  
  ###################RPORTABLE TEST LOCAL DATA
  output$bcplot <- renderPlot({
    cars = mtcars
    cars$carb = as.character(cars$carb)
    p = ggplot(cars, aes(x = carb, y = mpg))+
      geom_bar(stat = 'identity')
    print(p)
  })
  
  
  ##################
  
  
  
  
  ####experimental additions####experimental additions####experimental additions
  
  
  
  
  #############FHV INDICATORS
  output$fhvtrips_per_day  = renderPlotly({
    #start_date <- input$insightdate[1]
    #end_date <- input$insightdate[2]
    #td = subset(uber_monthly_indicators, (date >= start_date & date <= end_date), c('date', 'trips',
    #                                                                          #'type', 'shift'))
    start_date <- input$fhv_date[1]
    end_date <- input$fhv_date[2]
    td = subset(supra_monthly, (month_date >= start_date & month_date <= end_date),
                c('month_year', 'trips_per_day', 'type', 'month_date'))
    print(td)
    p = plot_ly(td, x = month_year, y = trips_per_day, group = type)
    p <- layout(p,              # all of layout's properties: /r/reference/#layout
                title = "Trips Per Day Over Time", # layout's title: /r/reference/#layout-title
                xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                  title = "Month & Year",     # xaxis's title: /r/reference/#layout-xaxis-title
                  showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                ),
                yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                  title = "Trips Per Day"      # yaxis's title: /r/reference/#layout-yaxis-title
                ))
  })
  
  output$fhvunique_drivers = renderPlotly({
    start_date <- input$fhv_date[1]
    end_date <- input$fhv_date[2]
    td = subset(supra_monthly, (month_date >= start_date & month_date <= end_date),
                c('month_year', 'unique_drivers', 'type', 'month_date'))
    print(td)
    p = plot_ly(td, x = month_year, y = unique_drivers, group = type)
    p <- layout(p,              # all of layout's properties: /r/reference/#layout
                title = "Unique Drivers Over Time", # layout's title: /r/reference/#layout-title
                xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                  title = "Month & Year",     # xaxis's title: /r/reference/#layout-xaxis-title
                  showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                ),
                yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                  title = "Unique Drivers"      # yaxis's title: /r/reference/#layout-yaxis-title
                ))
    
    #####################****************************** 
  })
  
  output$id1_fhv <- renderText({
    sprintf("You have selected %s on the x-axis", input$element_id1_fhv)
  })
  
  output$id2_fhv <- renderText({
    sprintf("You have selected %s on the y-axis", input$element_id2_fhv)
  })
  output$id3_fhv <- renderText({
    sprintf("You have selected %s as your grouping variable", input$element_id3_fhv)
  })
  output$plt2_fhv <- renderPlotly({
    #start processes
    start_date <- input$fhv_date[1]
    end_date <- input$fhv_date[2]
    td = subset(supra_monthly, (month_date >= start_date & month_date <= end_date),
                c("month_date",
                  "week",
                  "year",
                  "type",
                  "trips_per_day", 
                  "farebox_per_day",
                  "unique_drivers",
                  "unique_vehicles", 
                  "vehicles_per_day",
                  "avg_days_vehicles_on_road", 
                  "avg_hours_per_day_per_vehicle",
                  "days_per_driver",
                  "avg_hours_per_day_per_driver"))
    print(td)
    supra_plot = plot_ly(x = td[,input$element_id1_fhv], y = td[,input$element_id2_fhv], 
                         type = "line", data = td, group = td[,input$element_id3_fhv]) 
    
    supra_plot = layout(supra_plot,              # all of layout's properties: /r/reference/#layout
                        title = "Industry Trends", # layout's title: /r/reference/#layout-title
                        xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                          title = input$element_id1_fhv,     # xaxis's title: /r/reference/#layout-xaxis-title
                          showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                        ),
                        yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                          title = input$element_id2_fhv      # yaxis's title: /r/reference/#layout-yaxis-title
                        ))
  })
  
  #####################*****************************************
  
  #############################################################################EHAIL METRICS
  
  output$ehail1 = renderPlotly({
    TPEP2 = odbcConnect("TPEP2")
    daily_erequests = sqlQuery(TPEP2, paste("SELECT
                                            convert(char(10),request_dt,121) as date,app_id as application_used,
                                            count(request_id) as number_of_requests,sum(case
                                            when request_outcome = 4 then 1
                                            else 0 end)as passenger_cancelled,sum(case
                                            when request_outcome = 3 then 1
                                            else 0 end)as driver_canceled,sum(case
                                            when request_outcome = 2 then 1
                                            else 0 end)as no_response,sum(case
                                            when request_outcome = 1 then 1
                                            else 0 end)as trips_completed
                                            FROM
                                            EHAIL_Request
                                            WHERE 
                                            request_dt > '2015-06-01'
                                            GROUP BY
                                            convert(char(10),request_dt,121),
                                            app_id ;"))
    daily_erequests$date = as.Date(daily_erequests$date)
    start_date_ehail = print(input$ehaildates[1])
    end_date_ehail = print(input$ehaildates[2])
    td = subset(daily_erequests, (date >= start_date_ehail & date <= end_date_ehail), c("date", "number_of_requests", "trips_completed",
                                                                                        "application_used"))
    td$application_used = revalue(td$application_used, c("EH0003"="UBER", "EH0008"="WAY-TO-RIDE", "EH0010" = "ARRO"))
    td$application_used = as.character(td$application_used)
    #graph daily metrics
    plot2 = ggplot(td, aes(x = date, y = number_of_requests, group = application_used)) +
      geom_line(aes(color = application_used))
    ggplotly(plot2)
  })
  
  output$ehail2 = renderPlot({
    TPEP2 = odbcConnect("TPEP2")
    daily_erequests = sqlQuery(TPEP2, paste("SELECT
                                            convert(char(10),request_dt,121) as date,app_id as application_used,
                                            count(request_id) as number_of_requests,sum(case
                                            when request_outcome = 4 then 1
                                            else 0 end)as passenger_cancelled,sum(case
                                            when request_outcome = 3 then 1
                                            else 0 end)as driver_canceled,sum(case
                                            when request_outcome = 2 then 1
                                            else 0 end)as no_response,sum(case
                                            when request_outcome = 1 then 1
                                            else 0 end)as trips_completed
                                            FROM
                                            EHAIL_Request
                                            WHERE 
                                            request_dt > '2015-06-01'
                                            GROUP BY
                                            convert(char(10),request_dt,121),
                                            app_id ;"))
    daily_erequests$date = as.Date(daily_erequests$date)
    start_date_ehail = print(input$ehaildates[1])
    end_date_ehail = print(input$ehaildates[2])
    td = subset(daily_erequests, (date >= start_date_ehail & date <= end_date_ehail), c("date", "number_of_requests", "trips_completed",
                                                                                        "application_used"))
    td$application_used = revalue(td$application_used, c("EH0003"="UBER", "EH0008"="WAY-TO-RIDE", "EH0010" = "ARRO"))
    td$application_used = as.character(td$application_used)
    res = td
    #graph daily metrics
    plot3 = ggplot(td, aes(x = date, y = trips_completed, group = application_used)) +
      geom_line(aes(color = application_used))
    print(plot3)
    #ggplotly(plot3)
    
    
    output$ehaildatatable <- renderDataTable(res)
    output$downloadDataehail <- downloadHandler(
      filename = function() {
        paste('res', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(res, con)
      }
    )
    
  })
  
  
  
  ##############################################################################
  
  
  #############INSIGHTS TOOL
  output$id1 <- renderText({
    sprintf("You have selected %s on the x-axis", input$element_id1)
  })
  
  output$id2 <- renderText({
    sprintf("You have selected %s on the y-axis", input$element_id2)
  })
  output$id3 <- renderText({
    sprintf("You have selected %s as your grouping variable", input$element_id3)
  })
  
  output$plt2 <- renderPlotly({
    #start processes
    start_date <- input$insightdate[1]
    end_date <- input$insightdate[2]
    td = subset(daily_master_indicators, (date >= start_date & date <= end_date),
                c("date", 
                  "trips",
                  "farebox",
                  "uniq_drivers",
                  "uniq_medallions",
                  "total_trip_hours",
                  "trip_miles",
                  "cc_trips",
                  "jfk_trips",
                  "lga_trips",
                  "cc_fares",
                  "type",
                  "hours_per_trip",
                  "miles_per_trip",
                  "velocity_per_trip",
                  "weekday",
                  "month"))
    print(td)
    p = plot_ly(x = td[,input$element_id1], y = td[,input$element_id2], 
                type = "line", data = td, group = td[,input$element_id3]) 
    p =     layout(p,              # all of layout's properties: /r/reference/#layout
                   title = "Dialy Industry Trends", # layout's title: /r/reference/#layout-title
                   xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                     title = input$element_id1,     # xaxis's title: /r/reference/#layout-xaxis-title
                     showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                   ),
                   yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                     title = input$element_id2     # yaxis's title: /r/reference/#layout-yaxis-title
                   ))
  })
  
  output$insightplot3 = renderPlotly({
    #start processes
    start_date <- input$insightdate[1]
    end_date <- input$insightdate[2]
    td = subset(hourly_master_indicators, (date >= start_date & date <= end_date),
                c("trips",
                  "farebox",
                  "cc_trips",
                  "type",
                  "hours_per_trip",
                  "miles_per_trip",
                  "velocity",
                  "weekday",
                  "month",
                  "hour"))
    days_busy = aggregate(trips ~ hour + weekday + type, data = td, FUN = sum)
    p = plot_ly(days_busy[days_busy$type == 'shl',], x = hour, y = trips, color = weekday)
    p = layout(p,              # all of layout's properties: /r/reference/#layout
               title = "SHL trips per hour for each Weekday", # layout's title: /r/reference/#layout-title
               xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                 title = "hour",     # xaxis's title: /r/reference/#layout-xaxis-title
                 showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
               ),
               yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                 title = "trips per hour"      # yaxis's title: /r/reference/#layout-yaxis-title
               ))
    
    p
  })
  
  output$insightplot4 = renderPlotly({ 
    start_date <- input$insightdate[1]
    end_date <- input$insightdate[2]
    td = subset(hourly_master_indicators, (date >= start_date & date <= end_date),
                c("trips",
                  "farebox",
                  "cc_trips",
                  "type",
                  "hours_per_trip",
                  "miles_per_trip",
                  "velocity",
                  "weekday",
                  "month",
                  "hour"))
    days_busy = aggregate(trips ~ hour + weekday + type, data = td, FUN = sum)
    p = plot_ly(days_busy[days_busy$type == 'medallion',], x = hour, y = trips, color = weekday)
    p = layout(p,              # all of layout's properties: /r/reference/#layout
               title = "Medallion trips per hour for each Weekday", # layout's title: /r/reference/#layout-title
               xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                 title = "hour",     # xaxis's title: /r/reference/#layout-xaxis-title
                 showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
               ),
               yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                 title = "trips per hour"      # yaxis's title: /r/reference/#layout-yaxis-title
               ))
    p
  })
  output$insightplot5 = renderPlotly({
    start_date <- input$insightdate[1]
    end_date <- input$insightdate[2]
    td = subset(daily_master_indicators, (date >= start_date & date <= end_date), c('date', 'velocity_per_trip',
                                                                                    'type', 'weekday'))
    
    p <- plot_ly(td[td$type == "shl",], x = velocity_per_trip, color = weekday, type = "box") 
    p = layout(p,              # all of layout's properties: /r/reference/#layout
               title = "Average Velocity per Weekday for SHLS", # layout's title: /r/reference/#layout-title
               xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                 title = "Velocity",     # xaxis's title: /r/reference/#layout-xaxis-title
                 showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
               ),
               yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                 title = "Weekday"      # yaxis's title: /r/reference/#layout-yaxis-title
               ))
    p
  })
  output$insightplot6 = renderPlotly({
    start_date <- input$insightdate[1]
    end_date <- input$insightdate[2]
    td = subset(daily_master_indicators, (date >= start_date & date <= end_date), c('date', 'velocity_per_trip',
                                                                                    'type', 'weekday'))
    
    p <- plot_ly(td[td$type == "medallion",], x = velocity_per_trip, color = weekday, type = "box") 
    p = layout(p,              # all of layout's properties: /r/reference/#layout
               title = "Average Velocity per Weekday for Medallions", # layout's title: /r/reference/#layout-title
               xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                 title = "Velocity",     # xaxis's title: /r/reference/#layout-xaxis-title
                 showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
               ),
               yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                 title = "Weekday"      # yaxis's title: /r/reference/#layout-yaxis-title
               ))
    p
  })
  
  #############DRIVER EDUCATION
  output$compliancer = renderPlot({
    #start process
    start_date1 <- input$educationdate[1]
    end_date1 <- input$educationdate[2]
    td = subset(education, (month_date >= start_date1 & month_date <= end_date1), 
                c('applic_month', 'month_date', 'master_school', 'compliance_rate','type'))
    print(td)
    compliancer = ggplot(td, aes(x = master_school, y = compliance_rate, fill = type))+
      geom_bar(stat = 'identity', position = 'dodge')+
      facet_wrap(~applic_month)+
      labs(x = "Month", y = "(%)", 
           title = "Compliance Rates Across Schools")+
      scale_y_continuous(labels=percent, limits = c(0,1))+
      theme(panel.background = element_rect(),
            axis.title.x = element_text(size = 13, colour = 'black'),
            axis.title.y = element_text(size = 13, colour = 'black'),
            axis.title = element_text(size = 18, colour = 'black'),
            axis.text.x  = element_text(vjust=.5, size=9, angle = 90))
    print(compliancer)
    output$timecompliancetable = renderDataTable(td)
    output$timecompliancetable <- downloadHandler(
      filename = function() {
        paste('td', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(td, con)
      }
    )
    
  })
  output$timetocompliance = renderPlotly({
    start_date1 <- input$educationdate[1]
    end_date1 <- input$educationdate[2]
    td = subset(education, (month_date >= start_date1 & month_date <= end_date1) &
                  (type %in% c('fhv_new', 'medallion', 'overall')), 
                c('applic_month', 'master_school', 'mean_time_to_compliance_test','type'))
    ggplot(td, aes(x = applic_month, y = mean_time_to_compliance_test, fill = master_school))+
      geom_bar(stat = 'identity')+
      facet_wrap(~type)
    
  })
  #start process
  output$initialpr = renderPlotly({
    start_date1 <- input$educationdate[1]
    end_date1 <- input$educationdate[2]
    td = subset(education, (month_date >= start_date1 & month_date <= end_date1), 
                c('applic_month', 'master_school', 'initial_pass_rate','type'))
    p = aggregate(initial_pass_rate ~ applic_month + master_school + type, data = td, FUN =mean)
    px = ggplot(p, aes(x = applic_month, y = initial_pass_rate, colour = master_school))+
      geom_line(aes(group = master_school), size = 1.2)+
      geom_point()+
      facet_wrap(~type)+
      scale_y_continuous(labels=percent, limits = c(0, 1))+
      theme(panel.background = element_rect(colour = 'black', fill = 'grey'),
            axis.title.x = element_text(size = 13, colour = 'black'),
            axis.title.y = element_text(size = 13, colour = 'black'),
            axis.title = element_text(size = 18, colour = 'black'),
            axis.text.x  = element_text(vjust=.5, size=9, angle = 90))
    
    ggplotly(px)        
    
  })
  
  
  #####################################QUERY QIZARD
  observeEvent(input$clicked, {
    
    withProgress(message = 'querying database', value = 0, {
      n = input$hack
      for (i in n) {
        print(input$hack)
        print(input$wizarddate)
        TPEP2 =odbcConnect('TPEP2')
        #call taxi zones
        # taxi_zones = sqlQuery(TPEP2, paste("select Borough, LocaitonID from TPEP2_LOCATION_lookup"), stringsAsFactors = F)
        # bronx_zones = taxi_zones[taxi_zones$Borough == 'Bronx', 'LocationID']
        # queens_zones = taxi_zones[taxi_zones$Borough == 'Queens', 'LocationID']
        # manhattan_zones = taxi_zones[taxi_zones$Borough == 'Manhattan', 'LocationID']
        # brooklyn_zones = taxi_zones[taxi_zones$Borough == 'Brooklyn', 'LocationID']
        # statenisland_zones = taxi_zones[taxi_zones$Borough == 'Staten Island', 'LocationID']
        
        #conditional statements to fill query
        #date condition
        date =  " tpep_pickup_datetime >= 'DATE'"
        date = sub('DATE',input$wizarddate, date)
        
        #hack number condition
        hack = ifelse(is.null(input$hack), " ", "and Hack_number like 'HACK'")
        hack = sub('HACK',input$hack,hack)
        
        #medallion condition
        med_statement = "and medallion like 'MED'"
        med = ifelse(!is.null(input$medallion),';',sub('MED',input$medallion, med_statement))
        
        #pickup condition
        #pickup_statement = sqlFeed(taxi_zones,'PULocationid like ','or')
        #pickup = ifelse(!is.null(input$puboroughs),'',sub('pickup',input$puboroughs, pickup_statement)) 
        
        #print records to makes sure they are inputed
        print(hack)
        print(med)
        print(date)
        print(input$variable)
        variables = paste(input$variable, collapse = ",", sep = ' ')
        print(variables)
        #primary query  
        
        res = sqlQuery(TPEP2, paste("Select top 100 
                                    ",variables," 
                                    from 
                                    TPEP2_Triprecord 
                                    where  ",date,"
                                    ",hack,"
                                    ",med,""), stringsAsFactors = F)
        
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("still querying", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.5)
        print(res)
      }
      output$mytableuno <- DT::renderDataTable({
        DT::datatable(res)
      })
      output$downloadDatawizard <- downloadHandler(
        filename = function() {
          paste('res', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          write.csv(res, con)
        }
      )
    })
    
  })
  
  
  
  
  
  
  ########################################
  
  
  
  #############################TRIP VOLUME INSIGHT TOOL
  
  #begin plotting
  output$plot2 = renderPlot({
    
    #default taxi zone graph 
    start_date = print(input$dates[1])
    end_date = print(input$dates[2])
    my_query=
      "SELECT
    date,
    pulocationid,
    dolocationid,
    from_zone,
    from_borough,
    taxi_zones.zone as to_zone,
    taxi_zones.borough as to_borough,
    volume
    
    FROM
    (SELECT
    date,
    pulocationid,
    dolocationid,
    volume,
    taxi_zones.zone as from_zone,
    taxi_zones.borough as from_borough
    
    
    FROM
    (SELECT
    date,
    split_part(combo, ',', 1) as pulocationid, 
    split_part(combo, ',', 2) as dolocationid,
    volume
    
    FROM
    (SELECT
    tpep_pickup_datetime as date,
    pulocationid || ',' || dolocationid as combo,
    count(pulocationid || ',' || dolocationid) as volume
    FROM
    master_volume
    WHERE
    tpep_pickup_datetime >= 'DATE1' AND
    tpep_pickup_datetime < 'DATE2'
    GROUP BY
    tpep_pickup_datetime,
    pulocationid || ',' || dolocationid
    LIMIT
    100) as table_one
    
    GROUP BY
    date,
    split_part(combo, ',', 1),split_part(combo, ',', 2),
    volume) table_two
    
    INNER JOIN 
    taxi_zones
    on cast(table_two.pulocationid as integer) = taxi_zones.locationid) as table_three
    
    INNER JOIN 
    taxi_zones
    on cast(table_three.dolocationid as integer) = taxi_zones.locationid
    ORDER BY
    volume DESC;"
    my_query <- sub("DATE1",as.Date(start_date),my_query);
    my_query <- sub("DATE2",as.Date(end_date),my_query)
    sql = dbGetQuery(con, paste(my_query))
    sql = sql[,c(4,6,8)]
    sql$from_zone = trim(sql$from_zone)
    sql$to_zone = trim(sql$to_zone)
    sql$from_zone[sql$from_zone == "NV"] = NA
    sql$to_zone[sql$to_zone == "NV"] = NA
    sql = na.omit(sql)
    sql = sql[order(sql$volume, decreasing = T),] 
    sql = sql[1:50,]
    sql = sql[seq_len(input$slider),]
    chordDiagram(sql, annotationTrack = "grid", directional=1, self.link = 1,
                 direction.type = c("diffHeight","arrows"), link.arr.length = 0.2, link.arr.type = "big.arrow",
                 preAllocateTracks = list(track.height = 0.1))
    circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      xplot = get.cell.meta.data("xplot")
      ylim = get.cell.meta.data("ylim")
      sector.name = get.cell.meta.data("sector.index")
      circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", cex = .75,
                  niceFacing = TRUE, adj = c(0, 0.5)) } , bg.border = NA)
    circos.clear()
    output$table <- renderTable(sql) 
    output$datatable <- renderDataTable(sql) 
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('sql', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(sql, con)
      }
    )
    
    #if we want to see only yellow cabs
    if (input$type == '1') {
      start_date <- input$dates[1]
      end_date <- input$dates[2]
      start_date = print(input$dates[1])
      end_date = print(input$dates[2])
      my_query=
        "SELECT
      date,
      pulocationid,
      dolocationid,
      from_zone,
      from_borough,
      taxi_zones.zone as to_zone,
      taxi_zones.borough as to_borough,
      volume
      
      FROM
      (SELECT
      date,
      pulocationid,
      dolocationid,
      volume,
      taxi_zones.zone as from_zone,
      taxi_zones.borough as from_borough
      
      
      FROM
      (SELECT
      date,
      split_part(combo, ',', 1) as pulocationid, 
      split_part(combo, ',', 2) as dolocationid,
      volume
      
      FROM
      (SELECT
      tpep_pickup_datetime as date,
      pulocationid || ',' || dolocationid as combo,
      count(pulocationid || ',' || dolocationid) as volume
      FROM
      master_volume
      WHERE
      tpep_pickup_datetime >= 'DATE1' AND
      tpep_pickup_datetime < 'DATE2'
      GROUP BY
      tpep_pickup_datetime,
      pulocationid || ',' || dolocationid
      LIMIT
      100) as table_one
      
      GROUP BY
      date,
      split_part(combo, ',', 1),split_part(combo, ',', 2),
      volume) table_two
      
      INNER JOIN 
      taxi_zones
      on cast(table_two.pulocationid as integer) = taxi_zones.locationid) as table_three
      
      INNER JOIN 
      taxi_zones
      on cast(table_three.dolocationid as integer) = taxi_zones.locationid
      ORDER BY
      volume DESC;"
      my_query <- sub("DATE1",as.Date(start_date),my_query);
      my_query <- sub("DATE2",as.Date(end_date),my_query)
      sql = dbGetQuery(con, paste(my_query))
      sql = sql[,c(4,6,8)]
      sql$from_zone = trim(sql$from_zone)
      sql$to_zone = trim(sql$to_zone)
      sql$from_zone[sql$from_zone == "NV"] = NA
      sql$to_zone[sql$to_zone == "NV"] = NA
      sql = na.omit(sql)
      sql = sql[order(sql$volume, decreasing = T),] 
      sql = sql[1:50,]
      sql = sql[seq_len(input$slider),]
      chordDiagram(sql, annotationTrack = "grid", directional=1, self.link = 1,
                   direction.type = c("diffHeight","arrows"), link.arr.length = 0.2, link.arr.type = "big.arrow",
                   preAllocateTracks = list(track.height = 0.1))
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
        circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", cex = .75,
                    niceFacing = TRUE, adj = c(0, 0.5)) } , bg.border = NA)
      output$datable = renderDataTable(d)
    }
    #if we want to see only green cabs
    if (input$type == '2') {
      start_date <- input$dates[1]
      end_date <- input$dates[2]
      start_date = print(input$dates[1])
      end_date = print(input$dates[2])
      my_query=
        "SELECT
      date,
      PULocationID,
      DOLocationID,
      from_zone,
      from_borough,
      TPEP2_LOCATION_lookup.Zone as to_zone,
      TPEP2_LOCATION_lookup.Borough as to_borough,
      volume
      
      
      FROM
      (
      SELECT
      date,
      PULocationID,
      DOLocationID,
      volume,
      TPEP2_LOCATION_lookup.Zone as from_zone,
      TPEP2_LOCATION_lookup.Borough as from_borough
      
      FROM
      
      (SELECT
      date,
      LEFT(combo,CHARINDEX(',',combo)-1) as PULocationID,
      LTRIM(RIGHT(combo,LEN(combo) - CHARINDEX(',',combo) )) as DOLocationID,
      volume
      
      FROM
      
      (SELECT 
      top 100000
      convert(char(10),lpep_pickup_datetime,121) as date, 
      cast(PULocationID as varchar(3)) 
      + (',') + cast(DOLocationID as varchar(3)) as combo,
      count(cast(PULocationID as varchar(3)) 
      + (',') + cast(DOLocationID as varchar(3))) as volume
      FROM
      dbo.LPEP2_Triprecord
      WHERE lpep_pickup_datetime >= 'DATE1' and 
      lpep_pickup_datetime < 'DATE2'
      GROUP BY
      cast(PULocationID as varchar(3)) 
      + (',') + cast(DOLocationID as varchar(3)),
      convert(char(10),lpep_pickup_datetime,121)) as table_one
      
      
      GROUP BY
      date,
      LEFT(combo,CHARINDEX(',',combo)-1),
      LTRIM(RIGHT(combo,LEN(combo) - CHARINDEX(',',combo) )),
      volume) as table_two
      
      INNER JOIN 
      TPEP2_LOCATION_lookup
      on table_two.PULocationID = TPEP2_LOCATION_lookup.LocationID) AS table_three
      
      INNER JOIN
      TPEP2_LOCATION_lookup
      on table_three.DOLocationID = TPEP2_LOCATION_lookup.LocationID
      "
      my_query <- sub("DATE1",as.Date(start_date),my_query);
      my_query <- sub("DATE2",as.Date(end_date),my_query)
      sql = dbGetQuery(con, paste(my_query))
      sql = sql[,c(4,6,8)]
      sql$from_zone = trim(sql$from_zone)
      sql$to_zone = trim(sql$to_zone)
      sql$from_zone[sql$from_zone == "NV"] = NA
      sql$to_zone[sql$to_zone == "NV"] = NA
      sql = na.omit(sql)
      sql = sql[order(sql$volume, decreasing = T),] 
      sql = sql[1:50,]
      sql = sql[seq_len(input$slider),]
      chordDiagram(sql, annotationTrack = "grid", directional=1, self.link = 1,
                   direction.type = c("diffHeight","arrows"), link.arr.length = 0.2, link.arr.type = "big.arrow",
                   preAllocateTracks = list(track.height = 0.1))
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
        circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", cex = .75,
                    niceFacing = TRUE, adj = c(0, 0.5)) } , bg.border = NA)
      output$datable = renderDataTable(d)
    }
    #if we want to see all types
    if (input$type == '3') {
      start_date <- input$dates[1]
      end_date <- input$dates[2]
      d = subset(master_volumesample, (date >= start_date & date <= end_date)
                 , c("zone_from","zone_to", "type"))
      d$c = paste(d$zone_from, ",", d$zone_to)
      d = as.data.frame(table(d$c))
      d = data.frame(d, do.call(rbind, str_split(d$Var1, ',')))
      d$X1 = trim(d$X1)
      d$X2 = trim(d$X2)
      colnames(d)[colnames(d)=="X1"] <- "From"
      colnames(d)[colnames(d)=="X2"] <- "To"
      d = d[,c(3,4,2)]
      d = d[order(d$Freq, decreasing = T),]
      d = d[seq_len(input$slider),]
      chordDiagram(d, annotationTrack = "grid", directional=1, self.link = 1,
                   direction.type = c("diffHeight","arrows"), link.arr.length = 0.2, link.arr.type = "big.arrow",
                   preAllocateTracks = list(track.height = 0.1))
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
        circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", cex = .75,
                    niceFacing = TRUE, adj = c(0, 0.5)) } , bg.border = NA)
      circos.clear()
      output$datable = renderDataTable(d) 
    }
    #if we want to see boroughs
    if (input$list == "1") {
      start_date <- input$dates[1]
      end_date <- input$dates[2]
      d = subset(master_volumesample, (date >= start_date & date <= end_date)
                 , c("borough_from","borough_to", "type"))
      d$c = paste(d$borough_from, ",", d$borough_to)
      d = as.data.frame(table(d$c))
      d = data.frame(d, do.call(rbind, str_split(d$Var1, ',')))
      d$X1 = trim(d$X1)
      d$X2 = trim(d$X2)
      colnames(d)[colnames(d)=="X1"] <- "From"
      colnames(d)[colnames(d)=="X2"] <- "To"
      d = d[,c(3,4,2)]
      d = d[order(d$Freq, decreasing = T),]
      d = d[seq_len(input$slider),]
      chordDiagram(d, annotationTrack = "grid", directional=1, self.link = 1,
                   direction.type = c("diffHeight","arrows"), link.arr.length = 0.2, link.arr.type = "triangle",
                   preAllocateTracks = list(track.height = 0.1))
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
        circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", cex = .75,
                    niceFacing = TRUE, adj = c(0, 0.5)) } , bg.border = NA)
      circos.clear() 
      output$datable = renderDataTable(d)
    }
    #if we want to see boroughs and green cabs
    if (input$list == "1" & input$type == "2") {
      start_date <- input$dates[1]
      end_date <- input$dates[2]
      d = master_volumesample[master_volumesample$type == 'green',]
      d = subset(d, (date >= start_date & date <= end_date)
                 , c("borough_from","borough_to", "type"))
      d$c = paste(d$borough_from, ",", d$borough_to)
      d = as.data.frame(table(d$c))
      d = data.frame(d, do.call(rbind, str_split(d$Var1, ',')))
      d$X1 = trim(d$X1)
      d$X2 = trim(d$X2)
      colnames(d)[colnames(d)=="X1"] <- "From"
      colnames(d)[colnames(d)=="X2"] <- "To"
      d = d[,c(3,4,2)]
      d = d[order(d$Freq, decreasing = T),]
      d = d[seq_len(input$slider), ]
      chordDiagram(d, annotationTrack = "grid", directional=1, self.link = 1,
                   direction.type = c("diffHeight","arrows"), link.arr.length = 0.2, link.arr.type = "triangle",
                   preAllocateTracks = list(track.height = 0.1))
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
        circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", cex = .75,
                    niceFacing = TRUE, adj = c(0, 0.5)) } , bg.border = NA)
      circos.clear() 
      output$datable = renderDataTable(d)
    }
    #if we want to see boroughs and yellow cabs
    if (input$list == "1" & input$type == "1") {
      start_date <- input$dates[1]
      end_date <- input$dates[2]
      d = master_volumesample[master_volumesample$type == 'yellowcab',]
      d = subset(d, (date >= start_date & date <= end_date)
                 , c("borough_from","borough_to", "type"))
      d$c = paste(d$borough_from, ",", d$borough_to)
      d = as.data.frame(table(d$c))
      d = data.frame(d, do.call(rbind, str_split(d$Var1, ',')))
      d$X1 = trim(d$X1)
      d$X2 = trim(d$X2)
      colnames(d)[colnames(d)=="X1"] <- "From"
      colnames(d)[colnames(d)=="X2"] <- "To"
      d = d[,c(3,4,2)]
      d = d[order(d$Freq, decreasing = T),]
      d = d[seq_len(input$slider), ]
      chordDiagram(d, annotationTrack = "grid", directional=1, self.link = 1,
                   direction.type = c("diffHeight","arrows"), link.arr.length = 0.2, link.arr.type = "triangle",
                   preAllocateTracks = list(track.height = 0.1))
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
        circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", cex = .75,
                    niceFacing = TRUE, adj = c(0, 0.5)) } , bg.border = NA)
      circos.clear() 
      output$datable = renderDataTable(d)
    }
    #if we want to see service zones
    if (input$list == "3") {
      start_date <- input$dates[1]
      end_date <- input$dates[2]
      d = subset(master_volumesample, (date >= start_date & date <= end_date)
                 , c("servicezone_from","servicezone_to", "type"))
      d$c = paste(d$servicezone_from, ",", d$servicezone_to)
      d = as.data.frame(table(d$c))
      d = data.frame(d, do.call(rbind, str_split(d$Var1, ',')))
      d$X1 = trim(d$X1)
      d$X2 = trim(d$X2)
      colnames(d)[colnames(d)=="X1"] <- "From"
      colnames(d)[colnames(d)=="X2"] <- "To"
      d = d[,c(3,4,2)]
      d = d[order(d$Freq, decreasing = T),]
      d = d[seq_len(input$slider), ]
      chordDiagram(d, annotationTrack = "grid", directional=1, self.link = 1,
                   direction.type = c("diffHeight","arrows"), link.arr.length = 0.2, link.arr.type = "triangle",
                   preAllocateTracks = list(track.height = 0.1))
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
        circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", cex = .75,
                    niceFacing = TRUE, adj = c(0, 0.5)) } , bg.border = NA)
      circos.clear() 
      output$datable = renderDataTable(d)
      
    }
  })
  
  
  ###########MAPS
  output$pudos1 = renderPlotly({
    #start process
    date1 <- input$mapdate
    date1 = as.yearmon(date1)
    td = subset(monthly_pudos[monthly_pudos$type == "pickups",], as.yearmon(year_month) == date1, 
                c('year_month', 'month_date', 'zone', 'locationid', 'pickup_dropoff', 'type'))
    print(td)
    #merge with taxi zones
    zonedata = merge(zones, td, by.x = "OBJECTID", by.y = "locationid")
    #fortify zones
    zonedata_f = fortify(zonedata)
    #create an id variable for the old data
    zonedata$id = rownames(zonedata)
    #remerge data
    zonedata_f = merge(zonedata_f, zonedata@data, by.x = "id", by.y = "OBJECTID")
    
    pu = ggplot(zonedata_f, aes(long, lat, group = group, fill = pickup_dropoff))+
      geom_polygon()+
      coord_equal()+
      labs(x = "", y = "",
           fill = "Number of Pickups") +
      ggtitle("Medallion Pickups by Taxi Zone")+
      scale_fill_gradient(low = "yellow", high = "blue", label = comma)
    #+scale_fill_continuous(label = comma)
    ggplotly(pu)
    
    
  })
  output$pudos2 = renderPlotly({
    #start process
    date1 <- input$mapdate
    date1 = as.yearmon(date1)
    td = subset(monthly_pudos[monthly_pudos$type == "dropoffs",], as.yearmon(year_month) == date1, 
                c('year_month', 'month_date', 'zone', 'locationid', 'pickup_dropoff', 'type')) 
    
    #merge with taxi zones
    zonedata = merge(zones, td, by.x = "OBJECTID", by.y = "locationid")
    #fortify zones
    zonedata_f = fortify(zonedata)
    #create an id variable for the old data
    zonedata$id = rownames(zonedata)
    #remerge data
    zonedata_f = merge(zonedata_f, zonedata@data, by.x = "id", by.y = "OBJECTID")
    
    #map dropoffs
    do = ggplot(zonedata_f, aes(long, lat, group = group, fill = pickup_dropoff))+
      geom_polygon()+
      coord_equal()+
      labs(x = "", y = "",
           fill = "Number of Dropoffs") +
      ggtitle("Medallion Dropoffs by Taxi Zone")+
      scale_fill_gradient(low = "yellow", high = "blue", label = comma)
    #+scale_fill_continuous(label = comma)
    ggplotly(do)
    
  })
  
  ###########RENDER DATA BANKS
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(supra_monthly[, input$show_vars, drop = FALSE])
    
  })
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste('monthly_indicators', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(supra_monthly[, input$show_vars, drop = FALSE], con)
    }
  )
  
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(education, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste('supra_monthly', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(supra_monthly, con)
    }
  )
  
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(education, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  # output$downloadData1 <- downloadHandler(
  #   filename = function() {
  #     paste('supra_monthly', Sys.Date(), '.csv', sep='')
  #   },
  #   content = function(con) {
  #     write.csv(supra_monthly, con)
  #   }
  # )
  
  output$mytable1.5 <- DT::renderDataTable({
    DT::datatable(daily_master_indicators, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
  })  
  
  ###########TOP VALUE BOXES
  output$progressBox <- renderValueBox({
    #simple = read.csv('simple.csv', header = T)
    recent_date = as.character(supra_monthly[1, "month_date"])
    my_query = "*Average Medallion Trips per day as of 'SAMPLE'"
    med_trips = round(mean(supra_monthly[supra_monthly$type == 'medallion', "trips_per_day"])) #simple[1,2]
    valueBox(
      paste0(med_trips), sub("SAMPLE",recent_date,my_query), icon = icon("fa fa-taxi"),
      color = "yellow")
  }) 
  output$shltripbox <- renderValueBox({
    #simple = read.csv('simple.csv', header = T) 
    recent_date = as.character(supra_monthly[1, "month_date"])
    my_query = "*Average SHL Trips per day each Month as of 'SAMPLE'"
    shl_trips = round(mean(supra_monthly[supra_monthly$type == 'shl', "trips_per_day"])) #simple[2,2]
    valueBox(
      paste0(shl_trips), sub("SAMPLE",recent_date,my_query), icon = icon("fa fa-taxi"),
      color = "green")
  }) 
  output$ubers_etc <- renderValueBox({
    #simple = read.csv('simple.csv', header = T) 
    recent_date = as.character(supra_monthly[1, "month_date"])
    my_query = "Average FHV Trips per day each Month as of 'SAMPLE'"
    ubers_etc = round(mean(supra_monthly[supra_monthly$type == 'fhv all', "trips_per_day"])) #simple[3,2]
    valueBox(
      paste0(ubers_etc), sub("SAMPLE",recent_date,my_query), icon = icon("fa fa-taxi"),
      color = "maroon")
  })
  output$fhvtesters <- renderValueBox({
    datawarehouse = odbcConnect("datawarehouse")
    #all fhv test takers as of 2016
    tlc_education_fhv = sqlQuery(datawarehouse, paste("SELECT 
                                                      CAMIS_ID, 
                                                      LIC_NO, 
                                                      FED_ID, 
                                                      TEST_DATE, 
                                                      TEST_TYPE, 
                                                      TEST_GRADE, 
                                                      TEST_APPT, 
                                                      ACT_DATE_1 
                                                      FROM tlc_education
                                                      WHERE
                                                      (TEST_DATE >= '01-01-2016')
                                                      and 
                                                      (TEST_TYPE = 'HFT' OR
                                                      TEST_TYPE = 'KFT' OR
                                                      TEST_TYPE = 'LFT' OR
                                                      TEST_TYPE = 'MFT')"))
    tlc_education_fhv = tlc_education_fhv[!duplicated(tlc_education_fhv$CAMIS_ID),]
    valueBox(
      paste0(nrow(tlc_education_fhv)), "All FHV Testers Since 2016", icon = icon("fa fa-taxi"),
      color = "maroon")
  }) 
  output$medalliontesters <- renderValueBox({
    datawarehouse = odbcConnect("datawarehouse")
    #all medallion test takers as of 2015
    tlc_education_hack = sqlQuery(datawarehouse, paste("SELECT 
                                                       CAMIS_ID, 
                                                       LIC_NO, 
                                                       FED_ID, 
                                                       TEST_DATE, 
                                                       TEST_TYPE, 
                                                       TEST_GRADE, 
                                                       TEST_APPT, 
                                                       ACT_DATE_1 
                                                       FROM tlc_education
                                                       WHERE 
                                                       (TEST_DATE >= '01-01-2015') AND
                                                       (TEST_TYPE = 'HMT' OR
                                                       TEST_TYPE = 'KMT' OR
                                                       TEST_TYPE = 'LMT' OR
                                                       TEST_TYPE = 'MMT')"))
    tlc_education_hack = tlc_education_hack[!duplicated(tlc_education_hack$CAMIS_ID),]
    valueBox(
      paste0(nrow(tlc_education_hack)), "All Medallion Testers Since 2014", icon = icon("fa fa-taxi"),
      color = "yellow")
  })
  output$alltesters <- renderValueBox({
    
    valueBox(
      paste0(3000), "FHV & Medallion Aggregate", icon = icon("fa fa-taxi"),
      color = "aqua")
  })
  
  # choose columns to display
  output$mytable = renderDataTable({
    supra_monthly
  })
  
  output$textbox = renderText({
    print("*Note that the * designates these aggregations are based on daily averages and not on summations over selected periods")
  })
  
  output$textbox2 = renderText({
    print("*Input a date range to see changes over time")
  })
  
  output$textbox3 = renderText({
    print("*Use the dropdown menu to select different metrics")
  })
}


#############################

#Execute APP
shinyApp(ui, server)
