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


source("raw_data.R")


#----------------------------------------------------------------------------------------------------------------------------------------

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
                       menuItem("Objective", tabName = "objective", icon = icon("dashboard")),
                       menuItem("Shifts", tabName = "shifts", icon = icon("dashboard")),
                       menuItem("Driver Income", tabName = "driver_income", icon = icon("fa fa-bar-chart"),
                                menuSubItem("Yellow & Green Cabs", tabName = "ygcabs_income", href = NULL, newtab = TRUE,
                                            icon = shiny::icon("angle-double-right"), selected = NULL),
                                menuSubItem("FHV's", tabName = "fhv_income", href = NULL, newtab = TRUE,
                                            icon = shiny::icon("angle-double-right"), selected = NULL)
   
                       ),
                       
                       menuItem("Bases", tabName = "wizard", icon =icon("fa fa-magic"),
                                menuSubItem("Company Networks", tabName = "company_networks", href = NULL, newtab = TRUE,
                                            icon = shiny::icon("angle-double-right"), selected = NULL),
                                menuSubItem("Shifts in the industry", tabName = "industry_shifts", href = NULL, newtab = TRUE,
                                            icon = shiny::icon("angle-double-right"), selected = NULL)),
     
                       menuItem("Data Bank", tabName = "databank", icon =icon("fa fa-university"))
                       # menuItem("Source code", icon = icon("file-code-o"), 
                       #          href = "https://github.com/rstudio/shinydashboard/"),
                       #   sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                       #                     label = "Search...")
                     )
                     ),
                   # Body content---------------------------------------------------------------------------------------------
                   dashboardBody(
                     #objective------------------------------------------------------------------------------------------------
                     tabItems(
                       tabItem(tabName = "objective",
                               
                               
                               fluidRow(
                                 # Dynamic infoBoxes
                                 valueBoxOutput("shltripbox", width = 4),
                                 valueBoxOutput("progressBox", width = 4),
                                 valueBoxOutput("ubers_etc", width = 4)
                                 ,
                                 box(textOutput("textbox2")),
                                 box(textOutput("textbox3"))
                               )    
                       ),
                       
                       #shifts-----------------------------------------------------------------------------------------------------------
                       tabItem(tabName = "shifts",
                               
                               fluidRow(
                                 box(background = "black", dateRangeInput("monthdate", label = h3("Choose a Date Range"),
                                                                          start = '2015-01-01',
                                                                          end = as.Date(Sys.time())-365)),
                                 box(background="black", selectInput(inputId = "category", label = strong("Choose Metric"),
                                                                     choices = c('Trips, Drivers & Vehicles'='1', 'Time & Money' = '2'), 
                                                                     multiple = FALSE, selectize = TRUE))
                                 #box(textOutput("textboxshifts"))
                               ),
                               fluidRow(
                                 box(title = "shifts across time",
                                     status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                     plotlyOutput("plot_1"
                                                  , height = "400px", width = "600px"
                                                  )
                                 )
                               )
                            ),
                       
                       # driver income-------------------------------------------------------------------------------------------------
                       tabItem(tabName = "driver_income"),
                       
                       # yellow and green income---------------------------------------------------------------------------------------------        
                       tabItem(tabName = "ygcabs_income"),
                       
                       #fhv income-----------------------------------------------------------------------------------------------------------------
                       tabItem(tabName = "fhv_income"),
                                
                       #bases-------------------------------------------------------------------------------------------------------------------
                       tabItem(tabName = "wizard"),
                       
                       #company networks----------------------------------------------------------------------------------------------------------
                       
                       tabItem(tabName = "company_networks"),
                       
                       #mapping pickups and dropoffs
                       tabItem(tabName = "industry_shifts")
                               )
                   )
)

                   


server = function(input, output) {

  
  
  #shifts----------------------------------------------------------------------------------------------------------------------------
  
  output$plot_1 = renderPlotly({
    start_date1 = input$monthdate[1]
    end_date1 = input$monthdate[2]
    new_df = td = subset(shifts, (mon_year >= start_date1 & mon_year <= end_date1), c(1:6))
    
    print(new_df)
    
    #plot1 = plot_ly(new_df,x = shifts$quarter_hour,y = shifts$N,type = "bar")
    plot1 = 
      ggplotly(
      ggplot(NULL, aes(lab, perc)) + 
      geom_bar(aes(fill = "App"), data = new_df[app = "App"], alpha = 0.5) +
      geom_bar(aes(fill = "Shl"), data = new_df[app = "Shl"], alpha = 0.5)
      )    
    plot1
    
  })
  

  #TOP VALUE BOXES-------------------------------------------------------------------------------------------------------------------
  output$progressBox = renderValueBox({
    valueBox("yellow cabs",100000, icon = icon("fa fa-taxi"),
      color = "yellow")
  }) 
  output$shltripbox = renderValueBox({
    valueBox("green cabs",900000, icon = icon("fa fa-taxi"),
      color = "green")
  }) 
  
  output$ubers_etc = renderValueBox({
    valueBox("fhvs",80000, icon = icon("fa fa-taxi"),
      color = "maroon")
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
