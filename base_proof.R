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
                       menuItem("Shifts", tabName = "shifts_main", icon = icon("dashboard")
                                ,menuSubItem("Shifts", tabName = "shifts", href = NULL, newtab = TRUE,
                                             icon = shiny::icon("angle-double-right"), selected = NULL)
                                ,menuSubItem("Shift Statistics", tabName = "shift_stats", href = NULL, newtab = TRUE,
                                             icon = shiny::icon("angle-double-right"), selected = NULL)
                              ),
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
                                 box(background="black", selectInput(inputId = "weekday", label = strong("Choose weekday"),
                                                                     choices = c('Monday'='Monday', 'Tuesday' = 'Tuesday',
                                                                                 'Wednesday'='Wednesday', 'Thursday' = 'Thursday',
                                                                                 'Friday'='Friday', 'Saturday'='Saturday', 'Sunday' = 'Sunday'), 
                                                                     multiple = FALSE, selectize = TRUE))
                                 #box(textOutput("textboxshifts"))
                               ),
                               fluidRow(
                                 box(width = 12,title = "shifts across time",
                                     status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                     plotlyOutput("plot_1"
                                                  , height = 600, width = 1500
                                                  )
                                 )
                               ),
                               fluidRow(
                                 box(dataTableOutput('datatable'), 
                                     downloadButton('downloadData', 'Download'))
                               )
                            ),
                       
                       #shift statistics------------------------------
                       
                       tabItem(tabName = "shift_stats",
                               fluidRow(
                                 sidebarLayout(
                                   sidebarPanel(
                                     dateRangeInput("monthdate2", label = h3("Date Range"),start = '2016-01-01',
                                                    end = as.Date(Sys.time())-100),
                                     selectInput("element_id1_m", "Select Your Variable for x-axis", c("mon_year"), selected = "mon_year"),
                                     selectInput("element_id2_m", "Select Your Variable for y-axis", c("avg_freq"), selected = "avg_freq"),
                                     selectInput("element_id3_m", "Select Your Grouping Variable", c("industry"), selected = 'industry')),
                                   mainPanel(h3("Outputs"),
                                             textOutput("id1_m"),
                                             textOutput("id2_m"),
                                             textOutput("id3_m"),
                                             plotlyOutput("plt_m")
                                             ,plotlyOutput("plt2_m")
                                             )
                                   
                                   
                                 ))
                               
                               # fluidRow(
                               #   box(dataTableOutput('datatable2'), 
                               #       downloadButton('downloadData2', 'Download'))
                               # )
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
    
    print(input$weekday)
    
    new_df = subset(shifts, (mon_year >= start_date1 & 
                               mon_year <= end_date1 &
                               dayz == input$weekday), c(1:7))
    print(new_df)
    new_df = new_df[, .(freq = sum(N)), by= .(mon_year, industry, quarter_hour, dayz)]
    print(new_df)
    #plot1 = plot_ly(new_df,x = shifts$quarter_hour,y = shifts$N,type = "bar")
      ggplotly(
      ggplot(new_df, aes(x = quarter_hour, y = freq, fill = industry))+
      geom_bar(stat = "identity") +
      xlab("The Quarter Hours") +
        ylab("Frequency") +
        ggtitle(paste("Most Common Shift Start Times Rounded to the Nearest Quarter Hour"))
      +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
      #+ theme(axis.title.x=element_blank(),
       #       axis.text.x=element_blank(),
        #      axis.ticks.x=element_blank()) 
      )
    #download
  }
  )
  
  output$datatable = renderDataTable({
    start_date1 = input$monthdate[1]
    end_date1 = input$monthdate[2]
    new_df = subset(shifts, (mon_year >= start_date1 & 
                               mon_year <= end_date1 &
                               dayz == input$weekday), c(1:7))
    new_df = new_df[, .(freq = sum(N)), by= .(mon_year, industry, quarter_hour, dayz)]
  }
  )
  
  
  output$downloadData = downloadHandler(
    filename = function() {
      paste('data', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(new_df, con)
    }
  )
  
  

  
  #shift statistics-------------------------------------------------------------
  
  output$plt_m = renderPlotly({
    start_date = input$monthdate2[1]
    end_date = input$monthdate2[2]
    
    shift_sum_agg = shift_sum[,.(avg_freq= mean(number_of_breaks_trips_rests))
    ,by = .(industry, mon_year, shift_cat)]
    
    
    shift_sum_agg[,mon_year:=as.Date(paste0(mon_year, "-28"))]
    td =  subset(shift_sum_agg, 
                 (shift_cat == "shift_break") & 
                   (mon_year >= start_date & mon_year <= end_date), 
                 c("industry",
                   "mon_year",
                   "shift_cat",
                   "avg_freq"))

    print(str(td))
    #td$mon_year = as.Date(td$mon_year)
    print(td)
    #td$avg_freq = as.numeric(td$avg_freq)
    
    
    
     ggplotly(ggplot(td, aes_string(x = input$element_id1_m, y = input$element_id2_m, color = input$element_id3_m,
                                    group = input$element_id3_m
                                    )) + 
                geom_line(
                  #stat = 'identity', 
                  #position = "dodge"
                  ) + 
                ggtitle("Trends in Monthly TLC Metrics")+
                scale_y_continuous(labels = comma)+
                theme(panel.background = element_rect(fill = 'black'),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.title.x = element_text(size = 13, colour = 'black'),
                      axis.title.y = element_text(size = 13, colour = 'black'),
                      axis.title = element_text(size = 18, colour = 'black'),
                      axis.text.x  = element_text(vjust=.5, size=13, angle = 90)))
    
     
     # boots = plot_ly(x = td[,input$element_id1_m], y = td[,input$element_id2_m], type = "bar", 
    #                 data = td, split = td[,input$element_id3_m]) 
    # boots = layout(boots,              # all of layout's properties: /r/reference/#layout
    #                title = "Monthly Industry Trends Over Time", # layout's title: /r/reference/#layout-title
    #                xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
    #                  title = input$element_id1_m,     # xaxis's title: /r/reference/#layout-xaxis-title
    #                  showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
    #                ),
    #                yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
    #                  title = input$element_id2_m      # yaxis's title: /r/reference/#layout-yaxis-title
    #                ))
    # 
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
