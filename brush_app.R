#Import Packages
library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(shinythemes)

#Read in the well data

##Well 9
well9 <- read_csv("Water_table_WS9_WS_9_wells.dat",
                  skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", 
                                          X4= "ptemp_Max" , X5= "HB156_psi", X6= "HB156_rawdepth", 
                                          X7= "HB156_depth_corr", X8= "HB156_corr_depth", X9=  "HB156_welltemp" , 
                                          X10= "HB179s_psi", X11= "HB179s_rawdepth", X12=  "HB179s_depth_corr" , 
                                          X13= "HB179s_corr_depth" , X14= "HB179s_welltemp" , 
                                          X15= "HB176d_psi" , X16= "HB176d_rawdepth", 
                                          X17= "HB176d_depth_corr" , X18= "HB176d_corr_depth", X19 = "HB176d_welltemp")) %>% 
  select(TIMESTAMP, HB156_corr_depth, HB179s_corr_depth, HB176d_corr_depth) %>% 
  pivot_longer(cols = c(HB156_corr_depth, HB179s_corr_depth, HB176d_corr_depth))

##Well 3
well3 <- read_csv("Water_table_WS3upper_WS_3Up_wells.dat",
                  skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", X4= "ptemp_Max" ,
                                          X5= "WS3_N1_psi",X6="WS3_N1_rawdepth",X7="WS3_N1_depth_corr",X8="WS3_N1_corr_depth",
                                          X9="WS3_N1_welltemp",X10="WS3_N2_psi",X11="WS3_N2_rawdepth",X12="WS3_N2_depth_corr",
                                          X13="WS3_N2_corr_depth",X14="WS3_N2_welltemp",X15="WS3_42_4_d2_psi",X16="WS3_42_4_d2_rawdepth",
                                          X17="WS3_42_4_d2_depth_corr",X18="WS3_42_4_d2_corr_depth",X19="WS3_42_4_d2_welltemp")) %>% 
  select(TIMESTAMP, WS3_N1_corr_depth, WS3_N2_corr_depth, WS3_42_4_d2_corr_depth) %>% 
  pivot_longer(cols = c(WS3_N1_corr_depth, WS3_N2_corr_depth, WS3_42_4_d2_corr_depth))

well9_snowdat_15m <- read_csv("Water_table_WS9_WS_9_snowdat_15min.dat",
                              skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", 
                                                      X4= "ptemp" , X5= "H2O_Content_1", X6= "H2O_Content_2", 
                                                      X7= "Avg_Period_1", X8= "Avg_Period_2", X9=  "RTD(1)" , X10= "RTD(2)",
                                                      X11= "RTD(3)", X12=  "RTD(4)" , X13= "RTD(5)" , X14= "RTD(6)" , X15= "RTD(7)" , 
                                                      X16= "RTD(8)", X17= "RTD(9)" , X18= "Air_TempC_Avg", X19 = "Depthraw", X20= "Depthscaled")) %>% 
  select(TIMESTAMP, H2O_Content_1, H2O_Content_2, Depthscaled) %>% 
  pivot_longer(cols = c(H2O_Content_1, H2O_Content_2, Depthscaled))

well9_snowdat_hr <- read_csv("Water_table_WS9_WS_9_snowdat_hr.dat",
                             skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="H2O_Content_1_Avg", 
                                                     X4= "H2O_Content_2_Avg", X5= "Avg_Period_1_Avg", X6= "Avg_Period_2_Avg", X7="RTD_Avg(1)", 
                                                     X8= "RTD_Avg(2)", X9=  "RTD_Avg(3)" , X10= "RTD_Avg(4)", X11= "RTD_Avg(5)", 
                                                     X12=  "RTD_Avg(6)" , X13= "RTD_Avg(7)" , X14= "RTD_Avg(8)" , X15= "RTD_Avg(9)" ,
                                                     X16= "Air_TempC_Avg", X17= "Depthraw_Avg" , X18= "Depthscaled_Avg")) %>% 
  select(TIMESTAMP, H2O_Content_2_Avg, Depthscaled_Avg) %>% 
  pivot_longer(cols = c(H2O_Content_2_Avg, Depthscaled_Avg))




well3_snowdat_15m <- read_csv("Water_table_WS3upper_WS_3Up_snowdat_15min.dat",
                              skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", X4= "ptemp" ,
                                                      X5= "H2O_Content_1", X6= "H2O_Content_2", X7= "Avg_Period_1",
                                                      X8= "Avg_Period_2", X9=  "RTD(1)" , X10= "RTD(2)", X11= "RTD(3)",
                                                      X12=  "RTD(4)" , X13= "RTD(5)" , X14= "RTD(6)" , X15= "RTD(7)" , X16= "RTD(8)",
                                                      X17= "RTD(9)" , X18= "Air_TempC_Avg", X19 = "Depthraw", X20= "Depthscaled")) %>% 
  select(TIMESTAMP, H2O_Content_1, H2O_Content_2, Depthscaled) %>% 
  pivot_longer(cols = c(H2O_Content_1, H2O_Content_2, Depthscaled))



well3_snowdat_hr <- read_csv("Water_table_WS3upper_WS_3Up_snowdat_hr.dat",
                             skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="H2O_Content_1_Avg",
                                                     X4= "H2O_Content_2_Avg", X5= "Avg_Period_1_Avg", X6= "Avg_Period_2_Avg",
                                                     X7="RTD_Avg(1)", X8= "RTD_Avg(2)", X9=  "RTD_Avg(3)" , X10= "RTD_Avg(4)",
                                                     X11= "RTD_Avg(5)", X12=  "RTD_Avg(6)" , X13= "RTD_Avg(7)" , X14= "RTD_Avg(8)" ,
                                                     X15= "RTD_Avg(9)" , X16= "Air_TempC_Avg", X17= "Depthraw_Avg" , X18= "Depthscaled_Avg")) %>% 
  select(TIMESTAMP, H2O_Content_2_Avg, Depthscaled_Avg) %>% 
  pivot_longer(cols = c(H2O_Content_2_Avg, Depthscaled_Avg))


well_data <- bind_rows(well3, well9, .id = "well")
snowdat_15m <- bind_rows(well3_snowdat_15m, well9_snowdat_15m, .id = "well")
snowdat_hr <- bind_rows(well3_snowdat_hr, well9_snowdat_hr, .id = "well")

ui <- fluidPage(
  #Added theme
  theme = shinytheme("cosmo"),
  tabsetPanel(
    tabPanel("Home Page",
             sidebarLayout(
               
               #Added Image to home page
               
             sidebarPanel(img(src = "cap_home_pic.png", style="display: block; margin-left: auto;", height = 448, width = 582)
               ),
               mainPanel()),
             #Creates App home page tab
             titlePanel("Hubbard Brook Watershed Vizualization")),
    tabPanel("Timeseries analysis",
             sidebarLayout(
               sidebarPanel(
                 dateInput("startdate", label = "Start Date", value= "2020-12-14"), 
                 dateInput("enddate", label= "End Date", value= "2021-04-01"),
                 selectInput("var1", "What well would you like to plot over time?", 
                             choices = unique(well_data$name), selected = unique(well_data$name)[1], multiple = TRUE),
                 selectInput("var2", "What snow data (15m) would you like to plot over time?", 
                             choices = unique(snowdat_15m$name), selected = unique(snowdat_15m$name)[1], multiple = TRUE),
                 selectInput("var3", "What snow data (hr) would you like to plot over time?", 
                             choices = unique(snowdat_hr$name), selected = unique(snowdat_hr$name)[1], multiple = TRUE),
               ),
    ###
    mainPanel(plotOutput("var1", width = "100%", height = "215px",
                         dblclick = "plot1_dblclick",
                         brush = brushOpts(
                           id = "plot1_brush",
                           resetOnNew = TRUE)),
                         plotOutput("var2", width = "100%", height = "215px",
                                    dblclick = "plot2_dblclick",
                                    brush = brushOpts(
                                      id = "plot2_brush",
                                      resetOnNew = TRUE)),
                         plotOutput("var3", width = "100%", height = "215px",
                                    dblclick = "plot3_dblclick",
                                    brush = brushOpts(
                                      id = "plot3_brush",
                                      resetOnNew = TRUE)),
             )
    )),
    
    ###Creates tab and tab settings for Watershed 3
    tabPanel("Bivariate",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var_x", "What variable would you like to plot on the x axis?", 
                             choices = unique(well_data$name), selected = unique(well_data$name)[1], multiple = FALSE),
                 selectInput("var_y", "What variable would you like to plot on the y axis?", 
                             choices = unique(well_data$name), selected = unique(well_data$name)[2], multiple = FALSE),
               ),
               mainPanel(plotOutput("var_x"))
             )
             
    )
  ))

server <- function(input, output, sessions) {
  
filterdate <- reactiveValues(x = ymd(c("2020-12-14", "2021-04-01")))
  
  output$var1 <- renderPlot({
    well_data %>%  filter(name %in% input$var1 & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = value, color = name)) +
      geom_line() +
      labs(title = "Timeseries Analysis of Well Data",
           x = "Time", 
           y = "Depth (cm)",
           fill = "Wells") +
      theme_bw()
  })
  
  output$welldata_plot <- renderPlot({
    var1 <- var1 %>%
      mutate(TIMESTAMP = as.POSIXct(TIMESTAMP))
    
    ggplot(data = var1, mapping = aes(x= TIMESTAMP, y = value))+
      theme_bw()
  })
  output$var2 <- renderPlot({
    snowdat_15m %>%  filter(name %in% input$var2 & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = value, color = name)) +
      geom_line() +
      labs(title = "Timeseries Analysis of Snow Data (15m)",
           x = "Time", 
           y = "Depth (cm)") +
      theme_bw()
    
  })
  
  output$var3 <- renderPlot({
    snowdat_hr %>%  filter(name %in% input$var3 & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = value, color = name)) +
      geom_line() +
      labs(title = "Timeseries Analysis of Snow Data (hr)",
           x = "Time", 
           y = "Depth (cm)") +
      theme_bw()
    
  })
  
  #BiVariate Plots
  
  output$var_x <- renderPlot({
    
    varY <- well_data %>% 
      filter(name == input$var_y)
    
    varX <- well_data %>% 
      filter(name == input$var_x)
  })
  
  output$var3 <- renderPlot({
    snowdat_hr %>%  filter(name %in% input$var3 & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = value, color = name)) +
      geom_line() +
      labs(title = "Timeseries Analysis of Snow Data (hr)",
           x = "Time", 
           y = "Depth (cm)") +
      theme_bw()
    
  })
  
  
  
  output$var_x <- renderPlot({
    
    varY <- well_data %>% 
      filter(name == input$var_y)
    
    varX <- well_data %>% 
      filter(name == input$var_x)
    
    
    
    toPlot <- inner_join(varY, varX, by = "TIMESTAMP")
    
    toPlot %>% 
      ggplot(aes(x = value.x, y = value.y, color = TIMESTAMP)) +
      geom_point() +
      scale_color_gradientn(colours = rainbow(5)) +
      labs(title = "Bivariate Analysis of Well Data",
           x = unique(toPlot$name.x),
           y = unique(toPlot$name.y))
  })
 
 observeEvent(input$startdate, {filterdate$x[1] <- ymd(input$startdate)}) 
 observeEvent(input$enddate, {filterdate$x[2] <- ymd(input$enddate)}) 
  
  
  
   # ##---Brushing---
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    
    
    if (!is.null(brush)) {
      filterdate$x <- c(brush$xmin, brush$xmax)
    } 
    
    
    else {
      filterdate$x <- c(ymd(input$startdate), ymd(input$enddate))
    }
  }
  )
  observeEvent(input$plot2_dblclick, {
    brush <- input$plot2_brush
    
    
    if (!is.null(brush)) {
      filterdate$x <- c(brush$xmin, brush$xmax)
    } 
    
    
    else {
      filterdate$x <- c(ymd(input$startdate), ymd(input$endate))
    }
  }
  )
  observeEvent(input$plot3_dblclick, {
    brush <- input$plot3_brush
    
    
    if (!is.null(brush)) {
      filterdate$x <- c(brush$xmin, brush$xmax)
    } 
    
    
    else {
      filterdate$x <- c(ymd(input$startdate), ymd(input$endate))
    }
  }
  )
  
}

shinyApp(ui, server)