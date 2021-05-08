library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(googledrive)

#Reading in Datasets


well9 <- read_csv("Water_table_WS9_WS_9_wells.dat",
                  skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", 
                                          X4= "ptemp_Max" , X5= "HB156_psi", X6= "HB156_rawdepth", 
                                          X7= "HB156_depth_corr", X8= "HB156", X9=  "HB156_welltemp" , 
                                          X10= "HB179s_psi", X11= "HB179s_rawdepth", X12=  "HB179s_depth_corr" , 
                                          X13= "HB179s" , X14= "HB179s_welltemp" , 
                                          X15= "HB176d_psi" , X16= "HB176d_rawdepth", 
                                          X17= "HB176d_depth_corr" , X18= "HB176d", X19 = "HB176d_welltemp")) %>% 
  select(TIMESTAMP, HB156, HB179s, HB176d) %>% 
  pivot_longer(cols = c(HB156, HB179s, HB176d))

well3 <- read_csv("Water_table_WS3upper_WS_3Up_wells.dat",
                  skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", X4= "ptemp_Max" ,
                                          X5= "WS3_N1_psi",X6="WS3_N1_rawdepth",X7="WS3_N1_depth_corr",X8="WS3_N1",
                                          X9="WS3_N1_welltemp",X10="WS3_N2_psi",X11="WS3_N2_rawdepth",X12="WS3_N2_depth_corr",
                                          X13="WS3_N2",X14="WS3_N2_welltemp",X15="WS3_42_4_d2_psi",X16="WS3_42_4_d2_rawdepth",
                                          X17="WS3_42_4_d2_depth_corr",X18="WS3_42_4_d2",X19="WS3_42_4_d2_welltemp")) %>% 
  select(TIMESTAMP, WS3_N1, WS3_N2, WS3_42_4_d2) %>% 
  pivot_longer(cols = c(WS3_N1, WS3_N2, WS3_42_4_d2))

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

#binding common well/snow data
well_data <- bind_rows(well3, well9, .id = "well")
snowdat_15m <- bind_rows(well3_snowdat_15m, well9_snowdat_15m, .id = "well")
snowdat_hr <- bind_rows(well3_snowdat_hr, well9_snowdat_hr, .id = "well")

#seperating the depth of sensor (RTD) from the rest of the data so we can use this for the heat map
well9_15_RTD <- read_csv("Water_table_WS9_WS_9_snowdat_15min.dat",
                         skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", 
                                                 X4= "ptemp" , X5= "H2O_Content_1", X6= "H2O_Content_2", 
                                                 X7= "Avg_Period_1", X8= "Avg_Period_2", X9=  "RTD1" , X10= "RTD2",
                                                 X11= "RTD3", X12=  "RTD4" , X13= "RTD5" , X14= "RTD6" , X15= "RTD7" , 
                                                 X16= "RTD8", X17= "RTD9" , X18= "Air_TempC_Avg", X19 = "Depthraw", X20= "Depthscaled")) %>% 
  select(TIMESTAMP, RTD1 , RTD2 , RTD3 , RTD4 , RTD5 ,
         RTD6 , RTD7 , RTD8, RTD9) %>% 
  pivot_longer(cols = c(RTD1 , RTD2 , RTD3 , RTD4 , RTD5 ,
                        RTD6 , RTD7 , RTD8 , RTD9))
#seperating the depthraw from the rest of the data so we can use this for the depth line plot on heat map tab
ws9_depth <- read_csv("Water_table_WS9_WS_9_snowdat_15min.dat",
                   skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", 
                                           X4= "ptemp" , X5= "H2O_Content_1", X6= "H2O_Content_2", 
                                           X7= "Avg_Period_1", X8= "Avg_Period_2", X9=  "RTD1" , X10= "RTD2",
                                           X11= "RTD3", X12=  "RTD4" , X13= "RTD5" , X14= "RTD6" , X15= "RTD7" , 
                                           X16= "RTD8", X17= "RTD9" , X18= "Air_TempC_Avg", X19 = "Depthraw", X20= "Depthscaled")) %>% 
  select(TIMESTAMP, Depthraw) %>% 
  pivot_longer(cols = c(Depthraw))

#seperating the depth of sensor (RTD) from the rest of the data so we can use this for the heat map
well9_hr_RTD <- read_csv("Water_table_WS9_WS_9_snowdat_hr.dat",
                         skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="H2O_Content_1_Avg", 
                                                 X4= "H2O_Content_2_Avg", X5= "Avg_Period_1_Avg", X6= "Avg_Period_2_Avg", X7="RTD_Avg1", 
                                                 X8= "RTD_Avg2", X9=  "RTD_Avg3" , X10= "RTD_Avg4", X11= "RTD_Avg5", 
                                                 X12=  "RTD_Avg6" , X13= "RTD_Avg7" , X14= "RTD_Avg8" , X15= "RTD_Avg9" ,
                                                 X16= "Air_TempC_Avg", X17= "Depthraw_Avg" , X18= "Depthscaled_Avg")) %>% 
  select(TIMESTAMP, RTD_Avg1 , RTD_Avg2 , RTD_Avg3 , RTD_Avg4 , RTD_Avg5 ,
         RTD_Avg6 , RTD_Avg7 , RTD_Avg8 , RTD_Avg9) %>% 
  pivot_longer(cols = c(RTD_Avg1 , RTD_Avg2 , RTD_Avg3 , RTD_Avg4 , RTD_Avg5 ,
                        RTD_Avg6 , RTD_Avg7 , RTD_Avg8 , RTD_Avg9))

well3_15_RTD <- read_csv("Water_table_WS3upper_WS_3Up_snowdat_15min.dat",
                         skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", X4= "ptemp" ,
                                                 X5= "H2O_Content_1", X6= "H2O_Content_2", X7= "Avg_Period_1",
                                                 X8= "Avg_Period_2", X9=  "RTD1" , X10= "RTD2", X11= "RTD3",
                                                 X12=  "RTD4" , X13= "RTD5" , X14= "RTD6" , X15= "RTD7" , X16= "RTD8",
                                                 X17= "RTD9" , X18= "Air_TempC_Avg", X19 = "Depthraw", X20= "Depthscaled")) %>% 
  select(TIMESTAMP, RTD1 , RTD2 , RTD3 , RTD4 , RTD5 ,
         RTD6 , RTD7 , RTD8, RTD9) %>% 
  pivot_longer(cols = c(RTD1 , RTD2 , RTD3 , RTD4 , RTD5 ,
                        RTD6 , RTD7 , RTD8 , RTD9))

#seperating the depthraw from the rest of the data so we can use this for the depth line plot on heat map tab
ws3_depth <- read_csv("Water_table_WS3upper_WS_3Up_snowdat_15min.dat",
                   skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", X4= "ptemp" ,
                                           X5= "H2O_Content_1", X6= "H2O_Content_2", X7= "Avg_Period_1",
                                           X8= "Avg_Period_2", X9=  "RTD1" , X10= "RTD2", X11= "RTD3",
                                           X12=  "RTD4" , X13= "RTD5" , X14= "RTD6" , X15= "RTD7" , X16= "RTD8",
                                           X17= "RTD9" , X18= "Air_TempC_Avg", X19 = "Depthraw", X20= "Depthscaled")) %>% 
  select(TIMESTAMP, Depthraw) %>% 
  pivot_longer(cols = c(Depthraw))

well3_hr_RTD <- read_csv("Water_table_WS3upper_WS_3Up_snowdat_hr.dat",
                         skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="H2O_Content_1_Avg",
                                                 X4= "H2O_Content_2_Avg", X5= "Avg_Period_1_Avg", X6= "Avg_Period_2_Avg",
                                                 X7="RTD_Avg1", X8= "RTD_Avg2", X9=  "RTD_Avg3" , X10= "RTD_Avg4",
                                                 X11= "RTD_Avg5", X12=  "RTD_Avg6" , X13= "RTD_Avg7" , X14= "RTD_Avg8" ,
                                                 X15= "RTD_Avg9" , X16= "Air_TempC_Avg", X17= "Depthraw_Avg" , X18= "Depthscaled_Avg")) %>% 
  select(TIMESTAMP, RTD_Avg1 , RTD_Avg2 , RTD_Avg3 , RTD_Avg4 , RTD_Avg5 ,
         RTD_Avg6 , RTD_Avg7 , RTD_Avg8 , RTD_Avg9) %>% 
  pivot_longer(cols = c(RTD_Avg1 , RTD_Avg2 , RTD_Avg3 , RTD_Avg4 , RTD_Avg5 ,
                        RTD_Avg6 , RTD_Avg7 , RTD_Avg8 , RTD_Avg9))

#reading in discharge data
discharge_3 <- read_csv("weir3_Ws_3b.dat",
                        skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt",
                                                X4= "Ptemp", X5= "WLOptical_median", X6= "Optical_WL_max",
                                                X7="Optical_WL_min", X8= "flow_equation", x9 = "Q",
                                                x10 = "Specific_Discharge", x11 = "Streamtemp")) %>% 
  select(TIMESTAMP, Specific_Discharge) %>% 
  pivot_longer(cols = c(Specific_Discharge))

#reading in discharge data
discharge_9 <- read_csv("weir9_Ws_9b.dat",
                        skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt",
                                                X4= "Ptemp", X5= "WLOptical_median", X6= "Optical_WL_max",
                                                X7="Optical_WL_min", X8= "flow_equation", x9 = "Q",
                                                x10 = "Specific_Discharge", x11 = "Streamtemp")) %>% 
  select(TIMESTAMP, Specific_Discharge) %>% 
  pivot_longer(cols = c(Specific_Discharge))

#combining discharge data so we can access both in the same table
discharge_data <- bind_rows(discharge_3, discharge_9, .id = "watershed")

#reading in watershed 3 precip data
precip <- read_csv("wxsta1_Wx_1_rain.dat",
                   skip = 4, col_names = c(x1 = "TIMESTAMP", x2 = "RECORD", x3 = "GageMinV",
                                           x4 = "ActTemp", x5 = "ActDepth", x6 = "ReportPCP", 
                                           x7 = "ODPCounts", x8 = "blockedSec", x9 = "Scan10",
                                           x10 = "ActDepthRA")) %>% 
  select(TIMESTAMP, ReportPCP) %>% 
  pivot_longer(cols = c(ReportPCP))

#reading in watershed 9 precip data
well9_Precip <- read_csv("rrg19_Rg_19-2019-08-09.dat",
                         skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="GageMinV", 
                                                 X4= "ActTemp" , X5= "ActDepth", X6= "ReportPCP", 
                                                 X7= "ODPCounts", X8= "blockedSec", X9=  "Scan10" , 
                                                 X10= "ActDepthRA")) %>% 
  select(TIMESTAMP, ReportPCP) %>% 
  pivot_longer(cols = c(ReportPCP))

#combining precip data so we can access both in the same table
precip_data <- bind_rows(precip, well9_Precip, .id = "watershed")


#combining RTD data so we access both in the same table
RTD_15 <- bind_rows(well3_15_RTD, well9_15_RTD, .id = "well")

RTD_hr <- bind_rows(well3_hr_RTD, well9_hr_RTD, .id = "well")

#creating UI with theme "slate"
ui <- fluidPage(theme = shinytheme("slate"),
                tabsetPanel(
                  #Creates App home page tab
                  tabPanel("Home Page",
                           sidebarLayout(
                             sidebarPanel(width = 1),
                             titlePanel("Hubbard Brook Watershed Vizualization")),
                           mainPanel(
                             p("Welcome to our app!"),
                             
                             p("You will find that there are several options located on the top of your screen."),
                             
                             p("The first section includes times series data. On the top left-hand side of the screen you will be able to choose the start and end dates for the data you'd like to view. The next section asks which well you'd like to view over the time period. The next two dropdown menus ask which section of snow data you'd like to view over the same time period between fifteen minute and hour intervals. The last drop down menu asks which well you'd like to see the discharge data from (option 1 being well 3 and option 2 being well 9)"),
                             
                             p("The next tab is for a bivariate analysis of two variables of your choosing. On the left hand side of your screen you will find two drop down menus to choose the variables you'd like to analyze. At the bottom of the options you will be able to choose which well you'd like to view (once again option 1 being well 3 and option 2 being well 9)"),
                             
                             
                             p("The last tab contains data related to the snow's heat data. On the top left-hand side of the screen you will be able to pick your desired start and end dates. Below you will be able to choose which well you'd like to see over intervals of fifteen minutes and an hour (once again option 1 being well 3 and option 2 being well 9)."),
                             
                             
                             p("For a closer look at any of the data you can left-click and drag your cursor around a specific section in the data to highlight it. Double-click the highlighted section and it will be brought to the focus of the plots. To undo the zoomed in view, simply double-click anywhere on the screen and the original view will reset. I hope you have found this guide useful in navigating our app!"),
                             
                             p("App created by: Zach Silvasy, Morgan Wood, Brendan Spillare, and Caleb Coleman"),
                             
                             p(tags$a(href= "https://docs.google.com/document/d/1x6CzEVKis7nVVV-l_9W5YkR8WlApJyx7CpeNnTL5k-M/edit?usp=sharing", 
                                    "Here is a link to our final report! We hope you'll enjoy our work.")),
                             
                             p(tags$a(href= "https://github.com/jpgannon/EI-Capstone-21-WaterViz.git", 
                                    "Here is a link to our GitHub repository. In this, you will find the work and tweeks we made over the semester. You can click on the app.R file in the Final Product folder to see our code.")),
                             
                             p(tags$a(href= "https://docs.google.com/document/d/12TpsbnZTTd0i9m96_wYxjpPUoaX5u8s2OpcuHerPlYc/edit?usp=sharing", 
                                      "Here is a link to our Authorship info."))
                           )),
    #Creates Timeseries tab  
    tabPanel("Timeseries analysis",
          sidebarLayout(
            sidebarPanel(
              #date inputs
              dateInput("startdate", label = "Start Date", val= "2020-12-14"), 
              dateInput("enddate", label= "End Date", value= "2021-04-15"),
              #selecting inputs for each type of plot
              selectInput("var1", "What well would you like to plot over time?", 
                          choices = unique(well_data$name), selected = unique(well_data$name)[1], multiple = TRUE),
              checkboxInput("checkbox1", "Show plot?", TRUE),
              selectInput("var2", "What snow data (15m) would you like to plot over time?", 
                          choices = unique(snowdat_15m$name), selected = unique(snowdat_15m$name)[1], multiple = TRUE),
              checkboxInput("checkbox2", "Show plot?", TRUE),
              selectInput("var3", "What snow data (hr) would you like to plot over time?", 
                          choices = unique(snowdat_hr$name), selected = unique(snowdat_hr$name)[1], multiple = TRUE),
              checkboxInput("checkbox3", "Show plot?", TRUE),
              selectInput("var_dis", "What discharge data would you like to plot over time? Watershed 3 (1) / Watershed 9 (2)",
                          choices = unique(discharge_data$watershed), selected=unique(discharge_data$watershed)[1], multiple = TRUE),
              checkboxInput("checkbox4", "Show plot?", TRUE),
              selectInput("precip_plot", "Which precipitation data would you like to plot? Watershed 3 (1) / Watershed 9 (2)",
                          choices = unique(precip_data$watershed), selected=unique(discharge_data$watershed)[1], multiple = TRUE),
              checkboxInput("checkbox5", "Show precipitation plot?", TRUE),
              actionButton("dataDL", "Download most recent data"),
            ),
            #creates the plots on the main panel with same height and width
            mainPanel(conditionalPanel(condition = "input.checkbox1 == 1",plotOutput("var1", width="100%", height = "215px",
                                                                                     dblclick = "plot1_dblclick",
                                                                                     brush = brushOpts(
                                                                                       id = "plot1_brush",
                                                                                       resetOnNew = TRUE
                                                                                     ))), 
                      conditionalPanel(condition = "input.checkbox2 == 1",plotOutput("var2", width="100%", height = "215px",
                                                                                     dblclick = "plot2_dblclick",
                                                                                     brush = brushOpts(
                                                                                       id = "plot2_brush",
                                                                                       resetOnNew = TRUE
                                                                                     ))),
                      conditionalPanel(condition = "input.checkbox3 == 1",plotOutput("var3", width="100%", height = "215px",
                                                                                     dblclick = "plot3_dblclick",
                                                                                     brush = brushOpts(
                                                                                       id = "plot3_brush",
                                                                                       resetOnNew = TRUE))),
                      conditionalPanel(condition = "input.checkbox4 == 1",plotOutput("var_dis", width="100%", height = "215px",
                                                                                     dblclick = "plot4_dblclick",
                                                                                     brush = brushOpts(
                                                                                       id = "plot4_brush",
                                                                                       resetOnNew = TRUE
                                                                                     ))),
                      conditionalPanel(condition = "input.checkbox5 == 1",plotOutput("precip_plot", width="100%", height = "215px",
                                                                                     dblclick = "plot5_dblclick",
                                                                                     brush = brushOpts(
                                                                                       id = "plot5_brush",
                                                                                       resetOnNew = TRUE
                                                                                     ))))
          )
 ),
    #Creates Bivariate Analysis tab
    tabPanel("Bivariate Analysis",
        sidebarLayout(
          sidebarPanel(
            #allows user to select input for bivariate plot analysis
            selectInput("var_x", "What variable would you like to plot on the x axis?", 
                        choices = unique(well_data$name), selected = unique(well_data$name)[1], multiple = FALSE),
            selectInput("var_y", "What variable would you like to plot on the y axis?", 
                        choices = unique(well_data$name), selected = unique(well_data$name)[2], multiple = FALSE),
            #allows user to select input for discharge plot below bivariate plot
            selectInput("var_dis2", "What discharge data would you like to plot over time? Watershed 3 (1) / Watershed 9 (2)",
                        choices = unique(discharge_data$watershed), selected=unique(discharge_data$watershed)[1], multiple = FALSE),
          ),
          mainPanel(plotOutput("var_x"), 
                    plotOutput("var_dis2", width = "100%", height = "215px", 
                               dblclick = "plot5_dblclick",
                               brush = brushOpts(
                                 id = "plot5_brush",
                                 resetOnNew = TRUE
                               )))
        )
             
    ),
  #Creates Snow Depth Heat map tab 
  tabPanel("Snow Depth Heat Map", 
           sidebarLayout(
             sidebarPanel(
              #date inputs / allows users to select date ranges
              dateInput("startdate2", label = "Start Date", val= "2020-12-14"), 
              dateInput("enddate2", label= "End Date", val = "2021-04-15"),
              #allows users to plot snow depth heat map 
              selectInput("var4", "Which well depth of sensor heat map would you like to see (15min interval)? (Watershed 3(1) / Watershed 9(2))",
                          choices = unique(RTD_15$well), selected = unique(RTD_15$well[1]), multiple = FALSE),
              selectInput("var5", "Which well depth of sensor heat map would you like to see (hr interval)? (Watershed 3(1) / Watershed 9(2))",
                          choices = unique(RTD_hr$well), selected = unique(RTD_hr$well[1]), multiple = FALSE)
             ),
             #plots heats maps and snow depth plots 
             mainPanel(plotOutput("var4", width = "100%", height = "215px",
                                  dblclick = "plot6_dblclick", 
                                  brush = brushOpts(
                                    id = "plot6_brush",
                                    resetOnNew = TRUE
                                  )),
                       plotOutput("var5", width = "100%", height = "215px",
                                  dblclick = "plot7_dblclick",
                                  brush = brushOpts(
                                    id = "plot7_brush",
                                    resetOnNew = TRUE
                                  )),
                       plotOutput("line_snow1", width = "100%", height = "215px"),
                       plotOutput("line_snow2", width = "100%", height = "215px")
                       )
           ))
  )
)

server <- function(input, output, sessions) {
  
  #reactive filter dates
  filterdate <- reactiveValues(x = ymd(c("2020-12-14", "2021-04-15")))
  
  #plots well data over time on Timeseries tab
  output$var1 <- renderPlot({
    well_data %>%  filter(name %in% input$var1 & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = value, color = name)) +
      geom_line()+
      theme_bw() +
      theme(legend.position="bottom") +
      labs(title = "Timeseries Analysis of Well Data",
            
           y = "Depth (cm)",
           fill = "Wells") +
      xlab(element_blank())
      
      
  })
  
  #plots Snow data over time (15 min interval) on Timeseries tab
  output$var2 <- renderPlot({
    snowdat_15m %>%  filter(name %in% input$var2 & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = value, color = name)) +
      geom_line() +
      theme_bw() +
      theme(legend.position="bottom") +
      labs(title = "Timeseries Analysis of Snow Data (15m)",
           x = element_blank(), 
           y = "Depth (cm)") 
      
    
  })
  #plots Snow data over time (HR interval) on Timeseries tab
  output$var3 <- renderPlot({
    snowdat_hr %>%  filter(name %in% input$var3 & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = value, color = name)) +
      theme_bw() +
      theme(legend.position="bottom") +
      geom_line() +
      labs(title = "Timeseries Analysis of Snow Data (hr)",
           x = element_blank(), 
           y = "Depth (cm)") 
    
  })
  
  #plots discharge data over time on Timeseries tab
  output$var_dis <- renderPlot({
    discharge_data %>%  filter(watershed %in% input$var_dis & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = value, color = watershed)) +
      geom_line() +
      theme_bw() +
      theme(legend.direction = "vertical", legend.position="bottom") +
      labs(title = "Timeseries Analysis of Discharge data",
           x = element_blank(), 
           y = "mm/Hr") 
    
  })
  
  #plots precipitation data over time on Timeseries tab
  output$precip_plot <- renderPlot({
    precip_data %>% filter(watershed %in% input$precip_plot & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
    ggplot(aes(x= TIMESTAMP, y= value, color = watershed)) +
      geom_bar(stat= "identity") +
      xlab(element_blank())+
      ylab("Amount of Precipitation (cm)")+
      ggtitle("Precipitation Data")+
      theme_bw() +
      theme(legend.direction = "vertical", legend.position="bottom") 
  })
  
  #plots discharge data on Bivariate analysis tab
  output$var_dis2 <- renderPlot({
    discharge_data %>%  filter(watershed %in% input$var_dis2 & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = value, color = TIMESTAMP)) +
      geom_line() +
      theme_bw() +
      theme(legend.direction = "vertical", legend.position="right") +
      labs(title = "Timeseries Analysis of Discharge data",
           x = element_blank(), 
           y = "mm/Hr") 
  })
  
  #plot bivariate plots on Bivariate analysis tab
  output$var_x <- renderPlot({

    varY <- well_data %>% 
      filter(name == input$var_y)
    
    varX <- well_data %>% 
      filter(name == input$var_x)
    
    toPlot <- inner_join(varY, varX, by = "TIMESTAMP")
   
    toPlot %>% 
      filter(TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>%
      ggplot(aes(x = value.x, y = value.y, color = TIMESTAMP)) +
      geom_point() +
      theme_bw() +
      labs(title = "Bivariate Analysis of Well Data",
           x = unique(toPlot$name.x),
           y = unique(toPlot$name.y)) 
      
  })

  #Plots snow depth heat map on Snow depth tab (15 min interval)
  output$var4 <- renderPlot({
    RTD_15 %>% filter(well == input$var4 & value != 0 & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = name, fill = value)) +
      geom_tile() +  
      scale_fill_gradient(low = "#1fddff",
                          high = "#ff4b1f",
                          space = "Lab",
                          na.value = "grey50",
                          guide = "colourbar",
                          aesthetics = "fill",
                          name = "Snow Temperature (F)") +
    labs(x = element_blank(),
         y = "Snow Depth (cm)", 
         title = "RTD Snow Depth Heat map (15 min interval)") +
      scale_y_discrete(labels = c(("RTD1" = "0"), ("RTD2" = "5"), ("RTD3" = "10"), 
                                  ("RTD4" = "15"), ("RTD5" = "20"), ("RTD6" = "25"), 
                                  ("RTD7" = "30"), ("RTD8" = "35"), ("RTD9" = "40")))+
      theme_bw()
  })
  
  #Plots snow depth heat map on Snow depth tab (HR interval)
  output$var5 <- renderPlot({
    RTD_hr %>% filter(well == input$var5 & value != 0 & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = name, fill = value)) +
      geom_tile() +  
      scale_fill_gradient(low = "#1fddff",
                          high = "#ff4b1f",
                          space = "Lab",
                          na.value = "grey50",
                          guide = "colourbar",
                          aesthetics = "fill",
                          name = "Snow Temperature (F)") +
      scale_y_discrete(labels = c(("RTD_Avg1" = "0"), ("RTD_Avg2" = "5"), ("RTD_Avg3" = "10"), 
                                  ("RTD_Avg4" = "15"), ("RTD_Avg5" = "20"), ("RTD_Avg6" = "25"), 
                                  ("RTD_Avg7" = "30"), ("RTD_Avg8" = "35"), ("RTD_Avg9" = "40")))+
      labs(x = element_blank(),
           y = "Snow Depth (cm)", 
           title = "RTD Snow Depth Heat map (HR interval)") +
      theme_bw()
  })
  
  #Plots depth of snow on Snow depth heat map tab (Watershed 3)
  output$line_snow1 <- renderPlot({
    ws3_depth %>%  filter(name == "Depthraw" & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
    ggplot(mapping = aes(x= TIMESTAMP , y = value)) +
      geom_line() +
      theme_bw() +
      labs(title = "Time Series for Snow Pack Depth (Watershed 3)" , y = "Depth (cm)") 
    
  })
   
  #Plots depth of snow on Snow depth heat map tab (Watershed 3)
  output$line_snow2 <- renderPlot({
    ws9_depth %>%  filter(name == "Depthraw" & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(mapping = aes(x= TIMESTAMP , y = value)) +
      geom_line() +
      theme_bw() +
      labs(title = "Time Series for Snow Pack Depth (Watershed 9)" , y = "Depth (cm)") 
    
  })
  
  #Downloads data from google drive
  observeEvent(input$dataDL, {
    drive_deauth()
    
    drive_download(as_id("1yhfJ7zJdumI0cMxQ7i7Zzmebo5pdEV_E"), overwrite = TRUE)
    drive_download(as_id("1wsvu3CXHj81n81eS4wbLE1dj3kpSrOqH"), overwrite = TRUE)
    drive_download(as_id("175Ai1DzFq13ut2J1JD43HExgh7n15HVS"), overwrite = TRUE)
    drive_download(as_id("1eq62MIa6n0tpeLTG-4R64q8iuhOHDDAi"), overwrite = TRUE)
    drive_download(as_id("1g3iBLteoLn_ipqhbY25UahJd--bWlwHf"), overwrite = TRUE)
    drive_download(as_id("1Ckb2L41xRAopHM3y4nnv8JmFEY6JN599"), overwrite = TRUE)
    drive_download(as_id("12CQ5lF-dU9B950eaEOYWp27slikcyNS0"), overwrite = TRUE)
    drive_download(as_id("12CVHTfrD9Qef9GB_CTn0qX-EExo-HgNw"), overwrite = TRUE)
    drive_download(as_id("12C1vsIsA7Fs6pN-EbpF20Z0Lm5YsOsiN"), overwrite = TRUE)
    drive_download(as_id("12DGM7SBNwnXPL9K8I8dYo8huvVjBfifQ"), overwrite = TRUE)
    
  })

  #date filters for brushing application
  observeEvent(input$startdate, {filterdate$x[1] <- ymd(input$startdate)}) 
  observeEvent(input$enddate, {filterdate$x[2] <- ymd(input$enddate)}) 
  
  #implementing brushing for each plot
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
      filterdate$x <- c(ymd(input$startdate), ymd(input$enddate))
    }
  }
  )
  observeEvent(input$plot3_dblclick, {
    brush <- input$plot3_brush
    
    
    if (!is.null(brush)) {
      filterdate$x <- c(brush$xmin, brush$xmax)
    } 
    
    
    else {
      filterdate$x <- c(ymd(input$startdate), ymd(input$enddate))
    }
  }
  )
  observeEvent(input$plot4_dblclick, {
    brush <- input$plot4_brush
    
    
    if (!is.null(brush)) {
      filterdate$x <- c(brush$xmin, brush$xmax)
    } 
    
    
    else {
      filterdate$x <- c(ymd(input$startdate), ymd(input$enddate))
    }
  }
  )
  observeEvent(input$plot5_dblclick, {
    brush <- input$plot5_brush
    
    
    if (!is.null(brush)) {
      filterdate$x <- c(brush$xmin, brush$xmax)
    } 
    
    
    else {
      filterdate$x <- c(ymd(input$startdate), ymd(input$enddate))
    }
  }
  ) 
  
  observeEvent(input$plot6_dblclick, {
    brush <- input$plot6_brush
    
    
    if (!is.null(brush)) {
      filterdate$x <- c(brush$xmin, brush$xmax)
    } 
    
    
    else {
      filterdate$x <- c(ymd(input$startdate), ymd(input$enddate))
    }
  }
  )
  observeEvent(input$plot7_dblclick, {
    brush <- input$plot7_brush
    
    
    if (!is.null(brush)) {
      filterdate$x <- c(brush$xmin, brush$xmax)
    } 
    
    
    else {
      filterdate$x <- c(ymd(input$startdate), ymd(input$enddate))
    }
  }
  )
}

shinyApp(ui, server)
