library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(stringr)
library(dplyr)
library(rlang)
##Shiny initialization
ui <- fluidPage(
  titlePanel("User Guide"), 
  sidebarLayout(
  sidebarPanel( (
  textOutput("UserGuide"))),
  radioButtons("User Guide", "", choices = "UserGuide", selected = 0)),
  
  mainPanel("Welcome to our app!

You will find that there are several options located on the top of your screen.


The first section includes times series data. On the top left-hand side of the screen you will be able to choose the start and end dates for the data you’d like to view. The next section asks which well you’d like to view over the time period. The next two dropdown menus ask which section of snow data you’d like to view over the same time period between fifteen minute and hour intervals. The last drop down menu asks which well you’d like to see the discharge data from (option 1 being well 3 and option 2 being well 9).

The next tab is for a bivariate analysis of two variables of your choosing. On the left hand side of your screen you will find two drop down menus to choose the variables you’d like to analyze. At the bottom of the options you will be able to choose which well you’d like to view (once again option 1 being well 3 and option 2 being well 9). 

The last tab contains data related to the snow’s heat data. On the top left-hand side of the screen you will be able to pick your desired start and end dates. Below you will be able to choose which well you’d like to see over intervals of fifteen minutes and an hour (once again option 1 being well 3 and option 2 being well 9). 
 
For a closer look at any of the data you can left-click and drag your cursor around a specific section in the data to highlight it. Double-click the highlighted section and it will be brought to the focus of the plots. To undo the zoomed in view, simply double-click anywhere on the screen and the original view will reset. I hope you have found this guide useful in navigating our app!
")

server <- function(input, output) 
shinyApp(ui = ui, server = server)