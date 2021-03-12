#Added theme
theme = shinytheme("cosmo"),
tabsetPanel(
  tabPanel("Home Page",
           sidebarLayout(
             
             #Added Image to home page
             
             sidebarPanel(img(src = "cap_home_pic.png", style="display: block; margin-left: auto;", height = 448, width = 582)
             ),
             mainPanel()),