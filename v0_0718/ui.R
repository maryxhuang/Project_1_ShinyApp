# library(DT)
# library(shiny)
# library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = "ShinyApp"),
  dashboardSidebar(
    sidebarUserPanel("Mary (Xu Huang)",image = 'nycdsa_logo2.JPG'),
    sidebarMenu(
      menuItem("Overview", tabName = "overview")
      # menuItem("Data", tabName = "data", icon = icon("database"))
    ),
    sliderInput("year_selected", label = "Select Year to Display", 
                min = 1980, max = 2014, value = 1980)
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      tabItem(tabName = "overview", 
              fluidRow(box(htmlOutput("state_top10"))),
              fluidRow(box(htmlOutput("overview")))
      # tabItem(tabName = "map",
              # fluidRow(infoBoxOutput("maxBox"),
                       # infoBoxOutput("minBox"),
                       # infoBoxOutput("avgBox")),
              # fluidRow(box(htmlOutput("map"), height = 300),
                       # box(htmlOutput("hist"), height = 300))),
      # tabItem(tabName = "data",
              # fluidRow(box(DT::dataTableOutput("table"), width = 12)))
      )
    )
  )
))