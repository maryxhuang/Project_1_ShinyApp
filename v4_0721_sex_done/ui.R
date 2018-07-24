shinyUI(dashboardPage(
  dashboardHeader(title = "ShinyApp"),
  dashboardSidebar(
    sidebarUserPanel("Mary (Xu Huang)",image = 'nycdsa_logo2.JPG'),
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Details", tabName = "details"), 
      menuItem("Relationships", tabName = "relationships")
    ),
    sliderInput("year_selected", label = "Select Year to Display", 
                min = 1980, max = 2014, value = 2014, sep = '')
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      tabItem(tabName = "overview", 
              fluidRow(box(htmlOutput("state_top10"))),
              # fluidRow(plotOutput("test"), width = 400, height = 400),
              fluidRow(box(htmlOutput("overall")))),
      tabItem(tabName = "details", 
              tabsetPanel(
                tabPanel("Age", 
                         fluidRow(column(5, plotOutput("age_all_line")),
                                  column(7, fluidRow(tableOutput("age_all_stat1")), 
                                         fluidRow(tableOutput("age_all_stat2")))), 
                         fluidRow(column(5, plotOutput("age_spe_line")),
                                  column(7, fluidRow(tableOutput("age_spe_stat1")), 
                                         fluidRow(tableOutput("age_spe_stat2"))))), 
                tabPanel("Sex", 
                         fluidRow(column(6, plotOutput("sex_all_pie")), 
                                  column(6, plotOutput("sex_spe_pie")))),
                tabPanel("Weapon", 
                         fluidRow(plotOutput("wp_all_barperc")), 
                         fluidRow(selectInput("wp_info_selected", label = "Choose Details", 
                                              choices = list("None" = 1, "Age" = 2, "Sex" = 3), 
                                              selected = 1), 
                                  plotOutput("wp_spe_lineperc")))
              )), 
      tabItem(tabName = "relationships")
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
))