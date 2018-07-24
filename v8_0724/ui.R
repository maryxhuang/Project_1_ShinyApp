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
                         fluidRow(column(6, plotOutput("wp_all_fillperc")), 
                                  column(6, plotOutput("wp_spe_fillperc"))),
                         fluidRow(column(6), 
                                  column(3, fluidRow(selectInput("wp_sex_selected", label = "Perpetrator Sex:", 
                                                                 choices = list("Male" = 1, "Female" = 2), selected = 1)), 
                                         fluidRow(selectInput("wp_age_selected", label = "Perpetrator Age Group:", 
                                                              choices = list(">= 18" = 1, "< 18" = 2), selected = 1)))))
              )), 
      tabItem(tabName = "relationships", 
              fluidRow(column(6, plotOutput("rl_all_pie")), 
                       column(6, plotOutput("rl_spe_bar"))),
              fluidRow(column(6), 
                       column(3, fluidRow(selectInput("rl_selected", label = "Relationship Details:", 
                                                      choices = list("Family" = 1, "Close" = 2, "Friend" = 3, "Work" = 4, 
                                                                     "Acquaintance" = 5, "Stranger" = 6), selected = 1)))))
    )
  )
))