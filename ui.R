shinyUI(dashboardPage(
  dashboardHeader(title = "ShinyApp"),
  dashboardSidebar(
    sidebarUserPanel("Mary (Xu Huang)", image = 'xuhuang.jpg'),
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Details", tabName = "details"), 
      menuItem("Relationships", tabName = "relationships")
    ),
    sliderInput("year_selected", label = "Select Year to Display", 
                min = 1980, max = 2014, value = 2014, sep = '')
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css", class = "tab-pane")),
    tabItems(
      tabItem(tabName = "overview", 
              fluidRow(column(6, plotOutput("overall")), 
                       column(6, plotOutput("state_top10"))), 
              fluidRow(infoBoxOutput("avgBoxAall")), 
              fluidRow(infoBoxOutput("avgBox"), infoBoxOutput("minBox"), infoBoxOutput("maxBox")), 
              fluidRow(box(htmlOutput("map"), height = 400))
              ), 
      tabItem(tabName = "details", 
              tabsetPanel(
                tabPanel("Age", 
                         fluidRow(column(6, plotOutput("age_all_line")),
                                  column(6, fluidRow(tableOutput("age_all_stat1")), 
                                         fluidRow(tableOutput("age_all_stat2")))), 
                         fluidRow(column(6, plotOutput("age_spe_line")),
                                  column(6, fluidRow(tableOutput("age_spe_stat1")), 
                                         fluidRow(tableOutput("age_spe_stat2"))))), 
                tabPanel("Sex", 
                         fluidRow(column(6, fluidRow(plotOutput("sex_all_pie"), 
                                                     tableOutput("sex_all_tab"))), 
                                  column(6, fluidRow(plotOutput("sex_spe_pie"), 
                                                     tableOutput("sex_spe_tab"))))),
                tabPanel("Weapon", 
                         fluidRow(column(6, plotOutput("wp_all_fillperc")), 
                                  column(6, plotOutput("wp_spe_fillperc"))),
                         fluidRow(column(6, fluidRow(helpText("Firearm combines: Firearm, Rifle, Shotgun, Handgun, Gun")), 
                                         fluidRow(helpText("Suffocation combines: Suffocation, Strangulation, Drowning"))), 
                                  column(3, fluidRow(selectInput("wp_sex_selected", label = "Perpetrator Sex:", 
                                                                 choices = list("Male" = 1, "Female" = 2), selected = 1)), 
                                         fluidRow(selectInput("wp_age_selected", label = "Perpetrator Age Group:", 
                                                              choices = list(">= 18" = 1, "< 18" = 2), selected = 1)))))
              )), 
      tabItem(tabName = "relationships", 
              fluidRow(column(6, plotOutput("rl_all_pie")), 
                       column(6, plotOutput("rl_spe_bar"))),
              fluidRow(column(6, fluidRow(selectInput("rl_selected", label = "Relationship Details:", 
                                                      choices = list("Family" = 1, "Close" = 2, "Friend" = 3, "Work" = 4, 
                                                                     "Acquaintance" = 5, "Stranger" = 6), selected = 6))), 
                       column(6, tableOutput("rl_spe_tab"))))
    )
  )
))