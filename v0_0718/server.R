# library(DT)
# library(shiny)
# library(googleVis)

shinyServer(function(input, output){
  # show map using googleVis
  overview_tot_data <- ushr %>% 
    group_by(., Year) %>% 
    summarise(., incidence = n())
  output$overview <- renderGvis({
    gvisLineChart(overview_tot_data, xvar = 'Year', yvar = 'incidence', 
                  options = list(width = 400, height = 200, 
                                 title = 'Total Number of Incidents',
                                 series = "[{color:'red'}]",
                                 vAxes = "[{format:'#,###'}]", 
                                 hAxes = "[{title:'Year', textPosition:'out', format:'####'}]", 
                                 legend = "none"))
  })
  
  state_top10_data <- ushr %>% 
    group_by(., State) %>% 
    summarise(., incidence = n()) %>% 
    arrange(., desc(incidence)) %>% 
    top_n(10)
  state_top10_data
  output$state_top10 <- renderGvis({
    gvisBarChart(state_top10_data, xvar = 'State', yvar = 'incidence',  
                 options = list(width = 400, height = 400, 
                                title = 'Top 10 States', 
                                vAxes = "[{showTextEvery:1, slantedText:true}]", 
                                hAxes = "[{title:'Number of Incidence', textPosition:'out', format:'#,###'}]", 
                                legend = "none"))
  })
  
  # output$map <- renderGvis({
    # gvisGeoChart(state_stat, "state.name", input$selected,
                 # options=list(region="US", displayMode="regions", 
                              # resolution="provinces",
                              # width="auto", height="auto"))
  # })
  
  # show histogram using googleVis
  # output$hist <- renderGvis({
    # gvisHistogram(state_stat[,input$selected, drop=FALSE])
  # })
  
  # show data using DataTable
  # output$table <- DT::renderDataTable({
    # datatable(state_stat, rownames=FALSE) %>% 
      # formatStyle(input$selected, background="skyblue", fontWeight='bold')
  # })
  
  # show statistics using infoBox
  # output$maxBox <- renderInfoBox({
    # max_value <- max(state_stat[,input$selected])
    # max_state <- 
      # state_stat$state.name[state_stat[,input$selected] == max_value]
    # infoBox(max_state, max_value, icon = icon("hand-o-up"))
  # })
  # output$minBox <- renderInfoBox({
    # min_value <- min(state_stat[,input$selected])
    # min_state <- 
      # state_stat$state.name[state_stat[,input$selected] == min_value]
    # infoBox(min_state, min_value, icon = icon("hand-o-down"))
  # })
  # output$avgBox <- renderInfoBox(
    # infoBox(paste("AVG.", input$selected),
            # mean(state_stat[,input$selected]), 
            # icon = icon("calculator"), fill = TRUE))
})