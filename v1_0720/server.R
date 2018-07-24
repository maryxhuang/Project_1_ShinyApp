shinyServer(function(input, output){
  # output$value <- renderPrint({input$year_selected})
  
  # MenuItem: "Overview", overview
  overview_tot_data <- ushr %>% 
    group_by(., Year) %>% 
    summarise(., incidence = n())
  # output$test <- renderPlot({
    # print(ggplot(data = overview_tot_data, aes(x = Year, y = incidence)) + geom_line())
  # }, width = 400, height = 400)
  output$overall <- renderGvis({
    gvisLineChart(overview_tot_data, xvar = 'Year', yvar = 'incidence', 
                  options = list(width = 400, height = 200, 
                                 title = 'Total Number of Incidents',
                                 series = "[{color:'red'}]",
                                 vAxes = "[{format:'#,###'}]", 
                                 hAxes = "[{title:'Year', textPosition:'out', format:'####', sep:''}]", 
                                 legend = "none"))
  })
  state_top10_data <- ushr %>% 
    group_by(., State) %>% 
    summarise(., incidence = n()) %>% 
    arrange(., desc(incidence)) %>% 
    top_n(10)
  output$state_top10 <- renderGvis({
    gvisBarChart(state_top10_data, xvar = 'State', yvar = 'incidence',  
                 options = list(width = 400, height = 400, 
                                title = 'Top 10 States', 
                                vAxes = "[{showTextEvery:1, slantedText:true}]", 
                                hAxes = "[{title:'Number of Incidence', textPosition:'out', format:'#,###'}]", 
                                legend = "none"))
  })
  
  # MenuItem: "Details", details
  age_per_data <- ushr %>% 
    filter(., Crime.Solved == "Yes") %>% select(., Perpetrator.Age) %>% 
    filter(., Perpetrator.Age > 5) %>% group_by(., Age = Perpetrator.Age) %>% 
    summarise(., age_per_count = n()) %>% arrange(., Age)
  age_vic_data <- ushr %>% select(., Victim.Age) %>% filter(., Victim.Age < 99) %>%
    group_by(., Age = Victim.Age) %>% summarise(., age_vic_count = n()) %>% arrange(., Age)
  age_all_data <- full_join(age_per_data, age_vic_data, by='Age')
  age_all_data[is.na(age_all_data)] <- 0
  
  output$age_all_line <- renderPlot({
    p <- ggplot(data = age_all_data, aes(x = Age)) + 
      geom_line(aes(y = age_per_count, color = 'age_per_count'), size = 1) + 
      geom_line(aes(y = age_vic_count, color = 'age_vic_count'), size = 1) + 
      ggtitle('Age Distributions') + theme(legend.title = element_blank()) + xlab('Age') + ylab('Total Numbers') + scale_y_continuous(labels = scales::comma) +
      scale_colour_manual("", values=c("#F8766D","#00BFC4"), breaks=c('age_per_count', 'age_vic_count'), labels=c('Perpetrator', 'Victim'))
    print(p)
  }, width = 400, height = 200)
  age18_all_data <- age_all_data %>% 
    summarise(., Perpetrator = sum(subset(age_per_count, Age < 18))/sum(age_per_count), 
              Victim = sum(subset(age_vic_count, Age < 18))/sum(age_vic_count))
  output$age18_all <- renderText({
    p1 <- paste0("Perpetrators under 18 = ", as.character(sprintf("%.2f", age18_all_data[[1]]*100)), "%;")
    p2 <- paste0("Victimes under 18 = ", as.character(sprintf("%.2f", age18_all_data[[2]]*100)), "%")
    paste(p1, p2, sep='\n')
  })
  
  
  
  # output$age_all_hist <- renderPlot({
    
  # })
  
  
  
  # MenuItem: "Relationships", relationships
  
  
  # output$value <- renderPrint({ input$slider1 })
  
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