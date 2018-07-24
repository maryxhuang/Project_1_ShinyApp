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
    filter(., Crime.Solved == "Yes") %>% 
    select(., Perpetrator.Age) %>% 
    filter(., Perpetrator.Age > 5) %>% 
    group_by(., Age = Perpetrator.Age) %>% 
    summarise(., age_per_count = n()) %>% 
    arrange(., Age)
  age_vic_data <- ushr %>% 
    filter(., Crime.Solved == "Yes") %>% 
    select(., Victim.Age) %>% 
    filter(., Victim.Age < 99) %>%
    group_by(., Age = Victim.Age) %>% 
    summarise(., age_vic_count = n()) %>% 
    arrange(., Age)
  age_all_data <- full_join(age_per_data, age_vic_data, by='Age')
  age_all_data <- na.omit(age_all_data)
  
  output$age_all_line <- renderPlot({
    p <- ggplot(data = age_all_data, aes(x = Age)) + 
      geom_line(aes(y = age_per_count, color = 'age_per_count'), size = 1) + 
      geom_line(aes(y = age_vic_count, color = 'age_vic_count'), size = 1) + 
      ggtitle('Age Distributions (all years)') + theme(legend.title = element_blank()) + xlab('Age') + ylab('Total Numbers') + scale_y_continuous(labels = scales::comma) +
      scale_colour_manual("", values=c("#F8766D","#00BFC4"), breaks=c('age_per_count', 'age_vic_count'), labels=c('Perpetrator', 'Victim'))
    print(p)
  }, width = 400, height = 200)
  output$age_all_stat1 <- renderTable({
    age_all_data %>% 
      summarise(., Mean.Per.Age = weighted.mean(Age, age_per_count), 
                Sd.Per.Age = sqrt(sum(age_per_count*(Age - weighted.mean(Age, age_per_count))^2)/sum(age_per_count)), 
                Mean.Vic.Age = weighted.mean(Age, age_vic_count), 
                Sd.Vic.Age = sqrt(sum(age_vic_count*(Age - weighted.mean(Age, age_vic_count))^2)/sum(age_vic_count)))
  })
  output$age_all_stat2 <- renderTable({
    age_all_data %>% 
      summarise(., Percent.Per.under18 = 100*sum(subset(age_per_count, Age < 18))/sum(age_per_count), 
                Percent.Vic.under18 = 100*sum(subset(age_vic_count, Age < 18))/sum(age_vic_count))
  })

  
  age_year_info_per <- reactive({
    ushr %>% 
      filter(., Year == input$year_selected) %>% 
      filter(., Crime.Solved == "Yes") %>% 
      select(., Perpetrator.Age) %>% 
      filter(., Perpetrator.Age > 5) %>% 
      group_by(., Age = Perpetrator.Age) %>% 
      summarise(., age_per_count = n()) %>% 
      arrange(., Age)
  })
  age_year_info_vic <- reactive({
    ushr %>% 
      filter(., Year == input$year_selected) %>% 
      filter(., Crime.Solved == "Yes") %>% 
      select(., Victim.Age) %>% 
      filter(., Victim.Age < 99) %>%
      group_by(., Age = Victim.Age) %>% 
      summarise(., age_vic_count = n()) %>% 
      arrange(., Age)
  })
  age_year_info <-reactive({
    full_join(age_year_info_per(), age_year_info_vic(), by='Age') %>%
    na.omit()
  })
  
  
  output$age_spe_line <- renderPlot({
    age_year_info() %>% 
      ggplot(aes(x = Age)) + 
      geom_line(aes(y = age_per_count, color = 'age_per_count'), size = 1) + 
      geom_line(aes(y = age_vic_count, color = 'age_vic_count'), size = 1) + 
      ggtitle('Age Distributions') + theme(legend.title = element_blank()) + xlab('Age') + ylab('Total Numbers') + scale_y_continuous(labels = scales::comma) +
      scale_colour_manual("", values=c("#F8766D","#00BFC4"), breaks=c('age_per_count', 'age_vic_count'), labels=c('Perpetrator', 'Victim'))
  }, width = 400, height = 200)
  
  output$age_spe_stat1 <- renderTable({
    age_year_info() %>% 
      summarise(., Mean.Per.Age = weighted.mean(Age, age_per_count), 
                Sd.Per.Age = sqrt(sum(age_per_count*(Age - weighted.mean(Age, age_per_count))^2)/sum(age_per_count)), 
                Mean.Vic.Age = weighted.mean(Age, age_vic_count), 
                Sd.Vic.Age = sqrt(sum(age_vic_count*(Age - weighted.mean(Age, age_vic_count))^2)/sum(age_vic_count)))
  })
  
  output$age_spe_stat2 <- renderTable({
    age_year_info() %>% 
      summarise(., Percent.Per.under18 = 100*sum(subset(age_per_count, Age < 18))/sum(age_per_count), 
                Percent.Vic.under18 = 100*sum(subset(age_vic_count, Age < 18))/sum(age_vic_count))
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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