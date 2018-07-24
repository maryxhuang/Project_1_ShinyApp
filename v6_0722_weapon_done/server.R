shinyServer(function(input, output){
  
  # MenuItem: "Overview", overview
  
  overview_tot_data <- ushr %>% 
    group_by(., Year) %>% 
    summarise(., incidence = n())
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
  
  
  
  ### MenuItem: "Details", details
  ## Age
  
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

  solved_cases <- reactive({
    ushr %>% filter(., Crime.Solved == "Yes")
  })
  
  
  age_year_info_per <- reactive({
    solved_cases() %>% 
      filter(., Year == input$year_selected) %>% 
      select(., Perpetrator.Age) %>% 
      filter(., Perpetrator.Age > 5) %>% 
      group_by(., Age = Perpetrator.Age) %>% 
      summarise(., age_per_count = n()) %>% 
      arrange(., Age)
  })
  age_year_info_vic <- reactive({
    solved_cases() %>% 
      filter(., Year == input$year_selected) %>% 
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
  
  
  
  ## Sex
  
  output$sex_all_pie <- renderPlot({
    solved_cases() %>%
    select(., Perpetrator.Sex, Victim.Sex) %>% 
    filter(., Perpetrator.Sex != "Unknown" & Victim.Sex != "Unknown") %>% 
    mutate(., Incident = ifelse(Perpetrator.Sex == "Male" & Victim.Sex == "Male", 'Male.killed.Male', 
                                ifelse(Perpetrator.Sex == "Male" & Victim.Sex == "Female", 'Male.killed.Female', 
                                       ifelse(Perpetrator.Sex == "Female" & Victim.Sex == "Male", 'Female.killed.Male', 
                                              'Female.killed.Female')))) %>% 
    group_by(., Incident) %>%
    summarise(., Count = n()) %>% 
    mutate(., Percentage = Count/sum(Count)) %>% 
    arrange(., desc(Percentage)) %>% 
    ggplot(aes(x = 1, y = Percentage)) +
      geom_bar(aes(fill = Incident), position = 'fill', stat = "identity") + 
      coord_polar(theta = 'y') + 
      geom_text(aes(label = round(Percentage*100, digits = 1)), position = position_stack(vjust = 0.5))
  }, width = 500, height = 300)
  
  
  output$sex_spe_pie <- renderPlot({
    solved_cases() %>%  
      filter(., Year == input$year_selected) %>%
      select(., Perpetrator.Sex, Victim.Sex) %>% 
      filter(., Perpetrator.Sex != "Unknown" & Victim.Sex != "Unknown") %>% 
      mutate(., Incident = ifelse(Perpetrator.Sex == "Male" & Victim.Sex == "Male", 'Male.killed.Male', 
                                  ifelse(Perpetrator.Sex == "Male" & Victim.Sex == "Female", 'Male.killed.Female', 
                                         ifelse(Perpetrator.Sex == "Female" & Victim.Sex == "Male", 'Female.killed.Male', 
                                                'Female.killed.Female')))) %>% 
      group_by(., Incident) %>%
      summarise(., Count = n()) %>% 
      mutate(., Percentage = Count/sum(Count)) %>% 
      arrange(., desc(Percentage)) %>% 
      ggplot(aes(x = 1, y = Percentage)) +
      geom_bar(aes(fill = Incident), position = 'fill', stat = "identity") + 
      coord_polar(theta = 'y') + 
      geom_text(aes(label = round(Percentage*100, digits = 1)), position = position_stack(vjust = 0.5))
  }, width = 500, height = 300)
  
  
  
  ## Weapon
  
  weapon_all_data <- reactive({
    solved_cases() %>% 
    filter(., Weapon != "Unknown") %>% 
    select(., Year, Perpetrator.Sex, Perpetrator.Age, Weapon) %>% 
    mutate(., Weapon.Type = ifelse(Weapon=="Fall", "G. Fall", 
                                   ifelse(Weapon=="Knife", "B. Knife", 
                                          ifelse(Weapon=="Blunt Object", "C. Blunt.Obj", 
                                                 ifelse(Weapon=="Poison"|Weapon=="Drug", "F. Poison.Drug", 
                                                        ifelse(Weapon=="Explosives"|Weapon=="Fire", "E. Explo.Fire", 
                                                               ifelse(Weapon=="Strangulation"|Weapon=="Suffocation"|Weapon=="Drowning", "D. Suffocation", "A. Firearm")))))))
  })
  
  output$wp_all_fillperc <- renderPlot({
    weapon_all_data() %>% 
      group_by(., Year, Weapon.Type) %>% 
      summarise(., Incidence = n()) %>%
      ggplot(aes(x = Year, y = Incidence, group = Weapon.Type, fill = Weapon.Type)) +
      geom_area(position = 'fill') +
      scale_fill_brewer(palette = 'Spectral')
  }, width = 500)
  
  weapon_spe_data <- reactive({
    if (input$wp_age_selected == 1 & input$wp_sex_selected == 1) {
      weapon_all_data() %>% 
        filter(., Perpetrator.Age >= 18 & Perpetrator.Sex == "Male" )
    } else if (input$wp_age_selected == 1 & input$wp_sex_selected == 2) {
      weapon_all_data() %>% 
        filter(., Perpetrator.Age >= 18 & Perpetrator.Sex == "Female")
    } else if (input$wp_age_selected == 2 & input$wp_sex_selected == 1) {
      weapon_all_data() %>% 
        filter(., Perpetrator.Age < 18 & Perpetrator.Sex == "Male")
    } else {
      weapon_all_data() %>% 
        filter(., Perpetrator.Age < 18 & Perpetrator.Sex == "Female")
    }
  })
  
  output$wp_spe_fillperc <- renderPlot({
    weapon_spe_data() %>% 
      group_by(., Year, Weapon.Type) %>% 
      summarise(., Incidence = n()) %>%
      ggplot(aes(x = Year, y = Incidence, group = Weapon.Type, fill = Weapon.Type)) +
      geom_area(position = 'fill') +
      scale_fill_brewer(palette = 'Spectral')
  }, width = 500)
  
  
  
  
  
  
  # wp_all_fillperc
  
  
  # MenuItem: "Relationships", relationships
  
  
})