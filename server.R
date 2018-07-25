shinyServer(function(input, output){
  
  # MenuItem: "Overview"
  
  output$overall <- renderPlot({
    ushr %>% 
      group_by(., Year) %>% summarise(., incidence = n()) %>% 
      ggplot(aes(x = Year)) + 
      geom_line(aes(y = incidence), color = "#9933FF", size = 1) + xlab('Year') + ylab('Incidences') +
      ggtitle('Total Number of Incidents') + theme_bw() + scale_y_continuous(labels = scales::comma)
  }, width = 500, height = 300)
  
  output$state_top10 <- renderPlot({
    ushr %>% 
      group_by(., State) %>% summarise(., incidence = n(), solved = sum(Crime.Solved=="Yes"), unsolved = sum(Crime.Solved=="No")) %>% 
      top_n(10) %>% 
      ggplot(aes(x = reorder(State, incidence))) + 
      geom_bar(aes(x = reorder(State, incidence), y = incidence), fill = "#00BFC4", color = "lightblue4", stat = "identity", alpha = 0.2) + 
      geom_bar(aes(x = reorder(State, incidence), y = unsolved), fill = "#F8766D", color = "red", stat = "identity", alpha = 0.6) + 
      ggtitle("Top 10 States: Total Incidences and Unsolved Cases") + coord_flip() +
      xlab("Number of Incidence") + scale_y_continuous(labels = scales::comma, limits = c(0, 110000))
  }, width = 500, height = 300) 
  
  map_yearly_info <- reactive({
    ushr %>% 
      filter(., Year == input$year_selected) %>% 
      group_by(., State) %>% summarise(., Unsolved.Rate = sum(Crime.Solved=='No')/n())
  })
  
  output$map <- renderGvis({
    gvisGeoChart(map_yearly_info(), "State", "Unsolved.Rate",
                 options = list(region = "US", displayMode = "regions", resolution = "provinces",
                                width = "auto", height = "auto", title = 'Yearly Unsolved Rate',
                                colorAxis = "{colors:['#99CCFF', '#FFFFCC', '#FF3333']}"))
  })
  
  unsolved_ratio <- ushr %>% summarise(., unsolved_rate = sum(Crime.Solved == 'No')/n())
  output$avgBoxAall <- renderInfoBox({
    infoBox("AVG. 1980-2014 Unsolved Rate", paste0(round(unsolved_ratio$unsolved_rate*100, digits = 2), "%"), icon = icon("calculator"))
  })
  output$maxBox <- renderInfoBox({
    max_value <- max(map_yearly_info()$Unsolved.Rate)
    max_state <- map_yearly_info()$State[map_yearly_info()$Unsolved.Rate == max_value]
    infoBox(max_state, paste0(round(max_value*100, digits = 2), "%"), icon = icon("arrow-circle-up"))
  })
  output$minBox <- renderInfoBox({
    min_value <- min(map_yearly_info()$Unsolved.Rate)
    min_state <- map_yearly_info()$State[map_yearly_info()$Unsolved.Rate == min_value]
    infoBox(min_state, paste0(round(min_value*100, digits = 2), "%"), icon = icon("arrow-circle-down"))
  })
  output$avgBox <- renderInfoBox({
    mean_value <- mean(map_yearly_info()$Unsolved.Rate)
    infoBox(paste0("AVG.", input$year_selected), paste0(round(mean_value*100, digits = 2), "%"), icon = icon("balance-scale"))
  })
  
  
  
  ### MenuItem: "Details", details
  ## Age
  
  age_per_data <- ushr %>% 
    filter(., Crime.Solved == "Yes") %>% select(., Perpetrator.Age) %>% filter(., Perpetrator.Age > 5) %>% 
    group_by(., Age = Perpetrator.Age) %>% summarise(., age_per_count = n()) %>% arrange(., Age)
  age_vic_data <- ushr %>% 
    filter(., Crime.Solved == "Yes") %>% select(., Victim.Age) %>% filter(., Victim.Age < 99) %>%
    group_by(., Age = Victim.Age) %>% summarise(., age_vic_count = n()) %>% arrange(., Age)
  age_all_data <- full_join(age_per_data, age_vic_data, by='Age')
  age_all_data <- na.omit(age_all_data)
  
  output$age_all_line <- renderPlot({
    ggplot(data = age_all_data, aes(x = Age)) + theme_bw() +
      geom_line(aes(y = age_per_count, color = 'age_per_count'), size = 1) + 
      geom_line(aes(y = age_vic_count, color = 'age_vic_count'), size = 1) + 
      ggtitle('Age Distributions (all years)') + theme(legend.title = element_blank()) + xlab('Age') + ylab('Total Numbers') + scale_y_continuous(labels = scales::comma) +
      scale_colour_manual("", values=c("#F8766D","#00BFC4"), breaks=c('age_per_count', 'age_vic_count'), labels=c('Perpetrator', 'Victim'))
  }, width = 500, height = 300)
  
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
      filter(., Year == input$year_selected) %>% select(., Perpetrator.Age) %>% filter(., Perpetrator.Age > 5) %>% 
      group_by(., Age = Perpetrator.Age) %>% summarise(., age_per_count = n()) %>% arrange(., Age)
  })
  age_year_info_vic <- reactive({
    solved_cases() %>% 
      filter(., Year == input$year_selected) %>% select(., Victim.Age) %>% filter(., Victim.Age < 99) %>%
      group_by(., Age = Victim.Age) %>% summarise(., age_vic_count = n()) %>% arrange(., Age)
  })
  age_year_info <-reactive({
    full_join(age_year_info_per(), age_year_info_vic(), by='Age') %>% na.omit()
  })
  
  output$age_spe_line <- renderPlot({
    age_year_info() %>% 
      ggplot(aes(x = Age)) + theme_bw() +
      geom_line(aes(y = age_per_count, color = 'age_per_count'), size = 1) + 
      geom_line(aes(y = age_vic_count, color = 'age_vic_count'), size = 1) + 
      ggtitle('Age Distributions') + theme(legend.title = element_blank()) + 
      xlab('Age') + ylab('Total Numbers') + scale_y_continuous(labels = scales::comma) +
      scale_colour_manual("", values=c("#F8766D","#00BFC4"), breaks=c('age_per_count', 'age_vic_count'), labels=c('Perpetrator', 'Victim'))
  }, width = 500, height = 300)
  
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
  
  sex_all_info <- reactive({
    solved_cases() %>%
      select(., Perpetrator.Sex, Victim.Sex) %>% filter(., Perpetrator.Sex != "Unknown" & Victim.Sex != "Unknown") %>% 
      mutate(., Incident = ifelse(Perpetrator.Sex == "Male" & Victim.Sex == "Male", 'A. Male.killed.Male', 
                                  ifelse(Perpetrator.Sex == "Male" & Victim.Sex == "Female", 'B. Male.killed.Female', 
                                         ifelse(Perpetrator.Sex == "Female" & Victim.Sex == "Male", 'C. Female.killed.Male', 
                                                'D. Female.killed.Female')))) %>% 
      group_by(., Incident) %>% summarise(., Count = n()) %>% 
      mutate(., Percentage = Count/sum(Count)) %>% arrange(., desc(Percentage))
  })
  
  output$sex_all_pie <- renderPlot({
    sex_all_info() %>% 
      ggplot(aes(x = 1, y = Percentage)) +
      geom_bar(aes(fill = Incident), position = "fill", stat = "identity") + 
      coord_polar(theta = 'y') + ggtitle("Overall Gender Info of Perpetrators and Victims") +
      geom_text(aes(label = round(Percentage*100, digits = 1))) + scale_fill_brewer(palette = 'Set2')
  }, width = 500, height = 300)
  
  output$sex_all_tab <- renderTable({
    sex_all_info() %>% mutate(., Percentage = Percentage*100)
  })
  
  sex_spe_info <- reactive({
    solved_cases() %>%  
      filter(., Year == input$year_selected) %>% select(., Perpetrator.Sex, Victim.Sex) %>% 
      filter(., Perpetrator.Sex != "Unknown" & Victim.Sex != "Unknown") %>% 
      mutate(., Incident = ifelse(Perpetrator.Sex == "Male" & Victim.Sex == "Male", 'A. Male.killed.Male', 
                                  ifelse(Perpetrator.Sex == "Male" & Victim.Sex == "Female", 'B. Male.killed.Female', 
                                         ifelse(Perpetrator.Sex == "Female" & Victim.Sex == "Male", 'C. Female.killed.Male', 
                                                'D. Female.killed.Female')))) %>% 
      group_by(., Incident) %>% summarise(., Count = n()) %>% 
      mutate(., Percentage = Count/sum(Count)) %>% arrange(., desc(Percentage))
  })
  
  output$sex_spe_pie <- renderPlot({
    sex_spe_info() %>% 
      ggplot(aes(x = 1, y = Percentage)) +
      geom_bar(aes(fill = Incident), position = 'fill', stat = "identity") + 
      coord_polar(theta = 'y') + ggtitle("Specific Year Gender Info of Perpetrators and Victims") +
      geom_text(aes(label = round(Percentage*100, digits = 1))) + scale_fill_brewer(palette = 'Set2')
  }, width = 500, height = 300)
  
  output$sex_spe_tab <- renderTable({
    sex_spe_info() %>% mutate(., Percentage = Percentage*100)
  })
  
  
  
  ## Weapon
  
  weapon_all_data <- reactive({
    solved_cases() %>% 
    filter(., Weapon != "Unknown") %>% select(., Year, Perpetrator.Sex, Perpetrator.Age, Weapon) %>% 
    mutate(., Weapon.Type = ifelse(Weapon=="Fall", "G. Fall", 
                                   ifelse(Weapon=="Knife", "B. Knife", 
                                          ifelse(Weapon=="Blunt Object", "C. Blunt.Obj", 
                                                 ifelse(Weapon=="Poison"|Weapon=="Drug", "F. Poison.Drug", 
                                                        ifelse(Weapon=="Explosives"|Weapon=="Fire", "E. Explo.Fire", 
                                                               ifelse(Weapon=="Strangulation"|Weapon=="Suffocation"|Weapon=="Drowning", "D. Suffocation", "A. Firearm")))))))
  })
  
  output$wp_all_fillperc <- renderPlot({
    weapon_all_data() %>% 
      group_by(., Year, Weapon.Type) %>% summarise(., Weapon.Portion = n()) %>%
      ggplot(aes(x = Year, y = Weapon.Portion, group = Weapon.Type, fill = Weapon.Type)) +
      geom_area(position = 'fill') + ggtitle("Overall Weapon Use") + scale_fill_brewer(palette = 'Spectral')
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
      group_by(., Year, Weapon.Type) %>% summarise(., Weapon.Portion = n()) %>%
      ggplot(aes(x = Year, y = Weapon.Portion, group = Weapon.Type, fill = Weapon.Type)) +
      geom_area(position = 'fill') + ggtitle("Weapon Use of Specific Age and Sex Groups") +
      scale_fill_brewer(palette = 'Spectral')
  }, width = 500)
  
  
  # MenuItem: "Relationships", relationships
  
  overall_relation_data <- ushr %>% 
    filter(., Crime.Solved == "Yes" &Perpetrator.Sex != "Unknown" & Victim.Sex != "Unknown") %>% 
    select(., Perpetrator.Sex, Victim.Sex, Relationship) %>% 
    mutate(., in_short = paste0(substr(Perpetrator.Sex, 1, 1), ".k.", substr(Victim.Sex, 1, 1), ".", Relationship)) 
  
  overall_relation_data$in_short[overall_relation_data$in_short == 
                                   "M.k.M.Boyfriend/Girlfriend"] <- "M.k.M.Boyfriend"
  overall_relation_data$in_short[overall_relation_data$in_short == 
                                   "F.k.F.Boyfriend/Girlfriend"] <- "F.k.F.Girlfriend"
  overall_relation_data$in_short[overall_relation_data$in_short == 
                                   "F.k.M.Common-Law Husband"] <- "F.k.M.Husband"
  overall_relation_data$in_short[overall_relation_data$in_short == 
                                   "M.k.F.Common-Law Wife"] <- "M.k.F.Wife"
  
  relation_drop_edit <- overall_relation_data %>% 
    filter(., in_short != "M.k.F.Ex-Husband" & in_short != "F.k.F.Father" & in_short != "M.k.M.Ex-Husband" & in_short != "F.k.M.Girlfriend" & 
             in_short != "M.k.M.Stepmother" & in_short != "F.k.F.Husband" & in_short != "M.k.M.Stepdaughter" & in_short != "F.k.M.Mother" & 
             in_short != "M.k.M.Common-Law Wife" & in_short != "M.k.F.Stepfather" & in_short != "F.k.F.Wife" & in_short != "M.k.M.Common-Law Husband" & 
             in_short != "F.k.M.Wife" & in_short != "M.k.F.Boyfriend" & in_short != "M.k.F.Stepson" & in_short != "M.k.F.Husband" & 
             in_short != "M.k.M.Sister" & in_short != "M.k.M.Husband" & in_short != "M.k.M.Ex-Wife" & in_short != "M.k.F.Brother" & 
             in_short != "F.k.F.Son" & in_short != "M.k.M.Daughter" & in_short != "M.k.F.Father" & in_short != "M.k.M.Mother" & 
             in_short != "M.k.M.Girlfriend" & in_short != "M.k.F.Son" & in_short != "M.k.M.Wife") %>% 
    group_by(., in_short) %>% summarise(., tot = n()) %>% filter(., tot >= 15) %>% 
    mutate(., ps = substr(in_short, 1, 1), vs = substr(in_short, 5, 5), 
           Victim = substr(in_short, 7, nchar(in_short))) %>% 
    mutate(., Type = ifelse(Victim == "Stranger" | Victim == "Unknown", "Stranger", 
                            ifelse(Victim == "Employee" | Victim == "Employer", "Work", 
                                   ifelse(Victim == "Acquaintance" | Victim == "Neighbor", "Acquaintance",
                                          ifelse(Victim == "Friend", "Friend", 
                                                 ifelse(Victim == "Boyfriend" | Victim == "Girlfriend", "Close", "Family")))))) %>% 
    mutate(., Perpetrator = ifelse(Type == "Stranger", "Stranger", 
                                   ifelse(Victim == "Acquaintance", "Acquaintance", 
                                          ifelse(Victim == "Neighbor", "Neighbor", 
                                                 ifelse(Victim == "Employee", "Employer", 
                                                        ifelse(Victim == "Employer", "Employee", 
                                                               ifelse(Victim == "Friend", "Friend", 
                                                                      ifelse(Victim == "Family", "Family", 
                                                                             ifelse(Victim == "In-Law", "In-Law", "placeholder")))))))))
  relation_drop_edit$Victim[relation_drop_edit$Victim == 
                              "Unknown"] <- "Stranger"
  relation_drop_edit$Perpetrator[relation_drop_edit$Type == "Close" & 
                                   relation_drop_edit$ps == "M"] <- "Boyfriend"
  relation_drop_edit$Perpetrator[relation_drop_edit$Type == "Close" & 
                                   relation_drop_edit$ps == "F"] <- "Girlfriend"
  
  relation_drop_edit$Perpetrator[relation_drop_edit$Victim == "Wife"] <- "Husband"
  relation_drop_edit$Perpetrator[relation_drop_edit$Victim == "Husband"] <- "Wife"
  relation_drop_edit$Perpetrator[relation_drop_edit$Victim == "Ex-Wife"] <- "Ex-Husband"
  relation_drop_edit$Perpetrator[relation_drop_edit$Victim == "Ex-Husband"] <- "Ex-Wife"
  relation_drop_edit$Perpetrator[(relation_drop_edit$Victim == "Son" | 
                                    relation_drop_edit$Victim == "Daughter") & relation_drop_edit$ps == "M"] <- "Father"
  relation_drop_edit$Perpetrator[(relation_drop_edit$Victim == "Son" | 
                                    relation_drop_edit$Victim == "Daughter") & relation_drop_edit$ps == "F"] <- "Mother"
  relation_drop_edit$Perpetrator[(relation_drop_edit$Victim == "Stepson" | 
                                    relation_drop_edit$Victim == "Stepdaughter") & relation_drop_edit$ps == "M"] <- "Stepfather"
  relation_drop_edit$Perpetrator[(relation_drop_edit$Victim == "Stepson" | 
                                    relation_drop_edit$Victim == "Stepdaughter") & relation_drop_edit$ps == "F"] <- "Stepmother"
  relation_drop_edit$Perpetrator[(relation_drop_edit$Victim == "Father" | 
                                    relation_drop_edit$Victim == "Mother") & relation_drop_edit$ps == "M"] <- "Son"
  relation_drop_edit$Perpetrator[(relation_drop_edit$Victim == "Father" | 
                                    relation_drop_edit$Victim == "Mother") & relation_drop_edit$ps == "F"] <- "Daughter"
  relation_drop_edit$Perpetrator[(relation_drop_edit$Victim == "Stepfather" | 
                                    relation_drop_edit$Victim == "Stepmother") & relation_drop_edit$ps == "M"] <- "Stepson"
  relation_drop_edit$Perpetrator[(relation_drop_edit$Victim == "Stepfather" | 
                                    relation_drop_edit$Victim == "Stepmother") & relation_drop_edit$ps == "F"] <- "Stepdaughter"
  relation_drop_edit$Perpetrator[(relation_drop_edit$Victim == "Sister" | 
                                    relation_drop_edit$Victim == "Brother") & relation_drop_edit$ps == "M"] <- "Brother"
  relation_drop_edit$Perpetrator[(relation_drop_edit$Victim == "Sister" | 
                                    relation_drop_edit$Victim == "Brother") & relation_drop_edit$ps == "F"] <- "Sister"
  
  relation_optimize <- reactive({
    relation_drop_edit %>% 
      mutate(., Detail = ifelse(Type == "Family" | Type == "Close", paste0(Perpetrator, ".k.", Victim), 
                                paste0(ps, ".", Perpetrator, ".k.", vs, ".", Victim))) %>% 
      group_by(., Type, Detail) %>% summarise(., Total = sum(tot))
  })
  
  output$rl_all_pie <- renderPlot({
    relation_optimize() %>% 
      group_by(Type) %>% summarise(Total = sum(Total)) %>% arrange(desc(Total)) %>% 
      mutate(Percentage = Total/sum(Total)) %>% 
      mutate(Relationship = c("A. Stranger", "B. Acquaintance", "C. Family", "D. Close", "E. Friend", "F. Work")) %>% 
      ggplot(aes(x = 1, y = Percentage)) +
      geom_bar(aes(fill = Relationship), position = 'fill', stat = "identity") + 
      coord_polar(theta = 'y') + ggtitle('Types of Relationships between Perpetrators and Victims') +
      scale_fill_brewer(palette = 'Accent')
  })
  
  
  rl_spe_select <- reactive({
    if (input$rl_selected == 1) {
      relation_optimize() %>% 
        filter(., Type == "Family")
    } else if (input$rl_selected == 2) {
      relation_optimize() %>% 
        filter(., Type == "Close")
    } else if (input$rl_selected == 3) {
      relation_optimize() %>% 
        filter(., Type == "Friend")
    } else if (input$rl_selected == 4) {
      relation_optimize() %>% 
        filter(., Type == "Work")
    } else if (input$rl_selected == 5) {
      relation_optimize() %>% 
        filter(., Type == "Acquaintance")
    } else {
      relation_optimize() %>% 
        filter(., Type == "Stranger")
    }
  })
  
  rl_spe_info <- reactive({
    rl_spe_select() %>% group_by(., Detail) %>% summarise(., Total = sum(Total))
  })
  
  output$rl_spe_bar <- renderPlot({
    rl_spe_info() %>% top_n(12) %>%
      ggplot(aes(x = reorder(Detail, Total), y = Total)) + xlab('Total Number of Incidence') +
      geom_bar(aes(fill = Detail), stat = "identity", color = "#666666") + coord_flip() + 
      scale_fill_brewer(palette = 'Set3') + ggtitle('Details of Relationship Type') + 
      scale_y_continuous(labels = scales::comma)
  })
  
  output$rl_spe_tab <- renderTable({
    rl_spe_info() %>% arrange(., desc(Total))
  })
})