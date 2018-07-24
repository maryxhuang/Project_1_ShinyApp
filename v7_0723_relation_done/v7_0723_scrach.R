# library(googleVis) # library(plyr)    # library(tidyr)     # library(stringr)
# library(stringi)   # library(forcats) # library(gridExtra) # library(Hmisc)
# library(DT)

library(shiny)
library(shinydashboard)
library(googleVis)
library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)
library(datasets)


# data(state)
# rownames(state.x77)
#  [1] "Alabama"        "Alaska"         "Arizona"        "Arkansas"       "California"     "Colorado"      
#  [7] "Connecticut"    "Delaware"       "Florida"        "Georgia"        "Hawaii"         "Idaho"         
# [13] "Illinois"       "Indiana"        "Iowa"           "Kansas"         "Kentucky"       "Louisiana"     
# [19] "Maine"          "Maryland"       "Massachusetts"  "Michigan"       "Minnesota"      "Mississippi"   
# [25] "Missouri"       "Montana"        "Nebraska"       "Nevada"         "New Hampshire"  "New Jersey"    
# [31] "New Mexico"     "New York"       "North Carolina" "North Dakota"   "Ohio"           "Oklahoma"      
# [37] "Oregon"         "Pennsylvania"   "Rhode Island"   "South Carolina" "South Dakota"   "Tennessee"     
# [43] "Texas"          "Utah"           "Vermont"        "Virginia"       "Washington"     "West Virginia" 
# [49] "Wisconsin"      "Wyoming" 
# rm(state.abb, state.area, state.division, state.name, state.region, state.x77, state.center)

ushr <- read.csv('./hr_cut.csv', stringsAsFactors=F)

dim(ushr)
# [1] 638454     12
dim(ushr[complete.cases(ushr), ])
# [1] 638453     12

overview_tot_data <- ushr %>% 
  group_by(., Year) %>% 
  summarise(., incidence = n())
overview_tot_data
# A tibble: 35 x 2
#    Year incidence
#   <int>     <int>
# 1  1980     23092
# 2  1981     21208
# 3  1982     20544
# 4  1983     19653
# 5  1984     18093
# 6  1985     18386

state_top10_data <- ushr %>% 
  group_by(., State) %>% 
  summarise(., incidence = n()) %>% 
  arrange(., desc(incidence)) %>% 
  top_n(10)
state_top10_data
# A tibble: 10 x 2
#   State          incidence
#   <chr>              <int>
#  1 California         99783
#  2 Texas              62095
#  3 New York           49268
#  4 Florida            37164
#  5 Michigan           28448
#  6 Illinois           25871
#  7 Pennsylvania       24236
#  8 Georgia            21088
#  9 North Carolina     20390
# 10 Louisiana          19629

age_per_data <- ushr %>% 
  filter(., Crime.Solved == "Yes") %>% 
  select(., Perpetrator.Age) %>% 
  filter(., Perpetrator.Age > 5) %>% 
  group_by(., Age = Perpetrator.Age) %>% 
  summarise(., age_per_count = n()) %>% 
  arrange(., Age)
head(age_per_data, 4)
tail(age_per_data, 4)
# # A tibble: 4 x 2
#     Age age_per_count
#   <int>         <int>
# 1     6            50
# 2     7            48
# 3     8            62
# 4     9            86
# ......
# 1    96             7
# 2    97             3
# 3    98             6
# 4    99            38
age_vic_data <- ushr %>% 
  select(., Victim.Age) %>% 
  filter(., Victim.Age < 99) %>%
  group_by(., Age = Victim.Age) %>% 
  summarise(., age_vic_count = n()) %>% 
  arrange(., Age)
head(age_vic_data, 4)
tail(age_vic_data, 4)
# A tibble: 4 x 2
#     Age age_per_count
#   <int>         <int>
# 1     0          8444
# 2     1          5525
# 3     2          3805
# 4     3          2378
# ......
# 1    95            82
# 2    96            37
# 3    97            39
# 4    98            33

age_all_data <- full_join(age_per_data, age_vic_data, by='Age')
anyNA(age_all_data)
# [1] TRUE
age_all_data[is.na(age_all_data)] <- 0
anyNA(age_all_data)
# [1] FALSE
head(age_all_data)
# A tibble: 6 x 3
#     Age age_per_count.x age_per_count.y
#   <int>           <dbl>           <dbl>
# 1     6              50             999
# 2     7              48             915
# 3     8              62             852
# 4     9              86             834
# 5    10             145             854
# 6    11             225             911
ggplot(data = age_all_data, aes(x = Age)) + 
  geom_line(aes(y = age_per_count, color = 'age_per_count'), size = 1) + 
  geom_line(aes(y = age_vic_count, color = 'age_vic_count'), size = 1) + 
  ggtitle('Age Distributions') + 
  theme(legend.title = element_blank()) + 
  xlab('Age') + ylab('Total Count') + 
  scale_y_continuous(labels = scales::comma) +
  scale_colour_manual("", values=c("#F8766D","#00BFC4"),
                      breaks=c('age_per_count', 'age_vic_count'), 
                      labels=c('Perpetrator', 'Victim'))
age18_all_data <- age_all_data %>% 
  summarise(., Perpetrator = sum(subset(age_per_count, Age < 18))/sum(age_per_count), 
            Victim = sum(subset(age_vic_count, Age < 18))/sum(age_vic_count))
age18_all_data
#   Perpetrator Victim
#         <dbl>  <dbl>
# 1      0.0802  0.102
age18_all_data[[1]] # [1] 0.08017495
age18_all_data[[2]] # [1] 0.102173


p1 <- paste0("Perpetrators under 18 = ", as.character(sprintf("%.2f", age18_all_data[[1]]*100)), "%;")
p2 <- paste0("Victimes under 18 = ", as.character(sprintf("%.2f", age18_all_data[[2]]*100)), "%")
print(cat(paste(p1, p2, sep='\n')))

dim(ushr)
# [1] 638454     12
str(ushr)
# 'data.frame':	638454 obs. of  12 variables:
# $ Record.ID        : int  1 2 3 4 5 6 7 8 9 10 ...
# $ State            : chr  "Alaska" "Alaska" "Alaska" "Alaska" ...
# $ Year             : int  1980 1980 1980 1980 1980 1980 1980 1980 1980 1980 ...
# $ Crime.Solved     : chr  "Yes" "Yes" "No" "Yes" ...
# $ Victim.Sex       : chr  "Male" "Male" "Female" "Male" ...
# $ Victim.Age       : int  14 43 30 43 30 30 42 99 32 38 ...
# $ Perpetrator.Sex  : chr  "Male" "Male" "Unknown" "Male" ...
# $ Perpetrator.Age  : int  15 42 0 42 0 36 27 35 0 40 ...
# $ Relationship     : chr  "Acquaintance" "Acquaintance" "Unknown" "Acquaintance" ...
# $ Weapon           : chr  "Blunt Object" "Strangulation" "Unknown" "Strangulation" ...
# $ Victim.Count     : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Perpetrator.Count: int  0 0 0 0 1 0 0 0 0 1 ...
summary(ushr)
# Record.ID         State                Year      Crime.Solved        Victim.Sex       
# Min.   :     1   Length:638454      Min.   :1980   Length:638454      Length:638454     
# 1st Qu.:159614   Class :character   1st Qu.:1987   Class :character   Class :character  
# Median :319228   Mode  :character   Median :1995   Mode  :character   Mode  :character  
# Mean   :319228                      Mean   :1996                                        
# 3rd Qu.:478841                      3rd Qu.:2004                                        
# Max.   :638454                      Max.   :2014                                        
# Victim.Age     Perpetrator.Sex    Perpetrator.Age Relationship          Weapon         
# Min.   :  0.00   Length:638454      Min.   : 0.00   Length:638454      Length:638454     
# 1st Qu.: 22.00   Class :character   1st Qu.: 0.00   Class :character   Class :character  
# Median : 30.00   Mode  :character   Median :21.00   Mode  :character   Mode  :character  
# Mean   : 35.03                      Mean   :20.32                                        
# 3rd Qu.: 42.00                      3rd Qu.:31.00                                        
# Max.   :998.00                      Max.   :99.00                                        
# NA's   :1                                            
# Victim.Count     Perpetrator.Count
# Min.   : 0.0000   Min.   : 0.0000  
# 1st Qu.: 0.0000   1st Qu.: 0.0000  
# Median : 0.0000   Median : 0.0000  
# Mean   : 0.1233   Mean   : 0.1852  
# 3rd Qu.: 0.0000   3rd Qu.: 0.0000  
# Max.   :10.0000   Max.   :10.0000  


unique(ushr$Victim.Age)
#   [1]  14  43  30  42  99  32  38  36  20  48  31  16  33  27  21  60  40  18   8  25  24  46  23  22  45  26  58  17
# [29]  29  41  68  13  35  44  54  37  51  90  50  52  61  63  19  28  70  72  74  53  57  76  79  69  34  66  55  67
# [57]  65  47   1  49  62  75  56  59  78  39  15   0  71  77   2   4  64  82  85   6   5   7   9  84  10  86  11  73
# [85]  83 998  81  87  80   3  12  94  93  92  89  97  88  91  95  98  96
unique(ushr$Victim.Count)
#  [1]  0  2  1  3  6  4  5  9  7  8 10
vic_count_ratio <- ushr %>% 
  summarise(., more_vic_ratio = nrow(subset(ushr, Victim.Count != 0))/n())
vic_count_ratio
#   more_vic_ratio
# 1     0.08206543
extra_vic_abs_ratio <- ushr %>% 
  summarise(., extra_vic_abs_ratio = sum(Victim.Count)/(n()+sum(Victim.Count)))
extra_vic_abs_ratio
#   extra_vic_abs_ratio
# 1           0.1097927

unique(ushr$Perpetrator.Age)
#  [1] 15 42  0 36 27 35 40 49 39 29 19 23 33 26 41 28 61 25  7 17 34 21 43 38 66 32 37 22 30 24 65 51 60 45 64 18 20 44
# [39] 73 62 69 52 16 90 56 47 57 31 46 14 55 50 54 68 77 53 67 48 76 58 71 63 89 13 75 72 93 59 10 11 79 74 99 70 78 80
# [77]  9 12 81  6 87 82 83  8 84 85 86  5  3  1 88 95  4 91 92 96 94  2 98 97 NA
unique(ushr$Perpetrator.Count)
#  [1]  0  1  2  3  4  5  8 10  6  9  7
per_count_ratio <- ushr %>% 
  summarise(., more_vic_ratio = nrow(subset(ushr, Perpetrator.Count != 0))/n())
per_count_ratio
#   more_vic_ratio
# 1      0.1247012
extra_per_abs_ratio <- ushr %>% 
  summarise(., extra_vic_abs_ratio = sum(Perpetrator.Count)/(n()+sum(Perpetrator.Count)))
extra_per_abs_ratio
#   extra_vic_abs_ratio
# 1           0.1562776



sex_all_data <- ushr %>% 
  filter(., Crime.Solved == "Yes") %>%
  select(., Perpetrator.Sex, Victim.Sex)
dim(sex_all_data)
# [1] 448172      2
sex_all_data <- sex_all_data %>% 
  filter(., Perpetrator.Sex != "Unknown" & Victim.Sex != "Unknown")
dim(sex_all_data)
# [1] 447694      2
unique(sex_all_data$Perpetrator.Sex)
unique(sex_all_data$Victim.Sex)       # [1] "Male"   "Female", both; good

sex_summary <- sex_all_data %>% 
  summarise(., Male.killed.by.Male = sum(Perpetrator.Sex == "Male" & Victim.Sex == "Male"), 
            Female.killed.by.Male = sum(Perpetrator.Sex == "Male" & Victim.Sex == "Female"), 
            Male.killed.by.Female = sum(Perpetrator.Sex == "Female" & Victim.Sex == "Male"), 
            Female.killed.by.Female = sum(Perpetrator.Sex == "Female" & Victim.Sex == "Female"))
sex_summary
#   Male.killed.by.Male Female.killed.by.Male Male.killed.by.Female Female.killed.by.Female
# 1              299830                 99371                 37626                   10867

sex_summary2 <- sex_all_data %>% 
  mutate(., Incident = ifelse(Perpetrator.Sex == "Male" & Victim.Sex == "Male", 'Male.killed.by.Male', 
                              ifelse(Perpetrator.Sex == "Male" & Victim.Sex == "Female", 'Female.killed.by.Male', 
                                     ifelse(Perpetrator.Sex == "Female" & Victim.Sex == "Male", 'Male.killed.by.Female', 
                                            'Female.killed.by.Female')))) %>% 
  group_by(., Incident) %>%
  summarise(., Count = n()) %>% 
  mutate(., Percentage = Count/sum(Count)) %>% 
  arrange(., desc(Percentage))
sex_summary2
# A tibble: 4 x 3
#   Incident                 Count Percentage
#   <chr>                    <int>      <dbl>
# 1 Male.killed.by.Male     299830     0.670 
# 2 Female.killed.by.Male    99371     0.222 
# 3 Male.killed.by.Female    37626     0.0840
# 4 Female.killed.by.Female  10867     0.0243

sex_summary2$Incident <- factor(sex_summary2$Incident, levels = sex_summary2$Incident[order(sex_summary2$Percentage)])
ggplot(data = sex_summary2, aes(x = 1, y = Percentage)) +
  geom_bar(aes(fill = Incident), position = 'fill', stat = "identity") + 
  coord_polar(theta = 'y') + 
  geom_text(aes(label = round(Percentage*100, digits = 1)), position = position_stack(vjust = 0.5))


unique(ushr$Weapon)
# [1] "Blunt Object"  "Strangulation" "Unknown"       "Rifle"         "Knife"         "Firearm"       "Shotgun"      
# [8] "Fall"          "Handgun"       "Drowning"      "Suffocation"   "Explosives"    "Fire"          "Drugs"        
# [15] "Gun"           "Poison"  
dim(ushr %>% filter(., Crime.Solved == 'Yes'))
# [1] 448172     12
dim(ushr %>% filter(., Crime.Solved == 'Yes' & Weapon != 'Unknown'))
# [1] 429763     12

# "Unknown"
# "Poison" "Drugs"
# "Fall"
# "Strangulation" "Drowning" "Suffocation"
# "Explosives" "Fire"
# "Blunt Object"
# "Knife"
# "Rifle" "Firearm" "Shotgun" "Handgun" "Gun"

weapon_all_data <- ushr %>% 
  filter(., Crime.Solved == "Yes" & Weapon != "Unknown") %>% 
  select(., Year, Perpetrator.Sex, Weapon) %>% 
  mutate(., Weapon.Type = ifelse(Weapon=="Fall", "G. Fall", 
                                 ifelse(Weapon=="Knife", "B. Knife", 
                                        ifelse(Weapon=="Blunt Object", "C. Blunt.Obj", 
                                               ifelse(Weapon=="Poison"|Weapon=="Drug", "F. Poison.Drug", 
                                                      ifelse(Weapon=="Explosives"|Weapon=="Fire", "E. Explo.Fire", 
                                                             ifelse(Weapon=="Strangulation"|Weapon=="Suffocation"|Weapon=="Drowning", "D. Suffocation", "A. Firearm")))))))
head(weapon_all_data)
#   Year Perpetrator.Sex        Weapon    Weapon.Type
# 1 1980            Male  Blunt Object   C. Blunt.Obj
# 2 1980            Male Strangulation D. Suffocation
# 3 1980            Male Strangulation D. Suffocation
# 4 1980            Male         Rifle     A. Firearm
# 5 1980            Male         Knife       B. Knife
# 6 1980            Male         Knife       B. Knife
weapon_all_data_sum <- weapon_all_data %>%
  group_by(., Year, Weapon.Type) %>% 
  summarise(., Incidence = n())
head(weapon_all_data_sum)
# A tibble: 6 x 3
# Groups:   Year [1]
#    Year Weapon.Type    Incidence
#   <int> <chr>              <int>
# 1. 1980 A. Firearm         11027
# 2  1980 B. Knife            3179
# 3  1980 C. Blunt.Obj        1691
# 4  1980 D. Suffocation       280
# 5  1980 E. Explo.Fire        187
# 6  1980 F. Poison.Drug        14

library(RColorBrewer)
ggplot(weapon_all_data_sum, aes(x = Year, y = Incidence, group = Weapon.Type, fill = Weapon.Type)) +
  geom_area(position = 'fill') +
  scale_fill_brewer(palette = 'Spectral')
  # scale_fill_brewer(color = 'black', size = 0.2, palette = 'Spectral')

weapon_M_data <- weapon_all_data %>% 
  filter(., Perpetrator.Sex == "Male") %>%
  group_by(., Year, Weapon.Type) %>% 
  summarise(., Incidence = n())
head(weapon_M_data)
# A tibble: 6 x 3
# Groups:   Year [1]
#    Year Weapon.Type    Incidence
#   <int> <chr>              <int>
# 1  1980 A. Firearm          9657
# 2  1980 B. Knife            2540
# 3  1980 C. Blunt.Obj        1535
# 4  1980 D. Suffocation       229
# 5  1980 E. Explo.Fire        154
# 6  1980 F. Poison.Drug         8
ggplot(weapon_M_data, aes(x = Year, y = Incidence, group = Weapon.Type, fill = Weapon.Type)) +
  geom_area(position = 'fill') +
  scale_fill_brewer(palette = 'Spectral')

weapon_F_data <- weapon_all_data %>% 
  filter(., Perpetrator.Sex == "Female") %>%
  group_by(., Year, Weapon.Type) %>% 
  summarise(., Incidence = n())
head(weapon_F_data)
# A tibble: 6 x 3
# Groups:   Year [1]
#    Year Weapon.Type    Incidence
#   <int> <chr>              <int>
# 1  1980 A. Firearm          1370
# 2  1980 B. Knife             639
# 3  1980 C. Blunt.Obj         156
# 4  1980 D. Suffocation        51
# 5  1980 E. Explo.Fire         33
# 6  1980 F. Poison.Drug         6
ggplot(weapon_F_data, aes(x = Year, y = Incidence, group = Weapon.Type, fill = Weapon.Type)) +
  geom_area(position = 'fill') +
  scale_fill_brewer(palette = 'Spectral')




library(reshape2)
data <- read.csv('E:/E-Documents/NYC_Data_Science_Academy/0711_DV_and_CG_with_ggplot2/soft_impact.csv', TRUE)
head(data)
#   Year  JMP Minitab  SPSS   SAS Stata Systat  R
# 1 1995  257    1150  6450  8630    22   2480  8
# 2 1996  370    1290  7600  8700    91   2510  2
# 3 1997  550    1400 10500 10200   205   3390  6
# 4 1998  690    1460 14500 11100   322   2700 13
# 5 1999  865    1670 24300 12700   516   2650 25
# 6 2000 1060    1890 45600 16500   784   2780 51
dim(data)
# [1] 17  8
data.melt <- melt(data, id='Year')
head(data.melt)
#   Year variable value
# 1 1995      JMP   257
# 2 1996      JMP   370
# 3 1997      JMP   550
# 4 1998      JMP   690
# 5 1999      JMP   865
# 6 2000      JMP  1060
dim(data.melt)
# [1] 119   3
p <- ggplot(data.melt, aes(x = Year, y = value, group = variable, fill = variable)) +
  geom_area(color = 'black', size = 0.3, position = position_fill()) +
  scale_fill_brewer()
p #  g205_09.png





unique(ushr$Relationship)
#  [1] "Acquaintance"         "Unknown"              "Wife"                 "Stranger"             "Girlfriend"          
#  [6] "Ex-Husband"           "Brother"              "Stepdaughter"         "Husband"              "Sister"              
# [11] "Friend"               "Family"               "Neighbor"             "Father"               "In-Law"              
# [16] "Son"                  "Ex-Wife"              "Boyfriend"            "Mother"               "Common-Law Husband"  
# [21] "Common-Law Wife"      "Stepfather"           "Stepson"              "Stepmother"           "Daughter"            
# [26] "Boyfriend/Girlfriend" "Employer"             "Employee"    

test_str = "Male"
substr(test_str, 1, 1)

overall_relation_data <- ushr %>% 
  filter(., Crime.Solved == "Yes" & Perpetrator.Sex != "Unknown" & Victim.Sex != "Unknown") %>% 
  select(., Perpetrator.Sex, Victim.Sex, Relationship) %>% 
  mutate(., in_short = paste0(substr(Perpetrator.Sex, 1, 1), ".k.", substr(Victim.Sex, 1, 1), ".", Relationship))
head(overall_relation_data)
#   Perpetrator.Sex Victim.Sex Relationship           in_short
# 1            Male       Male Acquaintance M.k.M.Acquaintance
# 2            Male       Male Acquaintance M.k.M.Acquaintance
# 3            Male       Male Acquaintance M.k.M.Acquaintance
# 4            Male       Male Acquaintance M.k.M.Acquaintance
# 5            Male     Female         Wife         M.k.F.Wife
# 6            Male     Female         Wife         M.k.F.Wife
unique(overall_relation_data$in_short)
# [1] "M.k.M.Acquaintance"         "M.k.F.Wife"                 "M.k.M.Unknown"             
# [4] "M.k.M.Stranger"             "M.k.F.Girlfriend"           "F.k.M.Ex-Husband"          
# [7] "M.k.M.Brother"              "M.k.F.Stepdaughter"         "M.k.M.Stepdaughter"        
# [10] "F.k.M.Husband"              "F.k.M.Brother"              "M.k.F.Sister"              
# [13] "M.k.F.Unknown"              "M.k.M.Friend"               "M.k.M.Family"              
# [16] "M.k.F.Friend"               "M.k.M.Neighbor"             "M.k.F.Acquaintance"        
# [19] "M.k.M.Father"               "M.k.M.In-Law"               "F.k.M.Acquaintance"        
# [22] "F.k.M.Son"                  "F.k.F.Family"               "M.k.F.Neighbor"            
# [25] "M.k.F.Ex-Wife"              "F.k.F.Friend"               "F.k.M.Neighbor"            
# [28] "M.k.M.Husband"              "F.k.M.Boyfriend"            "M.k.F.Mother"              
# [31] "F.k.F.Acquaintance"         "F.k.M.Common-Law Husband"   "M.k.F.In-Law"              
# [34] "M.k.M.Son"                  "M.k.F.Stranger"             "M.k.F.Common-Law Wife"     
# [37] "M.k.F.Family"               "F.k.M.Stepfather"           "F.k.M.Father"              
# [40] "M.k.M.Stepfather"           "M.k.F.Son"                  "M.k.M.Stepson"             
# [43] "M.k.F.Stepmother"           "F.k.F.Mother"               "F.k.M.Stranger"            
# [46] "F.k.F.Daughter"             "M.k.F.Daughter"             "F.k.M.Unknown"             
# [49] "M.k.M.Boyfriend/Girlfriend" "M.k.M.Mother"               "F.k.M.Friend"              
# [52] "F.k.F.Sister"               "F.k.F.Unknown"              "F.k.M.Family"              
# [55] "M.k.M.Employer"             "F.k.M.Daughter"             "M.k.F.Father"              
# [58] "F.k.F.Neighbor"             "M.k.M.Common-Law Husband"   "M.k.F.Brother"             
# [61] "M.k.M.Ex-Wife"              "M.k.M.Daughter"             "M.k.M.Wife"                
# [64] "M.k.M.Girlfriend"           "F.k.F.In-Law"               "F.k.F.Son"                 
# [67] "M.k.M.Stepmother"           "M.k.M.Employee"             "F.k.M.In-Law"              
# [70] "F.k.F.Stranger"             "M.k.M.Boyfriend"            "F.k.F.Boyfriend/Girlfriend"
# [73] "F.k.M.Stepson"              "M.k.F.Boyfriend"            "M.k.M.Sister"              
# [76] "F.k.M.Employee"             "F.k.F.Girlfriend"           "F.k.F.Stepdaughter"        
# [79] "F.k.F.Father"               "F.k.M.Mother"               "M.k.F.Employee"            
# [82] "M.k.F.Stepson"              "F.k.F.Husband"              "F.k.M.Girlfriend"          
# [85] "F.k.M.Wife"                 "M.k.M.Common-Law Wife"      "F.k.M.Employer"            
# [88] "M.k.F.Common-Law Husband"   "M.k.F.Stepfather"           "M.k.F.Husband"             
# [91] "F.k.M.Common-Law Wife"      "F.k.F.Stepmother"           "F.k.F.Wife"                
# [94] "F.k.F.Common-Law Wife"      "M.k.F.Ex-Husband"           "F.k.M.Sister"              
# [97] "M.k.M.Ex-Husband"           "F.k.F.Ex-Wife"              "F.k.F.Employer"            
# [100] "F.k.F.Employee"             "F.k.M.Ex-Wife"              "F.k.F.Boyfriend"           
# [103] "F.k.F.Stepfather"           "M.k.F.Employer"             "F.k.M.Stepmother"          
# [106] "F.k.M.Stepdaughter"         "F.k.F.Brother"              "F.k.M.Boyfriend/Girlfriend"
# [109] "F.k.F.Common-Law Husband"   "F.k.F.Ex-Husband"           "M.k.F.Boyfriend/Girlfriend"

dim(overall_relation_data)
# [1] 447694      4
colnames(overall_relation_data)
# [1] "Perpetrator.Sex" "Victim.Sex"      "Relationship"    "in_short" 
drop_choice <- overall_relation_data %>% 
  group_by(., in_short) %>% 
  summarise(., tot = n()) %>%
  arrange(., desc(tot)) %>% 
  print(n=111)
print(drop_choice) # saved to notes

# in_short != "" & in_short != "" & in_short != "" & in_short != ""

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
  group_by(., in_short) %>% 
  summarise(., tot = n()) %>%
  filter(., tot >= 15)
  
unique(relation_drop_edit$in_short)
# [1] "F.k.F.Acquaintance" "F.k.F.Daughter"     "F.k.F.Employee"     "F.k.F.Employer"     "F.k.F.Family"      
# [6] "F.k.F.Friend"       "F.k.F.Girlfriend"   "F.k.F.In-Law"       "F.k.F.Mother"       "F.k.F.Neighbor"    
# [11] "F.k.F.Sister"       "F.k.F.Stepdaughter" "F.k.F.Stepmother"   "F.k.F.Stranger"     "F.k.F.Unknown"     
# [16] "F.k.M.Acquaintance" "F.k.M.Boyfriend"    "F.k.M.Brother"      "F.k.M.Daughter"     "F.k.M.Employee"    
# [21] "F.k.M.Employer"     "F.k.M.Ex-Husband"   "F.k.M.Family"       "F.k.M.Father"       "F.k.M.Friend"      
# [26] "F.k.M.Husband"      "F.k.M.In-Law"       "F.k.M.Neighbor"     "F.k.M.Son"          "F.k.M.Stepfather"  
# [31] "F.k.M.Stepson"      "F.k.M.Stranger"     "F.k.M.Unknown"      "M.k.F.Acquaintance" "M.k.F.Daughter"    
# [36] "M.k.F.Employee"     "M.k.F.Employer"     "M.k.F.Ex-Wife"      "M.k.F.Family"       "M.k.F.Friend"      
# [41] "M.k.F.Girlfriend"   "M.k.F.In-Law"       "M.k.F.Mother"       "M.k.F.Neighbor"     "M.k.F.Sister"      
# [46] "M.k.F.Stepdaughter" "M.k.F.Stepmother"   "M.k.F.Stranger"     "M.k.F.Unknown"      "M.k.F.Wife"        
# [51] "M.k.M.Acquaintance" "M.k.M.Boyfriend"    "M.k.M.Brother"      "M.k.M.Employee"     "M.k.M.Employer"    
# [56] "M.k.M.Family"       "M.k.M.Father"       "M.k.M.Friend"       "M.k.M.In-Law"       "M.k.M.Neighbor"    
# [61] "M.k.M.Son"          "M.k.M.Stepfather"   "M.k.M.Stepson"      "M.k.M.Stranger"     "M.k.M.Unknown"         

# Strangers <- Unknown, Stranger
# Work <- Employee, Employer
# Acquaintance <- Acquaintance, Neighbor
# Friend <- Friend
# Close <- Boyfriend, Girlfriend
# Family <- Family, Mother, Stepmother, Father, Stepson, In-Law, Daughter, Brother, Son, Sister, Stepfather
#           Husband, Ex-Husband, Stepdaughter, Wife, Ex-Wife

# relation_optimize <- relation_drop_edit %>% 
  # mutate(., ps = substr(in_short, 1, 1), vs = substr(in_short, 5, 5), 
         # Victim = substr(in_short, 7, nchar(in_short)))
         #Victim = strsplit(in_short, split="\\.")[[1]][4])
# head(relation_optimize)
# A tibble: 6 x 5
#   in_short             tot ps    vs    Victim      
#   <chr>              <int> <chr> <chr> <chr>       
# 1 F.k.F.Acquaintance  2505 F     F     Acquaintance
# 2 F.k.F.Daughter      3433 F     F     Daughter    
# 3 F.k.F.Employee        16 F     F     Employee    
# 4 F.k.F.Employer        17 F     F     Employer    
# 5 F.k.F.Family         542 F     F     Family      
# 6 F.k.F.Friend         576 F     F     Friend 



relation_optimize <- relation_drop_edit %>% 
  mutate(., ps = substr(in_short, 1, 1), vs = substr(in_short, 5, 5), 
         Victim = substr(in_short, 7, nchar(in_short))) %>% 
  mutate(., Type = ifelse(Victim == "Stranger" | Victim == "Unknown", "Stranger", 
         ifelse(Victim == "Employee" | Victim == "Employer", "Work", 
                ifelse(Victim == "Acquaintance" | Victim == "Neighbor", "Acquaintance",
                       ifelse(Victim == "Friend", "Friend", 
                              ifelse(Victim == "Boyfriend" | Victim == "Girlfriend", "Close", "Family"))))))
head(relation_optimize)
# A tibble: 6 x 6
#   in_short             tot ps    vs    Victim       Type        
#   <chr>              <int> <chr> <chr> <chr>        <chr>       
# 1 F.k.F.Acquaintance  2505 F     F     Acquaintance Acquaintance
# 2 F.k.F.Daughter      3433 F     F     Daughter     Family      
# 3 F.k.F.Employee        16 F     F     Employee     Work        
# 4 F.k.F.Employer        17 F     F     Employer     Work        
# 5 F.k.F.Family         542 F     F     Family       Family      
# 6 F.k.F.Friend         576 F     F     Friend       Friend
unique(relation_optimize$Type)
# [1] "Acquaintance" "Family"       "Work"         "Friend"       "Close"        "Stranger" 
unique(relation_optimize$Victim)
# [1] "Acquaintance" "Daughter"     "Employee"     "Employer"     "Family"       "Friend"       "Girlfriend"  
# [8] "In-Law"       "Mother"       "Neighbor"     "Sister"       "Stepdaughter" "Stepmother"   "Stranger"    
# [15] "Unknown"      "Boyfriend"    "Brother"      "Ex-Husband"   "Father"       "Husband"      "Son"         
# [22] "Stepfather"   "Stepson"      "Ex-Wife"      "Wife" 

relation_optimize2 <- relation_optimize %>% 
  mutate(., Perpetrator = ifelse(Type == "Stranger", "Stranger", 
                                 ifelse(Victim == "Acquaintance", "Acquaintance", 
                                        ifelse(Victim == "Neighbor", "Neighbor", 
                                               ifelse(Victim == "Employee", "Employer", 
                                                      ifelse(Victim == "Employer", "Employee", 
                                                             ifelse(Victim == "Friend", "Friend", 
                                                                    ifelse(Victim == "Family", "Family", 
                                                                           ifelse(Victim == "In-Law", "In-Law", "placeholder")))))))))
head(relation_optimize2, 3)
# A tibble: 3 x 7
#   in_short             tot ps    vs    Victim       Type         Perpetrator
#   <chr>              <int> <chr> <chr> <chr>        <chr>        <chr>      
# 1 F.k.F.Acquaintance  2505 F     F     Acquaintance Acquaintance Acquaintance
# 2 F.k.F.Daughter      3433 F     F     Daughter     Family       placeholder 
# 3 F.k.F.Employee        16 F     F     Employee     Work         Employer 
unique(relation_optimize2$Perpetrator)
# [1] "Acquaintance" "placeholder"  "Employer"     "Employee"     "Family"       "Friend"       "In-Law"      
# [8] "Neighbor"     "Stranger"  

relation_optimize2$Perpetrator[relation_optimize2$Type == "Close" & 
                                 relation_optimize2$ps == "M"] <- "Boyfriend"
relation_optimize2$Perpetrator[relation_optimize2$Type == "Close" & 
                                 relation_optimize2$ps == "F"] <- "Girlfriend"
unique(relation_optimize2$Perpetrator)
# [1] "Acquaintance" "placeholder"  "Employer"     "Employee"     "Family"       "Friend"       "Girlfriend"  
# [8] "In-Law"       "Neighbor"     "Stranger"     "Boyfriend"

#  "Sister" "Stepmother"
# "Brother"      
# "Stepfather"  

unique(relation_optimize2$Victim)
# [1] "Acquaintance" "Daughter"     "Employee"     "Employer"     "Family"       "Friend"       "Girlfriend"  
# [8] "In-Law"       "Mother"       "Neighbor"     "Sister"       "Stepdaughter" "Stepmother"   "Stranger"    
# [15] "Unknown"      "Boyfriend"    "Brother"      "Ex-Husband"   "Father"       "Husband"      "Son"         
# [22] "Stepfather"   "Stepson"      "Ex-Wife"      "Wife" 

relation_optimize2$Perpetrator[relation_optimize2$Victim == "Wife"] <- "Husband"
relation_optimize2$Perpetrator[relation_optimize2$Victim == "Husband"] <- "Wife"
relation_optimize2$Perpetrator[relation_optimize2$Victim == "Ex-Wife"] <- "Ex-Husband"
relation_optimize2$Perpetrator[relation_optimize2$Victim == "Ex-Husband"] <- "Ex-Wife"
relation_optimize2$Perpetrator[(relation_optimize2$Victim == "Son" | 
                                  relation_optimize2$Victim == "Daughter") & relation_optimize2$ps == "M"] <- "Father"
relation_optimize2$Perpetrator[(relation_optimize2$Victim == "Son" | 
                                  relation_optimize2$Victim == "Daughter") & relation_optimize2$ps == "F"] <- "Mother"
relation_optimize2$Perpetrator[(relation_optimize2$Victim == "Stepson" | 
                                  relation_optimize2$Victim == "Stepdaughter") & relation_optimize2$ps == "M"] <- "Stepfather"
relation_optimize2$Perpetrator[(relation_optimize2$Victim == "Stepson" | 
                                  relation_optimize2$Victim == "Stepdaughter") & relation_optimize2$ps == "F"] <- "Stepmother"
relation_optimize2$Perpetrator[(relation_optimize2$Victim == "Father" | 
                                  relation_optimize2$Victim == "Mother") & relation_optimize2$ps == "M"] <- "Son"
relation_optimize2$Perpetrator[(relation_optimize2$Victim == "Father" | 
                                  relation_optimize2$Victim == "Mother") & relation_optimize2$ps == "F"] <- "Daughter"
relation_optimize2$Perpetrator[(relation_optimize2$Victim == "Stepfather" | 
                                  relation_optimize2$Victim == "Stepmother") & relation_optimize2$ps == "M"] <- "Stepson"
relation_optimize2$Perpetrator[(relation_optimize2$Victim == "Stepfather" | 
                                  relation_optimize2$Victim == "Stepmother") & relation_optimize2$ps == "F"] <- "Stepdaughter"
relation_optimize2$Perpetrator[(relation_optimize2$Victim == "Sister" | 
                                  relation_optimize2$Victim == "Brother") & relation_optimize2$ps == "M"] <- "Brother"
relation_optimize2$Perpetrator[(relation_optimize2$Victim == "Sister" | 
                                  relation_optimize2$Victim == "Brother") & relation_optimize2$ps == "F"] <- "Sister"
unique(relation_optimize2$Perpetrator)
# relation_optimize2 %>% filter(., Perpetrator == "placeholder") --> 0, good

head(relation_optimize2)
# A tibble: 6 x 7
#   in_short             tot ps    vs    Victim       Type         Perpetrator 
#   <chr>              <int> <chr> <chr> <chr>        <chr>        <chr>       
# 1 F.k.F.Acquaintance  2505 F     F     Acquaintance Acquaintance Acquaintance
# 2 F.k.F.Daughter      3433 F     F     Daughter     Family       Mother      
# 3 F.k.F.Employee        16 F     F     Employee     Work         Employer    
# 4 F.k.F.Employer        17 F     F     Employer     Work         Employee    
# 5 F.k.F.Family         542 F     F     Family       Family       Family      
# 6 F.k.F.Friend         576 F     F     Friend       Friend       Friend   
unique(relation_optimize2$Type)
# [1] "Acquaintance" "Family"       "Work"         "Friend"       "Close"        "Stranger"

relation_optimize3 <- relation_optimize2 %>% 
  mutate(., Detail = ifelse(Type == "Family" | Type == "Close", paste0(Perpetrator, ".k.", Victim), 
                            paste0(ps, ".", Perpetrator, ".k.", vs, ".", Victim))) %>% 
  group_by(., Type, Detail) %>% 
  summarise(., Total = sum(tot))
head(relation_optimize3)
# A tibble: 6 x 3
# Groups:   Type [1]
#   Type         Detail                          Total
#   <chr>        <chr>                           <int>
# 1 Acquaintance F.Acquaintance.k.F.Acquaintance  2505
# 2 Acquaintance F.Acquaintance.k.M.Acquaintance  6217
# 3 Acquaintance F.Neighbor.k.F.Neighbor           211
# 4 Acquaintance F.Neighbor.k.M.Neighbor           271
# 5 Acquaintance M.Acquaintance.k.F.Acquaintance 16121
# 6 Acquaintance M.Acquaintance.k.M.Acquaintance 98980

relation_type_all <- relation_optimize3 %>% 
  group_by(., Type) %>% 
  summarise(., Total = sum(Total)) %>%
  arrange(., desc(Total)) %>% 
  mutate(., Percentage = Total/sum(Total)) %>% 
  mutate(., Relationship = c("A. Stranger", "B. Acquaintance", "C. Family", "D. Close", "E. Friend", "F. Work"))
relation_type_all
# # A tibble: 6 x 2
#   Type          Total Percentage Relationship  
#   <chr>         <int>      <dbl> <chr>         
# 1 Stranger     181862    0.409   A. Stranger    
# 2 Acquaintance 130078    0.293   B. Acquaintance
# 3 Family        85206    0.192   C. Family      
# 4 Close         24569    0.0553  D. Close       
# 5 Friend        21710    0.0489  E. Friend      
# 6 Work            888    0.00200 F. Work    

ggplot(data = relation_type_all, aes(x = 1, y = Percentage)) +
  geom_bar(aes(fill = Relationship), position = 'fill', stat = "identity") + 
  coord_polar(theta = 'y') # + 
  # geom_text(aes(label = round(Percentage*100, digits = 1)), position = position_stack(vjust = 0))

# Strangers <- Unknown, Stranger
# Work <- Employee, Employer
# Acquaintance <- Acquaintance, Neighbor
# Friend <- Friend
# Close <- Boyfriend, Girlfriend
# Family <- Family, Mother, Stepmother, Father, Stepson, In-Law, Daughter, Brother, Son, Sister, Stepfather
#           Husband, Ex-Husband, Stepdaughter, Wife, Ex-Wife


test_str = "M.k.F.Wife"
unlist(strsplit(test_str, split="\\."))[4]
# [1] "Wife"
substr(test_str, 7, nchar(test_str))


# in_short = paste0(substr(Perpetrator.Sex, 1, 1)


unknown_ratio <- ushr %>%
  summarise(., unknown_ratio = nrow(subset(ushr, Relationship == 'Unknown'))/n())
unknown_ratio
#   unknown_ratio
# 1     0.4276158

unsolved_ratio <- ushr %>%
  summarise(., unsolved_ratio = nrow(subset(ushr, Crime.Solved == 'No'))/n())
unsolved_ratio
#   unsolved_ratio
# 1      0.2980356

unsolv_unknown_ratio <- ushr %>%
  summarise(., unsolv_unknown_ratio = nrow(subset(ushr, Crime.Solved == 'No' & Relationship == 'Unknown'))/n())
unsolv_unknown_ratio
#   unsolv_unknown_ratio
# 1             0.278988

solv_known_ratio <- ushr %>%
  summarise(., solv_known_ratio = nrow(subset(ushr, Crime.Solved == 'Yes' & Relationship != 'Unknown'))/n())
solv_known_ratio
#   solv_known_ratio
# 1        0.5533367

unsolv_known_ratio <- ushr %>%
  summarise(., unsolv_known_ratio = nrow(subset(ushr, Crime.Solved == 'No' & Relationship != 'Unknown'))/n())
unsolv_known_ratio
#   unsolv_known_ratio
# 1         0.01904757

solv_unknown_ratio <- ushr %>%
  summarise(., solv_unknown_ratio = nrow(subset(ushr, Crime.Solved == 'Yes' & Relationship == 'Unknown'))/n())
solv_unknown_ratio
#   solv_unknown_ratio
# 1          0.1486278

# 0.1486278 + 0.01904757 + 0.5533367 + 0.278988 = 1, good

# 0.5533367/(0.1486278 + 0.5533367) = 0.7882688, hah!