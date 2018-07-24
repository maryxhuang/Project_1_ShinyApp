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

shades <- colorRampPalette(c("white", color))(100)

map("state", fill=TRUE, col=fills, resolution=0, lty=0, 
    projection='polyconic', myborder=0, mar=c(0,0,0,0))
map("state", col="white", fill=FALSE, add=TRUE, lty=1, lwd=1, 
    projection='polyconic', myborder=0, mar=c(0,0,0,0))



ushr <- read.csv('./hr_cut.csv', stringsAsFactors=F)

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


unique(ushr$Relationship)
#  [1] "Acquaintance"         "Unknown"              "Wife"                 "Stranger"             "Girlfriend"          
#  [6] "Ex-Husband"           "Brother"              "Stepdaughter"         "Husband"              "Sister"              
# [11] "Friend"               "Family"               "Neighbor"             "Father"               "In-Law"              
# [16] "Son"                  "Ex-Wife"              "Boyfriend"            "Mother"               "Common-Law Husband"  
# [21] "Common-Law Wife"      "Stepfather"           "Stepson"              "Stepmother"           "Daughter"            
# [26] "Boyfriend/Girlfriend" "Employer"             "Employee"    


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