library(dplyr)
library(tidyr)
library(DT)
source("import_murders.R")


# If you don't have readr installed yet, uncomment and run the line below
#install.packages("readr")

library(readr)

county.fips <- read_csv("data/fips_counties.csv")

head(county.fips)

# FIPS change over time. Data tends to do that when you've got decades of stuff
# We'll swap out some County Names (most are from Alaska) before we join the data sets

murders  <- murders %>%
  mutate(CNTYFIPS=as.numeric(as.character(CNTYFIPS))) %>% 
  mutate(CNTYFIPS=case_when(
    CNTYFIPS==51560 ~ 51005,
    CNTYFIPS==2232 ~ 2105,
    CNTYFIPS==2280 ~ 2195,
    CNTYFIPS==2201 ~ 2198,
    TRUE ~ CNTYFIPS
  )) %>% 
  left_join(county.fips, by=c("CNTYFIPS"="fips")) 


View(murders)




greenway1 <- murders %>% 
filter(State=="Washington" & name_of_county=="King") %>% 
filter(Year >=1982 & Year <=2001) %>% 
filter(VicSex_label=="Female") 

View(greenway1)


murders %>% 
  select(Weapon_label) %>% 
  unique()

greenway2 <- greenway1 %>% 
filter(Weapon_label=="Strangulation - hanging" |
Weapon_label=="Other or type unknown")

View(greenway2)

include_graphics("images/116.png")

greenway2 %>% 
group_by(Circumstance_label) %>% 
count() %>% 
arrange(desc(n))

greenway2 %>% 
group_by(Solved_label) %>% 
summarize(total=n()) %>% 
mutate(percent=round(total/sum(total)*100,2))

msagrp <- murders %>% 
mutate(solved_num = ifelse(Solved_label=="Yes", 1, 0)) %>% 
group_by(MSA_label, VicSex_label, Weapon_label) %>% 
summarize(cases=n(), solved=sum(solved_num)) %>%
mutate(clearance=round(solved/cases*100,2))

View(msagrp)


msagrp_filtered <- msagrp %>% 
filter(VicSex_label=="Female" & clearance <= 33 & cases > 1) 
datatable(msagrp_filtered)

countygrp <- murders %>% 
  mutate(solved_num = ifelse(Solved_label=="Yes", 1, 0)) %>% 
  group_by(county_state, VicSex_label, Weapon_label) %>% 
  summarize(cases=n(), solved=sum(solved_num)) %>%
  mutate(clearance=round(solved/cases*100,2)) %>% 
  filter(VicSex_label=="Female" & clearance <= 33 & cases > 1) %>% 
  arrange(desc(cases))

datatable(countygrp)

countygrp2 <- murders %>%
# year filter here | remember ":" stands for "through", so 2006:2016 is 2006 2007 2008 etc
filter(Year %in% 2006:2016) %>% 
mutate(solved_num = ifelse(Solved_label=="Yes", 1, 0)) %>% 
group_by(county_state, VicSex_label, Weapon_label) %>% 
summarize(cases=n(), solved=sum(solved_num)) %>%
mutate(clearance=round(solved/cases*100,2)) %>% 
filter(clearance <= 33 & cases > 1) %>% 
arrange(desc(cases))

datatable(countygrp2)

murders <- mutate(murders,
age_group=case_when(
VicAge %in% 0:14 ~ "0-14",
VicAge %in% 15:19 ~ "15-19",
VicAge %in% 20:50 ~ "20-50",
VicAge %in% 51:99 ~ "51-99",
TRUE ~ "Unknown"))

countygrp3 <- murders %>%
filter(Year %in% 2006:2016) %>% 
mutate(solved_num = ifelse(Solved_label=="Yes", 1, 0)) %>% 
group_by(county_state, VicSex_label, age_group, Weapon_label) %>% 
summarize(cases=n(), solved=sum(solved_num)) %>%
mutate(clearance=round(solved/cases*100,2)) %>% 
filter(VicSex_label=="Female" & clearance <= 33 & cases > 1) %>% 
arrange(desc(cases))

datatable(countygrp3)



