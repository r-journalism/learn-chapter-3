
source("import_murders.R")

View(murders)


nrow(murders)

# Make a list of cities based on the unique() function
how_many <- unique(murders$MSA_label)

# Count up how many are in the list
length(how_many)

glimpse(murders)


df1 <- filter(murders, Relationship_label=="Husband", VicAge > 60, Year==2016)

# same as the line above
df2 <- filter(murders, Relationship_label=="Husband" & VicAge > 60 & Year==2016) 

df3 <- filter(murders, Relationship_label %in% c("Husband", "Boyfriend") |
                Circumstance_label=="Lovers triangle")


# WRONG
filter(murders, fstate_label=District of Columbia)

# RIGHT
filter(murders, fstate_label="District of Columbia")

# WRONG

filter(murders, 1980 < year < 1990)

# RIGHT

filter(murders, 1980 < year, year < 1990)

# Not WRONG but INEFFICIENT to type out

filter(murders, VicRace_label=="Black" | VicRace_label="Unknown" | 
  VicRace_label=="Asian or Pacific Islander")

# RIGHT
filter(murders, VicRace_label %in% c("Black", "Unknown", "Asian or Pacific Islander"))

df1_narrow <- select(df1, State, Agency, Solved_label, Year)

View(df1_narrow)


df2_narrow <- select(df1, State, OffAge:OffRace_value, Weapon_label)

View(df2_narrow)


# modifying the data frame created above
df3_narrow <- select(df2_narrow, -Weapon_label)


View(df3_narrow)



# This extracts all variables with names that contain "_label"

labels_only_columns <- select(murders(contains("_label")))
str(labels_only_columns)

## arrange()



age_df1 <- arrange(murders, VicAge)

age_df2 <- arrange(murders, VicAge, OffAge)

age_df3 <- arrange(murders, VicAge, desc(OffAge))

# Same result as above
age_df3b <- arrange(murders, VicAge, -OffAge)

## mutate()
  
  

murders_ver2 <- mutate(murders,
                       age_difference=OffAge-VicAge)

View(murders_ver2)


# creates an age_difference column
# and creates a vic_category column that is populated with values 
# depending on the VicRace_label column

murders_ver3 <- mutate(murders,
                       age_difference=OffAge-VicAge,
                       vic_category=case_when(
                         VicRace_label == "White" ~ "White",
                         VicRace_label != "White" ~ "Non-White"
                       ))

View(murders_ver3)

## Rename

colnames(df3_narrow)

# OK, you see the column names above-- let's change a couple of them

df3_renamed <- rename(df3_narrow, 
                      offender_gender=OffSex_label,
                      offender_age=OffAge)
colnames(df3_renamed)

colnames(df3_narrow)

# Keeping only the State and offender gender and age columns but renaming the 
# OffSex_label and OffAge columns

df4_renamed <- select(df3_narrow,
                      State,
                      offender_gender=OffSex_label,
                      offender_age=OffAge)

df4_renamed

## summarize()
  

summarize(murders, average_victim_age=mean(VicAge))

summarize(murders, 
average_victim_age=mean(VicAge), 
average_offender_age=mean(OffAge))

summarize(murders, 
          first=min(Year), 
          last=max(Year),
          metro_areas=n_distinct(MSA_label),
          cases=n())

## group_by()

# This is the same process as before but we're telling R to group up 
# the metro areas before summarizing the data

murders <- group_by(murders, MSA_label)

summarize(murders, 
          first=min(Year), 
          last=max(Year),
          cases=n())
## pipe %>%
  


dc_annual_murders1 <- filter(murders, State=="District of Columbia")
dc_annual_murders2 <- group_by(dc_annual_murders1, Year)
dc_annual_murders3 <- summarize(dc_annual_murders2, total=n())
dc_annual_murders4 <- arrange(dc_annual_murders3, desc(total))

# looking at the first 6 rows of data

head(dc_annual_murders4)

dc_annual_murders <- arrange(
  summarize(
    group_by(
      filter(murders, State=="District of Columbia"), 
      Year), 
    total=n()), 
  desc(total))

# looking at the first 6 rows of data

head(dc_annual_murders)

filter(murders, OffAge==2)

murders %>% filter(OffAge==2)

filter(murders, State=="District of Columbia") %>% 
  group_by(Year) %>% 
  summarize(total=n()) %>%    
  arrange(desc(total)) %>% 
  head()

# We can keep the code from before but now we add a new mutate line
filter(murders, State=="District of Columbia") %>% 
  group_by(Year) %>% 
  summarize(total=n()) %>%
  mutate(previous_year=lag(total)) %>% 
  mutate(change=total-previous_year)

# Here's an example of the same code above but with mutate called just once
# previous_year was able to be referenced a second time because it was created in first
years <- filter(murders, State=="District of Columbia") %>% 
  group_by(Year) %>% 
  summarize(total=n()) %>%
  mutate(previous_year=lag(total), change=previous_year-total) 

years

years %>% mutate(all_murders=sum(total))

murders %>% 
  group_by(VicSex_label) %>% 
  summarize(total=n())

murders %>% 
  group_by(State, VicSex_label) %>% 
  summarize(total=n())

library(tidyr)

murders %>% 
  group_by(State, VicSex_label) %>% 
  summarize(total=n()) %>% 
  spread(VicSex_label, total)

percent_murders <- murders %>% 
  group_by(State, VicSex_label) %>% 
  summarize(total=n()) %>% 
  # okay, we've got the total, now we can do some math with mutate
  mutate(percent=total/sum(total, na.rm=T)*100) 
  # did you notice the na.rm=T added to the sum function? This removes NAs
  # That's necessary because if you have a single NA then it will not sum correctly 
  # (thanks, statisticians!)

percent_murders

percent_murders_women <- murders %>% 
  group_by(State, VicSex_label) %>% 
  summarize(total=n()) %>% 
  mutate(percent=total/sum(total, na.rm=T)*100) %>% 
  filter(VicSex_label=="Female") %>% 
  arrange(-percent)

# Using the DT (DataTables) library that lets us create searchable tables with the 
# plug-in for jQuery 
# If you don't have DT installed yet, uncomment the line below and run it
#install.packages("DT")

library(DT)

datatable(percent_murders_women)