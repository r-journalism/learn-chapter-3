
source("import_murders.R")

murders %>% 
  group_by(State, Year) %>% 
  summarize(cases=n(), solved=sum(Solved_value))

murders %>% 
  group_by(State, Year) %>% 
  summarize(cases=n(), solved=sum(Solved_value)) %>% 
  mutate(percent=solved/cases*100)

library(tidyr)

murders %>% 
  group_by(State, Year) %>% 
  summarize(cases=n(), solved=sum(Solved_value)) %>% 
  mutate(percent=solved/cases*100) %>% 
  gather("type", "n", 3:5) %>% 
  arrange(State, Year, desc(type))

# If you don't have DT installed yet, uncomment the line below and run it
#install.packages("DT")
library(DT)

unsolved <- murders %>% 
  group_by(MSA_label, Solved_label) %>% 
  filter(Year>2008) %>% 
  summarize(cases=n())

datatable(unsolved)

df1 <- murders %>% 
  group_by(MSA_label, Solved_label) %>% 
  filter(Year>2008) %>% 
  summarize(cases=n()) %>% 
  mutate(percent=cases/sum(cases)*100)

df1 %>% datatable() 

df2 <- murders %>% 
  group_by(MSA_label, Solved_label) %>% 
  filter(Year>2008) %>% 
  summarize(cases=n()) %>% 
  filter(sum(cases)>10) %>% 
  mutate(percent=cases/sum(cases)*100) %>% 
  filter(Solved_label=="No") %>% 
  select(Metro=MSA_label, cases_unsolved=cases, percent_unsolved=percent) %>% 
  arrange(desc(percent_unsolved)) 

df2 %>% datatable()

df3 <- murders %>% 
  group_by(MSA_label, VicRace_label, Solved_label) %>% 
  filter(Year>2008) %>% 
  summarize(cases=n()) %>% 
  mutate(percent=cases/sum(cases, na.rm=T)*100)

df3 %>% datatable() 

# Also, we're going to round the percents with the round() function

race <- murders %>% 
  group_by(MSA_label, VicRace_label, Solved_label) %>% 
  filter(Year>2008) %>% 
  summarize(cases=n()) %>% 
  mutate(percent=cases/sum(cases)*100) %>% 
  mutate(percent=round(percent, digits=2)) %>% 
  filter(Solved_label=="No") %>% 
  select(Metro=MSA_label, VicRace_label, cases_unsolved=cases, percent_unsolved=percent) %>% 
  arrange(desc(percent_unsolved)) 

datatable(race)

# We've saved our previous steps into the "race" dataframe
# So we can continue our steps because they've been saved

df5 <- race %>% 
  spread(VicRace_label, percent_unsolved)

df5 %>% datatable()

# This time we'll drop the cases_unsolved column before spreading

race_percent <- race %>% 
  select(-cases_unsolved) %>% 
  spread(VicRace_label, percent_unsolved)

datatable(race_percent)



# So 2:6 represents the column index, or where the columns are in the data frame--
# So columns 2 through 6

gathered1 <- race_percent %>% 
  gather("Race", "Percent_Unsolved", 2:6) %>% 
  arrange(desc(Metro)) 

gathered1 %>% datatable()

## Instead of numbers you can use column names
## This is a reminder that to reference column names with spaces, you have to
# use the ` back tick around the column names

gathered2 <- race_percent %>% 
  gather("Race", "Percent_Unsolved", `Asian or Pacific Islander`:White) %>% 
  arrange(desc(Metro))


gathered2 %>% datatable()

arrange_race <- race_percent %>% 
  arrange(desc(Black))

arrange_race %>% datatable()

race_percent <- race %>% 
  select(-cases_unsolved) %>% 
  spread(VicRace_label, percent_unsolved)

datatable(race_percent)

race_cases <- race %>% 
select(-percent_unsolved) %>% 
spread(VicRace_label, cases_unsolved)

race_cases %>% datatable(race_cases)


# If we don't use the by variable it would match nothing because the column names 
# are the exact same for both data frames

wide1 <- left_join(race_percent, race_cases, by="Metro")

View(wide1)

# Don't forget: If there are spaces in the column names, you have to use the ` back tick.

wide2 <- left_join(race_percent, race_cases, by="Metro") %>% 
  select(Metro, 
         `Asian cases`=`Asian or Pacific Islander.y`,
         `Asian percent`=`Asian or Pacific Islander.x`,
         `Native American cases`=`American Indian or Alaskan Native.y`,
         `Native American percent`=`American Indian or Alaskan Native.x`,
         `Black cases`=Black.y,
         `Black percent`=Black.x,
         `White cases`=White.y,
         `White percent`=White.x,
         `Unknown cases`=Unknown.y,
         `Unknown percent`=Unknown.x
  ) %>% 
  arrange(desc(`Black percent`))


View(wide2)

wide3 <- wide2 %>% 
filter(`Black cases` >=10 & `White cases`>=10) %>% 
mutate(Black_White=`Black percent`-`White percent`) %>% 
select(Metro, `Black cases`, `White cases`, `Black percent`, `White percent`, Black_White) %>%
arrange(desc(Black_White))

wide3 %>% datatable()


left <- data.frame(company=c("Mars", "Hershey", "Cadbury",  "Mondelez", "Haribo"),
candy=c("Skittles", "M&Ms", "Starbar", "Toblerone", "Goldbaren"))
right <- data.frame(company=c("Hershey", "Mondelez", "Cadbury", "Mars", "Colosinas Fini"),
location=c("Pennsylvania", "Switzerland", "Britain", "New Jersey", "Spain"))

left
right

# We don't have to use by="column_name" this time because both data frames 
# only have one matching column name to join on: Company

right_join(left, right)

full_join(left, right)

inner_join(left, right)
