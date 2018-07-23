
#install.packages("stringr")
library(stringr)

library(dplyr)

messy <- data.frame(name=c("Bill Smith", "jane doe", "John Forest-William"),
                    email=c("bsmith@themail.com", "jdoe@themail.com", 
                            "jfwilliams$geemail.com"),
                    income=c("$90,000", "$140,000", "E8500"),
                    phone=c("(203) 847-334", "207-999-1122", "2128487345"),
                    activites=c("fishing, sailing, planting flowers", "reading, 
                                raising flowers, biking", "hiking, fishing"))

messy

x <- c("Bill", "Bob", "William")
str_length(x)

data <- data.frame(place=c("HQ", "HQ", "HQ"),
id=c("A", "B", "C"),
number=c("001", "002", "003"))

data

data <- data %>% 
  mutate(combined=str_c("Num: ", number))

data

data$combined <- str_c("Num: ", data$number)

# or 

data <- data %>% 
  mutate(combined=str_c("Num", number, sep=": "))



data <- data.frame(place=c("HQ", "HQ", "HQ"),
id=c("A", "B", "C"),
number=c("001", "002", "003"))

data

data %>% 
  group_by(place) %>% 
  summarize(ids_combined=str_c(number, collapse="-"))

## subset strings


x <- "Dr. James"

str_sub(x, 1, 3)


str_sub(x, 1, 3) <- "Mr."
x


x <- "baby"
str_sub(x, -3, -1)
str_sub(x, -1, -1) <- "ies"



## detect matches


x <- c("Bill", "Bob", "David.Williams")
x
str_detect(x, "il")


## count matches


x <- c("Assault/Robbery/Kidnapping")
x
str_count(x, "/")


str_count(x, "/") + 1

## extract matches


x <- c("bsmith@microsoft.com", "jdoe@google.com", "jfwilliams@google.com")
str_extract(x, "@.+\\.com$")


## split strings

x <- c("john smith", "mary todd", "bill holis")

str_split(x, " ", simplify=TRUE)

first <- str_split(x, " ", simplify=TRUE)[,1]
last  <- str_split(x, " ", simplify=TRUE)[,2]



## replace a pattern


x <- c("john smith", "mary todd", "bill holis")
str_replace(x, "[aeiou]", "-")

str_replace_all(x, "[aeiou]", "-")

## change case


x <- c("john smith", "Mary Todd", "BILL HOLLIS")

str_to_upper(x)
str_to_lower(x)
str_to_title(x)

## trim strings


x <- c(" Assault", "Burglary ", " Kidnapping ")
str_trim(x)

