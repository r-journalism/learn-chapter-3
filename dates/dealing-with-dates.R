
some_date <- "12-31-1999"

# If you don't have lubridate installed yet uncomment the line below and run it
#install.packages("lubridate")

# NOTE: IF YOU GET AN ERROR ABOUTZ NOT HAVING A PACKAGE CALLED stringi
# UNCOMMENT AND RUN THE LINES BELOW IF YOU HAVE A WINDOWS MACHINE

#install.packages("glue", type="win.binary")
#install.packages("stringi", type="win.binary")
#install.packages("stringr", type="win.binary")
#install.packages("lubridate", type="win.binary")

# UNCOMMENT AND RUN THE LINES BELOW IF YOU HAVE A MAC MACHINE

#install.packages("glue", type="mac.binary")
#install.packages("stringi", type="mac.binary")
#install.packages("stringr", type="mac.binary")
#install.packages("lubridate", type="mac.binary")

library(lubridate)

mdy(some_date)

data <- data.frame(First=c("Charlie", "Lucy", "Peppermint"),
                   Last=c("Brown", "van Pelt", "Patty"),
                   birthday=c("10-31-06", "2/4/2007", "June 1, 2005"))

data$DOB <- mdy(data$birthday)

data

data$year <- year(data$DOB)
data$month <- month(data$DOB, label=TRUE)
data$day <- day(data$DOB)
data$weekday <- wday(data$DOB, label=TRUE, abbr=FALSE)

data

# We're going to use the now() function which brings in the date for today

today <- now()
data$age <- difftime(today, data$DOB)

data

data$age_years <- as.numeric(data$age) / 365.25 #.25 because of leap years

data
