library(rio)
library(tidyverse)
library(lubridate)
#Apparently some saucy palettes in here :O for scale continuous
library(viridis) 
#Supporting information
#https://digital.nhs.uk/data-and-information/publications/statistical/appointments-in-general-practice/appointments-in-general-practice-supporting-information

file_loc <- "C:/Users/Stephen/Desktop/University Work/Year 3 uni/St344/"
monthYears <- paste0(month.abb[c(3:12,1:8)], "_", c(rep(18,10),rep(19,8)))
d <- list()
for (i in 1:length(monthYears)) {
  filename <- paste0(file_loc, "Appointments_GP_Daily_Aug19/CCG_CSV_", monthYears[i], ".csv")
  d[[i]] <- import(filename, setclass = "tibble")
}

#Checking whether each file has the same columns
a <- TRUE
for(i in 1:length(d)){
  cols_1 <- length(d[[1]])
  if(cols_1 - length(d[[i]]) != 0){
    a <- FALSE
  }
  a
}

covData <- tibble()
for (i in 1:length(monthYears)) {
  covData <- rbind(covData, filter(d[[i]], CCG_NAME=="NHS Coventry and Rugby CCG"))
}
#This code removed the d variable from the environment and should speed things up
#As the d variable is over 500mb and causes performance issues in rstudio
#remove(d)


covData <- select(covData, Appointment_Date, APPT_STATUS, HCP_TYPE, APPT_MODE, TIME_BETWEEN_BOOK_AND_APPT, CCG_CODE,
                  COUNT_OF_APPOINTMENTS) %>%
  mutate(Appointment_Date = parse_date_time(Appointment_Date, "%d-%b-%Y"))


covData <- covData %>%
  mutate(appointment_date = mdy(appointment_date))

#Changing the letters to lower case to match Hadley Wickhams style guide
for (i in 1:length(colnames(covData))) {
  colnames(covData)[i] = tolower(colnames(covData)[i])
}

###Looking at number of appointments by each mode
appt_by_mode <- covData %>% 
  group_by(appt_mode) %>%
  summarise(count = sum(count_of_appointments))

###Produce a plot line in the lab to ensure my code thus far is correct

covData %>% 
  group_by(appointment_date) %>%
  summarise(count = sum(count_of_appointments)) %>%
  ggplot() +
  geom_point(aes(x = appointment_date, y = count, colour = wday(appointment_date, label = TRUE, abbr = TRUE))) +
  labs(x = "Date", y = "Number of Combined Hospital Appointments",
       title = "Combined Hospital Visits by Date",
       colour = "Day of Week") +
  theme_bw() +
  #Can customise in due course if required
  scale_colour_discrete()


#importing next dataset which involves which patients are registered at each practice
app_gp_coverage <- import(paste0(file_loc, "Appointments_GP_Daily_Aug19/APPOINTMENTS_GP_COVERAGE.csv"), setclass = "tibble")%>%
  mutate(Appointment_Month = parse_date_time(Appointment_Month, "%d-%b-%Y"))
#%>%
#  mutate(month_year = parse_date_time(Appointment_Month, "%Y-%m"))

for (i in 1:length(colnames(app_gp_coverage))) {
  colnames(app_gp_coverage)[i] = tolower(colnames(app_gp_coverage)[i])
}


##combining the two datasets for future use ~ Not sure from this point onwards tbh
#Filtered to only contain extra information where necessary
data <- left_join(covData, app_gp_coverage, by = c("ccg_code" = "commissioner_organisation_code"))
data <- inner_join(covData, app_gp_coverage, by = c("ccg_code" = "commissioner_organisation_code")) %>% 
  filter(year(appointment_date) == year(appointment_month) &
           month(appointment_date) == month(appointment_month))

##Copy pasted code from the modelling section of the lab, not sure if this is actually correct - but anyway

poisson_data <- data %>%
  group_by(appointment_date) %>%
  summarise(count = sum(count_of_appointments)) %>%
  mutate(weekday = wday(appointment_date, label=TRUE),
         month_year = paste(month(appointment_date, label = TRUE), year(appointment_date))) %>%
  left_join(data, app_gp_coverage, by = c("appointment_date")) %>%
  filter(year(appointment_date) == year(appointment_month) &
           month(appointment_date) == month(appointment_month)) %>%
  mutate(log_registered_patients = log(`patients registered at included practices`)) %>%
  select(appointment_date, count, weekday, log_registered_patients) %>%
  unique()

#Not wholey convinced by this lol  
my_model <- glm(count ~ 0 + weekday + offset(log_registered_patients), family="poisson", data=poisson_data)

#Not really sure about this at all lol
my_model$coefficients[2]/my_model$coefficients[6]


###The first question was provided by your client and explores if the demand on Coventry GPs has increased over the recent past.
###As there are various ways of measuring the demand on GPs across different appointment types, you will need to refine the question
###to make it sufficiently specific and answerable by using the data.

#The first step of this problem is to undestand how each appointment type is handled by a GP.
#We have the following options: Face to Face, Home Visit, Telephone, unknown and Video conference.
#Video conference is so few in number that they can be discarded (23 over the entire time period)
#Step 1, graph the data to see general trend over time.
#Graph datapoints by week to take account of different
#Colour the graph potentially by season and also by  type of appointment
#Getting data by week:

#Seasons data: https://www.timeanddate.com/calendar/seasons.html, based on the equinoxes
#
seasons <- tibble(season = c("winter", "spring", "summer","autumn"),
                  start_date = c(as.Date("2017-12-21"), as.Date("2018-03-20"),
                                 as.Date("2018-06-20"), as.Date("2018-09-23"),
                                 as.Date("2018-12-21"), as.Date("2019-03-20"),
                                 as.Date("2019-06-20"), as.Date("2019-09-23")),
                  end_date = c(as.Date("2018-03-19"), as.Date("2018-06-19"),
                               as.Date("2018-09-22"), as.Date("2018-12-20"),
                               as.Date("2019-03-19"), as.Date("2019-06-19"),
                               as.Date("2019-09-22"), as.Date("2019-12-20")))
data <- data %>%
  #Detecting week number of appointment date
  mutate(week_num = isoweek(appointment_date)) %>%
  mutate(year = year(appointment_date)) %>%
  mutate(year = if_else(year == 2018, 0, 1)) %>%
  mutate(week_num = week_num + (52 * year)) %>%
  #Taking account of the fact that 1 actually refers to week 53
  mutate(week_num = if_else(week_num == 1, 53, week_num)) %>%
  mutate(week_num = week_num - 8) %>%
  mutate(season = if_else(appointment_date ))


#Count summaries of the data by
appt_by_mode <- data %>% 
  group_by(appt_mode, week_num) %>%
  summarise(count = sum(count_of_appointments)) %>%
  ungroup()

#General plot for understanding
appt_by_mode %>%
  ggplot() +
  geom_point(aes(x = week_num, y = count, colour = appt_mode)) +
  theme_bw()

#Looking into the two low results for Face-to-Face
appt_by_mode %>% 
  filter(count < 20000 & appt_mode == "Face-to-Face")

#Returns week 9 (first week and not a full one, only 4 results) and 52 (over Christmas so less visits)
data %>%
  filter(week_num %in% c(9, 52)) %>%
  select(appointment_date) %>%
  unique()

#No appointments on 30th December
data %>% 
  filter(str_detect(appointment_date, '12-30'))


##Therefore we remove these two weeks (9 and 52).
#Note: don't have data for gp count per practice and hence modelling demand by overall appointments in the Cov Area
data <- data %>%
  filter(!week_num %in% c(9, 52))

appt_by_mode <- data %>% 
  group_by(appt_mode, week_num) %>%
  summarise(count = sum(count_of_appointments)) %>%
  ungroup()

appt_by_mode %>%
  ggplot() +
  geom_point(aes(x = week_num, y = count, colour = appt_mode)) +
  theme_bw()

appt_by_mode %>%
  filter(appt_mode == "Face-to-Face") %>%
  ggplot(aes(x = week_num, y = count)) +
  geom_point()

appt_by_mode %>%
  filter(appt_mode == "Home Visit") %>%
  ggplot(aes(x = week_num, y = count)) +
  geom_point()

appt_by_mode %>%
  filter(appt_mode == "Telephone") %>%
  ggplot(aes(x = week_num, y = count)) +
  geom_point()

#Don't know which category these should be in and therefore choosing to ignore although it seems to be the only with an
#Increase in the number
appt_by_mode %>%
  filter(appt_mode == "Unknown") %>%
  ggplot(aes(x = week_num, y = count)) +
  geom_point()
