library(rio)
library(tidyverse)
library(lubridate)
#Apparently some saucy palettes in here :O for scale continuous
library(viridis) 
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
  mutate(Appointment_Month = parse_date_time(Appointment_Month, "%d-%b-%Y")) %>%
  mutate(month_year = parse_date_time(Appointment_Month, "%Y-%m"))

for (i in 1:length(colnames(app_gp_coverage))) {
  colnames(app_gp_coverage)[i] = tolower(colnames(app_gp_coverage)[i])
}


##combining the two datasets for future use ~ Not sure from this point onwards tbh
#Filtered to only contain extra information where necessary
data <- left_join(covData, app_gp_coverage, by = c("ccg_code" = "commissioner_organisation_code"))
data <- inner_join(covData, app_gp_coverage, by = c("ccg_code" = "commissioner_organisation_code")) %>% 
  filter(year(appointment_date) == year(appointment_month) &
           month(appointment_date) == month(appointment_month))

##Copy pasted code from the modelling section of the lab

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

