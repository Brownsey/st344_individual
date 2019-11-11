library(rio)
library(tidyverse)
library(lubridate)
monthYears <- paste0(month.abb[c(3:12,1:8)], "_", c(rep(18,10),rep(19,8)))
d <- list()
for (i in 1:length(monthYears)) {
  filename <- paste0("Appointments_GP_Daily_Aug19/CCG_CSV_", monthYears[i], ".csv")
  d[[i]] <- import(filename, setclass = "tibble")
}

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
covData <- select(covData, Appointment_Date, APPT_STATUS, HCP_TYPE, APPT_MODE, TIME_BETWEEN_BOOK_AND_APPT,
                  COUNT_OF_APPOINTMENTS) %>%
  mutate(Appointment_date = parse_date_time(Appointment_date, orders = c("y", "ym","ymd")))

for (i in 1:length(colnames(covData))) {
  colnames(covData)[i] = tolower(colnames(covData)[i])
}

covData %>% 
  group_by(appt_mode) %>%
  summarise(count = n())


#importing next dataset
app_gp_coverage <- read_csv("Appointments_GP_Daily_Aug19/APPOINTMENTS_GP_COVERAGE.csv")
for (i in 1:length(colnames(app_gp_coverage))) {
  colnames(app_gp_coverage)[i] = tolower(colnames(app_gp_coverage)[i])
}

data <- tibble(covData, app_gp_coverage)
data %>%
  ggplot(aes(x = appointment_date, y = count(appointment_date)))


