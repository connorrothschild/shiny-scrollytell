library(here)
library(readxl)
library(tidyverse)
library(ggplot2)

education <- read_excel(here::here("data/education.xlsx"), skip=1)
salary <- read_excel(here::here("data/national_M2017_dl.xlsx"))
automation <- read_excel(here::here("data/raw_state_automation_data.xlsx"))
typicaleducation <- read_excel(here::here("data/typicaleducation.xlsx"))

salary1 <- salary %>% 
  group_by(OCC_TITLE) %>% 
  mutate(natlwage = TOT_EMP * as.numeric(A_MEAN)) %>%
  filter(!is.na(TOT_EMP)) %>%
  filter(!is.na(A_MEAN)) %>%
  filter(!is.na(A_MEDIAN))

salary1$A_MEDIAN = as.numeric(as.character(salary1$A_MEDIAN))
salary2 <- select(salary1, OCC_TITLE, TOT_EMP, A_MEDIAN, natlwage) %>% 
  distinct()

library(plyr)
education1 <- education %>% select(-...2)

education1 <- rename(education1, c("2016 National Employment Matrix title and code" = "occupation",
                                   "Less than high school diploma" = "lessthanhs", 
                                   "High school diploma or equivalent" = "hsdiploma",
                                   "Some college, no degree" = "somecollege",
                                   "Associate's degree" = "associates",
                                   "Bachelor's degree" = "bachelors",
                                   "Master's degree" = "masters",
                                   "Doctoral or professional degree" = "professional"))

education2 <- education1 %>% 
  group_by(occupation) %>%
  mutate(hsorless = lessthanhs + hsdiploma,
         somecollegeorassociates = somecollege + associates,
         postgrad = masters + professional)

education2 <- education2 %>% drop_na()

salary2 <- rename(salary2, c("OCC_TITLE" = "occupation"))
salary2$occupation <- tolower(salary2$occupation)
education2$occupation <- tolower(education2$occupation)
edsal <- merge(as.data.frame(education2), as.data.frame(salary2), by="occupation") %>% drop_na()

typicaleducation2 <- typicaleducation %>% select(occupation,typicaled,workexp)
typicaleducation2 <- typicaleducation2 %>% drop_na()
typicaleducation2$occupation <- tolower(typicaleducation2$occupation)
edsal2 <- merge(as.data.frame(edsal), as.data.frame(typicaleducation2), by="occupation")

detach(package:plyr)
edsal3 <- edsal2 %>% 
  group_by(typicaled) %>% 
  summarise(medianwage = mean(A_MEDIAN))

automationwstates <- automation %>% select(-soc)
automation1 <- automationwstates %>% select(occupation,probability,total)

automation1$occupation <- str_replace_all(automation1$occupation, ";", ",")
automation1$occupation <- tolower(automation$occupation)
data <- merge(as.data.frame(edsal2), as.data.frame(automation1), by="occupation")

data$occupation <- toTitleCase(data$occupation)

data <- data %>% 
  mutate(reveal = case_when(
    typicaled == "No formal educational credential" ~ 1,
    typicaled == "High school diploma or equivalent" ~ 2,
    typicaled == "Postsecondary nondegree award" ~ 3,
    typicaled == "Some college, no degree" ~ 0,
    typicaled == "Associate's degree" ~ 4,
    typicaled == "Bachelor's degree" ~ 5,
    typicaled == "Master's degree" ~ 6,
    typicaled == "Doctoral or professional degree" ~ 7,
  ))

data <- data %>% 
  mutate(probability = probability*100)

tot_emp <- data %>% 
  group_by(typicaled) %>% 
  summarise(sum_tot_emp = sum(TOT_EMP)) 

data <- merge(data, tot_emp)

# add weights for education-level probability

data <- data %>% 
  mutate(weight = TOT_EMP/sum_tot_emp) 

mean_prob <- data %>% 
  group_by(typicaled) %>%
  summarise(mean_prob = weighted.mean(probability, weight))
# %>%  summarise(mean_income = weighted.mean(A_MEDIAN, weight))

data <- merge(data, mean_prob)
write_csv(data, here::here("data/final_data.csv"))
