library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggrepel)
library(tools)
library(readxl)
library(tidyverse)
library(knitr)

options(scipen=999)
theme_set(theme_minimal())

education <- read_excel("education.xlsx", skip=1)
salary <- read_excel("national_M2017_dl.xlsx")
automation <- read_excel("raw_state_automation_data.xlsx")

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

  typicaleducation <- read_excel("typicaleducation.xlsx")
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

## FUNCTIONS

add_data <- function(add){

    vis <-  reactive({
      
      data <- data %>% 
        filter(typicaled != "Some college, no degree") %>%
        filter(if (add != 8) add == reveal else reveal %in% c(1:8)) %>% 
        ggvis(~A_MEDIAN, ~probability, opacity := 0.7, key := ~occupation) %>% 
        scale_numeric('size',domain = c(100000,500000),range=c(100,500)) %>%
        layer_points(fill = ~typicaled, size = ~TOT_EMP) %>% 
        add_tooltip(function(data) glue::glue('Occupation: {data$occupation}',
                                              'Number of Workers: {scales::comma(data$TOT_EMP)}',
                                              'Probability of Automation: {data$probability}%',
                                              'Income: {scales::dollar(data$A_MEDIAN)}',
                                              .sep = "<br />")) %>% 
        add_axis("x", title = "Median Income", grid = FALSE) %>%
        add_axis("y", title = "Probability of Automation", title_offset = 50, grid = FALSE) %>% 
        add_legend("fill", title = "Education", properties = legend_props(legend = list(y = 200))) %>%
        add_legend("size", title = "Number of Workers", properties = legend_props(legend = list(y = 50))) %>%
        scale_numeric("x", domain = c(25000, 200000), nice = FALSE) %>%
        scale_numeric("y", domain = c(0, 100), nice = FALSE) %>% 
        scale_nominal("fill", 
                      domain = c('No formal educational credential','High school diploma or equivalent', "Postsecondary nondegree award",
                                 "Associate's degree", "Bachelor's degree", "Master's degree", "Doctoral or professional degree"), 
                      range = c('#A00042', '#F56C42', '#AADDA3', '#3487BD', '#5E4FA2', '#C71C7E', "#1A1A1A")) %>% 
        set_options(duration = 0, width = "auto", height = "auto", resizable = FALSE)
      
    })
      
    vis %>% bind_shiny("vis")
   
}  

longdiv <- function(...){
  div(
    ...,
    class = "container",
    style = "height:100vh"
  )
}

render_text <- function(wp){
  
  div(
    text(wp), class = "subtitle"
  )
  
}

text1 <- paste0("Workers with no formal education credential have a median income of $25,636",
                " and, on average, a ", round(median(data$mean_prob[data$reveal==1])), "% chance of job automation.",
                " There are ", scales::comma(sum(data$TOT_EMP[data$reveal==1])), " workers with no formal education credential.")

text2 <- paste0("Workers with a high school diploma have a median income of $35,256",
                " and, on average, a ", round(median(data$mean_prob[data$reveal==2])), "% chance of job automation.",
                " There are ", scales::comma(sum(data$TOT_EMP[data$reveal==2])), " workers with only a high school diploma.")

text3 <- paste0("Workers with a postsecondary nondegree award (e.g. actors) have a median income of ", scales::dollar(median(data$A_MEDIAN[data$reveal==3])),
                " and, on average, a ", round(median(data$mean_prob[data$reveal==3])), "% chance of job automation.",
                " There are ", scales::comma(sum(data$TOT_EMP[data$reveal==3])), " workers with a postsecondary nondegree award.")

text4 <- paste0("Workers with an associate's degree have a median income of $41,496",
                " and, on average, a ", round(median(data$mean_prob[data$reveal==4])), "% chance of job automation.",
                " There are ", scales::comma(sum(data$TOT_EMP[data$reveal==4])), " workers with an associate's degree.")

text5 <- paste0("Workers with a bachelor's degree have a median income of $59,124",
                " and, on average, a ", round(median(data$mean_prob[data$reveal==5])), "% chance of job automation.",
                " There are ", scales::comma(sum(data$TOT_EMP[data$reveal==5])), " workers with a bachelor's degree.")

text6 <- paste0("Workers with a master's degree have a median income of $69,732",
                " and, on average, a ", round(median(data$mean_prob[data$reveal==6])), "% chance of job automation.",
                " There are ", scales::comma(sum(data$TOT_EMP[data$reveal==6])), " workers with a master's degree.")

text7 <- paste0("Workers with a doctoral degree have a median income of $84,396",
                " and, on average, a ", round(median(data$mean_prob[data$reveal==7])), "% chance of job automation.",
                " There are ", scales::comma(sum(data$TOT_EMP[data$reveal==7])), " workers with a doctoral degree.")

text8 <- paste0("All things considered, the nominal median income of an average US worker is $31,786",
                " and, on average, their job has a ", round(median(data$mean_prob)), "% chance of being automated.")

text <- function(wp){
  p(
  switch(wp,
         text1,
         text2,
         text3,
         text4,
         text5,
         text6,
         text7,
         text8
         )
  )
}
