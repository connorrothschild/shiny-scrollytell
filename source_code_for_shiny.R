library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggrepel)
library(tools)
library(readxl)
library(tidyverse)
library(knitr)

options(scipen=999)
# theme_set(theme_minimal())

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

render_text <- function(num){
  
  div(
    text(num), class = "text"
  )
  
}

text0 <- HTML("<span style='font-size:20px'> How do jobs differ in their susceptibility to automation? </span>
              <br><br> 
              <p> Introductory paragraph.
              <br> Sentence two.
              <br><br> Sentence three.<p>")

text1 <- HTML("<H2> No education credentials </H2>
              <br> <p> Workers with <font color='#A00042'>no formal education credential</font> have a median income of $25,636.
              <br> On average, those occupations have a <b>90% chance</b> of job automation.
              <br><br> There are 23,765,700 workers with <font color='#A00042'>no formal education credential</font>.<p>")

text2 <- HTML("<H2> High school diplomas </H2>
              <br> <p>Workers with <font color='#F56C42'>high school diplomas</font> have a median income of $25,636.
              <br> On average, those occupations have a <b>60% chance</b> of job automation.
              <br><br> There are 33,129,910 workers with a <font color='#F56C42'>high school diploma</font>.<p>")

text3 <- HTML("<H2> Postsecondary nondegree awards </H2>
              <br> <p>Workers with <font color='#008640'>postsecondary nondegree awards</font> (e.g. actors) have a median income of $39,990.
              <br> On average, those occupations have a <b>52% chance</b> of job automation.
              <br><br> There are 5,904,150 workers with a <font color='#008640'>postsecondary nondegree award</font>.<p>")

text4 <- HTML("<H2> Associate's degrees </H2>
              <br> <p>Workers with <font color='#3487BD'>associate's degrees</font> have a median income of $41,496.
              <br> On average, those occupations have a <b>50% chance</b> of job automation.
              <br><br> There are 1,869,840 workers with an <font color='#3487BD'>associate's degree</font>.<p>")

text5 <- HTML("<H2> Bachelor's degrees </H2>
              <br> <p>Workers with <font color='#C71C7E'>bachelor's degrees</font> have a median income of $59,124.
              <br> On average, those occupations have a <b>20% chance</b> of job automation.
              <br><br> There are 18,399,270 workers with a <font color='#C71C7E'>bachelor's degree</font>.<p>")

text6 <- HTML("<H2> Master's degrees </H2>
              <br> <p>Workers with <font color='#5E4FA2'>master's degree</font> have a median income of $69,732.
              <br> On average, those occupations have a <b>10% chance</b> of job automation.
              <br><br> There are 1,281,710 workers with a <font color='#5E4FA2'>master's degree</font>.<p>")

text7 <- HTML("<H2> Doctoral degrees </H2>
              <br> <p>Workers with <b>doctoral degrees</b> have a median income of $84,396.
              <br> On average, those occupations have a <b>3% chance</b> of job automation.
              <br><br> There are 1,386,850 workers with a <b>doctoral degree</b>.<p>")

text8 <- HTML("<H2> In Sum </H2>
              <br> <p>All things considered, the nominal median income of an average US worker is <b>$31,786</b>.
              <br> 47% of jobs are expected to face a high risk of automatization in the near future.<sup>1</sup><p>
              <br><br><br>
              <span style='font-size:11px'><sup>1</sup><a href='https://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf' target='_blank'>Frey and Osborne (2013)</a>
               write that 'associated occupations are potentially automatable over
              some unspecified number of years, <i>perhaps a decade or two.'</i></span>")

text <- function(num){
  p(
  switch(num,
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

cols <- c('No formal educational credential' = '#A00042','High school diploma or equivalent' = '#F56C42',
          "Postsecondary nondegree award" = '#008640', "Associate's degree" = '#3487BD', 
          "Bachelor's degree" = '#C71C7E', "Master's degree" = '#5E4FA2',
          "Doctoral or professional degree" = '#1A1A1A') 

legend_ord <- levels(with(data, reorder(typicaled, reveal)))

introPlot <- data %>% 
  filter(typicaled != "Some college, no degree") %>%
  ggplot() +
  geom_point(mapping=aes(x=A_MEDIAN, y=probability, size=TOT_EMP,
                         alpha=1/5, col=typicaled))+
  scale_size(range = c(1, 20), guide = 'none') +
  xlab("\nMedian Income") +
  ylab("Probability of Automation") +
  ggtitle("Likelihood of Job Automation vs Median Income") +
  labs(size=element_blank(), col=element_blank()) +
  labs(alpha=NULL) +
  guides(alpha=FALSE) +
  scale_color_manual(values = cols, breaks = legend_ord) +
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), limits = c(25000,200000)) +
  scale_y_continuous(labels=scales::number_format(suffix="%"), limits = c(0,100)) +
  # theme(legend.position = "top", legend.direction = "horizontal", 
  #       legend.text = element_text(colour="black", size = 12)) +
  cr::drop_axis(axis = "y")
