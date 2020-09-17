library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggrepel)
library(tools)
library(readxl)
library(tidyverse)
library(knitr)
library(shiny)
library(shinyjs)
library(ggvis)
library(plotly)
library(metathis)
library(formattable)
# devtools::install_github("statistiekcbs/scrollytell")
library(scrollytell)
library(here)

options(scipen=999)
theme_set(theme_minimal())
data <- readr::read_csv(here("data/final_data.csv"))

### FUNCTIONS & TEXT

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

text0 <- HTML("<span style='font-size:20px'> How do jobs differ in their susceptibility to automation? </span>
              <br><br> 
              <p> The topic of job automation has rapidly entered the national discourse. 
              Although it was once a topic reserved for policy wonks, college students, and Reddit Libertarians, it's now a serious talking point&mdash;from 
              the multitude of news articles documenting the risk of automation, to <a href='https://www.forbes.com/sites/quora/2019/09/27/automation-will-dramatically-change-the-workforce-andrew-yang-has-a-plan-to-bridge-the-gap/#52f0e5df204b' target='_blank'>Andrew Yang's presidential proposal</a> to solve job automation via a universal basic income. 
              <br> Experts have put forth a wide range of estimates for the true impact of job automization. No matter how severe it is, few argue that automation will have <i>no impact</i>.
              Most agree that automation will impact the way Americans work, and that it may put many workers out of a job (and many argue it already has).
              <br><br> But will different workers experience the impacts of automation in different ways? In this post, I combine data from two Oxford researchers, <a href='https://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf' target='_blank'>Carl Benedikt Frey and Michael A. Osborne</a>,
              with employment statistics from the Bureau of Labor Statistics to answer the question: 
              <br><br>
              <span style='font-size:18px'> How do jobs differ in their susceptibility to automation? </span>
              <br> Specifically, how does a worker's <b>level of education</b> and <b>current income</b> influence their risk of job loss to the rise of the robots? <p>")

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
              <br> <p>Workers with <font color='#5E4FA2'>master's degrees</font> have a median income of $69,732.
              <br> On average, those occupations have a <b>10% chance</b> of job automation.
              <br><br> There are 1,281,710 workers with a <font color='#5E4FA2'>master's degree</font>.<p>")

text7 <- HTML("<H2> Doctoral degrees </H2>
              <br> <p>Workers with <b>doctoral degrees</b> have a median income of $84,396.
              <br> On average, those occupations have a <b>3% chance</b> of job automation.
              <br><br> There are 1,386,850 workers with a <b>doctoral degree</b>.<p>")

text8 <- HTML("<H2> In Sum </H2>
              <br> <p>All things considered, the nominal median income of an average US worker is <b>$31,786</b>.
              <br>
              <br> 47% of jobs are expected to face a high risk of automatization in the near future.<sup>1</sup><p>
              <br><br><br>
              <span style='font-size:11px'><sup>1</sup><a href='https://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf' target='_blank'>Frey and Osborne (2013)</a>
               write that 'associated occupations are potentially automatable over
              some unspecified number of years, <i>perhaps a decade or two.'</i></span>")

concludingtext <- HTML("<p><span style='font-size:24px'><b>The Risk of Automation</b></span>
                        <br>
                            <span style='font-size:18px'>This data led researchers Carl Frey and Michael Osborne to predict that 47% of jobs are at serious risk of automation over the next couple decades.
                        <br>
                            <br>The visuals above suggest that the ills of automation may not be evenly distributed across jobs.
                            Less educated workers are more likely to face job loss as a product of automation. Those with high school diplomas or less find themself concentrated near the top of the y-axis, while those with bachelor’s degrees or higher face a lower risk of automation.
                        <br>
                            <br>A job’s salary is also predictive of automation probability. As the median income of a profession increases, the likelihood of automation displacing its workers decreases.
                            This could suggest that automation will increasingly bifurcate the already divided labor market, making those at the top wealthier at the expense of the worse-off.
                        <br>
                            <br>Automation’s impact on work necessitates a policy response. The fact that automation will have different effects on different industries and different workers is a reminder that this public policy will have to be strategic and thoughtful.</span></p>")

technicalnotes <- HTML("<p>
                <span style='font-size:18px'><i>Technical Notes</i></span><br>
                <br>
                <span style='font-size:12px'>
                To learn more about how I made this app, please see the <a href='https://connorrothschild.github.io/r/automation-scrollytell/' target='_blank'>accompanying blog post</a>
                <br>
                Employment and education data comes from the
                <a href='https://www.bls.gov/emp/documentation/education-training-system.htm' target='_blank'>Bureau of Labor Statistics</a>. 
                <br>
                Employment and income data also comes from the <a href='https://www.bls.gov/oes/current/oes_nat.htm#11-0000' target='_blank'>BLS</a>.
                <br>
                Data on occupation and the risk of automation comes from <a href='https://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf' target='_blank'>Frey and Osborne (2013)</a>. 
                <br>
                <br>
                Education is coded as typical education, meaning that the coded variable corresponds to the level of education that is most prevalent within a given occupation.
                If 51% of accountants hold a bachelor's degree, their typical education will be coded as such.
                Summary statistics for each level of education are calculated via the weighted mean of each occupation given its number of workers.
                <br>
                <br>
                For more information on the technical details of this analysis, please see the <a href='https://connorrothschild.github.io/r/automation/' target='_blank'>accompanying blog post</a>. 
                <br>
                <br>
                The R packages powering this site include 
                <a href='https://www.tidyverse.org/' target='_blank'>tidyverse</a>,
                <a href='http://shiny.rstudio.com/' target='_blank'>shiny</a>,
                <a href='https://github.com/ropensci/plotly' target='_blank'>plotly</a>, and 
                <a href='https://github.com/statistiekcbs/scrollytell' target='_blank'>scrollytell</a>.
                </span>
                </p>")


### ALL PLOT OBJECTS

# helpers for all plots:
cols <- c('No formal educational credential' = '#A00042','High school diploma or equivalent' = '#F56C42',
          "Postsecondary nondegree award" = '#008640', "Associate's degree" = '#3487BD', 
          "Bachelor's degree" = '#C71C7E', "Master's degree" = '#5E4FA2',
          "Doctoral or professional degree" = '#1A1A1A') 

legend_ord <- levels(with(data, reorder(typicaled, reveal)))

## Intro plot
# Intro static ggplot
introggPlot <- data %>% 
  filter(typicaled != "Some college, no degree") %>%
  ggplot() +
  geom_point(mapping=aes(x=A_MEDIAN, y=probability, size=TOT_EMP,
                         alpha= 1/7, col=typicaled,
                         text = glue::glue('<span style = "font-size:1.5em">{occupation}</span><br>
                                                <i>Probability of Automation</i>: {probability}%
                                                <i>Median Income</i>: ${comma(A_MEDIAN, digits = 0)}
                                                <i>Number of Workers</i>: {comma(TOT_EMP, digits = 0)}'))) +
  scale_size(range = c(1, 20), guide = 'none') +
  xlab("\nMedian Income") +
  ylab("Probability of Automation") +
  # ggtitle("Likelihood of Job Automation vs Median Income") +
  labs(size= "", col= "", alpha = "") + 
  scale_color_manual(values = cols, breaks = legend_ord) +
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), limits = c(25000,200000)) +
  scale_y_continuous(labels=scales::number_format(suffix="%"), limits = c(0,100)) +
  # theme(legend.position = "top", legend.direction = "horizontal") +
  # legend.text = element_text(colour = ifelse(add == reveal, "black", "grey"))) +
  # legend.text = element_text(colour="black", size = ifelse(add == reveal, 20, 12))) +
  # cr::drop_axis(axis = "y")
  theme(axis.line.x = ggplot2::element_line(colour = NULL, 
                                            size = NULL, linetype = NULL, lineend = NULL), 
        axis.line.y = ggplot2::element_blank(),
        panel.grid.major.x = element_blank()) 

# Convert into ggplotly
introPlot <- ggplotly(introggPlot, tooltip = 'text') %>%
  layout(
    title = element_blank(),
    legend = list(x = 0.65, y = 0.925),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)
