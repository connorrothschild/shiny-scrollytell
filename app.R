library(shiny)
library(scrollytell)
library(shinyjs)
library(ggvis)
library(waypointer)
library(shticky)
library(plotly)

# cr::set_cr_theme(font="lato")
theme_set(theme_minimal())

source("source_code_for_shiny.R")

ui <- fluidPage(
  
  # suppress warning messages while data is loading on-screen 
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tags$head(
    includeCSS("www/style.css")
  ),
  
  # article title & name
  fluidRow(HTML("<center>
                <h1>Automation and Its Impact on Jobs</h1>
                <p style='font-size:26px'> by <a href='https://connorrothschild.github.io/' target='_blank'>Connor Rothschild</a></p>
                </center>")
  ),
  
  br(),
  
  fluidRow(
    column(1),
    
    column(10,
           # intro text
           fluidRow(id='text',
                    column(2),
                    column(8, 
                           br(),
                           text0),
                    column(2)),
           # plot object for intro
           plotlyOutput("introPlot", height = '400px')
           ),
    
    column(1),
    
           ),
  
  # scrollytelling plot
  scrolly_container("scr"
                    , scrolly_graph( br(), 
                                     br(),
                                     textOutput("section"),
                                     br(),
                                     HTML('<center>'),
                                     plotlyOutput("plot", height = '600px'),
                                     HTML('</center>')
                                     
                    )
                    , scrolly_sections(
                      HTML('<center>'),
                      scrolly_section(id = 0, render_text(0)),
                      scrolly_section(id = 1, render_text(1)),
                      scrolly_section(id = 2, render_text(2)),
                      scrolly_section(id = 3, render_text(3)),
                      scrolly_section(id = 4, render_text(4)),
                      scrolly_section(id = 5, render_text(5)),
                      scrolly_section(id = 6, render_text(6)),
                      scrolly_section(id = 7, render_text(7)),
                      scrolly_section(id = 8, render_text(8)),
                      # add a scrolly_section with nothing in it; 
                      # this buffer prevents the plot from disappearing while reading last section
                      scrolly_section(id = "buffer", br()),
                      HTML('</center>')
                    )
                    
  ),
  
  # Concluding text
  div(fluidRow(id = 'text',
               column(2),
               column(8, 
                      HTML("<p><span style='font-size:24px'><b>The Risk of Automation</b></span>
                        <br>
                            <span style='font-size:18px'>Using the dataset I’ve used in this project, researchers Carl Frey and Michael Osborne predicted that 47% of jobs are at risk of automation over the next couple decades.
                        <br>
                            <br>The visuals above suggest that the ills of automation may not be evenly distributed across jobs.
                            Less educated workers are more likely to face job loss as a product of automation. Those with high school diplomas or less find themself concentrated near the top of the y-axis, while those with bachelor’s degrees or higher face a lower risk of automation.
                        <br>
                            <br>A job’s salary is also predictive of automation probability. As the median income of a profession increases, the likelihood of automation displacing its workers decreases.
                            This could suggest that automation will increasingly bifurcate the already divided labor market, making those at the top wealthier at the expense of the worse-off.
                        <br>
                            <br>Automation’s impact on work necessitates a policy response. The fact that automation will have different effects on different industries and different workers is a reminder that this public policy will have to be strategic and thoughtful.</span></p>"),
                      br()
               ),
               column(2)
  ), style = 'margin-top: -300px;'),
  
  br(),
  br(),
  br(),
  hr(),
  
  fluidRow(
    column(1),
    column(10,
           HTML("<p>
                <span style='font-size:18px'><i>Technical Notes</i></span><br>
                <br>
                <span style='font-size:12px'>
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
                This post may have technical errors. This post was an exercise to learn R and is not a comprehensive nor verifiably accurate account of automation's impact on jobs. 
                Please refrain from citing this as anything other than an example of R usage in a scrollytelling context.
                <br>
                For more information on the technical details of this analysis, please see the <a href='https://connorrothschild.github.io/r/automation/' target='_blank'>accompanying blog post</a>. 
                <br>
                <br>
                The R packages powering this site include 
                <a href='https://www.tidyverse.org/' target='_blank'>tidyverse</a>,
                <a href='http://shiny.rstudio.com/' target='_blank'>shiny</a>,
                <a href='https://ggvis.rstudio.com' target='_blank'>ggvis</a>,
                <a href='https://github.com/RinteRface/waypointer' target='_blank'>waypointer</a>, and 
                <a href='https://github.com/JohnCoene/shticky' target='_blank'>shticky</a>.
                </span>
                </p>")
    ),
    column(1)
  ),
  br(),
  br(),
column(1)

)

# Define server logic
server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    
    add <- input$scr
    
    plot <- data %>% 
      filter(typicaled != "Some college, no degree") %>%
      filter(if (add != 8) add >= reveal else reveal %in% c(1:8)) %>%
      ggplot() +
      geom_point(mapping=aes(x=A_MEDIAN, y=probability, size=TOT_EMP,
                             alpha=ifelse(add == reveal, 1/5, 1/10), col=typicaled,
                             text = glue::glue('<b>Occupation</b>: {occupation}
                                                <b>Probability of Automation</b>: {probability}%
                                                <b>Median Income</b>: ${A_MEDIAN}
                                                <b>Number of Workers</b>: {TOT_EMP}'))) +
      scale_size(range = c(1, 20)) +
      xlab("\nMedian Income") +
      ylab("Probability of Automation") +
      labs(size= "", col= "", alpha = "") +
      scale_color_manual(values = cols, breaks = legend_ord) +
      scale_x_continuous(labels=scales::dollar_format(prefix="$"), limits = c(25000,200000)) +
      scale_y_continuous(labels=scales::number_format(suffix="%"), limits = c(0,100)) +
      # cr::drop_axis(axis = "y") +
      theme(axis.line.x = ggplot2::element_line(colour = NULL, 
                                                size = NULL, linetype = NULL, lineend = NULL), 
            axis.line.y = ggplot2::element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly(plot, tooltip = 'text') %>%
    layout(
      title = list(element_blank()),
      legend = list(x = 0.65, y = 0.925),
      font = list(family = 'Lato'),
      margin=list(t=50),
      hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>%
    config(displaylogo = F, showSendToCloud = F, displayModeBar = F)
    
  })
    
  output$introPlot <- renderPlotly({introPlot})
  output$scr <- renderScrollytell({scrollytell()})
  renderText(paste0("Section: ", input$scr))
  observe({cat("section:", input$scr, "\n")})
  
}
# Run the application
shinyApp(ui = ui, server = server)
