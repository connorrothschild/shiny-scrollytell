#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scrollytell)
library(shinyjs)
library(ggvis)
library(waypointer)
library(shticky)

cr::set_cr_theme()

source("source_code_for_shiny.R")

# Define UI for application that draws a histogram

ui <- fluidPage(
  
  # Code to suppress warning messages while data is loading on-screen 
  # reference (https://groups.google.com/forum/#!topic/shiny-discuss/FyMGa2R_Mgs)
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tags$head(
    includeCSS("www/style.css")
  ),
  
  # Application title
  # Article title
  fluidRow(HTML("<center>
                <h1>1992: Basketball's Dream Summer</h1>
                <p style='size:18px';> by <a href='https://datascott.com/' target='_blank'>Scott Davis</a></p>
                </center>")
  ),
  br(),
  
  fluidRow(
    column(1),
    column(10,
           # Introduction
           fluidRow(id='text',
                    column(2),
                    column(8, 
                           br(),
                           HTML("<p><span style='font-size:30px'><b>Basketball fans</b></span>. If you could travel to a place and time where local teams (at every level) were at the center of the basketball discussion, where and when would it be?
                           <br><br>
                           For me, that question's easy. I lived it. Growing up in Michigan and reaching peak basketball fandom in the late 1980's and early 1990's, I witnessed championship caliber basketball on multiple levels. The Detroit Pistons' group of <a href='http://www.espn.com/30for30/film/_/page/badboys' target='_blank'>Bad Boys</a> were winning back-to-back titles in the NBA, and the University of Michigan's <a href='http://www.espn.com/watch/film/932e63d5-df33-4303-bc0f-590f6037b3a9/the-fab-five' target='_blank'>Fab Five</a> were all the rage in the NCAA.
           <br><br>
           While these teams were collecting wins and polarizing their respective leagues, another team, soon to be assembled, would unite basketball fans across the country. Prior to the 1992 Summer Olympics, rules dictated that men's basketball teams had to be comprised of amateurs and/or non-NBA professionals. As a result, the United States gathered the country's top collegiate talent to compete on the global stage, often against international teams made up of professionals. During the summer of 1992, however, everything changed. The ban on NBA players was lifted, and Team USA assembled it's first roster composed entirely of NBA players (well, one college player), aptly referred to as The Dream Team.</p>"),
                           br()
                    ),
                    column(2)
           ),
           
           br(), 
           br()
           )
    ),
  scrolly_container("scr"
                    , scrolly_graph( br(), 
                                     br(),
                                     textOutput("section"),
                                     br(),
                                     HTML('<center>'),
                                     plotOutput("plot"),
                                     HTML('</center>')
                                     
                    )
                    , scrolly_sections(
                      # HTML('<center>'),
                      scrolly_section(id = 8),
                      scrolly_section(id = 1, render_text(1)),
                      scrolly_section(id = 2, render_text(2)),
                      scrolly_section(id = 3, render_text(3)),
                      scrolly_section(id = 4, render_text(4)),
                      scrolly_section(id = 5, render_text(5)),
                      scrolly_section(id = 6, render_text(6)),
                      scrolly_section(id = 7, render_text(7)),
                      scrolly_section(id = 8, render_text(8))
                      # HTML('</center>')
                    )
                    
  )
)

# Define server logic
server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    add <- input$scr
    
    legend_ord <- levels(with(data, reorder(typicaled, reveal)))
    
    data %>% 
      filter(typicaled != "Some college, no degree") %>%
      filter(if (add != 8) add >= reveal else reveal %in% c(1:8)) %>%
    ggplot() +
      geom_point(mapping=aes(x=A_MEDIAN, y=probability, size=TOT_EMP,
                             alpha=ifelse(add == reveal, 1/5, 1/10), col=typicaled))+
      scale_size(range = c(1, 20), guide = 'none') +
      xlab("\nMedian Income") +
      ylab("Probability of Automation") +
      # ggtitle("Likelihood of Job Automation vs Median Income") +
      labs(size=element_blank(), col=element_blank()) +
      labs(alpha=NULL) +
      guides(alpha=FALSE) +
      scale_color_manual(values = cols, breaks = legend_ord) +
      scale_x_continuous(labels=scales::dollar_format(prefix="$"), limits = c(25000,200000)) +
      scale_y_continuous(labels=scales::number_format(suffix="%"), limits = c(0,100)) +
      theme(legend.position = "top", legend.direction = "horizontal", 
            legend.text = element_text(colour="black", size = 12)) +
                                         #ifelse(add != 8, 20, 12))) +
      cr::drop_axis(axis = "y")
    
  })
    
  
  output$scr <- renderScrollytell({scrollytell()})
  renderText(paste0("Section: ", input$scr))
  observe({cat("section:", input$scr, "\n")})
  
}
# Run the application
shinyApp(ui = ui, server = server)