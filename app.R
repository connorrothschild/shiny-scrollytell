library(shiny)
library(scrollytell)
library(shinyjs)
library(ggvis)
library(waypointer)
library(shticky)

cr::set_cr_theme()

source("source_code_for_shiny.R")

# Define UI

ui <- fluidPage(
  
  # Code to suppress warning messages while data is loading on-screen 
  # reference (https://groups.google.com/forum/#!topic/shiny-discuss/FyMGa2R_Mgs)
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tags$head(
    includeCSS("www/style.css")
  ),
  
  # Article title
  fluidRow(HTML("<center>
                <h1>Automation and Its Impact on Jobs</h1>
                <p style='size:18px';> by <a href='https://connorrothschild.github.io/' target='_blank'>Connor Rothschild</a></p>
                </center>")
  ),
  br(),
  
  # plot object for intro
  fluidRow(
    column(1),
    column(10,
           # Introduction
           fluidRow(id='text',
                    column(1),
                    column(10, 
                           br(),
                           text0,
                           br(),
                           # ggvisOutput("interactivePlot"),
                           plotOutput("introPlot", height = '600px')
                    ),
                    column(1)
           ),
           
           br(), 
           br()
           )
    ),
  
  # scrollytelling plot
  scrolly_container("scr"
                    , scrolly_graph( br(), 
                                     br(),
                                     textOutput("section"),
                                     br(),
                                     HTML('<center>'),
                                     plotOutput("plot", height = '600px'),
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
                      br(),
                      HTML("<p><span style='font-size:30px'><b>The tournament</b></span>. As expected, once the men's Olympic basketball competition began, there was only one team that could leave Barcelona with the gold medal.  The United States cruised through all eight of its contests. In fact, during several games, it appeared as though the opposing team was just happy to be on the same floor as the NBA players.<br><br>
                                
                                Team USA averaged 117 points per game and held opponents to a measly 73 points per game over the two-week stretch.  Charles Barkley (18 PPG) and Michael Jordan (14.9 PPG) led the way in scoring.<br><br>
                                Below is a look at player contributions in each of their eight victories.</p>"),
                      br()
               ),
               column(2)
  ), style = 'margin-top: -300px;'),
  
  br(),
  br(),
  br(),
  
  fluidRow(id = 'text',
           column(2),
           column(8, 
                  br(),
                  HTML("<p><span style='font-size:30px'><b>The impact</b></span>. The Dream Team's gold medal finish at the 1992 Olympics avenged a third place finish in 1988 and put the USA back on top of the basketball world - by a large margin. NBA popularity skyrocketed around the globe, thanks in part to the larger-than-life personalities of superstars like Michael Jordan, Magic Johnson, and Charles Barkley.<br><br>
                                    
                                    Team USA's decisive victory sent a message to the rest of the world that in order to compete for a championship on the global stage, huge improvements in skill and player development was essential. The world responded.  By 2004, international teams and players had elevated their games so much so that team USA settled for the bronze medal that year.<br><br>
                                    
                                    Since the 1992 games, numerous international players (such as Dirk Nowitzki, Manu Ginobili, Tony Parker, Pau and Marc Gasol) have come to the NBA, won championships, and found great success. While team USA is still a major favorite to stay on top, the Dream Team's influence on the international competition has ensured that this will be no easy task.<br><br>
                                    
                                    The game of basketball is better for it. </p>"),
                  br()
           ),
           column(2)
  ),
  br(),
  br(),
  hr(),
  br(),
  
  fluidRow(
    column(2),
    column(8,
           HTML("<p>
                <span style='font-size:18px'><i><u>Technical Notes</u></i></span><br>
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
    column(2)
  ),
  br(),
  br(),
column(1)

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
      theme(legend.position = "top", legend.direction = "horizontal") +
            # legend.text = element_text(colour = ifelse(add == reveal, "black", "grey"))) +
            # legend.text = element_text(colour="black", size = ifelse(add == reveal, 20, 12))) +
      cr::drop_axis(axis = "y")
    
  })
    
  # output$interactivePlot <- vis %>% bind_shiny("vis")
  output$introPlot <- renderPlot({introPlot})
  output$scr <- renderScrollytell({scrollytell()})
  renderText(paste0("Section: ", input$scr))
  observe({cat("section:", input$scr, "\n")})
  
}
# Run the application
shinyApp(ui = ui, server = server)