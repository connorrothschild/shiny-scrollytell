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

source("source_code_for_shiny.R")

# Define UI for application that draws a histogram

ui <- fluidPage(
  
  # Application title
  titlePanel("Scrolly Telling"),
              scrolly_container("scr"
                                , scrolly_graph( textOutput("section"),
                                                 ggvisOutput("vis")
                                                 
                                )
                                , scrolly_sections(
                                  scrolly_section(id = 1, render_text(1)),
                                  scrolly_section(id = 2, render_text(2)),
                                  scrolly_section(id = 3, render_text(3)),
                                  scrolly_section(id = 4, render_text(4)),
                                  scrolly_section(id = 5, render_text(5)),
                                  scrolly_section(id = 6, render_text(6)),
                                  scrolly_section(id = 7, render_text(7)),
                                  scrolly_section(id = 8, render_text(8)),
                                  scrolly_section(id = 9, render_text(9))
                                )
              
            ),
  div("Footer")
)

# Define server logic
server <- function(input, output, session) {
    
   observeEvent(input$scr, {
  
     add <- input$scr
     
    vis <-  reactive({
      
      data %>% 
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
     
   })
    
  output$scr <- renderScrollytell({scrollytell()})
  output$section <- renderScrollytell({scrollytell()})
    # renderText(paste0("Section: ", input$scr))
  observe({cat("section:", input$scr, "\n")})

}
# Run the application
shinyApp(ui = ui, server = server)