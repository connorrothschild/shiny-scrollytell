library(here)
source(here("scripts/source_code_for_shiny.R"))

ui <- fluidPage(
  
  title = "Automation and Its Impact on Jobs",
  
  # meta tags
  meta() %>%
    meta_social(
      title = "Automation and Its Impact on Jobs",
      description = "And an exercise in Shiny Scrollytelling",
      # url = "https://connorrothschild.shinyapps.io/automation/",
      image = "https://raw.githubusercontent.com/connorrothschild/shiny-scrollytell/master/images/thumbnail.png",
      image_alt = "Automation and Its Impact on Jobs",
      twitter_creator = "@CL_Rothschild",
      twitter_card_type = "summary_large_image",
      twitter_site = "@CL_Rothschild"
    ),
  
  # suppress warning messages while data is loading on-screen
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  tags$head(includeCSS("www/style.css")),
  
  # article title & name
  fluidRow(
    HTML(
      "<center>
                <h1>Automation and Its Impact on Jobs</h1>
                <p style='font-size:26px'> by <a href='https://connorrothschild.github.io/' target='_blank'>Connor Rothschild</a></p>
                </center>"
    )
  ),
  
  br(),
  
  fluidRow(column(1),
           
           column(
             10,
             # intro text
             fluidRow(id = 'text',
                      column(1),
                      column(
                        10,
                        br(),
                        text0,
                        hr(),
                        h1(
                          class = "instructions",
                          "How to read this chart:",
                          br(),
                          br(),
                          "The size of each",
                          icon("circle"),
                          "corresponds to the number of workers in that job.",
                          br(),
                          "Hover over each",
                          icon("circle"),
                          "to see details on the occupation's income and probability of automation.",
                          br(),
                          "Double click on a",
                          icon("circle"),
                          "in the legend to focus on a specific level of education."
                        )
                      ),
                      column(1)),
             # plot object for intro
             plotlyOutput("introPlot", height = '400px')
           ),
           
           column(1)),
  
  # scrollytelling plot
  scrolly_container(
    "scr"
    ,
    scrolly_graph(
      br(),
      br(),
      textOutput("section"),
      br(),
      HTML('<center>'),
      plotlyOutput("plot", height = '600px'),
      HTML('</center>')
      
    )
    ,
    scrolly_sections(
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
  
  # concluding text
  div(fluidRow(
    id = 'text',
    column(2),
    column(8,
           concludingtext,
           br()),
    column(2)
  ), style = 'margin-top: -300px;'),
  
  br(),
  br(),
  br(),
  hr(),
  
  fluidRow(column(1),
           column(10,
                  technicalnotes),
           column(1)),
  br(),
  br(),
  column(1)
  
)

# server
server <- function(input, output, session) {
  output$plot <- renderPlotly({
    add <- input$scr
    
    plot <- data %>%
      filter(typicaled != "Some college, no degree") %>%
      filter(if (add != 8)
        add >= reveal
        else
          reveal %in% c(1:8)) %>%
      ggplot() +
      geom_point(
        mapping = aes(
          x = A_MEDIAN,
          y = probability,
          size = TOT_EMP,
          alpha = ifelse(add == reveal, 1 / 5, 1 / 10),
          col = typicaled,
          text = glue::glue(
            '<span style = "font-size:1.5em">{occupation}</span><br>
                                                <i>Probability of Automation</i>: {probability}%
                                                <i>Median Income</i>: ${comma(A_MEDIAN, digits = 0)}
                                                <i>Number of Workers</i>: {comma(TOT_EMP, digits = 0)}'
          )
        )
      ) +
      scale_size(range = c(1, 20)) +
      xlab("\nMedian Income") +
      ylab("Probability of Automation") +
      labs(size = "",
           col = "",
           alpha = "") +
      scale_color_manual(values = cols, breaks = legend_ord) +
      scale_x_continuous(
        labels = scales::dollar_format(prefix = "$"),
        limits = c(25000, 200000)
      ) +
      scale_y_continuous(labels = scales::number_format(suffix = "%"),
                         limits = c(0, 100)) +
      # cr::drop_axis(axis = "y") +
      theme(
        axis.line.x = ggplot2::element_line(
          colour = NULL,
          size = NULL,
          linetype = NULL,
          lineend = NULL
        ),
        axis.line.y = ggplot2::element_blank(),
        panel.grid.major.x = element_blank()
      )
    
    ggplotly(plot, tooltip = 'text') %>%
      layout(
        title = list(element_blank()),
        legend = list(x = 0.65, y = 0.925),
        font = list(family = 'Lato'),
        margin = list(t = 50),
        hoverlabel = list(bgcolor = 'whitesmoke', color = 'darkGray')
      ) %>%
      config(
        displaylogo = F,
        showSendToCloud = F,
        displayModeBar = F
      )
    
  })
  
  output$introPlot <- renderPlotly({
    introPlot
  })
  output$scr <- renderScrollytell({
    scrollytell()
  })
  renderText(paste0("Section: ", input$scr))
  observe({
    cat("section:", input$scr, "\n")
  })
  
}
# Run the application
shinyApp(ui = ui, server = server)
