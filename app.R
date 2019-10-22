library(shiny)
library(shinyjs)
library(ggvis)
library(waypointer)
library(shticky)

source("source_code_for_shiny.R")

OFFSET <- "0"
# ANIMATION <- "slideInUp"

ui <- fluidPage(
  tags$head(
    includeCSS("www/style.css")
  ),
  use_shticky(),
  use_waypointer(),
  div(
    div(
      id = "stick",
      style = "position:relative;width:100%;height:600px;",
      fluidRow(
        # column(width = 1),
        column(width = 12,
               ggvisOutput("plot")
               )
      )
    ),
    longdiv(
      h1("Automation and Its Impact on Jobs", class = "title"),
      # br(),
      h1(
        class = "subtitle",
        "How do different occupations differ in their susceptibility to automation?"
      )
      # br(),
      # p(
      #   # style = "text-align:center;",
      #   "How do different occupations differ in their susceptibility to automation?",
      #   tags$a(
      #     class = "sg",
      #     tags$i(class = "fas fa-external-link-alt"),
      #     target = "_blank",
      #     href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-01-01"
      #   )
      # ),
      # br(),
      # br(),
      # br()
      # p(
      #   style = "word-wrap:break-word;white-space:normal;",
      #   tags$i(class = "fas fa-chevron-down fa-3x")
      # )
    ),
  longdiv(
    div(
      id = "m1",
      uiOutput("1")
    )
  ),
  longdiv(
    div(
      id = "m2",
      uiOutput("2")
    )
  ),
  longdiv(
    div(
      id = "m3",
      uiOutput("3")
    )
  ),
  longdiv(
    div(
      id = "m4",
      uiOutput("4")
    )
  ),
  longdiv(
    div(
      id = "m5",
      uiOutput("5")
    )
  ),
  longdiv(
    div(
      id = "m6",
      uiOutput("6")
    )
  ),
  longdiv(
    div(
      id = "m7",
      uiOutput("7")
    )
  ),
  longdiv(
    div(
      id = "m8",
      uiOutput("8")
    )
  ),
  longdiv(
    id = "m9",
    uiOutput("9")
  )
  ),
  longdiv(
    id = "m10",
    h1("Thank you!", class = "title"),
    br(),
    p(
      "Please visit my blog, my GitHub, or my Twitter.", class = "subtitle"
      ),
    br(),
    div(
    tags$a("Personal Website",
      href = "https://connorrothschild.github.io", class = "subtitle"
    ),
    br(),
    tags$a("Twitter",
      href = "https://twitter.com/CL_Rothschild", class = "subtitle"
    ),
    br(),
    tags$a("GitHub",
      href = "https://github.com/connorrothschild/", class = "subtitle"
    ), style = "text-align:center;"
    )
    )
)

server <- function(input, output, session) {
  
  w1 <- Waypoint$
    new("m1", offset = OFFSET, animate = FALSE)$
    start()
  w2 <- Waypoint$
    new("m2", offset = OFFSET, animate = FALSE)$
    start()
  w3 <- Waypoint$
    new("m3", offset = OFFSET, animate = FALSE)$
    start()
  w4 <- Waypoint$
    new("m4", offset = OFFSET, animate = FALSE)$
    start()
  w5 <- Waypoint$
    new("m5", offset = OFFSET, animate = FALSE)$
    start()
  w6 <- Waypoint$
    new("m6", offset = OFFSET, animate = FALSE)$
    start()
  w7 <- Waypoint$
    new("m7", offset = OFFSET, animate = FALSE)$
    start()
  w8 <- Waypoint$
    new("m8", offset = OFFSET, animate = FALSE)$
    start()
  w9 <- Waypoint$
    new("m9", offset = OFFSET, animate = FALSE)$
    start()
  
  output$`1` <- renderUI({
    # req(w1$get_triggered())
    # if(w1$get_triggered() == TRUE) 
      # render_text(1)
  })
  
  output$`2` <- renderUI({
    # req(w2$get_triggered())
    # if(w2$get_triggered() == TRUE) 
      render_text(1)
  })
  
  output$`3` <- renderUI({
    # req(w3$get_triggered())
    # if(w3$get_triggered() == TRUE) 
      render_text(2)
  })
  
  output$`4` <- renderUI({
    # req(w4$get_triggered())
    # if(w4$get_triggered() == TRUE) 
      render_text(3)
  })
  
  output$`5` <- renderUI({
    # req(w5$get_triggered())
    # if(w5$get_triggered() == TRUE) 
      render_text(4)
  })
  
  output$`6` <- renderUI({
    # req(w6$get_triggered())
    # if(w6$get_triggered() == TRUE) 
      render_text(5)
  })
  
  output$`7` <- renderUI({
    # req(w7$get_triggered())
    # if(w7$get_triggered() == TRUE) 
      render_text(6)
  })
  
  output$`8` <- renderUI({
    # req(w8$get_triggered())
    # if(w8$get_triggered() == TRUE) 
      render_text(7)
  })
  
  output$`9` <- renderUI({
    # req(w8$get_triggered())
    # if(w8$get_triggered() == TRUE) 
    render_text(8)
  })
  
  # Our sticky plot
  shtick <- Shtick$
    new("#stick")$
    shtick()

  vis <-  reactive({
 
    data %>% 
      filter(typicaled != "Some college, no degree") %>% 
      ggvis(~A_MEDIAN, ~probability, opacity := 0.5, key := ~occupation) %>% 
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
  
output$plot <-
   vis %>% bind_shiny("plot")

observeEvent(w1$get_direction(), {
  if(w1$get_direction() == "down") add_data(1)
})

observeEvent(w2$get_direction(), {
  if(w2$get_direction() == "down") add_data(2)
})

observeEvent(w3$get_direction(), {
  if(w3$get_direction() == "down") add_data(3)
})

observeEvent(w4$get_direction(), {
  if(w4$get_direction() == "down") add_data(4)
})

observeEvent(w5$get_direction(), {
  if(w5$get_direction() == "down") add_data(5)
})

observeEvent(w6$get_direction(), {
  if(w6$get_direction() == "down") add_data(6)
})

observeEvent(w7$get_direction(), {
  if(w7$get_direction() == "down") add_data(7)
})

observeEvent(w8$get_direction(), {
  if(w8$get_direction() == "down") add_data(8)
})

} 

shinyApp(ui, server)
