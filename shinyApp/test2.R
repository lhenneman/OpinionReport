library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(shinySignals)

ui <- fluidPage(

  titlePanel("Interactive Plot"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("points",
                  "Number of points:",
                  min = 10,
                  max = 50,
                  value = 25),
      textOutput(outputId = "x.pos"),
      textOutput(outputId = "y.pos"),
      textOutput(outputId = "num_points")
    ),

    mainPanel(
      plotOutput("distPlot", hover = hoverOpts(id = "plot_hover",
                                               delay = 100,
                                               delayType = "debounce")))))

server <- function(input, output) {


  # Create dataframe and plot object
  plot_data <- reactive({
    x <- 1:input$points
    y1 <- seq(1,10 * input$points, 10)
    y2 <- seq(20,20 * input$points, 20)

    df <- data.frame(x,y1,y2)
    df <- df %>% gather(key = series, value = value, y1:y2)
    return(df)
  })

  # use reactive values -------------------------------
  values <- reactiveValues(loc = 0)

  observeEvent(input$plot_hover$x, {
    values$loc <- input$plot_hover$x
  })

  # if you want to reset the initial position of the vertical line when input$points changes
  observeEvent(input$points, {
    values$loc <- 0
  })

  # Render Plot --------------------------------------
  output$distPlot <- renderPlot({
    ggplot(plot_data(),aes(x=x, y=value, group=series, color=series))+
      geom_line() +
      geom_point()+
      geom_vline(aes(xintercept = values$loc))
  })

  # Render mouse position into text

  output$x.pos <- renderText(paste0("values$loc = ",values$loc))
  output$y.pos <- renderText(paste0("input$plot_hover$x = ",input$plot_hover$x ))
}

# Run the application
shinyApp(ui = ui, server = server)
