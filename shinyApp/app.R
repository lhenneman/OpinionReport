#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# https://stackoverflow.com/questions/42104031/shiny-interactive-ggplot-with-vertical-line-and-data-labels-at-mouse-hover-poin
#https://stackoverflow.com/questions/46309675/ggplot2-in-r-annotate-outside-of-plot-and-underline-text
# label colors: https://stackoverflow.com/questions/25104213/multiple-colors-in-axes-titles-in-ggplot
setwd( '~/Dropbox/Rpackages/OpinionReport/shinyApp')

library(shiny)
library(ggplot2)
library(jsonlite)
library(data.table)
#library(plotly)

## source json read/merge funciton
source( "../R/merge_AN_data.R")

PAGE_TITLE <- "Abridge News Reaction Data"

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel(windowTitle = PAGE_TITLE,
             title = div( shiny::img(src = "an.jpg", height = 50, width = 50),
                          PAGE_TITLE)),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tags$style(".well {background-color:#ffffff;}"),
      checkboxGroupInput("opinion",
                         "Opinions",
                         choiceNames = c( 'Agree',
                                          'Disagree',
                                          'Undecided'),
                         choiceValues = c( 'agree',
                                           'disagree',
                                           'hmmmm'),
                         selected = c( 'agree',
                                       'disagree',
                                       'hmmmm')),
      textInput("article",
                "Enter Keyword")
    ),


    # Show a plot of the generated distribution
    mainPanel(
      # this is an extra div used ONLY to create positioned ancestor for tooltip
      # we don't change its position
      div(
        style = "position:relative",
        plotOutput("distPlot",
                   hover = hoverOpts("plot_hover",
                                     delay = 50,
                                     delayType = "debounce",
                                     clip = TRUE,
                                     nullOutside = TRUE),
                   click = clickOpts("plot_click",
                                     clip = F)),
        #   uiOutput("hover_info"),
        uiOutput("click_info")
      )
    )
  )#,
  #  verbatimTextOutput("info")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  dt.wgtsums <- read.merge.AN.data()[type == 'political' & is2018midterm == F & date > as.Date( '2018-04-19')]


  # use reactive values -------------------------------
  values.hov <- reactiveValues(loc = NA)
  values.click <- reactiveValues(loc = NULL)

  observeEvent(input$plot_hover, {
    values.hov$loc <- input$plot_hover$y
  })

  # reactive value for the click
  observeEvent(input$plot_click, {
    values.click$loc <- input$plot_click
  })

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    xcut <- dt.wgtsums[ grep( input$article, name, ignore.case = T)][rxnCategory %in% input$opinion]

    vline.pos <- as.numeric( xcut[ which.min( abs( as.numeric (date) - values.hov$loc))]$date)

    rxn.weights.axislimits <- c( -1, 2)
    labels.rl <- data.table( labs = c( 'Left Skew', 'Right Skew'),
                             y = c( -.5, .5),
                             x = max( xcut$date + 10),
                             colors = c('blue', 'red'))

    # Make the plot
    ggplot( data = xcut,
            aes( x = date,
                 y = wgtsums,
                 color = rxnCategory,
                 fill = rxnCategory,
                 group = rxnCategory)) +
      coord_flip() +
      geom_vline( xintercept = ifelse( is.na( vline.pos), NA, vline.pos)) +
      geom_hline( yintercept = 0,
                  color = 'black',
                  lwd = 1) +
      geom_text( data = labels.rl,
                 aes( x = x,
                      y = y,
                      label = labs,
                      color = colors),
                 color = c( 'red' = 'red',
                            'blue' = 'blue'),
                 face = 'bold',
                 size = 12,
                 inherit.aes = F,
                 show.legend = F) +
      geom_line( lwd = 1,
                 alpha = .7) +
      geom_smooth( lwd = 2,
                   formula = "y ~ x",
                   method = ifelse( nrow( xcut) > 20, "loess", 'lm'),
                   fill = 'grey90') +
      scale_x_date( limits = c( min( dt.wgtsums$date),
                                max( dt.wgtsums$date) + 10)) +
      scale_y_continuous( position = "right",
                          limits = rxn.weights.axislimits) +
      scale_color_manual( values = c( 'disagree' = '#F44336',
                                      'agree' = '#4ECB49',
                                      'hmmmm' = '#FED34C')) +
      expand_limits( y = c(-1, 1)) +
      theme_bw() +
      ylab( "Left Skew            Right Skew") +
      theme( axis.title.x  = element_blank(),#text( size = 22, #26,
             #           face = 'bold',
             #           color = 'purple',
             #           hjust = diff( c( rxn.weights.axislimits[1], -.5)) /
             #                       diff( rxn.weights.axislimits)),
             axis.title.y  = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_text( size = 18, #26,
                                         color = 'grey60'),
             axis.ticks = element_blank(),
             panel.border = element_blank(),
             panel.grid = element_blank(),
             plot.title = element_text( size = 26,
                                        hjust = 0.5,
                                        face = 'bold'),
             legend.position = 'none')
    #ggplotly()
  })

  output$click_info <- renderUI({
    click <- values.click$loc
    if( is.null(click)) return( NULL)

    xcut <- dt.wgtsums[ grep( input$article, slug)][rxnCategory %in% input$opinion]
    point <- xcut[ which.min( abs( as.numeric (date) - click$y))]

    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    right_pct <- 1 #click$domain$right / (click$domain$right - click$domain$left)
    top_pct <- (click$domain$top - click$y) / (click$domain$top - click$domain$bottom)

    # calculate distance from left and bottom side of the picture in pixels
    right_px <- click$range$right - right_pct * (click$range$right - click$range$left)
    top_px <- click$range$top + top_pct * (click$range$bottom - click$range$top)

    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "right:", right_px + 2, "px; top:", top_px + 2, "px;")

    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0(
        '<a href=', paste0( '"http://www.abridgenews.com/topic/', point$slug, '"'), 'target="_blank"> ',
        "<b> ", point$name, "</b> <br/> </a>",
        format( point$date, format="%B %d, %Y"), "<br/>")))
    )
  })

  # observeEvent(input$plot_click, {
  # #  print(input$plot_click)
  #   removeUI(
  #     #https://shiny.rstudio.com/reference/shiny/1.1.0/removeUI.html
  #     #https://api.jquery.com/last-selector/
  #     selector = ".well:last"
  #   )
  # })


  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }

    paste0(
      "hover: ", xy_str(input$plot_hover), "\n",
      "click: ", xy_str(input$plot_click)
    )
  })

}

# Run the application
shinyApp(ui = ui, server = server)

