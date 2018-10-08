#use reactive function to grab data - http://shiny.rstudio.com/tutorial/written-tutorial/lesson6/
# find where the cursor is. https://github.com/rstudio/webinars/blob/master/16-Interactive-graphics/shiny-interactive-graphics.pdf
clickInput <- reactive({
  xcut <- dt.wgtsums[ grep( input$article, slug)][rxnCategory %in% input$opinion]

  hover <- input$plot_click
  x.hover <- hover$x
  y.hover <- hover$y
  x.order <- levels(factor(xcut$slug))
  if( !is.null( x.hover)){
    label.dt <- data.table( x = round( x.hover),
                            y = round( y.hover),
                            lab = x.order[round( y.hover)])
  } else
    label.dt <- data.table( x = 0,
                            y = 0,
                            lab = "trumps-debt-ceiling-deal",
                            stringsAsFactors = T)

  return( label.dt)
})

