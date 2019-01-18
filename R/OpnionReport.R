

OpRep <- function( title = NULL,
                   directory.in,
                   pattern.in = '.csv',
                   directory.out,
                   text.colorbounds = c("#1028C7", "#E8273A"),
                   plottype = c("original", "4horizontal"),
                   max.label = 0.1){

  #select plottype
  if( length( plottype) == 2){
    plottype <- "original"
    print( "Default plottype -- original")
  }

  #load emoji font
  list.emojifonts()
  load.emojifont('OpenSansEmoji.ttf')
  emos <-  c( 'Agree' = emo::ji("+1"),
              'Disagree' = emo::ji("-1"),
              'Undecided' = emo::ji("confused"))

  # import files - defaults to most recent
  file.name <- tail( list.files( path = directory.in,
                                 pattern = pattern.in),
                     n = 1)
  topic.name <-  gsub( '\\d{4}-\\d{2}-\\d{2}|.csv',
                       '',
                       file.name)
  topic.date <- gsub( paste0( c( topic.name,
                                 '.csv'),
                              collapse =  '|'),
                      '',
                      file.name)

  dataday1 <- fread( file.path( directory.in,
                                file.name))

  # assign emojis
  dataday1$emost <- emos[dataday1$opinion]
  dataday1[, emotext := paste0( values * 100,
                                '% ',
                                emost)]

  # order based on opinions
  order_opinions <- c( 'Agree', 'Undecided', 'Disagree')
  dataday1$opinion <- factor( dataday1$opinion,
                              levels = order_opinions)

  # define color function to grab pallette
  colfunc <- colorRampPalette( text.colorbounds)

  # Get numeric version of x values
  dataday1$xvarn <- cumsum( c( 1,
                               as.numeric( diff( factor( dataday1$article))) != 0))
  dataday1[, xlab := xvarn]
  dataday1[, articles := factor( xvarn)]

  # Make new df with xvarn values shifted to right and left by .3
  df2 <- rbind(transform(dataday1, xvarn = xvarn-.45),
               transform(dataday1, xvarn = xvarn+.45))

  # define outname (file for original plot, directory for 4horizontal)
  outname <- file.path( directory.out,
                        gsub( '.csv$',
                              '',
                              file.name))


  # make the plot!
  if( plottype == "original"){
    suppressWarnings(
      ggsmiley2 <- ggplot( data = df2,
                           aes( fill = opinion,
                                # colour = opinion,
                                x = xvarn,
                                y = values)) +
        ggtitle( title) +
        geom_area( colour=NA,
                   alpha = 0.7) +
        geom_col( data = dataday1,
                  position = "stack",
                  stat = "identity",
                  aes( width = 0.9)) +
        scale_x_continuous( breaks = 1:max( df2$xvarn),
                            labels = unique( df2$article),
                            position = "top",
                            expand = c( 0, 0)) +
        scale_fill_manual( values = c( 'Disagree' = '#F44336',
                                       'Agree' = '#4ECB49',
                                       'Undecided' = '#FED34C')) +
        scale_y_discrete( expand = c( 0, 0)) +
        geom_text( data = dataday1[values != 0],
                   aes( label = emotext),
                   family='OpenSansEmoji',
                   position = position_stack( vjust = 0.5),
                   size = 8,
                   parse = F,
                   colour = "white",
                   fontface = "bold") +
        annotate( "text",
                  colour = "white",
                  x = 4,
                  y = .03,
                  label= "www.abridgenews.com",
                  size = 3.5) +
        annotate( "text",
                  colour = "white",
                  x = .825,
                  y = .03,
                  label= format.Date( topic.date,
                                      format = '%d %B, %Y'),
                  size = 3.5) +
        theme_bw() +
        theme( axis.title = element_blank(),
               axis.text.x = element_text( size = 18, #26,
                                           face = 'bold',
                                           colour = colfunc(4)),
               axis.text.y = element_blank(),
               axis.ticks = element_blank(),
               panel.border = element_blank(),
               panel.grid = element_blank(),
               plot.title = element_text( size = 26,
                                          hjust = 0.5,
                                          face = 'bold'),
               legend.position = 'none')
    )
    ggsave(ggsmiley2,
           filename = paste0( outname,'.jpg'),
           width = 7,
           height = 7,
           units = "in")
  } else {
    dir.create( outname, recursive = T, showWarnings = F)

    for( a in unique( dataday1[,article])){

      top.dt <- dataday1[ article == a]
      top.dt[, opinion := factor( top.dt$opinion,
                                  levels = rev( levels( top.dt$opinion)))]
      top.dt[ values < max.label, emotext := ""]

      gg4 <- ggplot( data = top.dt,
                     aes( fill = opinion,
                          x = 1,
                          y = values)) +
        geom_bar( stat = 'identity') +
        coord_flip() +
        scale_fill_manual( values = c( 'Disagree' = '#F44336',
                                       'Agree' = '#4ECB49',
                                       'Undecided' = '#FED34C')) +
        scale_y_discrete( expand = c( 0, 0)) +
        geom_text( data = top.dt,
                   aes( label = emotext),
                   family='OpenSansEmoji',
                   position = position_stack( vjust = 0.5),
                   size = 1.5,
                   parse = F,
                   #    colour = "white",
                   fontface = "bold") +
        theme_bw() +
        theme( axis.title = element_blank(),
               axis.text.x = element_blank(),
               axis.text.y = element_blank(),
               axis.ticks = element_blank(),
               panel.border = element_blank(),
               panel.grid = element_blank(),
               plot.title = element_text( size = 26,
                                          hjust = 0.5,
                                          face = 'bold'),
               plot.margin=grid::unit(c(0,0,0,0),"cm"),
               legend.position = 'none')

      ggsave(gg4,
             filename = file.path( outname,
                                   paste0( topic.name, "_",
                                           a, '.jpg')),
             width = 2,
             height = .2,
             units = "in",
             bg = "transparent",
             dpi = 600
      )

    }
  }
}
