

OpRep <- function( title = NULL,
                   directory.in,
                   pattern.in = '.csv',
                   directory.out,
                   text.colorbounds = c("#1028C7", "#E8273A"),
                   plottype = c("original", "4horizontal"),
                   max.label = 0.1,
                   max.emoji = 0.15){

  #select plottype
  if( length( plottype) == 2){
    plottype <- "original"
    print( "Default plottype -- original")
  }

  #load emoji font
  emos <-  c( 'Agree' = system.file("icons", "Agree.png", package = "OpinionReport"),
              'Undecided' = system.file("icons", "Undecided.png", package = "OpinionReport"),
              'Disagree' = system.file("icons", "Disagree.png", package = "OpinionReport"))

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
  dataday1$emost <- factor( dataday1$emost,
                            levels = unique( c( grep( 'Agree', dataday1$emost, value = T),
                                                grep( 'Undecided', dataday1$emost, value = T),
                                                grep( 'Disagree', dataday1$emost, value = T))))

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
                  position = "stack"#,
                  #  stat = "identity",
                  #  aes( width = 0.9)
        ) +
        scale_x_continuous( breaks = 1:max( df2$xvarn),
                            labels = unique( df2$article),
                            position = "top",
                            expand = c( 0, 0)) +
        scale_fill_manual( values = c( 'Disagree' = '#F44336',
                                       'Agree' = '#4ECB49',
                                       'Undecided' = '#FED34C')) +
        #   scale_y_discrete( expand = c( 0, 0)) +
        # geom_image( data = dataday1[values != 0],
        #             aes( image = emost),
        #             #  family='OpenSansEmoji',
        #             position = position_stack( vjust = 0.5),
        #             hjust = 0#,
        #             # size = 8,
        #             #  parse = F,
        #             #  colour = NA
        # ) +
        geom_text(data = dataday1[values != 0],
                  aes( label = emotext),
                  position = position_stack( vjust = 0.5),
                  color = 'white',
                  size = 8,
                  hjust = 1
        ) +
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
      top.dt[, opinion := factor( opinion,
                                  levels = rev( levels( opinion)))]
      top.dt[ values > max.label, emotext := scales::percent( values, accuracy = 1)]
      top.dt[ values < max.emoji, emost := NA]
      print( top.dt)

      gg4 <- ggplot( data = top.dt,
                     aes( x = 1,
                          y = values,
                          fill = opinion)) +
        geom_bar( stat = 'identity') +
        geom_text( aes( label = emotext,
                        x = 1),
                   position = position_stack( vjust = 0.5),
                   size = 1.5,
                   parse = F,
                   fontface = "bold") +
        scale_fill_manual( values = c( 'Disagree' = '#EB0427',
                                       'Agree' = '#389D28',
                                       'Undecided' = '#EFC003')) +
        scale_y_discrete( expand = c( 0, 0)) +
        coord_flip() +
        image_annotater( input.dt = top.dt, opinion.use = 'Agree',
                         ymin.ratio = .5) +
        image_annotater( input.dt = top.dt, opinion.use = 'Undecided',
                         ymin.ratio = .5) +
        image_annotater( input.dt = top.dt, opinion.use = 'Disagree',
                         ymin.ratio = .5) +
        theme_bw() +
        theme( axis.title = element_blank(),
               axis.text.x = element_blank(),
               axis.text.y = element_blank(),
               axis.ticks = element_blank(),
               panel.border = element_blank(),
               panel.grid = element_blank(),
               plot.background = element_rect(fill = "transparent",colour = NA),
               plot.margin = grid::unit(c(0,0,0,0),"cm"),
               legend.position = 'none',
               rect = element_rect(fill = "transparent")
        )

      ## save file, then crop
      filename.png <- file.path( outname,
                                 paste0( topic.name, "_",
                                         a, '.png'))
      ggsave(gg4,
             filename = filename.png,
             width = 2,
             height = .2,
             units = "in",
             dpi = 900,
             bg = 'transparent'
      )

    }
  }
}
