image_annotater <- function( input.dt = top.dt,
                             opinion.use = 'Agree',
                             ymin.ratio = .5,
                             emoji.hspace = 0.035){
  # use specific opinion
  dt.use <- input.dt[opinion == opinion.use]

  # if not plotting this image, return blank geom
  if( is.na( dt.use$emost))
    return( geom_blank())

  # load image, rasterify
  inagein <- image_read( as( dt.use$emost, 'character'))
  raster <- as.raster(inagein)

  # specify ymin location
  ymin.Agree <- input.dt[opinion == 'Agree']$values * ymin.ratio + emoji.hspace
  ymin.Undecided <- ( input.dt[opinion == 'Undecided']$values * ymin.ratio + emoji.hspace +
                        input.dt[opinion == 'Agree']$values)
  ymin.Disagree <- ( input.dt[opinion == 'Disagree']$values * ymin.ratio + emoji.hspace +
                       input.dt[opinion == 'Agree']$values +
                       input.dt[opinion == 'Undecided']$values)
  if( opinion.use == 'Agree'){
    ymin = ymin.Agree
  } else if( opinion.use == 'Undecided'){
    ymin = ymin.Undecided
  } else if( opinion.use == 'Disagree'){
    ymin = ymin.Disagree
  }

  # specify other locations
  xmin = 0.85
  xmax = xmin + 0.3
  ymax = ymin + 0.03

  return( annotation_raster(raster, xmin = xmin,
                            xmax = xmax,
                            ymin = ymin,
                            ymax = ymax))

}
