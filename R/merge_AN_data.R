
read.merge.AN.data <- function( rxn.jsn.file = 'data/abridge-news-reactions-export.json',
                                tpc.jsn.file = 'data/abridge-news-topics-export (1).json'){

  `%ni%` <- Negate(`%in%`)

  ## ======================================== ##
  ## read in reaction data
  ## ======================================== ##
  rxn.jsn <- jsonlite::fromJSON(rxn.jsn.file)
  rxn.df <- sapply( rxn.jsn, '[')
  rxn.cat <- rownames( rxn.df)

  rxn.dt <- data.table( rxn.df)[, categories := rxn.cat]

  rxn.dt.m <- dcast( melt(rxn.dt, id.vars = "categories"), variable ~ categories)
  rxn.dt.m <- data.table( sapply( rxn.dt.m, as, "character"))[, topicSlug := NULL]
  setnames( rxn.dt.m,
            c( 'variable', 'category'),
            c( 'topicKey', 'rxnCategory'))

  ## ======================================== ##
  ## read in topic data
  ## ======================================== ##
  # Define extract functions
  extract_listelements <- function( extractname,
                                    list.jsn = tpc.jsn){
    df <- unlist( sapply( list.jsn, '[[', extractname))
    topics <- names( df)
    dt <- data.table( topickey = topics,
                      V1 = df)
    setnames( dt, 'V1', extractname)
    return( dt)
  }

  extract_articles <- function( topickey = "-KtXrJ_CecxXSfGml22J", #"-KwGKIexq86U5CO9cRsP", #'-KtXpA9MtM7_EMcBZZL-',
                                list.jsn = tpc.jsn){
    tpc.l <- lapply( list.jsn, '[[', 'articles')
    if( is.null( tpc.l[topickey][[1]]))
      return( data.table( topickey=character(), topickey=numeric(), topickey=numeric()))

    tpc.a.l <- lapply( tpc.l[topickey], '[')[[1]]

    tpc.a.dt <- rbindlist( lapply( seq_along( tpc.a.l),
                                   function( l,
                                             art.list = tpc.a.l,
                                             tpc.name = topickey){
                                     df <- sapply( art.list[[l]], '[')
                                     names.df <- names( df)
                                     dt <- data.table( df)[, `:=` (art.cat = names.df,
                                                                   topickey = topickey)]

                                     dt.cast <- dcast( dt, topickey ~ art.cat, value.var = 'df')[, slug := NULL]
                                     if( 'key' %ni% names( dt.cast))
                                       dt.cast[, key := names( art.list)[l]]

                                     setnames( dt.cast, 'key', 'articleKey')
                                     return( dt.cast)
                                   }))

    setkey( tpc.a.dt, 'order')
    tpc.a.dt[, `order`:=as.double(`order`)]
    tpc.a.dt[, order := seq( -2, 2, length.out = nrow( tpc.a.dt))]

    return( tpc.a.dt)
  }

  # Read from json files
  tpc.jsn <- jsonlite::fromJSON(tpc.jsn.file)

  # Define elements of interest
  tpc.extractnames <- c( 'category', 'datePublished', 'dateUpdated', 'slug', 'type',
                         'is2018midterm', 'isPublished', 'name', 'shortTitle')

  # execute the extract, reduce to single data.table
  tpc.dt <- Reduce(function(...) merge(..., by = 'topickey', all = TRUE), lapply( tpc.extractnames, extract_listelements))

  #read in article data
  tpc.a.dt <- rbindlist( lapply( tpc.dt$topickey,
                                 extract_articles), use.names = T, fill = T)[, .(topickey, articleKey, order)]

  #merge article data and topic data
  tpc.a.dt.m <- na.omit( merge( tpc.dt, tpc.a.dt, by = 'topickey', all = T))
  # tpc.a.dt.m[, date := as.Date( as.POSIXct( max( dateUpdated, datePublished) / 1000, origin="1970-01-01")),
  #            by = 'articleKey']
  tpc.a.dt.m[, date := as.Date( as.POSIXct( datePublished / 1000, origin="1970-01-01"))]


  ## ======================================== ##
  ## Merge data, create data.table for plotting
  ## ======================================== ##
  # merge article/topic data with reaction data
  tpc.a.rxn.dt <- merge( tpc.a.dt.m, rxn.dt.m, 'articleKey')

  ## tot.count: number of reactions per topic
  ## cat.count: number of reactions per category (agree, hmmmm, disagree) per topic
  tpc.a.rxn.dt[ , cat.count := .N , by = c('rxnCategory', 'articleKey') ]
  tpc.a.rxn.dt[ , tot.count := .N , by = c('topickey') ]

  ## extract unique reaction-article combos
  dt.u <- unique( tpc.a.rxn.dt[, .( articleKey, topickey, category, slug, is2018midterm, type,
                                    isPublished, name, shortTitle, order, date, rxnCategory,
                                    cat.count, tot.count)])

  ## summarize reaction data
  dt.u[, cat.frac := cat.count / tot.count]
  dt.u[, weighter := cat.frac * order]

  dt.wgtsums <- dt.u[, sum( weighter), by = .( topickey, category, slug, is2018midterm, type,
                                               isPublished, name, shortTitle, date, rxnCategory)]
  setnames( dt.wgtsums, 'V1', 'wgtsums')

  return( dt.wgtsums)

}

