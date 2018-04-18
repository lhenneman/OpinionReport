OpData <- function( LSname = NULL,
                    mLSname = NULL,
                    mRSname = NULL,
                    RSname = NULL,
                    LSopinion,
                    mLSopinion,
                    mRSopinion,
                    RSopinion,
                    title = 'todaystopic',
                    outdir = '~/Dropbox/HomeLife/Laura/AbridgeNews'){

  LSname <-  ifelse( is.null( LSname),    ' ', LSname)
  mLSname <- ifelse( is.null( mLSname),  '  ', mLSname)
  mRSname <- ifelse( is.null( mRSname), '   ', mRSname)
  RSname <-  ifelse( is.null( RSname), '    ', RSname)

  dataday1 <- data.table( article = factor( rep( c( LSname,
                                                    mLSname,
                                                    mRSname,
                                                    RSname),
                                                 each = 3)),
                          opinion = c( names( LSopinion),
                                       names( mLSopinion),
                                       names( mRSopinion),
                                       names( RSopinion)),
                          values = c( rps( LSopinion  / sum( LSopinion),  2),
                                      rps( mLSopinion / sum( mLSopinion), 2),
                                      rps( mRSopinion / sum( mRSopinion), 2),
                                      rps( RSopinion  / sum( RSopinion),  2)))
  today <- Sys.Date()
  write.csv(dataday1,
            file = file.path( outdir,
                              paste0( title,
                                      today,
                                      '.csv')))
}
