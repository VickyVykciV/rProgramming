WoE <- function(var, datafile, ResVar, returnWOE = F) {
  SmryData <-
    dcast(
      data = datafile,
      formula(paste(var, '~', ResVar)),
      value.var = ResVar,
      fun.aggregate = length
    )
  SmryData[, -1] <- apply(SmryData[, -1], 2, function(x)
    x * 100 / sum(x))
  SmryData$WOE <- log(SmryData$`0` / SmryData$`1`)
  if (returnWOE) {
    SmryData <- SmryData[, c(var, 'WOE')]
    colnames(SmryData) <- c(var, paste0(var, '_WOE'))
    return(SmryData)
  }
  return(data.frame(Feature = var, IV = sum((SmryData$`0` - SmryData$`1`) *
                                              SmryData$WOE
  )))
}