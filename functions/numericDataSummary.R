NumericSummary <- function(var,datafile){
    x <- datafile[,var]
    return(data.frame(Feature = var,
                      MissingValues = sum(is.na(x)),
                      ZeroValues = sum(x==0,na.rm=T),
                      UniquePCT = length(unique(x))/length(x),
                      Mean = mean(x,na.rm=T),
                      STD = sd(x,na.rm=T),
                      data.frame(t(quantile(x,p=seq(0,1,.1))))
    ))
}