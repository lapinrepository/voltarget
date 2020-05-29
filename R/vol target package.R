
buildBasket<-function(dataxts, weights = NA){

  colnums <- length(colnames(dataxts))

  if(anyNA(weights))
    weights <- c(rep(1, colnums)) / colnums

  if(sum(weights) != 1.)
    stop("Sum of weights should be equal to 100%")

  if(colnums != length(weights))
    stop("'Weight vector needs to be the same size as the number of underlying." )

  data_returns = exp(diff(log(dataxts)))-1

  data_returns <- as.xts(t(t(data_returns) * weights), dateFormat="Date")

  data_returns<-cbind(data_returns, EW = rowSums(data_returns))

  data_returns[1,]<-0

  final <- log(data_returns$EW+1)
  return(exp(cumsum(final))*100.)
}



loaddata <- function(file, ratio = 1., dateformat = "%m/%d/%Y", nastrings = c("","#N/A","!VALUE","#N/A N/A","N/A")){
  z <- read.zoo(file,
                header = TRUE,
                sep = ",",
                format= dateformat,
                drop=FALSE,
                na.strings=nastrings)
  z <- as.xts(z)/ratio
  z <- na.omit(z)

  index(z)<-as.Date(as.POSIXct(index(z), tz="GMT"))
  #index(z)<-as.Date(as.POSIXlt(index(z)))

  return(z)
}

rvCitiEquityDesk<-function(x, scale = NA, ...)
{
  if(is.na(scale) && !xtsible(x))
    stop("'x' needs to be timeBased or xtsible, or scale must be specified." )

  if(is.na(scale)) {
    freq = periodicity(x)
    switch(freq$scale,
           minute = {stop("Data periodicity too high")},
           hourly = {stop("Data periodicity too high")},
           daily = {scale = 252},
           weekly = {scale = 52},
           monthly = {scale = 12},
           quarterly = {scale = 4},
           yearly = {scale = 1}
    )
  }

  if (is.vector(x)) {
    #scale standard deviation by multiplying by the square root of the number of periods to scale by
    #sqrt(scale * sum(x^2) / length(x))
  } else {
    if(!xtsible(x) & is.na(scale))
      stop("'x' needs to be timeBased or xtsible, or scale must be specified." )

    returns.mean <- sum(x) / (length(x))
    x.local = x - returns.mean

    result =  sqrt(scale * sum(x.local^2) / (length(x)-1))
    #result = apply(x, 2, sd.multiperiod, scale=scale)

    dim(result) = c(1,NCOL(x))
    colnames(result) = colnames(x)
    rownames(result) = "Realized Volatility"

    return(result)
  }
}

rvCitiFundDesk<-function(x, scale = NA, ...)
{
  if(is.na(scale) && !xtsible(x))
    stop("'x' needs to be timeBased or xtsible, or scale must be specified." )

  if(is.na(scale)) {
    freq = periodicity(x)
    switch(freq$scale,
           minute = {stop("Data periodicity too high")},
           hourly = {stop("Data periodicity too high")},
           daily = {scale = 252},
           weekly = {scale = 52},
           monthly = {scale = 12},
           quarterly = {scale = 4},
           yearly = {scale = 1}
    )
  }

  if (is.vector(x)) {
    #scale standard deviation by multiplying by the square root of the number of periods to scale by
    sqrt(scale * sum(x^2) / length(x))
  } else {
    if(!xtsible(x) & is.na(scale))
      stop("'x' needs to be timeBased or xtsible, or scale must be specified." )
    #x = checkData (x)

    result =  sqrt(scale * sum(x^2) / length(x))

    dim(result) = c(1,NCOL(x))
    colnames(result) = colnames(x)
    rownames(result) = "Realized Volatility"
    return(result)
  }
}


vol_handler <- function(vol_dataframe,
                        vol_metrics_xts,
                        vol_target,
                        max.leverage = 1.50,
                        synth.div = 0 ,
                        desc = 'max20D60D'){

  vol_dataframe_local <- merge(vol_dataframe,vol_metrics_xts)

  #vol_dataframe_local$target_vol <- mean(na.omit(vol_dataframe_local[,2]))
  vol_dataframe_local$target_vol <- vol_target

  vol_dataframe_local$weight <- vol_dataframe_local$target_vol/vol_dataframe_local$vol_metrics
  vol_dataframe_local$weight[vol_dataframe_local$weight > max.leverage] <- max.leverage

  vol_dataframe_local$cash_weight <- 1 - vol_dataframe_local$weight

  vol_dataframe_local$ndays <- as.numeric(difftime(index(vol_dataframe_local),
                                             lag.xts(index(vol_dataframe_local),1), "days"),
                                          units = "days")

  vol_dataframe_local <- na.omit(vol_dataframe_local)


  #Test with cash @ 0%
  #Returns are passed in LN format. We have to pass it in daily normal returns
  #vol_dataframe_local$portfolio_return <- log(1. +
  #                                              (exp(vol_dataframe_local[,1])-1.) * vol_dataframe_local$weight
  #                                            - synth.div * vol_dataframe_local$ndays / 365)

  vol_dataframe_local$portfolio_return <- (exp(vol_dataframe_local[,1])-1.) * vol_dataframe_local$weight - synth.div * vol_dataframe_local$ndays / 365

  vol_dataframe_local$daily_bkt_return <- exp(vol_dataframe_local[,1])-1.


  vol_dataframe_local$log_portfolio_return <- log(vol_dataframe_local$portfolio_return + 1.)
  vol_dataframe_local$log_bkt_return <- vol_dataframe_local[,1]


  colnames(vol_dataframe_local) = c('p',
                                    paste(desc,'vol_metrics'),
                                    paste(desc,'target_vol'),
                                    paste(desc,'weight'),
                                    paste(desc,'cash_weight'),
                                    paste(desc,'ndays'),
                                    paste(desc,'daily_portfolio_return'),
                                    paste(desc,'daily_basket_return'),
                                    paste(desc,'daily_portfolio_log_return'),
                                    paste(desc,'daily_basket_log_return'))

  return(vol_dataframe_local[,c(2,3,4,5,6,7,8, 9, 10)])
}


run_voltarget_prices <- function(investment.instrument,
                          vol_function = realizedVol,
                          vol_days = c(20,60),
                          vol.target = NA,
                          max.leverage = 1.5,
                          synth.div = synth.div,
                          lag.days = 0,
                          method.desc = 'max20D60D'){

  vol_dataframe <- na.omit(PerformanceAnalytics::Return.calculate(investment.instrument[,1]
                                                                  ,method='log'))
  colnames(vol_dataframe) = c('returns')

  #set the Vol Target to the annualized realized vol by the underlying
  if(is.na(vol.target)){
    vol.target <- vol_function(vol_dataframe)[1]
  }


  #Look at the vol metrics of interest
  #We pass the number of returns (so number of days - 1)
  vol_list = list()

  if(length(vol_days)>1){
    for(i in 1:length(vol_days)){
      vol_list[[i]] <- rollapply(vol_dataframe[,1], FUN = vol_function, width = vol_days[i] - 1, fill = NA)
    }

    investment.vol = do.call(cbind, vol_list)

  } else {
    investment.vol <- rollapply(vol_dataframe[,1], FUN = vol_function, width = vol_days - 1, fill = NA)
  }

  vol_metrics <- apply(investment.vol, FUN = max, MARGIN = 1)

  vol_metrics_xts <- as.xts(as.data.frame(vol_metrics), dateFormat = "Date")

  #We need to lag the vol in order to take into account the correct leverage
  lagged_vol_metrics_xts <- lag.xts(vol_metrics_xts[,1], 1 + lag.days )

  vt.simulation <- vol_handler(vol_dataframe[,1],
                               lagged_vol_metrics_xts,
                               vol.target ,
                               max.leverage = max.leverage,
                               synth.div = synth.div,
                               desc = method.desc)


  vol_dataframe <- merge(vol_dataframe, vt.simulation)

  vol_dataframe<-na.omit(vol_dataframe)

  return(vol_dataframe)
}
