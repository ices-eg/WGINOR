# utilities
#' ## The 2 main functions: ATAC (modelling) and ggATAC (plotting)
#' Modelling is done with the [BRMS package](https://paul-buerkner.github.io/brms/)
#' Plotting is done using the [ggplot2 package](https://ggplot2.tidyverse.org)
ATAC <- function(data,year_start=NA,year_end_trend=NA,year_end_FO=NA,n_forecasts=3,family=gaussian(),echo=TRUE){
  # assumes that data contains year i column 1 and values in column 2
  if(is.na(year_start)==TRUE) year_start<-min(data[,1]) # set minimum year for Trend and FO if not specified
  year_start<-max(min(data[,1]),year_start) # make sure the start_year doesn't start before the data series starts
  if(is.na(year_end_trend)==TRUE) year_end_trend<-max(data[,1]) # set maximum year for Trend if not specified
  if(is.na(year_end_FO)==TRUE) year_end_FO<-max(data[,1])-n_forecasts # set maximum year for FO if not specified
  duration <- (year_end_trend-year_start)+1 # number of years of data in the time series
  names(data) <- c("x","y")
  results <- tibble(year=data$x,param="observation",value=data$y) # initialise the result table
  data2 <- data %>% filter((x>=year_start)&(x<=year_end_trend)) # filter data for the selected time-period
  nCores=max(1,parallel::detectCores()) # identify number of core processors
  niter = 5000 # max number of iterations for the modelling
  # STEP #1: Trend model
  fit0 <- brms::brm(y ~ s(x), data = data2, family=family, chains = max(3,nCores), cores = nCores, iter=niter) # No AR process
  if (duration >=20){ # if the series is long enough, and if it was possible to fit a spline model then try AR(1) and AR(2) models
    fit1 <- brms::brm(y ~ s(x), autocor = cor_ar(p=1), data = data2, family=family, chains = max(3,nCores), cores = nCores, iter=niter) # AR1 pprocess
    fit2 <- brms::brm(y ~ s(x), autocor = cor_ar(p=2), data = data2, family=family, chains = max(3,nCores), cores = nCores, iter=niter) # AR2 process
    Loo <- c(brms::LOO(fit0)$estimates[3,1],brms::LOO(fit1)$estimates[3,1],brms::LOO(fit2)$estimates[3,1]) # LOO scores for the three models
    best_model <- which(Loo==min(Loo))} else {# identify the best model of the three models
      best_model <- 1} # if the series is too short the best model is without AR
  if (best_model==1) {fit<-fit0; AR<-0}
  if (best_model==2) {fit<-fit1; AR<-1}
  if (best_model==3) {fit<-fit2; AR<-2}
  print(paste("the selected model is model",best_model))
  model.trend <- fit
  trend.brm <- as_tibble(predict(fit,incl_autocor=FALSE)) # get the trend estimate with the smoother only (no AR)
  results <- bind_rows(results,tibble(year=data2$x,param="trend",value=trend.brm$Estimate)) # output the trend estimates
  # STEP #2: Flagged Observations
  data3 <- data %>% filter((x>=year_start)&(x<=year_end_FO)) # dataset used to fit the model for FO
  if (best_model==1) fit <-brms::brm(y ~ s(x), data = data3, family=family, chains = max(3,nCores), cores = nCores, iter=niter) # No AR process
  if (best_model==2) fit <- brms::brm(y ~ s(x), autocor = cor_ar(p=1), data = data3, family=family, chains = max(3,nCores), cores = nCores, iter=niter) # AR1 pprocess
  if (best_model==3) fit <- brms::brm(y ~ s(x), autocor = cor_ar(p=2), data = data3, family=family, chains = max(3,nCores), cores = nCores, iter=niter) # AR2 process
  model.FO <- fit
  data4 <- bind_rows(data3,tibble(x=(year_end_FO+1):(year_end_FO+n_forecasts),y=NA))  # dataset used to predict past observations and new obs (FO)
  FO.trend.brm <- as_tibble(predict(fit,newdata=data4,incl_autocor=FALSE)) # get the trend (without AR)
  trendFO <- FO.trend.brm %>% mutate(year=data4$x) %>% filter(year<=year_end_FO) %>% .$Estimate
  results <- bind_rows(results,tibble(year=data3$x,param="trendFO",value=trendFO)) # output the trend estimates (based on the 'FO' years only)
  FO.brm <- as_tibble(predict(fit,newdata=data4,probs=c(0.025,.125,.25,.75,.875,.975),incl_autocor=TRUE)) # get the predictions (with trend and AR)
  pred <- FO.brm %>% mutate(year=data4$x) %>% filter(year<=year_end_FO)
  results <- bind_rows(results,tibble(year=pred$year,param="prediction",value=pred$Estimate)) # output the trend estimates (based on the 'FO' years only)
  forecast <- FO.brm %>% mutate(year=data4$x) %>% filter(year>year_end_FO)
  results <- bind_rows(results,tibble(year=forecast$year,param="forecast",value=forecast$Estimate)) # output the trend estimates (based on the 'FO' years only)
  pred_band <- FO.brm %>% mutate(year=data4$x) %>% filter(year<=year_end_FO) %>% select(-Estimate,-Est.Error) %>%
    pivot_longer(cols = 1:6,names_to = "param", values_to = "value") %>%
    mutate(param=paste0("pred",param))
  forecast_band <- FO.brm %>% mutate(year=data4$x) %>% filter(year>year_end_FO) %>% select(-Estimate,-Est.Error) %>%
    pivot_longer(cols = 1:6,names_to = "param", values_to = "value") %>%
    mutate(param=paste0("forecast",param))
  results <- bind_rows(results,pred_band,forecast_band) # output the trend estimates (based on the 'FO' years only)
  out<-list(model.trend=model.trend,model.FO=model.FO,results=results,AR=AR)
  return(out)
}

ggATAC <- function(results,backtransform=FALSE,trend=FALSE){
  if (backtransform==TRUE) results$value<-results$value^4
  fit.wide <- results %>% pivot_wider(names_from = "param")
  gg <- ggplot(data=fit.wide,aes(x=year))+
    geom_ribbon(aes(ymin=predQ2.5,ymax=predQ97.5),fill = "#104E8B", alpha=0.25) +
    geom_ribbon(aes(ymin=forecastQ2.5,ymax=forecastQ97.5),fill='#8B1A1A',alpha=0.25) +
    geom_ribbon(aes(ymin=forecastQ12.5,ymax=forecastQ87.5),fill='#8B1A1A',alpha=0.25) +
    geom_ribbon(aes(ymin=forecastQ25,ymax=forecastQ75),fill='#8B1A1A',alpha=0.25) +
    geom_path(aes(y=trendFO),col='#FFFACD',lwd=1,alpha=0.75) +
    geom_path(aes(y=prediction),col='#104E8B',lwd=0.6,lty=2) +
    geom_path(aes(y=forecast),col='#8B1A1A',lwd=1) +
    geom_point(aes(y=observation),col = "#104E8B",pch=19,cex=1.5) + 
    theme_bw()
  if (trend==TRUE){
    gg <- gg + geom_path(aes(y=trend),col='black',alpha=0.25,lwd=1.5) }
  return(gg)
}

