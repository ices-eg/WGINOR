# utilities
#' ## The 2 main functions: ATAC (modelling) and ggATAC (plotting)
#' Modelling is done with the [BRMS package](https://paul-buerkner.github.io/brms/)
#' Plotting is done using the [ggplot2 package](https://ggplot2.tidyverse.org)
#' data = dataset with two columns: year and value
#' year_start = first year that is be used when fitting the models
#' year_end_trend = last year that is used when fitting the overall trend model (using all observations). By default this is the last year of the series
#' year_end_OL = last year that is used when fitting the model that is used for forecasting. By default this will be the last year of data minus the n years of forecasts
#' n_forecasts = number of forecasts to the produced. By default 3.
ATAC <- function(data,year_start=NA,year_end_trend=NA,year_end_OL=NA,n_forecasts=3,family=gaussian(),echo=TRUE){
  # assumes that data contains year i column 1 and values in column 2
  if(is.na(year_start)==TRUE) year_start<-min(data[,1]) # set minimum year for Trend and OL if not specified
  year_start<-max(min(data[,1]),year_start) # make sure the start_year doesn't start before the data series starts
  if(is.na(year_end_trend)==TRUE) year_end_trend<-max(data[,1]) # set maximum year for Trend if not specified
  if(is.na(year_end_OL)==TRUE) {
    year_end_OL<-max(data[,1])-n_forecasts # set maximum year for OL if not specified
  } 
  duration <- (year_end_trend-year_start)+1 # number of years of data in the time series
  names(data) <- c("x","y")
  results <- tibble(year=data$x,param="observation",value=data$y) # initialise the result table
  data2 <- data %>% filter((x>=year_start)&(x<=year_end_trend)) # filter data for the selected time-period
  nCores=max(1,parallel::detectCores()) # identify number of core processors
  niter = 5000 # max number of iterations for the modelling
  if (duration>=10){ # check that the series contains at least 10 observations
    # STEP #1: Trend model
    fit0 <- brms::brm(y ~ s(x), data = data2, family=family, chains = max(3,nCores), cores = nCores, iter=niter) # No AR process
    if (duration >=20){ # if the series is long enough, and if it was possible to fit a spline model then try AR(1) and AR(2) models
      fit1 <- brms::brm(y ~ s(x), autocor = cor_ar(p=1), data = data2, family=family, chains = max(3,nCores), cores = nCores, iter=niter) # AR1 pprocess
      fit2 <- brms::brm(y ~ s(x), autocor = cor_ar(p=2), data = data2, family=family, chains = max(3,nCores), cores = nCores, iter=niter) # AR2 pprocess
      Loo <- c(brms::LOO(fit0)$estimates[3,1],brms::LOO(fit1)$estimates[3,1],brms::LOO(fit2)$estimates[3,1]) # LOO scores for the three models
      best_model <- which(Loo==min(Loo)) # identify the best model of the two models
    } else {
      best_model <- 1
    } # if the series is too short the best model is without AR
    if (best_model==1) {fit<-fit0; AR<-0}
    if (best_model==2) {fit<-fit1; AR<-1}
    if (best_model==3) {fit<-fit2; AR<-2}
    print(paste("the selected model is model",best_model))
    model.trend <- fit
    trend.brm <- as_tibble(predict(fit,incl_autocor=FALSE)) # get the trend estimate with the smoother only (no AR)
    results <- bind_rows(results,tibble(year=data2$x,param="trend",value=trend.brm$Estimate)) # output the trend estimates
    # STEP #2: Identification of outlier forecasts (OL)
    data3 <- data %>% filter((x>=year_start)&(x<=year_end_OL)) # dataset used to fit the model for OL
    if (best_model==1) fit <-brms::brm(y ~ s(x), data = data3, family=family, chains = max(3,nCores), cores = nCores, iter=niter) # No AR process
    if (best_model==2) fit <- brms::brm(y ~ s(x), autocor = cor_ar(p=1), data = data3, family=family, chains = max(3,nCores), cores = nCores, iter=niter) # AR1 pprocess
    if (best_model==3) fit <- brms::brm(y ~ s(x), autocor = cor_ar(p=2), data = data3, family=family, chains = max(3,nCores), cores = nCores, iter=niter) # AR2 pprocess
    model.OL <- fit
    data4 <- bind_rows(data3,tibble(x=(year_end_OL+1):(year_end_OL+n_forecasts),y=NA))  # dataset used to predict past observations and new obs (OL)
    OL.trend.brm <- as_tibble(predict(fit,newdata=data4,incl_autocor=FALSE)) # get the trend (without AR)
    trendOL <- OL.trend.brm %>% mutate(year=data4$x) %>% filter(year<=year_end_OL) %>% .$Estimate
    results <- bind_rows(results,tibble(year=data3$x,param="trendOL",value=trendOL)) # output the trend estimates (based on the 'OL' years only)
    OL.brm <- as_tibble(predict(fit,newdata=data4,probs=c(0.025,.125,.25,.75,.875,.975),incl_autocor=TRUE)) # get the predictions (with trend and AR)
    pred <- OL.brm %>% mutate(year=data4$x) %>% filter(year<=year_end_OL)
    results <- bind_rows(results,tibble(year=pred$year,param="prediction",value=pred$Estimate)) # output the trend estimates (based on the 'OL' years only)
    forecast <- OL.brm %>% mutate(year=data4$x) %>% filter(year>=year_end_OL)
    results <- bind_rows(results,tibble(year=forecast$year,param="forecast",value=forecast$Estimate)) # output the trend estimates (based on the 'OL' years only)
    pred_band <- OL.brm %>% mutate(year=data4$x) %>% filter(year<=year_end_OL) %>% select(-Estimate,-Est.Error) %>%
      pivot_longer(cols = 1:6,names_to = "param", values_to = "value") %>%
      mutate(param=paste0("pred",param))
    forecast_band <- OL.brm %>% mutate(year=data4$x) %>% filter(year>=year_end_OL) %>% select(-Estimate,-Est.Error) %>%
      pivot_longer(cols = 1:6,names_to = "param", values_to = "value") %>%
      mutate(param=paste0("forecast",param))
    results <- bind_rows(results,pred_band,forecast_band) # output the trend estimates (based on the 'OL' years only)
  } else { # if the series contains less than 10 observations
    model.trend <- {}
    model.OL <- {}
    AR <- NA
  }
  out<-list(model.trend=model.trend,model.OL=model.OL,results=results,AR=AR)
  return(out)
}

ggATAC <- function(results,backtransform=FALSE,trend=FALSE){
  if (backtransform==TRUE) results$value<-results$value^4
  fit.wide <- results %>% 
    distinct(year, param, .keep_all = TRUE) %>% 
    pivot_wider(names_from = "param")
  gg <- ggplot(data=fit.wide,aes(x=year))
  if (dim(fit.wide)[2]>3) { #check that a model was actually fitted (enough columns with results)
    gg <- gg +
      geom_path(aes(y=trendOL),col='#FFB302',lwd=1,alpha=0.75,lty=1)
    if (sum(is.na(fit.wide$prediction)==FALSE)>=2){
    gg <- gg +
      geom_ribbon(aes(ymin=predQ2.5,ymax=predQ97.5),fill = "#104E8B", alpha=0.25) +
      geom_path(aes(y=prediction),col='#104E8B',lwd=0.6,lty=2)
    }
    if (sum(is.na(fit.wide$forecast)==FALSE)>=2){
      gg <- gg +
        geom_ribbon(aes(ymin=forecastQ2.5,ymax=forecastQ97.5),fill='#8B1A1A',alpha=0.25) +
        geom_ribbon(aes(ymin=forecastQ12.5,ymax=forecastQ87.5),fill='#8B1A1A',alpha=0.25) +
        geom_ribbon(aes(ymin=forecastQ25,ymax=forecastQ75),fill='#8B1A1A',alpha=0.25) +
        geom_path(aes(y=forecast),col='#8B1A1A',lwd=.6,lty=2)
    }
  }
  if (trend==TRUE){
    gg <- gg + geom_path(aes(y=trend),col='black',alpha=0.25,lwd=1.5) 
  }
  gg <- gg +
    geom_point(aes(y=observation),col = "#104E8B",pch=19,cex=1.5) + 
    theme_bw() 
  return(gg)
}

