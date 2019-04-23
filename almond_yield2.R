#' Almond Yield Anomaly
#'
#' Computes crop yield anomaly from time series of min, max temperature and precipitation
#' @param data dataframe of time series data including month, year, tmax_c (maximum temperature °C), tmin_c (minimum temeprature°C), precip (precipitation mm)
#' @author Seleni Cruz and Juliette Verstaen
#' @return yield anomaly for each year, plot of variables and yield anomaly over time series, and max and minimum yields over a time series of multiple year inputs


almond_yield_anomaly <- function (data, result){
  
  yearly <- data %>%
    group_by(month, year)%>%
    summarize(tmax_c = mean(tmax_c),
              tmin_c = mean(tmin_c),
              precip = mean(precip))
  
  crop<- yearly%>%
    filter(month== 2)%>%
    select(year, month, tmin_c)
  
  crop <- yearly%>%
    filter(month == 1)%>%
    select(year, month, precip)%>%
    merge(crop, by="year")
  
  crop$anomaly <- -0.015*crop$tmin_c - 0.0046*(crop$tmin_c**2) - 0.07*crop$precip  + 0.0043*(crop$precip**2) + 0.28  
  
  results <- crop%>% select(year, anomaly)
  
  plot(x=crop$year, y=crop$anomaly, xlab="Year", lwd=1, col="red",
       ylab="ton per acre", 
       type="l", yaxs="i", xaxs="i", main= "Yield anomaly: Temperature sensitivity anlysis")
  
  for (i in 1:10){
    increase <- runif(1,0,3)
    
    crop$anomaly <- -0.015*(crop$tmin_c + increase) - 0.0046*((crop$tmin_c + increase)**2) - 0.07*crop$precip  + 0.0043*(crop$precip**2) + 0.28  
    
    lines(x=crop$year, y=crop$anomaly, lwd=0.3)
  }
  
  if (result == "average"){
  return(list(average = mean(results$anomaly), min = min(results$anomaly), max = max(results$anomaly)))
  
    }else {
    return(list(results = results, min = min(results$anomaly), max = max(results$anomaly)))
  }
}

