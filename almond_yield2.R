#' Almond Yield Anomaly
#'
#' Computes crop yield anomaly from time series of min, max temperature and precipitation
#' @param data dataframe of time series data including month, year, tmax_c (maximum temperature °C), tmin_c (minimum temeprature°C), precip (precipitation mm)
#' @param result indicated output of average or yearly yield anomaly 
#' @param t_change temperature change for warming scenatios (1, 2 ,3)
#' @param p_change percent precipitation change (2, 0.5)
#' @param min_price minimum per yield ($ per ton/acre)
#' @param max_price maximum per yield ($ per ton/acre)
#' @author Seleni Cruz and Juliette Verstaen
#' @return yield anomaly for each year, plot of variables and yield anomaly over time series, and max and minimum yields over a time series of multiple year inputs


almond_yield_anomaly <- function (data, result, t_change, p_change, min_price, max_price){
  
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
  
  crop$anomaly <- (-0.015*(crop$tmin_c + t_change) - 0.0046*((crop$tmin_c + t_change)**2) 
                   - 0.07*(crop$precip*p_change)  + 0.0043*((crop$precip*p_change)**2) + 0.28)  
  results <- crop%>% select(year, anomaly)
  
  plot(x=crop$year, y=crop$anomaly, xlab="Year", lwd=1, col="red",
       ylab="ton per acre", 
       type="l", yaxs="i", xaxs="i", main= "Yield anomaly: Temperature sensitivity anlysis")
  
  
  Years <- seq(1, nrow(results), 1)
  
  source("profit_NPV.R")
  
  NPV_min <- NPV_profit(price = min_price, year = Years, anomaly = crop$anomaly)
  NPV_max <- NPV_profit(price = max_price, year = Years, anomaly = crop$anomaly)
  
  
  
  if (result == "average"){
  return(cbind(average = mean(results$anomaly), min = min(results$anomaly), max = max(results$anomaly), min_profit = sum(NPV_min), max_profit = sum(NPV_max)))
  
    }else {
    return(list(results = results, min = min(results$anomaly), max = max(results$anomaly), min_profit = sum(NPV_min), max_profit = sum(NPV_max)))
  }
}

