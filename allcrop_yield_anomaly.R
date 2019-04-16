#' Crop Yield Anomaly
#'
#' Computes crop yield anomaly from time series of min, max temperature and precipitation
#' @param data dataframe of time series data including month, year, tmax_c (maximum temperature °C), tmin_c (minimum temeprature°C), precip (precipitation mm)
#' @param crop wine_grapes, almonds, table_grapes, oranges, walnuts, avocados
#' @authors Seleni Cruz and Juliette Verstaen
#' @return yield anomaly for each year (tons/acres), plot of variables and yield anomaly over time series, and max and minimum yields over a time series of multiple year inputs


allcrop_yield_anomaly <- function (data, crop){
    yearly <- data %>%
    group_by(month, year)%>%
    summarize(tmax_c = mean(tmax_c),
              tmin_c = mean(tmin_c),
              precip = sum(precip))
 Crop <- crop
####almonds  
   if(crop == "almond") {
    crop<- yearly %>%
       filter(month== 2)%>%
       select(year, month, tmin_c)
  
    crop <- yearly%>%
       filter(month == 1)%>%
        select(year, month, precip)%>%
        merge(crop, by="year")
  
    crop$anomaly <- -0.015*crop$tmin_c - 0.0046*(crop$tmin_c**2) - 0.07*crop$precip  + 0.0043*(crop$precip**2) + 0.28  
  
    results <- crop%>% 
      select(year, anomaly) 
    
    }else{ 
 
####avocados
  if(crop == "avocados"){
    crop<- yearly %>%
      filter(month== 8)%>%
      mutate(year = year -1) %>%
      select(year, month, tmax_c)
    
    crop <- yearly %>%
      filter(month == 5) %>%
      select(year, month, tmin_c)%>%
      merge(crop, by = "year")
  
    crop <- yearly%>%
      filter(month == 10)%>%
      mutate(year = year - 1) %>%
     select(year, precip, month)%>%
     merge(crop, by="year")
  
  crop$anomaly <- 17.71*(crop$tmax_c) - 0.29*(crop$tmax_c**2) + 3.25*(crop$tmin_c) - 0.14*(crop$tmin_c**2) + 1*(crop$precip) + 0.31*(crop$precip**2) + 288.09  
  
  results <- crop%>% 
    select(year, anomaly)
  }else{

#### wine_grapes    
  if(crop == "wine_grapes") {
    crop<- yearly %>%
      filter(month== 4)%>%
      select(year, month, tmin_c)
    
    crop <- yearly%>%
      filter(month == 6)%>%
      mutate(precip_june = precip) %>%
      select(year, month, precip_june)%>%
      merge(crop, by="year")
      
    crop <- yearly%>%
        filter(month == 9)%>%
        mutate( year = year - 1) %>%
        mutate(precip_psep = precip) %>%
        select(year, month, precip_psep)%>%
        merge(crop, by="year")
    
    crop$anomaly <- 2.65*(crop$tmin_c) - 0.17*(crop$tmin_c**2) + 4.88*(crop$precip_june) - 4.93*(crop$precip_june**2) - 2.24*(crop$precip_psep) + 1.54*(crop$precip_psep**2) - 10.50  
    
    results <- crop%>% 
      select(year, anomaly)
  }else{
    
### walnuts
    if(crop == "walnuts") {
        crop<- yearly %>%
        filter(month == 11)%>%
        mutate(year = year -1) %>%
        select(year, month, tmax_c)
      
        crop <- yearly %>%
        filter(month == 2)%>%
        select(year, month, precip)%>%
        merge(crop, by="year")
      
      crop$anomaly <- 0.68*(crop$tmax_c) - 0.020*(crop$tmax_c**2) + 0.038*(crop$precip) - 0.0051*(crop$precip**2)  - 5.83
      
      results <- crop%>% 
        select(year, anomaly)
   }else{

#### Orange 
  if (crop == "orange"){
  yearly <- data %>%
    group_by(month, year)%>%
    summarize(tmax_c = mean(tmax_c),
              tmin_c = mean(tmin_c),
              precip = sum(precip))
  
  crop <- yearly%>%
    filter(month == 12)%>%
    mutate(year = year-1)%>%
    select(year, month, tmin_c)
  
  crop <- yearly%>%
    filter(month == 5)%>%
    select(year, month, precip)%>%
    merge(crop, by="year")
  
  crop$anomaly <- 1.08 * crop$tmin_c - 0.20 * (crop$tmin_c**2) + 4.99 * crop$precip - 1.97 * (crop$precip**2) - 2.47
  
  results <- crop %>% select(year, anomaly)
  } else {

####Table grapes 
  if (crop =="table_grapes")
    crop <- yearly%>%
      filter(month == 7)%>%
      mutate(tmin_7 = tmin_c)%>%
      select(year, month, tmin_7)
  
    crop <- yearly%>%
      filter(month == 4)%>%
      mutate(tmin_4 = tmin_c)%>%
      select(year, month, tmin_4)%>%
      merge(crop, by="year")
    
    crop <- yearly%>%
      filter(month == 1)%>%
      mutate(precip1 = precip)%>%
      select(year, month, precip1)%>%
      merge(crop, by="year")
      
    crop<- yearly %>%
      filter(month == 10)%>%
      mutate(year = year -1, precip10 = precip) %>%
      select(year, month, precip10)%>%
      merge(crop, by="year")
      
    crop$anomaly <- (6.93 * crop$tmin_7  - 0.19 * (crop$tmin_7**2) + 2.61 * crop$tmin_4 - 0.15 * (crop$tmin_4**2) + 
                  0.035 * crop$precip1 + 0.024 * (crop$precip1**2) +1.71 * crop$precip10 - 0.637 * (crop$precip10**2) -73.89)
    
    results<- crop %>% select(year, anomaly)
      
  }}}}}
 
    plot(x=crop$year, y=crop$anomaly, xlab="Year", lwd=1,
         ylab="ton per acre", 
         type="l", yaxs="i", xaxs="i", main=paste(Crop,"Yield Anomaly"))
    
    return(list(results= results, min = min(results$anomaly), max=max(results$anomaly)))
}
