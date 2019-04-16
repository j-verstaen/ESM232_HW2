#' Crop Yield Anomaly
#'
#' Computes crop yield anomaly from time series of min, max temperature and precipitation
#' @param data dataframe of time series data including month, year, tmax_c (maximum temperature °C), tmin_c (minimum temeprature°C), precip (precipitation mm)
#' @param crop wine_grapes, almonds, table_grapes, oranges, walnuts, avocados
#' @authors Seleni Cruz and Juliette Verstaen
#' @return yield anomaly for each year (tons/acres), plot of variables and yield anomaly over time series, and max and minimum yields over a time series of multiple year inputs


almond_yield_anomaly <- function (data, crop){
  
  yearly <- data %>%
    group_by(month, year)%>%
    summarize(tmax_c = mean(tmax_c),
              tmin_c = mean(tmin_c),
              precip = sum(precip))
 
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
  }
 
####avocados
  
  else{ if(crop == "avocados") {

    crop<- yearly %>%
    filter(month== 8)%>%
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

#### wine_grapes    
  
  else{ if(crop == "wine_grapes") {
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

### walnuts
    
   else{ if(crop == "walnuts") {
  
        crop<- yearly %>%
        filter(month== 11)%>%
        mutate(year = year -1) %>%
        select(year, month, tmax_c)
      
      crop <- yearly%>%
        filter(month == 2)%>%
        select(year, month, precip)%>%
        merge(crop, by="year")
      
      crop$anomaly <- 0.68*(crop$tmax_c) - 0.020*(crop$tmax_c**2) + 0.038*(crop$precip) - 0.0051*(crop$precip**2)  - 5.83
      
      results <- crop%>% 
        select(year, anomaly)

    
    }}
  
  
  
  
  
  t <- ggplot(almond, aes(year, tmin_c))+
    geom_line(size=1.5)+
    labs(x="Year", y="°C", subtitle = "Minimum temperature")+
    theme_classic()
  
  p <- ggplot(almond, aes(year, precip))+
    geom_line(size=1.5)+
    labs(x="Year", y="mm", subtitle = "Precipitation")+
    theme_classic()
  
  a <- ggplot(almond, aes(year, anomaly))+
    geom_line(size=1.5)+
    labs(x="Year", y=expression("ton acre"^-1), subtitle = "Yield anomaly")+
    theme_classic()
  
  
  plot(x=almond$year, y=almond$anomaly, xlab="Year", lwd=1,
       ylab="ton acre", 
       type="l", yaxs="i", xaxs="i")
  
  for (i in 1:1000){
    increase <- runif(1,0,3)
    
    almond$anomaly <- -0.015*(almond$tmin_c + increase) - 0.0046*((almond$tmin_c + increase)**2) - 0.07*almond$precip  + 0.0043*(almond$precip**2) + 0.28  
    
    lines(x=almond$year, y=almond$anomaly, lwd=2)
  }
  all <- ggarrange(t, p, a, ncol=1, nrow=3)
  
  return(list(results = results, min = min(results$anomaly), max = max(results$anomaly), plot = all))
}
