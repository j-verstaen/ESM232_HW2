#' Crop Yield Anomaly
#'
#' Computes crop yield anomaly from time series of min, max temperature and precipitation
#' @param data dataframe of time series data including month, year, tmax_c (maximum temperature °C), tmin_c (minimum temeprature°C), precip (precipitation mm)
#' @param crop wine_grapes, almonds, table_grapes, oranges, walnuts, avocdos
#' @author Seleni
#' @return yield anomaly for each year, plot of variables and yield anomaly over time series, and max and minimum yields over a time series of multiple year inputs


almond_yield_anomaly <- function (data){
    
    yearly <- data %>%
      group_by(month, year)%>%
      summarize(tmax_c = mean(tmax_c),
                tmin_c = mean(tmin_c),
                precip = sum(precip))
    
    almond<- yearly%>%
      filter(month== 2)%>%
      select(year, month, tmin_c)
    
    almond <- yearly%>%
      filter(month == 1)%>%
      select(year, month, precip)%>%
      merge(almond, by="year")
    
    almond$anomaly <- -0.015*almond$tmin_c - 0.0046*(almond$tmin_c**2) - 0.07*almond$precip  + 0.0043*(almond$precip**2) + 0.28  
    
    results <- almond%>% select(year, anomaly)
    
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
