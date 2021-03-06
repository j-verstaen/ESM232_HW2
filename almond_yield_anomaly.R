#' Almond Yield Anomaly
#'
#' Computes crop yield anomaly from time series of min, max temperature and precipitation
#' @param data dataframe of time series data including month, year, tmax_c (maximum temperature °C), tmin_c (minimum temeprature°C), precip (precipitation mm)
#' @author Seleni Cruz and Juliette Verstaen
#' @return yield anomaly for each year (tons/acre), plot of variables and yield anomaly over time series, and max and minimum yields over a time series of multiple year inputs


almond_yield_anomaly <- function (data){
    
    yearly <- data %>%
      group_by(month, year)%>%
      summarize(tmax_c = mean(tmax_c),
                tmin_c = mean(tmin_c),
                precip = sum(precip))
    
    crop<- yearly%>%
      filter(month== 2)%>%
      select(year, month, tmin_c)
    
    crop <- yearly%>%
      filter(month == 1)%>%
      select(year, month, precip)%>%
      merge(crop, by="year")
    
    crop$anomaly <- -0.015*crop$tmin_c - 0.0046*(crop$tmin_c**2) - 0.07*crop$precip  + 0.0043*(crop$precip**2) + 0.28  
    
    results <- crop%>% select(year, anomaly)
    
    t <- ggplot(crop, aes(year, tmin_c))+
      geom_line(size=1.5)+
      labs(x="Year", y="°C", subtitle = "Minimum temperature")+
      theme_classic()
    
    p <- ggplot(crop, aes(year, precip))+
      geom_line(size=1.5)+
      labs(x="Year", y="mm", subtitle = "Precipitation")+
      theme_classic()
    
    a <- ggplot(crop, aes(year, anomaly))+
      geom_line(size=1.5)+
      labs(x="Year", y=expression("ton acre"^-1), subtitle = "Yield anomaly")+
      theme_classic()
  
    plot(x=crop$year, y=crop$anomaly, xlab="Year", lwd=1,
         ylab="ton per acre", 
         type="l", yaxs="i", xaxs="i", main= "Yield anomaly: Temperature sensitivity anlysis")
    
    for (i in 1:1000){
      increase <- runif(1,0,3)
  
      crop$anomaly <- -0.015*(crop$tmin_c + increase) - 0.0046*((crop$tmin_c + increase)**2) - 0.07*crop$precip  + 0.0043*(crop$precip**2) + 0.28  
      
      lines(x=crop$year, y=crop$anomaly, lwd=2)
    }
    all <- ggarrange(t, p, a, ncol=1, nrow=3)

    return(list(results = results, min = min(results$anomaly), max = max(results$anomaly), plot = all))
}

