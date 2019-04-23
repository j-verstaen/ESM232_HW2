#' Profit in Net Present Value 
#'
#' Computes change in gross profit given price per yield
#' @param price per yield ($ per ton/acre)
#' @param discout rate ; default 10%
#' @param year vector of 1:n years 
#' @param anomaly vector of yild anomaly (ton/acre)
#' @author Seleni Cruz and Juliette Verstaen
#' @return total change in gross prifit aggregated over all years in NPV for the first year of data ($)



NPV_profit <- function(price, anomaly, year, discount=0.10) {
  
  result <- (price * anomly) / (1 + discount)**year
  
  return(profit = sum(result))
}
