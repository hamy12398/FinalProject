#' linear_reg
#'
#' This function is used to fit linear models. It can tested the continuous variable and dummy variable
#'
#' @param response the vector value of variable/ column in data that is response
#'  variable in regression (numerical value)
#' @param predictor the vector value of variable/ column in data that is predictor
#'  variable in regression (continuous numerical value or dummy variable)
#' @param data data frame where the variable values come from
#'
#' @return The result from the regression test including numerical results,
#'assumption plots, and plot of fitted values in the original points
#'
#'@examples
#' data(apps)
#'linear_reg(apps$Rating,apps$Reviews,apps)
#' @export

#### Linear Regression
linear_reg <- function(response, predictor, data){
  if(is.numeric(response)==F) {stop("Response values needs to be numeric values")}
  ifelse(is.character(predictor)==F,
         ln <- lm(response ~predictor), ln<- lm(response~factor(predictor)))
  ln.sum <- summary(ln)
  #### Assumption plots
  par(mfrow=c(2,2))
  pl.2 <- plot(ln)
  #### create fitted plot
  library(tidyverse)
  pl.1 <- data %>% ggplot(aes(x ={{predictor}}, y={{response}}))+
    geom_point()+ geom_smooth(method="lm")
  return(list(ln.sum,pl.2,pl.1))
}



#' poly_reg
#'
#' This function is used to polynomial regression models. The model is the linear combination of the predictor in ascending ordered power.
#'
#' @param response the vector value of variable/ column in data that is response
#'  variable in regression (numerical value)
#' @param predictor the vector value of variable/ column in data that is predictor
#'  variable in regression (numerical value )
#' @param dorder the number of ordered power assigned for the predictors
#'
#' @return The result from the regression test including numerical results, 95% confidence interval, and
#'assumption plots.
#'
#'@examples
#' data(apps)
#'poly_reg(apps$Rating, apps$Size, order =3)
#' @export

poly_reg <- function(response, predictor,order){
  if(!is.numeric(response)|!is.numeric(predictor)) {stop("Response and predictor values needs to be numeric values")}
  poly <- lm(response~poly(predictor,order))
  sum <- summary(poly)
  ci <- confint(poly, level=0.95)
  par(mfrow=c(2,2))
  pl <- plot(poly)
  return(list(sum,ci,pl))
}

