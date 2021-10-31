#' summary_data
#'
#' This function loads a summary of specific variables/column in data that is
#' defined by another variable into group. The summary should be grouped by a character variable
#'
#' @param variable name of variable/column in data that is needed to summarize (numeric variable)
#' @param group name vector of variable/column defines/groups the data (character variable)
#' @param data a data frame where the variable and group values come from
#' @return A tibble table with column of group variable's elements
#' and the summarized variable's sample size, mean, standard deviation, min, max
#' @examples
#' data(apps)
#'summary_data(variable = Size, group = Type, data = apps)
#' @export

### Summary data
summary_data <- function(variable, group, data){
  library(dplyr)
  x1<- data%>% pull({{group}})
  if(is.character(x1)==F) {warning("Group values should be character")}
  x2<- data%>% pull({{variable}})
  if(is.numeric(x2)==F) {stop("Variable values needs to be only numeric values")}
  data %>% dplyr::group_by({{group}}) %>%
    dplyr::summarize(N= length({{variable}}), Mean = mean({{variable}}),
                     Sd = sd({{variable}}), Min =min({{variable}}),
                     Max = max({{variable}}))
}


#' plot_data
#'
#' This function loads a scatter plot of 2 assigned numeric variable from the data that
#' is defined by another variable into group. The summary should be grouped by a character variable.
#'
#' @param x the name of variable/ column in data that is assigned as the x coordinate of points in the plot
#' @param y the name of variable/ column in data that is assigned as the y coordinate of points in the plot
#' @param group name vector of variable/column defines/groups the data (character variable)
#' @param data  data frame where the variable values come from
#' @param xlab a title for the x axis
#' @param ylab a title for the y axis
#'
#' @return a scatter plot of 2 numeric variables corresponding the x and y coordinate
#' #' @examples
#' data(apps)
#'plot_data(x=Rating, y=Size, data = apps)
#' @export

### Scatter Plot by Group
plot_data <- function(x,y,group=NULL, data, xlab=NULL, ylab=NULL){
  library(dplyr)
  x1<- data%>% dplyr::pull({{x}})
  y1<- data%>% dplyr::pull({{y}})
  if(!is.numeric(x1)|!is.numeric(y1)) {stop("x,y values needs to be only numeric values")}
  library(tidyverse)
  data %>% ggplot(aes(x ={{x}}, y={{y}})) +geom_point(aes(color = {{group}}))+
    labs(x= xlab,y= ylab)
}


#' apps_time.plot
#'
#' This function loads a plot of time series of the assigned variable with the time variable in (mdy format) in the data
#' @param variable the name of variable/ column in data that is assigned as the x coordinate of points in the time plot
#' @param frep the number of observations per unit of time.
#' @param data data frame where the variable values come from
#' @param xlab a title for the x axis
#' @param ylab a title for the y axis
#'
#' @return a line plot of time vs the assigned variable values
#' @examples
#' data(apps)
#'apps_time.plot(variable= Rating, freq = 12, data = apps)
#' @export


### Time series plot
apps_time.plot <- function(variable, freq, data, xlab=NULL, ylab=NULL){
  library(dplyr)
  time.dat <- data%>% dplyr::arrange(`Last Updated`)%>% dplyr::select({{variable}})
  Rating_Over_Time <- stats::ts(time.dat, start=c(2010, 1), end=c(2018, freq), frequency=freq)
  plot(Rating_Over_Time, xlab = xlab, ylab= ylab)
}



