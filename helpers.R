library(tidyverse)
library(ggplot2)
library(shiny)
library(rlang)
library(zeallot)
library(shinydashboard)

auta <- read_csv("auta.csv")

mask1 <- quantile(auta$Przebieg.w.km, probs = 0.9995, na.rm= TRUE) 

auta <- auta %>%
  filter(Przebieg.w.km < mask1 & KM < 1100 & Pojemnosc.skokowa < 18000)

# Columns used to filter main plot in APP
numericColnames <- colnames(select_if(auta, is.numeric))

##### FUNCTIONS #######
### interval_slider_values ####
interval_slider_values <- function(currently_selected_interval_column) {
  #' @return arguments to update `select_interval_value_UI` based on `currently_selected_interval_column`
  
  switch(currently_selected_interval_column,
         "Cena" = list(min = 10e+3, max = 10e+5, step = 10e+3, value = 5*10e+4),
         "Cena.w.PLN" = list(min = 10e+3, max = 10e+5, step = 10e+3, value = 5*10e+4),
         "KM" = list(min = 10, max = 100, step = 10, value = 50),
         "kW" = list(min = 10, max = 100, step = 10, value = 50),
         "Pojemnosc.skokowa" = list(min = 100, max = 1000, step = 100, value = 500),
         "Przebieg.w.km" = list(min = 10e+3, max = 10e+5, step = 10e+3, value = 5*10e+4),
         "Rok.produkcji" = list(min = 1, max = 10, step = 1, value = 5),
  )
}

##### interval_group_by #####
interval_group_by <- function(df, column_interval, column_calculate, interval = 5, filter_range_vals = c(1900, 2020)) {
  #' @param DF to calculate
  #' @param column_interval `STRING` <-  column turned into intervals with cut and group_by
  #' @param column_calculate `STRING` <- column to calculate mean and median in intervals
  #' @param interval `INTEGER` <- value of the interval
  #' @return DATA FRAME grouped by @param column_interval and calculated mean, median for @param column_calculate
  
  df_grouped_by_interval <- df %>%
    filter(!! sym(column_interval) >= filter_range_vals[1] & !! sym(column_interval) <= filter_range_vals[2]) %>%
    mutate(intervals = cut(!!sym(column_interval), 
                           seq_last(!! sym(column_interval), interval), 
                           include.lowest = TRUE,
                           dig.lab = 10)
    ) %>%
    group_by(intervals) %>%
    summarise(mean_by_interval = mean(!! sym(column_calculate), na.rm = TRUE),
              median_by_interval = median(!! sym(column_calculate), na.rm = TRUE),
              n= n()) %>% 
    drop_na()
  
  return(df_grouped_by_interval)
}

interval_group_by(auta, "Cena.w.PLN", "Rok.produkcji", 100)
##### seq_last ####
seq_last <- function(x, interval) {
  
  #' @param x is vector
  #' 
  #' @return sequence like `seq()` function, but include last not full interval
 
  my_seq <- seq(from = min(x), to = max(x), by = interval)
  
  if (tail(my_seq, 1) == max(x)) {
    return(my_seq)
  } else {
    my_seq <- c(my_seq, max(x))
    return(my_seq)
  }
}