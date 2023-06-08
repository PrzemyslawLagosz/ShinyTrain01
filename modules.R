library(tidyverse)
library(ggplot2)
library(shiny)
library(rlang)
library(zeallot)
library(shinydashboard)

#load("auta.Rdata") ######## TU JEST PROBLEM

auta <- read_csv("auta.csv")

mask1 <- quantile(auta$Przebieg.w.km, probs = 0.9995, na.rm= TRUE) 

auta <- auta %>%
  filter(Przebieg.w.km < mask1 & KM < 1100 & Pojemnosc.skokowa < 18000)

# Columns used to filter main plot in APP
numericColnames <- colnames(select_if(auta, is.numeric))

##### FUNCTIONS #######
interval_slider_values <- function(currently_selected_interval_column) {
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

interval_group_by <- function(df, column_interval, column_calculate, interval = 5) {
  #' @param DF to calculate
  #' @param column_interval `STRING` <-  column turned into intervals with cut and group_by
  #' @param column_calculate `STRING` <- column to calculate mean and median in intervals
  #' @param interval `INTEGER` <- value of the interval
  #' @return DATA FRAME grouped by @param column_interval and calculated mean, median for @param column_calculate
  
  df_grouped_by_interval <- df %>%
    mutate(intervals = cut(!!sym(column_interval), 
                           seq_last(!! sym(column_interval), interval), 
                           include.lowest = TRUE)
    ) %>%
    group_by(intervals) %>%
    summarise(mean_by_interval = mean(!! sym(column_calculate), na.rm = TRUE),
              median_by_interval = median(!! sym(column_calculate), na.rm = TRUE),
              n= n()) %>% 
    drop_na()
  
  return(df_grouped_by_interval)
}

seq_last <- function(x, interval) {
  #' @param x is vector
  #' 
  #' @return sequence like `seq()` function, but include last not full interval
  #' 
  #' 
  my_seq <- seq(from = min(x), to = max(x), by = interval)
  
  if (tail(my_seq, 1) == max(x)) {
    return(my_seq)
  } else {
    my_seq <- c(my_seq, max(x))
    return(my_seq)
  }
}

#interval_group_by(auta, "Rok.produkcji", "Cena", 10)

#######   select_interval_value_MODULE ###########
select_interval_value_UI <- function(id) {
  ns <- NS(id)
  
  sliderInput(ns("interval_slider"), "Select value of the interval", min = 1, max = 10, value = 5, step = 1)
  
  
}

select_interval_value_Server <- function(id, currently_selected_interval_column) {
  moduleServer(id, function(input, output, session) {
    
    slider_values <- reactive(
      interval_slider_values( currently_selected_interval_column() )
    )
    
    observeEvent(currently_selected_interval_column(), {
      freezeReactiveValue(input, "interval_slider")
      
      updateSliderInput(session, 
                        inputId = "interval_slider", 
                        min = slider_values()$min, 
                        max = slider_values()$max, 
                        step = slider_values()$step, 
                        value = slider_values()$value)
    })
    
    return(
      reactive(
        input$interval_slider
      )
    )
     
})
}


#######   select_numeric_columns_MODULE ###########
select_numeric_columns_UI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6,
           selectInput(ns("interval_column_selector"), "Select interval column", choices = numericColnames, selected = "Rok.produkcji")
           ),
    column(6,
           selectInput(ns("calculate_column_selector"), "Select column to calculate", choices = numericColnames, selected = "Cena.w.PLN")
           )
    )
  
}

select_numeric_columns_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    list(
      reactive(input$interval_column_selector),
      reactive(input$calculate_column_selector)
    )
})
}


#######   plot_output_MODULE #######
plotUI <- function(id) {
  ns <- NS(id)
  
  plotOutput(ns("main_plot"))
}
plotServer <- function(id,
                       currently_selected_interval_column,
                       currently_selected_calculate_column,
                       currently_selected_interval_value) {
  moduleServer(id, function(input, output, session) {
    
    
    observeEvent(currently_selected_interval_column(), {
      message("currently_selected_interval_column: ", currently_selected_interval_column(), "\n",
              "currently_selected_calculate_column: ", currently_selected_calculate_column(), "\n",
              "currently_selected_interval_value: ", currently_selected_interval_value(), "\n"
              )
    })
    
    plot_df <- reactive({
      req(currently_selected_interval_column(),
          currently_selected_calculate_column(),
          currently_selected_interval_value())
      
      interval_group_by(auta,
                        currently_selected_interval_column(),
                        currently_selected_calculate_column(),
                        currently_selected_interval_value())
    })
    
    output$main_plot <- renderPlot({
      req(currently_selected_interval_column(),
          currently_selected_calculate_column(),
          currently_selected_interval_value())
      
      ggplot(plot_df(), aes(x = intervals, y= mean_by_interval /1000, fill = n)) +
        geom_col() +
        geom_text(aes(label = n), nudge_y = (0.00005*max(plot_df()$mean_by_interval)), check_overlap = TRUE) +
        labs(
          title = sprintf("Mean %s", currently_selected_calculate_column()),
          subtitle = sprintf("Interval %s of %s", currently_selected_interval_value(),currently_selected_interval_column()),
          x = NULL,
          y = sprintf("%s", currently_selected_calculate_column())
        ) +
        scale_y_continuous(labels = scales::number_format(suffix = " k")) +
        theme(legend.position="right",
              axis.text.x = element_text(angle = 45, vjust = 0.5)) +
        geom_hline(yintercept = mean(plot_df()$mean_by_interval )/1000,
                   linetype='dashed')+
        annotate(geom = "text",
                 label = sprintf("Mean %s", currently_selected_calculate_column()),
                 y = mean(plot_df()$mean_by_interval )/1000,
                 x = nrow(plot_df())-1,
                 vjust = -1)
    })
})
}

# max(interval_group_by(auta, "Rok.produkcji", "Cena.w.PLN", 10)$mean_by_interval)
# max(interval_group_by(auta, "Rok.produkcji", "KM", 10)$mean_by_interval)
