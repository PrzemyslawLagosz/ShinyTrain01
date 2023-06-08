source("helpers.R")

#######   select_interval_value_MODULE ###########
select_interval_value_UI <- function(id) {
  ns <- NS(id)
  
  sliderInput(ns("interval_slider"), "Select value of the interval", min = 1, max = 10, value = 5, step = 1)
  
  
}

select_interval_value_Server <- function(id, currently_selected_interval_column) {
  #' This module allows to select interval value, (range of value) which is passed to `plot_output_server`
  #' 
  #' @param currently_selected_interval_column - `str` column name recived from `select_numeric_columns_Server`
  #' 
  #' @return `input$interval_slider` - `int` value of interval (X axis)

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
  #' Allows to select only numeric values
  #' 
  #' @return `input$interval_column_selector` - `str` column name (X axis)
  #' @return `input$calculate_column_selector` - `str` column name (Y axis)

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
                       currently_selected_interval_value,
                       filter_range_vals) {
  
  #' This module produce `plot_df` based on inputs, and render main_plot
  #' 
  #' @param currently_selected_interval_column  - `str` - receive from `select_numeric_columns_Server`
  #' @param currently_selected_calculate_column - `str` - receive from `select_numeric_columns_Server`
  #' @param currently_selected_interval_value   - `int` - receive from `select_interval_value_Server`
  #' @param filter_range_vals                   - `list of 2 int` - receive from `select_filter_range_Server`
  
  moduleServer(id, function(input, output, session) {
    
    plot_df <- reactive({
      req(currently_selected_interval_column(),
          currently_selected_calculate_column(),
          currently_selected_interval_value())
      
      interval_group_by(auta,
                        column_interval = currently_selected_interval_column(),
                        column_calculate = currently_selected_calculate_column(),
                        interval = currently_selected_interval_value(),
                        filter_range_vals = filter_range_vals())
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
          fill = "Occurances",
          x = NULL,
          y = sprintf("%s", currently_selected_calculate_column())
        ) +
        scale_y_continuous(labels = scales::number_format(suffix = " k")) +
        theme(legend.position="right",
              axis.text.x = element_text(angle = 45, vjust = 0.5)) +
        geom_hline(yintercept = mean(plot_df()$mean_by_interval )/1000,
                   linetype='dashed',
                   color = rgb(190/255, 115/255, 38/255),
                   size = 1)+
        annotate(geom = "text",
                 label = sprintf("Mean %s", currently_selected_calculate_column()),
                 y = mean(plot_df()$mean_by_interval )/1000,
                 x = nrow(plot_df())-1,
                 vjust = -1,
                 color = rgb(190/255, 115/255, 38/255))
    }, res = 96, height =650)
})
}


######  select_filter_range_MODULE ####

select_filter_range_UI <- function(id) {
  ns <- NS(id)
  
  sliderInput(ns("filter_range_slider"), "Filter range on X axis to zoom in/out", min = 1900, max = 2012, step = 1, value = c(1900, 2012))
}

select_filter_range_Server <- function(id, currently_selected_interval_column) {
  
  #' This module allows to filter whole data frame to ZOOM IN/OUT the plot, and cut the outliners.
  #' It updated witch `swich()` based on `currently_selected_interval_column`
  #' 
  #' @param currently_selected_interval_column  - `str` - receive from `select_numeric_columns_Server`
  #' 
  #' @return `input$filter_range_slider` - `list of 2 int`, range of `currently_selected_interval_column`

  moduleServer(id, function(input, output, session) {
    
    current_range <- reactive(range(auta[[currently_selected_interval_column()]]))
    
    observeEvent(currently_selected_interval_column(), {
      updateSliderInput(session, 
                        inputId = "filter_range_slider", 
                        min = current_range()[1],
                        max = current_range()[2],
                        step = 1,
                        value = current_range())
    })
    
    reactive(input$filter_range_slider)
})
}