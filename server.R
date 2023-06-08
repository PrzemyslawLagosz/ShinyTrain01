server <- function(input, output, session) {
  
  select_interval_value_Server("interval_value_selector", currently_selected_interval_column = currently_selected_interval_column)
  currently_selected_interval_value <- select_interval_value_Server("interval_value_selector", currently_selected_interval_column = currently_selected_interval_column)
  
  select_numeric_columns_Server("numeric_columns")
  c(currently_selected_interval_column, currently_selected_calculate_column) %<-% select_numeric_columns_Server("numeric_columns")
  
  plotServer("main_plot", currently_selected_interval_column, currently_selected_calculate_column, currently_selected_interval_value)
}