source("modules.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "Train"
  ),
  dashboardSidebar(disable = TRUE, collapsed = TRUE),
  dashboardBody(
    fluidRow(
      column(4,
             wellPanel(
              select_interval_value_UI("interval_value_selector")
              )
             ),
      column(6,
             wellPanel(
               select_numeric_columns_UI("numeric_columns")
               )
             )
    ),
    fluidRow(
      column(12,
             wellPanel(
               style = "height: 740px; overflow-y: auto;",
               plotUI("main_plot")
             )
             )
    ),
    fluidRow(
      column(12,
             wellPanel(
               select_filter_range_UI("select_filter_range")
               )
             )
    )
    )
  )