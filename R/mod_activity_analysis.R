activity_analysis_ui <- function(id) {
  tagList(
    plotlyOutput(NS(id, "meas_ts_plot"), height = "1000px")
  )
}

activity_analysis_server <- function(id, sel_meas, keep_moving) {
  moduleServer(id, function(input, output, session) {
    sel_meas_ts <- reactive({
      meas_ts(sel_meas(), keep_moving())
    })

    output$meas_ts_plot <- renderPlotly({
      gg <- vis_meas_ts(sel_meas_ts())
      ggplotly(gg)
    })
  })
}
