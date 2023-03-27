act_overview_ui <- function(id) {
  tagList(
    reactableOutput(NS(id, "act_rel_tbl")),
    plotOutput(NS(id, "overview_plot"))
  )
}

act_overview_server <- function(id, user_board, act_rel) {
  moduleServer(id, function(input, output, session) {

    output$act_rel_tbl <- renderReactable({
      act_tbl(act_rel())
    })

    sel_act_ids <- reactive({
      sel <- getReactableState("act_rel_tbl", "selected")

      if (is.null(sel))
        stop(safeError("No Activities selected. Please select at least one."))

      act_ids <- act_rel() |>
        slice(sel) |>
        pull(id)
    })

    sel_act <- reactive({
      map_df(sel_act_ids(), ~ pin_read(user_board, .x))
    })

    output$overview_plot <- renderPlot({
      vis_meas(sel_act())
    })

    return(sel_act)
  })
}
