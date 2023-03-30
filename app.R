#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("libraries.R")

walk(dir_ls("R"), source)

user_board <- board_folder("26845822/")
df_act <- pin_read(user_board, "df_act")

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_tabs",
      menuItem(
        tabName = "overview_act", "Overview Activities",
        icon = icon("person-running"),
        startExpanded = TRUE, expandedName = "overview_act_expanded",
        dateInput(
          "earliest_start", "Earliest Start Date",
          value = today() - dweeks(16)),
        selectInput(
          "rel_types", label = "Activity Types",
          choices = unique(df_act$type), selected = "Run")
      ),
      hidden(
        menuItem("hidden_overview_act", tabName = "hidden_overview_act")
      ),
      menuItem(
        tabName = "activity_analysis", text = "Activity Analysis",
        expandedName = "activity_analysis_expanded",
        icon = icon("magnifying-glass-chart"),
        checkboxInput(
          "keep_moving", label = "Discard 'Breaks'?",
          value = TRUE)
      ),
      hidden(
        menuItem(
          "hidden_activity_analysis", tabName = "hidden_activity_analysis")
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "hidden_overview_act", text = "Overview Activities",
        act_overview_ui("act_overview_1")
      ),
      tabItem(
        tabName = "hidden_activity_analysis", text = "Activity Analysis",
        activity_analysis_ui("activity_analysis_1"))
    )
  )
)

server <- function(input, output, session) {

  act_rel <- reactive({
    df_act |>
      filter(
        as_date(start_date) >= input$earliest_start, type %in% input$rel_types)
  })

  keep_moving <- reactive({input$keep_moving})

  sel_act <- act_overview_server("act_overview_1", user_board, act_rel)

  activity_analysis_server("activity_analysis_1", sel_act, keep_moving)

  observeEvent(input$sidebarItemExpanded, {
    if (input$sidebarItemExpanded == "overview_act_expanded") {
      updateTabItems(
        session, inputId = "sidebar_tabs", selected = "hidden_overview_act")
    }
    if (input$sidebarItemExpanded == "activity_analysis_expanded") {
      updateTabItems(
        session, inputId = "sidebar_tabs",
        selected = "hidden_activity_analysis")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
