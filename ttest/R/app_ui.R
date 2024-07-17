#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import DT
#' @import car
#' @import datasets
#' @import stringr
#' @import nortest
#' @import shinydashboard
#' @import shinyjs
#' @noRd
app_ui <- function() {
  sidebar <- dashboardSidebar(
    width = 300,
    fileInput("file",  c("Choose .CSV File"), accept = c(".csv"),placeholder = "No file selected"),
    uiOutput("Sidebar")
  )

  body <- dashboardBody(
    useShinyjs(),
    uiOutput("TABUI")
  )
  dashboardPage(
    dashboardHeader(title = "t test"),
    sidebar,
    body,
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ttest"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
