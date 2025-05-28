#' Launch the Shiny App
#'
#' @export
run_medcodeapp <- function() {
  app_dir <- system.file("app", package = "medcodeapp")
  shiny::runApp(app_dir)
}
