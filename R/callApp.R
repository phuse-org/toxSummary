
#' run toxsummary app
#' @return run shiny app  in browser
#' @export
#' @import shiny
#' @import data.table
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom stats  setNames
#' @importFrom utils read.csv
#' @importFrom utils tail
#' @importFrom utils tar
#' @importFrom htmltools attachDependencies

run_toxSummary <- function() {
  shiny::runApp(system.file('app', package = 'toxSummary'))
}