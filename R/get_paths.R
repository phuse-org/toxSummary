
################################################################################
## The function for getting paths
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2022-10-06   Yousuf Ali             Initial version
################################################################################

#'
#' @param database_path Mandatory, character\cr
#' where is the database located
#' @param save_file_path Mandaroty, character\cr
#' where user want to save the rds and docs files
#' @param where_to_run Mandatory, character\cr
#' where user want to run the app. It could be local, shinyappsio or rsconnect
#'
#' @return a list
#' 
#'
#' @example
#' \dontrun {
#'
#' paths <- get_paths(
#'  database_path = "path/to/database.db",
#' save_file_path = "directory/where/to/save",
#' 	 where_to_run = "local")
#'
#'
#' }

get_paths <- function(database_path, save_file_path, where_to_run) {
    database_path <- as.character(database_path)
    save_file_path <- as.character(save_file_path)
    where_to_run <- as.character(where_to_run)
    if (file.exists(database_path)) {
        database_path <- database_path
    } else {
        stop(paste0(database_path, " file does not exist"))
    }

    if (dir.exists(save_file_path)) {
        save_file_path <- save_file_path
    } else {
        stop(paste0(save_file_path, " directory does not exist"))
    }

    list(
        database_path = database_path,
        save_file_path = save_file_path,
        where_to_run = where_to_run
    )
}
