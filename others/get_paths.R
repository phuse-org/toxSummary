
################################################################################
## The function for getting paths
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2022-10-06   Yousuf Ali             Initial version
################################################################################


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
        save_file_path <- fs::path(save_file_path)
    } else {
        stop(paste0(save_file_path, " directory does not exist"))
    }

    list(
        database_path = database_path,
        save_file_path = save_file_path,
        where_to_run = where_to_run
    )
}
