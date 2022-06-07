


#' @importFrom utils globalVariables

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))




# read IND number from file and connect to database
# read IND mapping file
read_ind_table <- function() {
	path <- system.file("extdata/IND_with_studies_2.csv", package = "toxSummary")
    #read_path <- ini::read.ini(path)
    #ind_map_path <- as.character(read_path[["paths"]][["ind_mapping"]])
    col_name <- c(
        "application_type",
        "IND_num",
        "studyID"
    )
    ind_table <- read.csv(path,
        col.names = col_name
    )
    ind_table <- data.table::as.data.table(ind_table)
    return(ind_table)
}


# get IND list
get_ind_list <- function() {
    ind_table <- read_ind_table()
    ind_table <- ind_table[application_type == "IND", c("IND_num", "studyID")]
    ind_number_list <- ind_table[!duplicated(IND_num), c("IND_num")]
    ind_number_list <- ind_number_list$IND_num
    return(ind_number_list)
}

#### connect to database

connect_db <- function() {
    # read_path <- ini::read.ini(path)
    db_path <- system.file("extdata/test_db.db", package = "toxSummary")
    # db_path <- as.character(read_path[["paths"]][["database"]])
    conn <- RSQLite::dbConnect(drv = RSQLite::SQLite(), db_path)
	return(conn)
}

############
application_type <-  IND_num <- NULL