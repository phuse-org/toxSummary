# read IND number from file and connect to database

# read IND mapping file
read_ind_table <- function(path = "../paths.ini") {
    read_path <- ini::read.ini("paths.ini")
    ind_map_path <- as.character(read_path[["paths"]][["ind_mapping"]])
    col_name <- c(
        "application_type",
        "IND_num",
        "studyID"
    )
    ind_table <- read.csv(ind_map_path,
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

connect_db <- function(path = "../paths.ini") {
    read_path <- ini::read.ini("paths.ini")
    db_path <- as.character(read_path[["paths"]][["database"]])
    conn <- RSQLite::dbConnect(drv = RSQLite::SQLite(), db_path)
	return(conn)
}
