# read IND number from file and connect to database

read_path <- ini::read.ini("paths.ini")
ind_map_path <- as.character(read_path[["paths"]][["ind_mapping"]])
db_path <- as.character(read_path[["paths"]][["database"]])

col_name <- c(
    "application_type",
    "IND_num",
    "studyID"
)
ind_table <- data.table::fread(ind_map_path,
    col.names = col_name
)

ind_table <- ind_table[application_type == "IND", .(IND_num, studyID)]
# ind_number_list <- ind_table[!duplicated(IND_num), c("IND_num")]
ind_number_list <- ind_number_list$IND_num

### extract studyid from database ----

conn <- RSQLite::dbConnect(drv = SQLite(), db_path)
sd_id <- RSQLite::dbGetQuery(conn = conn, "SELECT DISTINCT STUDYID FROM TX")
sd_id <- data.table::as.data.table(sd_id)
