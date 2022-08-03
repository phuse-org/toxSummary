read_path <- ini::read.ini("paths.ini")
db_path <- as.character(read_path[["paths"]][["database"]])

conn <- RSQLite::dbConnect(drv = SQLite(), db_path)
ind_table <- data.table::as.data.table(RSQLite::dbGetQuery(conn = conn, "SELECT *  FROM ID"))

ind_table <- ind_table[APPID %like% "IND", ]

ind_table[, `:=`(IND_num=APPID, studyID = STUDYID, APPID=NULL, STUDYID=NULL)]
ind_number_list <- ind_table[!duplicated(IND_num), c("IND_num")]
ind_number_list <- ind_number_list$IND_num

