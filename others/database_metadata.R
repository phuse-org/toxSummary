

#############################################################################
# create metadata table                                                     #
#############################################################################

library(RSQLite)
library(data.table)

#  connect database
db_path <- "C:/Users/Md.Ali/OneDrive - FDA/yousuf/10_DATA/SEND_unclean.db"
conn <- RSQLite::dbConnect(drv = SQLite(), db_path)
ind_table <- data.table::as.data.table(RSQLite::dbGetQuery(
    conn = conn, "SELECT *  FROM ID"
))
ind_table <- ind_table[APPID %like% "IND", ]
ind_table <- ind_table[1:1000]
# get_pp <- function(conn, studyid) {

#  df <- RSQLite::dbGetQuery(conn = conn,
#   'SELECT * FROM PP WHERE STUDYID==:x',
#   params = list(x = studyid))
#   df <- data.table::as.data.table(df)
#   df

# }

pp <- RSQLite::dbGetQuery(
    conn = conn,
    "SELECT *  FROM PP "
)
pp <- data.table::as.data.table(pp)

num_row <- NA
USUBJID <- NA
POOLID <- NA
PPTESTCD <- list()
VISITDY_populated <- NA
PPNOMDY_populated <- NA


for (i in 1:nrow(ind_table)) {
    studyid <- ind_table[["STUDYID"]][i]
    df <- pp[STUDYID == studyid]
    num_row[i] <- nrow(df)
    if (nrow(df) != 0) {
        USUBJID[i] <- ifelse(nrow(df) == sum(df$USUBJID == ""), "empty", ifelse(
            nrow(df) == sum(df$USUBJID != ""), "populated",
            ifelse(all(is.na(df$USUBJID)), NA, "mixed")
        ))
        POOLID[i] <- ifelse(nrow(df) == sum(df$POOLID == ""), "empty", ifelse(
            nrow(df) == sum(df$POOLID != ""), "populated",
            ifelse(all(is.na(df$POOLID)), NA, "mixed")
        ))
        PPTESTCD[[i]] <- unique(df[["PPTESTCD"]])
        VISITDY_populated[i] <- ifelse(all(is.na(df[["VISITDY"]])), FALSE, TRUE)
        PPNOMDY_populated[i] <- ifelse(all(is.na(df[["PPNOMDY"]])), FALSE, TRUE)
    } else {
        USUBJID[i] <- "no observation"
        POOLID[i] <- "no observation"
        PPTESTCD[i] <- NA
        VISITDY_populated[i] <- NA
        PPNOMDY_populated[i] <- NA
    }
}


final_df <- data.table::copy(ind_table)
final_df[, `:=`(
	num_row = num_row,
	usubjid = USUBJID,
	poolid = POOLID,
	visitdy_populated = VISITDY_populated,
	ppnomdy_populated = PPNOMDY_populated,
	unq_testcd = PPTESTCD
)]

#data.table::fwrite(final_df, "data/pp_metadata.csv")
# end


#######################################################


get_pk_param(conn= conn, "AP203", pk_param = "AUCLST")
	# debugonce(get_pk_param)
#######################################################################
# exploratory
######################

# load package
library(RSQLite)
library(data.table)
source("get_dose_pp.R")

#  connect database
db_path <- "C:/Users/Md.Ali/OneDrive - FDA/yousuf/10_DATA/SEND_unclean.db"
conn <- RSQLite::dbConnect(drv = SQLite(), db_path)

# get studyid
ind_table <- data.table::as.data.table(RSQLite::dbGetQuery(
    conn = conn, "SELECT *  FROM ID"
))
head(ind_table)
dim(ind_table)
# 8110 * 2
length(unique(ind_table[["STUDYID"]]))
# 7471


length(unique(ind_table[["APPID"]]))
# 2479


dt <- data.table::as.data.table(ind_table)

unq_dt <- dt[!duplicated(STUDYID, APPID)]

dt[, .(n = .N), by = STUDYID][n>1,][order(-n)][1:30, ]

dt[STUDYID=="2769-005"]
dt[STUDYID=="2.917E+11"]


tictoc::tic()
pp <- RSQLite::dbGetQuery(
    conn = conn,
    "SELECT *  FROM PP "
)
tictoc::toc()

pp <- data.table::as.data.table(pp)