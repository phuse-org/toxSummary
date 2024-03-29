

#############################################################################
# create metadata table                                                     #
#############################################################################

library(RSQLite)
library(data.table)
library(tictoc)
#  connect database
tic("start")
db_path <- "C:/Users/Md.Ali/OneDrive - FDA/yousuf/10_DATA/SEND_unclean.db"
conn <- RSQLite::dbConnect(drv = SQLite(), db_path)
ind_table <- data.table::as.data.table(RSQLite::dbGetQuery(
    conn = conn, "SELECT *  FROM ID"
))
ind_table <- ind_table[APPID %like% "IND", ]
# ind_table <- ind_table[1:1000]
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
toc()
#saveRDS(final_df, "C:/Users/Md.Ali/OneDrive - FDA/yousuf/00_github_projects/toxSummary/my_data_fda/pp_metadata_final.rds")
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
#################################################################################################

# find empty pp, tx, domain list

db_path <- "C:/Users/Md.Ali/OneDrive - FDA/yousuf/10_DATA/SEND_unclean.db"
conn <- RSQLite::dbConnect(drv = SQLite(), db_path)
ind_table <- data.table::as.data.table(RSQLite::dbGetQuery(
    conn = conn, "SELECT *  FROM ID"
))
ind_table <- ind_table[APPID %like% "IND", ]



##########

pp <- RSQLite::dbGetQuery(
    conn = conn,
    "SELECT *  FROM PP "
)
pp <- data.table::as.data.table(pp)

tx <- RSQLite::dbGetQuery(
    conn = conn,
    "SELECT *  FROM TX "
)
tx <- data.table::as.data.table(tx)

ts <- RSQLite::dbGetQuery(
    conn = conn,
    "SELECT *  FROM TX "
)
ts <- data.table::as.data.table(ts)

# ex <- RSQLite::dbGetQuery(
#     conn = conn,
#     "SELECT *  FROM EX "
# )
# ex <- data.table::as.data.table(ex)
study <- ind_table$STUDYID
tic()
pp_num_row <- NA
for (i in 1:length(study)) {
    pp_num_row[i] <- nrow(pp[STUDYID == study[i]])
}
toc()


tx_num_row <- NA
for (i in 1:nrow(ind_table)) {
    studyid <- ind_table[["STUDYID"]][i]
    df <- tx[STUDYID == studyid]
    tx_num_row[i] <- nrow(df)
}

tic()
ts_num_row <- NA
for (i in 1:length(study)) {
    ts_num_row[i] <- nrow(ts[STUDYID == study[i]])
}
toc()

new_ind_table <- data.table(pp = pp_num_row, ts = ts_num_row, tx = ts_num_row)
head(new_ind_table)
nrow(new_ind_table)

new_ind_table$dm <- dm_num_row

saveRDS(new_ind_table, "C:/Users/Md.Ali/OneDrive - FDA/yousuf/00_github_projects/toxSummary/my_data_fda/pp_ts_tx_nrow_all.rds")

new_ind_table[pp == 0 & ts == 0]
#1197
new_ind_table[pp == 0 & ts != 0]
new_ind_table[dm ==0 & pp != 0 ]

#############################################
dm <- RSQLite::dbGetQuery(
    conn = conn,
    "SELECT *  FROM DM "
)
dm <- data.table::as.data.table(dm)
tic()
dm_num_row <- NA
for (i in 1:length(study)) {
    dm_num_row[i] <- nrow(dm[STUDYID == study[i]])
}
toc()

# whether cmax and auc common in pp domain
library(data.table)
pp_meta_org <- readRDS("my_data_fda/pp_metadata_final.rds")
str(pp_meta_org)
dim(pp_meta_org)
pp_meta <- pp_meta[num_row != 0L]
dim(pp_meta)
testcd <- pp_meta$unq_testcd
testcd[1:5]
index <- seq(length(testcd))
#find where cmax not available
cmax <- grep("CMAX", testcd, ignore.case = TRUE)
`%not_in%`  <- Negate("%in%")
no_cmax <- which(index %not_in% cmax == TRUE)
no_cmax_list <- testcd[no_cmax]
pp_meta[unq_testcd %in% no_cmax_list, ]

# find where auc not available
auc <- grep("AUC", testcd, ignore.case = TRUE)

no_auc <- which(index %not_in% auc == TRUE)
no_auc_list <- testcd[no_auc]
pp_meta[unq_testcd %in% no_auc_list, ]
