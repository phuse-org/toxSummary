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
ind_table <- as.data.table(ind_table)

# st_id <- ind_table[,STUDYID]
species_title <- RSQLite::dbGetQuery(conn=conn,
	'SELECT STUDYID,  TSPARMCD,TSPARM, TSVAL 
FROM TS 
WHERE
TSPARMCD IN (
"SPECIES",
"STITLE")'
)

species_title <- as.data.table(species_title)

fn <- species_title[ind_table, on = "STUDYID"]
fn <- fn[, c("APPID", "STUDYID", "TSPARMCD",  "TSVAL")]

### get info from IND number

get_studyid <- fn[APPID == "IND012285", ]

get_studyid <- fn[APPID == "IND012285",]

get_studyid





get_title_list <- get_studyid[TSPARMCD == "STITLE", TSVAL]
get_sex <- get_studyid[]


unq_studyid <- unique(ind_table[["STUDYID"]])

species <- NA
title <- NA

for (i in 1:length(unq_studyid)) {




}

st_id_df <- data.table::as.data.table(st_id_df)
st_id_df
  
st_id_df[STUDYID == 8416388 & TSPARMCD %in% c("SPECIES", "STITLE")]

final_df  <- data.table::copy(ind_table)

final_df[, .N, by=STUDYID][order(-N)][1:30, ]

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
