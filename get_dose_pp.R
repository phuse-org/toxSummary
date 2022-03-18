
# function to get dose and Pharmacokinetics parameter for a specific study
# library(DBI)
# library(data.table)

#' Title
#'
#' @param db 
#' @param studyid 
#' @param pk_param 
#'
#' @return
#' @export
#'
#' @examples
get_pk_param <- function(conn, studyid, pk_param="AUCLST", sex_include="ALL"){
  '%ni%' <- Negate('%in%')
  studyid <- as.character(studyid)
  pk_param <- toupper(as.character(pk_param))
  studyid_list_all <- RSQLite::dbGetQuery(conn = conn,
  'SELECT DISTINCT STUDYID FROM TX')
  studyid_list_pp <- RSQLite::dbGetQuery(conn = conn,
  'SELECT DISTINCT STUDYID FROM PP')
  testcode_list_pp <- RSQLite::dbGetQuery(conn = conn,
  'SELECT DISTINCT PPTESTCD FROM PP WHERE STUDYID==:x',
  params = list(x = studyid))

  if (studyid %ni% studyid_list_all$STUDYID) {
    stop("Study not found in the database")
  }
  if (studyid %ni% studyid_list_pp$STUDYID) {
    stop("Study does not contain PP domain")
  }
  if (pk_param %ni% testcode_list_pp$PPTESTCD) {
    stop(paste0(pk_param,
	 " is not found in PPTESTCD variable in PP domain for ",
	  studyid, " study"))
  }

  #  get sex argument
  sex_include <- toupper(sex_include)
  if (sex_include %ni% c("ALL", "M", "F")) {
	  stop("Please select sex_include as ALL, M or F")
  }

  pp_domain <- RSQLite::dbGetQuery(conn = conn,
  'SELECT * FROM PP WHERE STUDYID==:x AND PPTESTCD IN ($CMAX, $AUC)',
  params = list(x = studyid, CMAX = "CMAX", AUC = pk_param))

  # get the dose information
  dose <- RSQLite::dbGetQuery(conn=conn,
  'SELECT TX.STUDYID, TX.TXPARMCD, TX.TXVAL, TX.SETCD,
  DM.USUBJID, DM.SEX FROM TX INNER JOIN DM 
  ON (TX.STUDYID=DM.STUDYID AND TX.SETCD=DM.SETCD)
  WHERE TX.STUDYID==:x AND (TX.TXPARMCD="TRTDOS" OR TX.TXPARMCD="TRTDOSU")',
  params = list(x = studyid))
  
  dose <- data.table::as.data.table(dose)
  dose_wide <- data.table::dcast.data.table(dose, ... ~ TXPARMCD,
                                            value.var = "TXVAL")
  dose_wide$TRTDOS <- clean_txval_dose(dose = dose_wide$TRTDOS)
 
  # select only required columns
  pp_domain <- data.table::as.data.table(pp_domain)
  pp_domain <-  pp_domain[, .(STUDYID, DOMAIN, USUBJID,
  POOLID, PPTESTCD,  PPTEST,
  PPORRES, PPORRESU, PPSTRESC,
  PPSTRESU, PPSTRESN)]

  if (nrow(pp_domain) == sum(pp_domain$USUBJID != "")) {
    df <- merge(dose_wide, pp_domain, by = c("STUDYID", "USUBJID"))
 
	# filter sex
	  if (sex_include != "ALL") {
	  df <- df[SEX == sex_include, ]
  }

    df <- df[, .(TRTDOS, TRTDOSU, PPSTRESN, PPSTRESU, PPTESTCD)]
    df <- df[, .(mean=mean(PPSTRESN)), by=.(TRTDOS, TRTDOSU,PPTESTCD,PPSTRESU)]
    df <- df[, .SD, .SDcols=c(1, 2, 3, 5, 4)]

  } else if (nrow(pp_domain) == sum(pp_domain$USUBJID == "")) {
    pooldef_list <- RSQLite::dbGetQuery(conn = conn,
	 'SELECT DISTINCT STUDYID FROM POOLDEF')

    if (studyid %ni% pooldef_list$STUDYID) {
      stop("USUBJID in PP domain empty and this study does not have POOLDEF domain to merge with PP domain")
    }
    pooldef <- RSQLite::dbGetQuery(conn = conn,
	 'SELECT * FROM POOLDEF WHERE STUDYID==:x',
	  params = list(x=studyid))
    pooldef <- data.table::as.data.table(pooldef)
    pp_domain <- pp_domain[, USUBJID := NULL]
    df <- data.table::merge.data.table(pp_domain, pooldef,
	 by=c("STUDYID", "POOLID"),
	 allow.cartesian=T)
    df <- data.table::merge.data.table(df,dose_wide,
	 by=c("STUDYID","USUBJID"))

	if (sex_include != "ALL") {
	df <- df[SEX == sex_include, ]
  }
    df <- df[, .(TRTDOS,TRTDOSU, PPSTRESN, PPSTRESU,PPTESTCD)]
    df <- df[, .(mean=mean(PPSTRESN)),
	 by=.(TRTDOS, TRTDOSU,PPTESTCD,PPSTRESU)]
    #names(df)[5] <- paste0(pk_param, "_Mean")
    df <- df[, .SD, .SDcols=c(1,2,3,5,4)]

  } else {
    # filter only poolid populated
    poolid_pp <- pp_domain[POOLID != "", ]
    poolid_pp <- poolid_pp[, USUBJID := NULL]
    #filter only usubjid populated
    usubjid_pp <- pp_domain[USUBJID != ""]
    usubjid_pp <- usubjid_pp[, POOLID := NULL]
    # get dose info for poolid 
    pooldef <- RSQLite::dbGetQuery(conn = conn,
	 'SELECT * FROM POOLDEF WHERE STUDYID==:x',
	 params = list(x=studyid))
    pooldef <- data.table::as.data.table(pooldef)
    df <- data.table::merge.data.table(poolid_pp, pooldef,
	by=c("STUDYID", "POOLID"),
	allow.cartesian = T)
    df <- data.table::merge.data.table(df,dose_wide,
	by=c("STUDYID","USUBJID"),
	allow.cartesian = TRUE)
    
    df <- df[, POOLID:=NULL]
    # get dose info for usubjid
    df_usubjid <- data.table::merge.data.table(usubjid_pp,dose_wide,
	by=c("STUDYID","USUBJID"),
	allow.cartesian = TRUE)
    
    row_bind <- list(df,df_usubjid)
    df_all <- data.table::rbindlist(row_bind, use.names = T)
	if (sex_include != "ALL") {
	df_all <- df_all[SEX == sex_include, ]
  }
    # check duplicated value dataframe
    df <- df_all[, .(mean_cmax=mean(PPSTRESN)),
	 by=.(TRTDOS, TRTDOSU,PPTESTCD,PPSTRESU)]
    #names(df)[5] <- paste0(pk_param, "_Mean")
    df <- df[, .SD, .SDcols=c(1,2,3,5,4)]
  }
  df <- df[order(PPTESTCD,TRTDOS),]
  return(df)
}




clean_txval_dose <- function(dose) {
  index <- 1:length(dose)
  dose_char <- dose
  dose_num <- as.numeric(dose)
  final_dose <- NA

  for (i in index) {
    if (is.na(dose_num[i])) {
      x <- gsub(";|-|\\/|\\|", ",", dose_char[i])
      x <- max(as.numeric(unlist(strsplit(x,","))))
      final_dose[i] <- x
    } else {
      final_dose[i] <- dose_num[i]
    }
  }
  return(final_dose)
}