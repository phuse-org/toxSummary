

################################################################################
## function to get dose and Pharmacokinetics parameter for a specific study       
## 
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2022-12-20   Yousuf Ali,           Initial version
################################################################################

get_pk_param <- function(conn, studyid, pk_param="AUCLST", sex_include=NULL, visit_day=NULL){
    # function to get dose and Pharmacokinetics parameter for a specific study

  '%ni%' <- Negate('%in%')
  studyid <- as.character(studyid)
  pk_param <- toupper(as.character(pk_param))
  if(!is.null(visit_day)) {

  visit_day <- as.integer(visit_day)
  }
  studyid_list_all <- DBI::dbGetQuery(conn = conn,
  'SELECT DISTINCT STUDYID FROM TX')
  studyid_list_pp <- DBI::dbGetQuery(conn = conn,
  'SELECT DISTINCT STUDYID FROM PP')
  testcode_list_pp <- DBI::dbGetQuery(conn = conn,
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
  if(!is.null(sex_include)) {
  sex_include <- toupper(sex_include)
  }


  pp_domain <- DBI::dbGetQuery(conn = conn,
  'SELECT * FROM PP WHERE STUDYID==:x AND PPTESTCD IN ($CMAX, $AUC)',
  params = list(x = studyid, CMAX = "CMAX", AUC = pk_param))

  # get the dose information
  dose <- DBI::dbGetQuery(conn=conn,
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
  pp_domain$PPSTRESN <- as.numeric(pp_domain$PPSTRESN)
  if ("PPNOMDY" %in% names(pp_domain)) {
    pp_domain <-  pp_domain[, .(STUDYID, DOMAIN, USUBJID,
                                POOLID, PPTESTCD,  PPTEST,
                                PPORRES, PPORRESU, PPSTRESC,
                                PPSTRESU, PPSTRESN,VISITDY, PPNOMDY)]

  } else {
    pp_domain <-  pp_domain[, .(STUDYID, DOMAIN, USUBJID,
                                POOLID, PPTESTCD,  PPTEST,
                                PPORRES, PPORRESU, PPSTRESC,
                                PPSTRESU, PPSTRESN,VISITDY)]
  }
# if all USUBJID populated in PP domain
  if (nrow(pp_domain) == sum(pp_domain$USUBJID != "")) {
    df <- merge(dose_wide, pp_domain, by = c("STUDYID", "USUBJID"))
 
	# filter sex
	  if (!is.null(sex_include)) {
	  df <- df[SEX %in% sex_include, ]
  }

	  if (!is.null(visit_day)) {

		if (!all(is.na(pp_domain[["PPNOMDY"]]))) {
			df <- df[PPNOMDY %in% visit_day, ]

		} else {
			df <- df[VISITDY %in% visit_day, ]
		}
		 
	  
  }


    df <- df[, .(TRTDOS, TRTDOSU, PPSTRESN, PPSTRESU, PPTESTCD)]
    df <- df[, .(mean=mean(PPSTRESN)), by=.(TRTDOS, TRTDOSU,PPTESTCD,PPSTRESU)]
    df <- df[, .SD, .SDcols=c(1, 2, 3, 5, 4)]

# if all USUBJID empty in pp domain

  } else if (nrow(pp_domain) == sum(pp_domain$USUBJID == "")) {
    pooldef_list <- DBI::dbGetQuery(conn = conn,
	 'SELECT DISTINCT STUDYID FROM POOLDEF')

    if (studyid %ni% pooldef_list$STUDYID) {
      stop("USUBJID in PP domain empty and this study does not have POOLDEF domain to merge with PP domain")
    }
    pooldef <- DBI::dbGetQuery(conn = conn,
	 'SELECT * FROM POOLDEF WHERE STUDYID==:x',
	  params = list(x=studyid))
    pooldef <- data.table::as.data.table(pooldef)
    pp_domain <- pp_domain[, USUBJID := NULL]
    df <- data.table::merge.data.table(pp_domain, pooldef,
	 by=c("STUDYID", "POOLID"),
	 allow.cartesian=T)
    df <- data.table::merge.data.table(df,dose_wide,
	 by=c("STUDYID","USUBJID"))

	if (!is.null(sex_include)) {
	df <- df[SEX %in% sex_include, ]
  }

	  if (!is.null(visit_day)) {

		if (!all(is.na(pp_domain[["PPNOMDY"]]))) {
			df <- df[PPNOMDY %in% visit_day, ]

		} else {
			df <- df[VISITDY %in% visit_day, ]
		}
		 
	  
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

	if (!is.null(sex_include)) {
	df_all <- df_all[SEX %in% sex_include, ]
  }

	  if (!is.null(visit_day)) {

		if (!all(is.na(pp_domain[["PPNOMDY"]]))) {
			df <- df[PPNOMDY %in% visit_day, ]

		} else {
			df <- df[VISITDY %in% visit_day, ]
		}
		 
	  
  }

    # check duplicated value dataframe
    df <- df_all[, .(mean_cmax=mean(PPSTRESN)),
	 by=.(TRTDOS, TRTDOSU,PPTESTCD,PPSTRESU)]
    #names(df)[5] <- paste0(pk_param, "_Mean")
    df <- df[, .SD, .SDcols=c(1,2,3,5,4)]
  }
  df <- df[order(PPTESTCD,TRTDOS),]
  df <- na.omit(df)

  return(df)
}

get_only_dose <- function(conn, studyid) {
	studyid <- as.character(studyid)

	dose <- DBI::dbGetQuery(conn=conn,
  'SELECT TX.STUDYID, TX.TXPARMCD, TX.TXVAL, TX.SETCD,
  DM.USUBJID, DM.SEX FROM TX INNER JOIN DM 
  ON (TX.STUDYID=DM.STUDYID AND TX.SETCD=DM.SETCD)
  WHERE TX.STUDYID==:x AND (TX.TXPARMCD="TRTDOS" OR TX.TXPARMCD="TRTDOSU")',
  params = list(x = studyid))
  
  dose <- data.table::as.data.table(dose)
  dose_wide <- data.table::dcast.data.table(dose, ... ~ TXPARMCD,
                                            value.var = "TXVAL")
  dose_wide$TRTDOS <- clean_txval_dose(dose = dose_wide$TRTDOS)
  df <- dose_wide[!duplicated(TRTDOS), .(TRTDOS, TRTDOSU)]
 df1 <- df
 df2 <- df
 df1$PPTESTCD <- "AUC"
 df1$mean <- NA
 df1$PPSTRESU <- "h*ng/mL"
 df2$PPTESTCD <- "CMAX"
 df2$mean <- NA
 df2$PPSTRESU <- "ng/mL"
df_ls <- list(df1,df2)
final_df <- data.table::rbindlist(df_ls)
final_df <- final_df[TRTDOS != 0]
final_df <- na.omit(final_df, cols = c("TRTDOS"))
 final_df <- final_df[order(PPTESTCD,TRTDOS),]
final_df
}


clean_txval_dose <- function(dose) {
    # function to clean doses
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
