
################################################################################
## some helper functions
## 
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2022-12-20   Yousuf Ali,           Initial version
##              Kevin Snyder
################################################################################


# not in function
"%ni%" <- Negate("%in%")
### function for getting paths

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


# function for using whether there are any value that is not NULL
# fundctin will return sum of all clinical doses

clin_data <- function(Data_rds) {
  Data <- Data_rds[["Clinical Information"]]
  dose_list <- list(
    start_dose = Data[["Start Dose"]][["StartDose"]],
    mrhd = Data[["MRHD"]][["MRHD"]],
    custom = Data[["Custom Dose"]][["CustomDose"]],
    start_dose_kg = Data[["Start Dose"]][["StartDoseMgKg"]],
    mrhd_kg = Data[["MRHD"]][["MRHDMgKg"]],
    custom_kg = Data[["Custom Dose"]][["CustomDoseMgKg"]]
  )
  dose_value <- sum(unlist(dose_list))
  dose_value
}


#### dependency 
addUIDep <- function(x) {
    jqueryUIDep <- htmltools::htmlDependency("jqueryui", "1.10.4",
        c(href = "shared/jqueryui/1.10.4"),
        script = "jquery-ui.min.js",
        stylesheet = "jquery-ui.min.css"
    )
    htmltools::attachDependencies(x, c(htmltools::htmlDependencies(x), list(jqueryUIDep)))
}

## significant figure
sigfigs <- function(x) {
    orig_scipen <- getOption("scipen")
    options(scipen = 999)
    on.exit(options(scipen = orig_scipen))
    x <- as.character(x)
    x <- sub("\\.", "", x)
    x <- gsub("(^0+|0+$)", "", x)
    nchar(x)
}

roundSigfigs <- function(x, N = 2) {
    if (is.na(x)) {
        return(x)
    } else {
        roundNumber <- round(x, digits = 0)
        if (sigfigs(roundNumber) <= N) {
            roundNumber <- signif(x, digits = N)
        }
        return(roundNumber)
    }
}