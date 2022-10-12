
# notes from 07/13/2020

# nonclinical 
#### group findings, rearranging like study option and put right side of Study # done
##### fix finding plot so that dose text readable when there are lot more findings --
##### (text size 4, when more than 6 findings, else textsize 6)

##### add autocompletion for adding findings # done
# make a list for possible findings and provide that list as choices in findings # yousuf
# warning message for save study while required field empty
# save automatically for study 
##### double save button not working properly for savestudy # fixed
# male/female (sex) severity filtered in plot

#clinical
# fix the issue that two start dose appeared 
# dosing units

#table 
# check filter option for numeric column (only slider option available)
# table 2 does not show table sometimes (only shows NOAEL and no absent severity)
# export any appication (whole dataset in rds)



# Notes from 6/29: ################################

# Data Selection:
#### - Change Enter Tox Program to Enter Application Number # done

# - Automatically open new application after entering it rather than having user select from list

# Clinical Data:
# - Set default to check Start Dose and MRHD
# - Fix that need to enter both a Start Dose and MRHD
#### pop up delete button to confirm delete # added by yousuf

#### - Add solid-lines above Start Dose/MRHD/Custom Dose ## Done

# - Wait for feedback on everything above Start Dose Information: in Clinical Data

# Nonclinical Data:
#### - Move study name below Species and Duration  ## Done
#### - Add a save button at bottom of Nonclincial Data 
#### - Add dashed-lines above Dose 2/3/etc., and above Findings 2/3/etc.  ## Done # dashed line above 1/2/3
#### - Move NOAEL checkbox below Cmax and AUC # done
#### - Add solid-lines above number of Dose levels and above number of findings # done
# - Add asterisk next to Dose 1/2/3/etc. ???
#### - Fix typo in "Partially Revesible" # done

# Main Panel:
# - Generate informative error message if safety margin calculation method of Cmax or
#   AUC is selected but no Cmax or AUC clinical (or nonclinical) data has been provided.

# - Wait for feedback on table names

# General Notes:
#### - Fix numericInputs to not take negative values for dose and Cmax and AUC # done, what should be the minimum number? 0?
# - Figure out how to handle data entry in the context of updates to the application'
# - Explore User-based updates

###################################################

# Project Improvement Ideas:
# - Add legend to figure that lists dose compared and PK/HED option
# - Allow user to create display names of findings with legend at bottom
# - Add option to display margin on top of figure
# - Make an optional figure legend (with checkbox)
# - Color "errorbar" to indicate severity (white for no toxicity at dose)
#   Color by the lowest dose on the ladder and switch color half-way between dose edges if space allows
#     on the UI bar side, change checkboxes to selectInputs to indicate dose severity
# - For table export, generate the three tables from the smart template in Word format
# - Add footnotes tied to findings (numbered) as well as a general footnote
# - Start with Smart Template as default table layout
# - Allow table to be flexibly modified
# - Brackets for findings
# - Text wrap finding names so that they don't overlap and use bullets to denote findings
# - Stagger doses (down -> up) so they don't overlap when close
# - use error bar to combine findings across doses

## added by Yousuf


# apply roundSigigs funciton to plotData_p$SM 
# remove findings string from hovertext in findings figure

### need to add or change in 3rd table of template
# correct the HED calculation
# add starting Dose and MHRD 
# add 

# 

"%ni%" <- Negate("%in%")

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


####
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