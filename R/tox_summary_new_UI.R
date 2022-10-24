
#' @title run toxSummary app
#' @param database_path Mandatory/cr
#'    file path for database
#' @param studyid_file Mandatory/cr
#' 		file path for studyid
#' 
#' @param save_file_path optional
#' 		path where created application file will be saved.
#' 		If NULL, file will be saved in  working directory
#' 
#' @param where_to_run Mandatory/cr
#' 		where run will be running. Default is local
#' 
#' @return function run the app.
#' 
#' @export
#' 





#' @importFrom data.table .N
#' @importFrom data.table .SD
#' @importFrom data.table %like%
#' @importFrom data.table :=
#' @importFrom stats na.omit
#' @importFrom magrittr %>%
#' @importFrom utils tar
#' @importFrom patchwork plot_layout



toxSummary_app <- function(database_path, studyid_file,
 save_file_path = NULL, where_to_run= "local") {

 if (is.null(save_file_path)) {
     save_file_path <- getwd()
 } else {
     save_file_path <- save_file_path
 }
 paths <- get_paths(
     database_path = database_path,
     save_file_path = save_file_path, where_to_run = where_to_run
 )


if (paths$save_file_path == getwd()) {
    if (!dir.exists("Applications")) {
        dir.create("Applications")
    }

    paths$save_file_path <- fs::path(getwd(), "Applications")
	# message("file will be saved in ", paths$save_file_path)
}

 conn <- DBI::dbConnect(drv = RSQLite::SQLite(), paths$database_path)


## study id file
# study id file should a csv with IND number and studyid
 col_name <- c(
    "application_type",
    "IND_num",
    "studyID")


  ind_table <- data.table::fread(studyid_file,
    col.names = col_name)

ind_table <- ind_table[application_type == "IND",]
# ind_number_list <- ind_table[!duplicated(IND_num), c("IND_num")]
ind_number_list <- ind_table$IND_num
# ind_number_list <- ind_number_list[!duplicated(ind_number_list)]


#########
www_path <- system.file("", package = "toxSummary")
dt_extension <- paste0(www_path, "/www/DT_extension" )

### create blankData.rds and Applications_Demo.rds files

if(!file.exists("blankData.rds")) {
	saveRDS(blank_data, "blankData.rds")
}
if(!file.exists("Applications_Demo.rds")){
	saveRDS(applications_demo, "Applications_Demo.rds")
}



speciesConversion <- c(
    6.2, 1.8, 3.1, 3.1,
    12.3, 1.1, 4.6, 7.4
)
names(speciesConversion) <- c(
    "Rat", "Dog", "Monkey", "Rabbit",
    "Mouse", "Mini-pig", "Guinea pig", "Hamster"
)

## clinical  dosing  options
clinDosingOptions <- c("Start Dose", "MRHD", "Custom Dose")

choices_sex <- c("M", "F")
names(choices_sex) <- choices_sex
choices_sex <- sort(choices_sex)

# Server function started here (selectData) ----
server <- function(input, output, session) {

# Species Conversion ----


########

values <- shiny::reactiveValues()
values$Application <- NULL
values$SM <- NULL
values$selectData <- NULL
values$tmpData <- NULL
values$changeStudyFlag <- F
values$Findings <- ''

# user folder  ----
  user <- shiny::reactive({
      if (paths$where_to_run == "local") {
          username <- fs::path(paths$save_file_path, basename(fs::path_home()))
      } else if (paths$where_to_run == "rsconnect") {
          username <- session$user

          username <- tolower(username)
          username <- fs::path(paths$save_file_path, username)
          
      }
      username
  })

  # create folder and copy Aplication_Demo.rds file that folder
  shiny::observeEvent(user(), {
      dir_list <- list.dirs(paths$save_file_path, full.names = F, recursive = F)
      if (!basename(user()) %in% dir_list) {
          dir.create(user())
          file.copy("Applications_Demo.rds", user())
      }
  })

  ###
 
  
  ### Study Name ----
  output$studyName <- shiny::renderUI({
      shiny::req(input$selectData)
      if (input$selectData != "blankData.rds") {
          htmltools::HTML(paste(
              htmltools::p(htmltools::HTML(paste0(
                  '<h4>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<u>Selected Application:</u></h4>
                      <h4 style= "color:#337ab7"> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;',
                  (basename(unlist(strsplit(input$selectData, ".rds")))), "</h4>"
              )))
          ))
      }
  })

# getData ------

  getData <- shiny::reactive({
      input$refreshPlot
      shiny::req(input$selectData)
       input$selectStudy
      Data <- readRDS(input$selectData)
  })
  
  shiny::observe({
      shiny::req(input$selectData)
      if (input$selectData == "blankData.rds") {
          values$Application <- paste0(user(), "/", input$newApplication, ".rds")
      } else {
          values$Application <- input$selectData
      }
  })
  
  # 
  shiny::observeEvent(input$saveData, {
      Data <- getData()
      saveRDS(Data, values$Application)
      datasets <- c(
          "blankData.rds",
          grep(".rds", list.files(user(), full.names = T), value = T)
      )
      names(datasets) <- basename(unlist(strsplit(datasets, ".rds")))
      names(datasets)[which(datasets == "blankData.rds")] <- "New Application"
    #   shiny::selectInput("selectData", "Select Application:", datasets)
      shiny::updateSelectInput(session, "selectData",
          choices = datasets, selected = values$Application
      )
  })

  
# delete application ----

  shiny::observeEvent(input$deleteData, {
      shiny::showModal(shiny::modalDialog(
          title = "Delete Application?",
          footer = htmltools::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton("confirmDelete", "Delete")
          )
      ))
  })
  
  
  # Confirm delete application ----
  shiny::observeEvent(input$confirmDelete, {
      file.remove(values$Application)
      datasets <- c(
          "blankData.rds",
          grep(".rds", list.files(user(), full.names = T), value = T)
      )
      names(datasets) <- basename(unlist(strsplit(datasets, ".rds")))
      names(datasets)[which(datasets == "blankData.rds")] <- "New Application"
    #   shiny::selectInput("selectData", "Select Application:", datasets)
      shiny::updateSelectInput(session, "selectData",
          choices = datasets, selected = "blankData.rds"
      )

      shiny::removeModal()
  })
  
  # select study ----
  output$selectStudy <- shiny::renderUI({
      shiny::req(input$selectData)
      input$selectData
      shiny::isolate(Data <- getData())
      studyList <- names(Data[["Nonclinical Information"]])
      shiny::selectInput("selectStudy", "Select Study:", choices = studyList)
  })
  
############## Auto-Save Dose ######################
  
  # read data from disk into values$tmpData upon study selection
  shiny::observeEvent(input$selectStudy,ignoreNULL=T,{
    values$changeStudyFlag <- F
    Data <- getData()
    values$tmpData <- Data[['Nonclinical Information']][[input$selectStudy]]
    if (input$selectStudy=='New Study') {
      blankData <- readRDS('blankData.rds')
      values$tmpData <- blankData[['Nonclinical Information']][[input$selectStudy]]
      #values$tmpData <- Data[['Nonclinical Information']][['New Study']]
    }
  })
  
  # Flip changeStudyFlag after "New Study" has loaded
  shiny::observe({
    if (is.null(input$dose1)) {
      values$changeStudyFlag <- T
    } else if (is.na(input$dose1)) {
      values$changeStudyFlag <- T
    }
  })
  
  # Flip changeStudyFlag after study has loaded and update tmpData to match UI
  shiny::observe({
    shiny::req(input$nDoses)
    shiny::req(input$dose1)
    shiny::req(input$nFindings)
    shiny::req(input[[paste0('Severity',input$nFindings,'_',input$nDoses)]])
    if (!is.na(input$dose1)) {
      if ((values$tmpData$Doses$Dose1$Dose == input$dose1)&(values$tmpData$nDoses == input$nDoses)&
          (values$tmpData$Findings$Finding1$Finding == input$Finding1)&(values$tmpData$nFindings == input$nFindings)&
          (values$tmpData$Findings$Finding1$Severity[[paste0('Dose',values$tmpData$nDoses)]] == input[[paste0('Severity',input$nFindings,'_',input$nDoses)]])) {
        values$changeStudyFlag <- T
      }
    }
    if (values$changeStudyFlag==T) {
      for (i in seq(input$nDoses)) {
        if (!is.null(input[[paste0('dose',i)]])) {
          newList <- list(
            Dose = input[[paste0('dose',i)]],
            NOAEL = input[[paste0('NOAEL',i)]],
            Cmax = input[[paste0('Cmax',i)]],
            AUC = input[[paste0('AUC',i)]]
          )
          values$tmpData[['Doses']][[paste0('Dose',i)]] <- newList
        }
      }
    }
  })
  
  
 # Add findings to the list
          
  shiny::observeEvent(input$selectData, ignoreNULL = T, {
      Data <- getData()
      for (Study in names(Data[["Nonclinical Information"]])) {
          if (Study != "New Study") {
              studyData <- Data[["Nonclinical Information"]][[Study]]

              for (i in seq(studyData$nFindings)) {
                  Finding <- studyData[["Findings"]][[paste0("Finding", i)]][["Finding"]]
                  if (Finding %ni% values$Findings) {
                      values$Findings <- c(values$Findings, Finding)
                  }
              }
          }
      }
  })

  ########### Auto-save findings ###############
  
  shiny::observe({
    shiny::req(input$nFindings)
    shiny::req(input$Finding1)
    if (values$changeStudyFlag==T) {
      for (i in seq(input$nFindings)) {
        if (!is.null(input[[paste0('Finding',i)]])) {
          Finding_list= input[[paste0('Finding',i)]]
          if (Finding_list %ni% values$Findings) {
            values$Findings <- c(values$Findings, Finding_list)
          }
          newList <- list(
            Finding= input[[paste0('Finding',i)]],
            Reversibility = input[[paste0('Reversibility',i)]])
          sev_list <- list()
          for (j in seq(input$nDoses)) {
            finding_seq <- input[[paste0('Severity', i, '_', j)]]
            if (!is.null(finding_seq)) {
            names(finding_seq) <- paste0("Dose", j)
            }
            sev_list <- c(sev_list, finding_seq)
          }
          newList <- c(newList, list(Severity= sev_list))
          values$tmpData[['Findings']][[paste0('Finding',i)]] <- newList
         }
      }
    }
  })
  
  
  # Clinical information -----
  
  shiny::observeEvent(input$selectData,ignoreNULL = T,{
	 Data <- getData()
    #update units for Cmax/AUC
    shiny::updateTextInput(session, "cmax_unit", value=Data[["CmaxUnit"]])
    shiny::updateTextInput(session, "auc_unit", value=Data[["AUCUnit"]])
   
  })
  
# Nonclinical data update ------
  
  shiny::observeEvent(input$selectStudy, ignoreNULL = T, {
      Data <- getData()
      studyData <- Data[["Nonclinical Information"]][[input$selectStudy]]
	#  print(studyData$studyid_name)
	#   names(studyData$studyid_name) <- paste0("\U25FC ", studyData$studyid_name)
	#   print(studyData$studyid_name)
	  shiny::updateSelectizeInput(session, "ind_id", selected = studyData$IND_number)
	#   updateSelectizeInput(session, "study_id", selected = studyData$studyid_name)
      shiny::updateSelectInput(session, "Species", selected = studyData$Species)
      shiny::updateCheckboxGroupInput(session, "which_sex", selected = studyData$Sex_include)
      shiny::updateTextInput(session, "Duration", value = studyData$Duration)
      shiny::updateNumericInput(session, "nDoses", value = studyData$nDoses)
      shiny::updateNumericInput(session, "nFindings", value = studyData$nFindings)
      shiny::updateCheckboxInput(session, "notes", value = studyData$check_note)
  })
  
  
# first save study button ----
  
  shiny::observeEvent(eventExpr = input$saveStudy, {
    doseList <- as.list(seq(input$nDoses))
    names(doseList) <- paste0('Dose',seq(input$nDoses))
    for (i in seq(input$nDoses)) {
      doseList[[i]] <- list(Dose=input[[paste0('dose',i)]],
                            NOAEL = input[[paste0('NOAEL',i)]],
                            Cmax = input[[paste0('Cmax',i)]],
                            AUC = input[[paste0('AUC',i)]]
      )
    }
    
    findingList <- as.list(seq(input$nFindings))
    names(findingList) <- paste0('Finding',seq(input$nFindings))
    if (input$nFindings > 0) {
      for (i in seq(input$nFindings)) {
        severity <- list()
        for (j in seq(input$nDoses)) {
        severity[[paste0("Dose", j)]] <- input[[paste0("Severity", i, "_", j)]]
        }
        if ((is.null(input[[paste0('Finding',i)]])) | (input[[paste0('Finding',i)]]=='')) {
          finding_null <- "No Finding"
        } else {
          finding_null <- input[[paste0('Finding',i)]]
        }
        findingList[[i]] <- list(Finding=finding_null,
                                 Reversibility = input[[paste0('Reversibility',i)]],
                                 # FindingDoses = input[[paste0('FindingDoses',i)]],
                                 Severity = severity
        )
      }
    } else {
      findingList[[1]] <- NULL
    }
    
    Data <- getData()
    studyName <- paste(input$Species,input$Duration,sep=': ')
    Data[['Nonclinical Information']][[studyName]] <- list(
	  IND_number = input$ind_id,
	  studyid_name  = input$study_id,
	#   studyid_name_att  = paste0("", input$study_id),
      Species = input$Species,
	  Sex_include = input$which_sex,
      Duration = input$Duration,
      Notes = input$note_text,
      check_note = input$notes,
      nDoses = input$nDoses,
      Doses = doseList,
      nFindings = input$nFindings,
      Findings = findingList
      
    )
    saveRDS(Data,values$Application)
    shiny::showNotification("Saved", duration = 3)
    studyList <- names(Data[['Nonclinical Information']])
    shiny::updateSelectInput(session,'selectStudy',choices=studyList,selected=studyName)
    input$refreshPlot
  })
  
  # second save study button ----

    shiny::observeEvent(input$saveStudy_02, {
		session$sendCustomMessage("mymessage", "saveStudy")
    #   shinyjs::click("saveStudy")
    })
  

## save clinical information ---- 
  
  shiny::observeEvent(input$saveClinicalInfo, {
    Data <- getData()
    clinData <- Data[['Clinical Information']]
    if (input$MgKg==F) {
      clinData[['HumanWeight']] <- input$HumanWeight
    } else {
      clinData[['HumanWeight']] <- NULL
    }
    clinData[['MgKg']] <- input$MgKg
    if (length(input$clinDosing)>0) {
      for (clinDose in input$clinDosing) {
        clinDoseName <- gsub(' ','',clinDose)
        if (input$MgKg==F) {
          clinData[[clinDose]][[clinDoseName]] <- input[[clinDoseName]]
        } else {
          clinData[[clinDose]][[paste0(clinDoseName,'MgKg')]] <- input[[paste0(clinDoseName,'MgKg')]]
        }
        clinData[[clinDose]][[paste0(clinDoseName,'Cmax')]] <- input[[paste0(clinDoseName,'Cmax')]]
        clinData[[clinDose]][[paste0(clinDoseName,'AUC')]] <- input[[paste0(clinDoseName,'AUC')]]
      }
    }
    Data[['Clinical Information']] <- clinData
    saveRDS(Data,values$Application)
    shiny::showNotification("saved", duration = 3)
	session$sendCustomMessage("save_clin_info", "refreshPlot")
	# click('refreshPlot')
  })
  
# click refresh button after save clinical information
#   observeEvent(input$saveClinicalInfo, {
#     click('refreshPlot')
#   })
  
  ## delete study ---- 
  shiny::observeEvent(input$deleteStudy, {
    shiny::showModal(shiny::modalDialog(
      title="Delete Study?",
      footer = htmltools::tagList(shiny::modalButton("Cancel"),
                       shiny::actionButton("confirmRemove", "Delete")
      )
    ))
  })
 
  # confirm delete study 
  
  shiny::observeEvent(input$confirmRemove, {
    Data <- getData()
    studyIndex <- which(names(Data[['Nonclinical Information']])==input$selectStudy)
    restIndex <- seq(length(names(Data[['Nonclinical Information']])))[-studyIndex]
    restNames <- names(Data[['Nonclinical Information']])[restIndex]
    Data[['Nonclinical Information']] <- Data[['Nonclinical Information']][restNames]
    saveRDS(Data,values$Application)
    studyList <- names(Data[['Nonclinical Information']])
    shiny::updateSelectInput(session,'selectStudy',choices=studyList,selected='New Study')
	session$sendCustomMessage("confirm_study_remove", "refreshPlot")
    shiny::removeModal()
	
  })
  
# title 
  output$studyTitle <- shiny::renderText({
    text  <- paste(input$Species,input$Duration,sep=': ')
	text <- strwrap(text, 30)
	text
  })
  
  # display Studies ----
  
  output$displayStudies <- shiny::renderUI({
    # shiny::req(input$clinDosing)
    input$selectData
    input$selectStudy
    shiny::isolate(Data <- getData())
    studyList <- names(Data[['Nonclinical Information']])
    studyList <- studyList[-which(studyList=='New Study')]
    studyList <- stringr::str_sort(studyList, numeric = T)
    addUIDep(shiny::selectizeInput("displayStudies",
        label = "Select and Order Studies to Display:", choices = studyList,
        selected = studyList,
        multiple = TRUE,
        width = "100%", options = list(plugins = list("drag_drop", "remove_button"))
    ))
  })
  
  ## display findings ----
  
  output$displayFindings <- shiny::renderUI({
    #   shiny::req(input$clinDosing)
      input$selectData
      input$selectStudy
      data <- getPlotData()
      find_fact <- as.factor(data$Findings)
      findings <- unique(find_fact)
      findings <- stringr::str_sort(findings, numeric = T)
      addUIDep(shiny::selectizeInput("displayFindings",
          label = "Select and Order Findings to Display:",
          choice = findings, selected = findings,
          multiple = TRUE, width = "100%",
          options = list(plugins = list("drag_drop", "remove_button"))
      ))
  })
  
  ## get dose and pk values
  get_dose_pk_for_study <- shiny::reactive({
      if (!is.null(input$study_id) & !is.null(input$auc_db)) {
          df <- get_pk_param(
              conn = conn, studyid_selected(), pk_param = input$auc_db,
              sex_include = input$which_sex, visit_day = input$pp_visitday
          )
          df
      }
  })
  
 
#   shiny::observe({
#       req(input$selectStudy)
#       if (input$get_from_database) {
#           df <- get_dose_pk_for_study()
#           n_dose <- length(unique(df[, TRTDOS]))

#           updateNumericInput(session, "nDoses", value = n_dose)
#       }
#   })

    shiny::observeEvent(input$get_from_db,{
      shiny::req(input$selectStudy)
	  shiny::req(input$ind_id)
	  shiny::req(input$study_id)
      
          df <- get_dose_pk_for_study()
          n_dose <- length(unique(df[, TRTDOS]))

          shiny::updateNumericInput(session, "nDoses", value = n_dose)
      
  })

#### toggle button ----
    toggle <- shiny::reactiveValues(db = "no run")

 shiny::observeEvent(input$get_from_db, {
     toggle$db <- "run"
     shiny::updateActionButton(
         session = session,
         "get_from_db", "Clicked: Data updated  from database",
         icon = shiny::icon("mouse-pointer")
     )
 })

# update toggle to "no run" when ind, studyid changes 

 shiny::observe({
     input$ind_id
     input$study_id
     input$selectStudy
     shiny::req(input$ind_id)
     shiny::req(input$study_id)

     toggle$db <- "no run"

     shiny::updateActionButton(
         session = session,
         "get_from_db",
         "Click me to populate dose and pk from  database",
         icon = shiny::icon("mouse-pointer")
     )
 })


  
  ## output$Doses -----
  
  output$Doses <- shiny::renderUI({
    shiny::req(input$selectStudy)
    if (toggle$db == "run") {
		shiny::req(input$study_id)
      
      df <- get_dose_pk_for_study()
      n_dose <-   length(unique(df[,TRTDOS]))
      
      
      cmax <- df[PPTESTCD=="CMAX"]
      auc <- df[PPTESTCD!="CMAX"]

      lapply(1:(4*input$nDoses), function(i) {
        I <- ceiling(i/4)
        #doseName <- names(studyData$Doses)[I]
        if (i %% 4 == 1) {
          htmltools::div(htmltools::hr(style = "border-top: 1px dashed skyblue"),
              shiny::numericInput(paste0('dose',I),paste0('*Dose ',I,  " ",cmax[I, .(TRTDOSU)], ":"),
			   min=0, value = cmax[I, .(TRTDOS)]))
        } else if (i %% 4 == 2) {
          htmltools::div(style="display: inline-block;vertical-align:top; width: 115px;",
              shiny::numericInput(paste0('Cmax',I),paste0('Cmax ',I, " ", cmax[I, .(PPSTRESU)], ":"),
			   min=0, value=cmax[I, .(mean)]))
        }
        else if (i %% 4 == 3) {
          htmltools::div(style="display: inline-block;vertical-align:top; width: 115px;",
              shiny::numericInput(paste0("AUC",I),paste0(input$auc_db, " ",I, " ",auc[I, PPSTRESU], ":"),
			   min=0, value=auc[I, .(mean)]))
          
        }
        else {
          htmltools::div(shiny::checkboxInput(paste0('NOAEL',I),'NOAEL?',value= F))
        }
      })
      
    } else {
    
    cmax_unit <- paste0(" Cmax (", input$cmax_unit, ")")
    auc_unit <- paste0(" AUC (", input$auc_unit, ")")
    studyData <- values$tmpData
    lapply(1:(4*input$nDoses), function(i) {
      I <- ceiling(i/4)
      doseName <- names(studyData$Doses)[I]
      if (i %% 4 == 1) {
        htmltools::div(htmltools::hr(style = "border-top: 1px dashed skyblue"),
            shiny::numericInput(paste0('dose',I),paste0('*Dose ',I,' (mg/kg/day):'),
			 min=0, value =studyData$Doses[[doseName]][['Dose']]))
      } else if (i %% 4 == 2) {
        htmltools::div(style="display: inline-block;vertical-align:top; width: 115px;",
            shiny::numericInput(paste0('Cmax',I),paste0('Dose ',I, cmax_unit), 
			min=0, value=studyData$Doses[[doseName]][['Cmax']]))
      }
      else if (i %% 4 == 3) {
        htmltools::div(style="display: inline-block;vertical-align:top; width: 115px;",
            shiny::numericInput(paste0('AUC',I),paste0('Dose ',I, auc_unit),
			 min=0, value=studyData$Doses[[doseName]][['AUC']]))
        
      } else {
        htmltools::div(shiny::checkboxInput(paste0('NOAEL',I),'NOAEL?',value=studyData$Doses[[doseName]][['NOAEL']]))
      }
    })}
  })
  
  #  Findings -----
  
  output$Findings <- shiny::renderUI({
    shiny::req(input$selectStudy)
      studyData <- values$tmpData
      if (input$nFindings>0) {
        numerator <- 2 + input$nDoses
        lapply(1:(numerator*input$nFindings), function(i) {
          I <- ceiling(i/numerator)
          if (i %% numerator == 1) {
            findings <- stringr::str_sort(unique(values$Findings))
            htmltools::div(
              htmltools::hr(style = "border-top: 1px dashed skyblue"),
              shiny::selectizeInput(paste0('Finding',I),paste0('*Finding ',I,':'), choices= findings,
                             selected = studyData$Findings[[paste0('Finding',I)]]$Finding,
                             options = list(create = TRUE)))
          } else if (i %% numerator == 2) {
            shiny::radioButtons(paste0('Reversibility',I),'Reversibility:',
                         choiceNames=c('Reversible [Rev]','Not Reversible [NR]',
                                       'Partially Reversible [PR]','Not Assessed'),
                         choiceValues=c('[Rev]','[NR]','[PR]',''),
                         selected=studyData$Findings[[paste0('Finding',I)]]$Reversibility)
          } else {
            lapply(1:input$nDoses, function(j) {
              if ((i %% numerator == 2+j)|((i %% numerator == 0)&(j==input$nDoses))) {
                shiny::selectInput(inputId = paste0('Severity',I,'_',j),
                            label = paste0('Select Severity at Dose ',j,' (',input[[paste0('dose',j)]],' mg/kg/day)'),
                            choices = c('Absent','Present','Minimal','Mild','Moderate','Marked','Severe'),
                            selected=studyData$Findings[[paste0('Finding',I)]]$Severity[[paste0('Dose',j)]])
              }
            })
          }
        })
      }
  })


  
  ### add note for study ----
  
  output$study_note <- shiny::renderUI({
    shiny::req(input$selectStudy)
    Data <- getData()
    studyData <- Data[['Nonclinical Information']][[input$selectStudy]]
    
    if (input$selectStudy=='New Study') {
      if (input$notes ==T) {
        shiny::textAreaInput("note_text", "Notes:", 
		placeholder = "Enter Note here for this Study", height = "100px")
      }
    } else{
        if (input$notes==T) {
          shiny::textAreaInput("note_text", "Notes:",
		   value = studyData$Notes, height = "100px")
        }
      }
  })
  
  
  # Create PlotData (changed) -----
  
 getPlotData <- shiny::reactive({
  Data <- getData()
  plotData <- data.frame(matrix(ncol = 17 ))
  column_names <- c("Study", "Dose", 
                    "NOAEL", "Cmax", "AUC", "Findings",
                    "Reversibility", "Severity",  "Value_order", 
                    "SM", "HED_value", "SM_start_dose", "SM_MRHD",
					 "noael_value", "Severity_max", "Severity_num", "Study_note")
  colnames(plotData) <- column_names
  
  count <- 1
  
  for (Study in names(Data[["Nonclinical Information"]])) {
    if (Study != "New Study") {
      studyData <- Data[["Nonclinical Information"]][[Study]]
      
      for (i in seq(studyData$nFindings)){
        for (j in seq(studyData$nDoses)){
          
          plotData[count, "Study"] <- Study
          plotData[count, "Dose"] <- studyData[["Doses"]][[paste0("Dose", j)]][["Dose"]]
          plotData[count, "NOAEL"] <- studyData[["Doses"]][[paste0("Dose",j)]][["NOAEL"]]
          plotData[count, "Cmax"] <- studyData[["Doses"]][[paste0("Dose", j)]][["Cmax"]]
          plotData[count, "AUC"] <- studyData[["Doses"]][[paste0("Dose", j)]][["AUC"]]
          plotData[count, "Findings"] <- studyData[["Findings"]][[paste0("Finding", i)]][["Finding"]]
          plotData[count, "Reversibility"] <- studyData[["Findings"]][[paste0("Finding", i)]][["Reversibility"]]
          plotData[count, "Severity"] <- studyData[["Findings"]][[paste0("Finding", i)]][["Severity"]][[paste0("Dose", j)]]
          plotData[count, "Value_order"] <- j
          plotData[count, "SM"] <- NA
          plotData[count, "HED_value"] <- NA
          plotData[count, "SM_start_dose"] <- NA
          plotData[count, "SM_MRHD"] <- NA
          plotData[count, "noael_value"] <- NA
          plotData[count, "Severity_max"] <- NA
          plotData[count, "Severity_num"] <- NA
          
          if (!is.null(studyData[["Notes"]])) {
            plotData[count, "Study_note"] <- studyData[["Notes"]]
            } else {plotData[count, "Study_note"] <- NA}
          
          count <- count+1
        }
      }
    }
  }
  
  plotData$Rev <- gsub("\\[|\\]", "", plotData$Reversibility)
  plotData$Dose <- as.numeric(plotData$Dose)
  plotData$Value <- 1
  plotData$Rev[plotData$Rev == ""] <- "Not Assessed"
  plotData$Rev[plotData$Rev == "Rev"] <- "Reversible"
  plotData$Rev[plotData$Rev == "NR"] <- "Not Reversible"
  plotData$Rev[plotData$Rev == "PR"] <- "Partially Reversible"
  plotData <- plotData[which(plotData$Study %in% input$displayStudies),]
  plotData$Severity <- factor(plotData$Severity, 
                              levels= c('Absent','Present','Minimal', 'Mild',
                                        'Moderate', 'Marked', 'Severe'), ordered = TRUE)
  
  plotData$Severity_num <- as.numeric(plotData$Severity)
  return(plotData)
})
 
## human dose ----
  
  output$humanDosing <- shiny::renderUI({
    # shiny::req(input$clinDosing)
    Data <- getData()
	  clinData <- Data[['Clinical Information']]
    clinDosing <- NULL
    for (dose in clinDosingOptions) {
      clin_dose <- clinData[[dose]][[gsub(' ','',dose)]]
      clin_dose_mgkg <- clinData[[dose]][[paste0(gsub(' ','',dose), 'MgKg')]]
      if ((!is.null(clin_dose)) | (!is.null(clin_dose_mgkg))) {
        clinDosing <- c(clinDosing,dose)
      }
    }
    clinDosingNames <- clinDosing
    names(clinDosingNames) <- clinDosingNames
    if (length(clinDosingNames)>0) {
      for (clinDose in clinDosing) {
        if (Data[['Clinical Information']][['MgKg']]==F) {
          names(clinDosingNames)[which(clinDosingNames==clinDose)] <- paste0(clinDose,
                                                                             ': (', Data[['Clinical Information']][[clinDose]][[paste0(unlist(strsplit(clinDose,' ')),
                                                                                                                                      collapse='')]],' mg)')
        } else {
          names(clinDosingNames)[which(clinDosingNames==clinDose)] <- paste0(clinDose,': (', Data[['Clinical Information']][[clinDose]][[paste0(gsub(' ', '', clinDose), 'MgKg')]],' mg/kg)')
        }
      }
    }
    shiny::selectInput('humanDosing','Select Human Dose:',choices=clinDosingNames)
  })
  
  ##  filter NOAEL data preparation ----
  
  filter_NOAEL <- shiny::reactive({
    df_plot <- getPlotData()
    count <- 0
    for (i in unique(df_plot$Study)){
      ind <- which(df_plot$Study == i)
      study <- df_plot[ind,]
      max_severe <- max(study$Severity)
      row_num <- nrow(study)
      
      for (j in seq(nrow(study))) {
        if (any(study$NOAEL == TRUE)) {
          dose <- study$Dose[which(study$NOAEL == TRUE)]
          dose <- unique(dose)
          k <- count+j
          df_plot[k, "noael_value"] <- as.numeric(dose)
          df_plot[k, "Severity_max"] <- max_severe
          
        } else {
          dose <- min(study$Dose)
          dose <- as.numeric(dose) - 1
          k <- count + j
          df_plot[k, "noael_value"] <- as.numeric(dose)
          df_plot[k, "Severity_max"] <- max_severe
        }
      }
      count <- count +row_num
    }
    df_plot
  })
  
  
  
# ## calculate safety margin (SM) ------
#
  calculateSM <- shiny::reactive({
    Data <- getData()
    plotData <- filter_NOAEL()
    if (nrow(plotData)>0) {
      for (i in seq(nrow(plotData))) {
        if (input$SMbasis=='HED') {
          Dose <- as.numeric(plotData[i,'Dose'])
        } else if (input$SMbasis=='Cmax') {
          Dose <- as.numeric(plotData[i,'Cmax'])
        } else if (input$SMbasis=='AUC') {
          Dose <- as.numeric(plotData[i,'AUC'])
        }
        Species <- unlist(strsplit(plotData[i,'Study'],':'))[1]
        humanDoseName <- gsub(' ','',input$humanDosing)
        if (input$SMbasis=='HED') {
          
          HED <- Dose/speciesConversion[[Species]]

          if (Data[["Clinical Information"]][["MgKg"]]=="FALSE") {
            humanDose <- Data[['Clinical Information']][[input$humanDosing]][[humanDoseName]]
            HED <- HED*Data[['Clinical Information']][['HumanWeight']]
                     
            if (!is.null(Data[["Clinical Information"]][["Start Dose"]][["StartDose"]])) {
              SM_start <- HED/(Data[["Clinical Information"]][["Start Dose"]][["StartDose"]])
            } else {SM_start <- NA}
            
            if (!is.null(Data[["Clinical Information"]][["MRHD"]][["MRHD"]])) {
              SM_MRHD <- HED/(Data[["Clinical Information"]][["MRHD"]][["MRHD"]])
              
            } else {SM_MRHD <- NA}
            
          }  else {
            
            humanDose <- Data[['Clinical Information']][[input$humanDosing]][[paste0(humanDoseName, "MgKg")]]

             if (!is.null(Data[["Clinical Information"]][["Start Dose"]][["StartDoseMgKg"]])){
               SM_start <- HED/(Data[["Clinical Information"]][["Start Dose"]][["StartDoseMgKg"]])
             } else {SM_start <- NA}
            
             if (!is.null(Data[["Clinical Information"]][["MRHD"]][["MRHDMgKg"]])) {
               SM_MRHD <- HED/(Data[["Clinical Information"]][["MRHD"]][["MRHDMgKg"]])
             } else {SM_MRHD <- NA}
          }

        } else if (input$SMbasis=='Cmax') {
          
          if (!is.null(Data[['Clinical Information']][[input$humanDosing]][[paste0(humanDoseName,input$SMbasis)]])) {
            humanDose <- Data[['Clinical Information']][[input$humanDosing]][[paste0(humanDoseName,input$SMbasis)]]
          } else {humanDose <- NA}
          
          HED <- Dose
          
          if (!is.null(Data[["Clinical Information"]][["Start Dose"]][["StartDoseCmax"]])) {
            SM_start <- HED/(Data[["Clinical Information"]][["Start Dose"]][["StartDoseCmax"]])
          } else {SM_start <- NA}
          
          if (!is.null(Data[["Clinical Information"]][["MRHD"]][["MRHDCmax"]])) {
            SM_MRHD <- HED/(Data[["Clinical Information"]][["MRHD"]][["MRHDCmax"]])
          } else (SM_MRHD <- NA)
          
        } else {
          if (!is.null(Data[['Clinical Information']][[input$humanDosing]][[paste0(humanDoseName,input$SMbasis)]])) {
            humanDose <- Data[['Clinical Information']][[input$humanDosing]][[paste0(humanDoseName,input$SMbasis)]]
          } else {humanDose <- NA}
          
          HED <- Dose
          if (!is.null(Data[["Clinical Information"]][["Start Dose"]][["StartDoseAUC"]])) {
            SM_start <- HED/(Data[["Clinical Information"]][["Start Dose"]][["StartDoseAUC"]])
          } else (SM_start <- NA)
          
          if (!is.null(Data[["Clinical Information"]][["MRHD"]][["MRHDAUC"]])) {
            SM_MRHD <- HED/(Data[["Clinical Information"]][["MRHD"]][["MRHDAUC"]])
          } else {SM_MRHD <- NA}
        }
        plotData[i, "HED_value"]<- round(HED, digits = 2) ##for table 03
        plotData[i, "SM"] <- round(HED/humanDose, 2)
        plotData[i, "SM_start_dose"] <- round(SM_start, digits = 2)
        plotData[i, "SM_MRHD"] <- round(SM_MRHD, digits = 2)
      }
    }
    return(plotData)
  })
  
# table 01 ----
  
  dt_01 <- shiny::reactive({
    plotData_tab <- calculateSM()
    plotData_tab <- plotData_tab %>% 
      dplyr::mutate(Findings = as.factor(Findings),
             Rev = as.factor(Rev),
             Study = as.factor(Study),
             Dose = as.numeric(Dose),
             SM = as.numeric(SM),
             Severity = as.factor(Severity))
    
    plotData_tab <- plotData_tab %>% 
      dplyr::select( Findings,Rev, Study, Dose, SM, Severity) %>%
      dplyr::filter(Severity != "Absent") %>% 
      dplyr::select(-Severity) %>% 
      dplyr::rename(Reversibility = Rev,
             "Clinical Exposure Margin" = SM,
             "Dose (mg/kg/day)" = Dose)
    
    plotData_tab$Findings <- factor(plotData_tab$Findings,levels= input$displayFindings)
    plotData_tab <- plotData_tab %>%
      dplyr::arrange(Findings)
    plotData_tab
  })
  

  # table 01 UI side
  output$table_01 <- DT::renderDT({
    data <- getData()
    clin_dose <- clin_data(data)
    if (clin_dose>0) {
    plotData_tab <- dt_01()
    plotData_tab <- DT::datatable(plotData_tab, rownames = FALSE,
                              class = "cell-border stripe",
                              filter = list(position = 'top'),
                              extensions = list("Buttons" = NULL,
                                                "ColReorder" = NULL),
                              caption = htmltools::tags$caption(
                                style = "caption-side: top; text-align: center; font-size: 20px; color: black",
                                "Table :", htmltools::strong("Nonclinical Findings of Potential Clinical Relevance")
                              ),
                              options = list(
                                dom = "lfrtipB",
                                buttons = c("csv", "excel", "copy", "print"),
                                colReorder = TRUE,
                                scrollY = TRUE,
                                pageLength = 25,
                                columnDefs = list(list(className = "dt-center", targets = "_all")),
                                initComplete = DT::JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                  "}"),
                                rowsGroup = list(0,1,2))) %>%
      DT::formatStyle(columns = colnames(plotData_tab), `font-size` = "18px")
    
    path <- dt_extension # folder containing dataTables.rowsGroup.js
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0",
      path, script = "dataTables.rowsGroup.js")
    plotData_tab$dependencies <- c(plotData_tab$dependencies, list(dep))
    plotData_tab
  }})
  
  # DT table to flex table
  filtered_tab_01 <- shiny::reactive({
    shiny::req(input$table_01_rows_all)
    data <- dt_01()
    data[input$table_01_rows_all, ]
  })


# Flextable for docx file
  dt_to_flex_01 <- shiny::reactive({
    plotData_tab <- filtered_tab_01()
    plotData_tab <- plotData_tab %>%
      dplyr::arrange(Findings, Reversibility, Study) %>%
      flextable::flextable() %>%
      flextable::merge_v(j = ~ Findings + Reversibility + Study) %>%
      flextable::autofit() %>%
      flextable::add_header_row(values = c("Nonclinical Findings of Potential Clinical Relevance"), colwidths = c(5)) %>%
      flextable::theme_box()
    plotData_tab
  })

# download table 01 
  output$down_01_doc <- shiny::downloadHandler(
    filename = function() {
      Sys.sleep(2)
      paste0("clinical_relevance", ".docx")
    },
    content = function(file) {
      flextable::save_as_docx(dt_to_flex_01(), path = paste0(user(), "/clinical_relevance.docx"))
      file.copy(paste0(user(),"/clinical_relevance.docx"), file)
    }
  )

  #### table 02 ----
  
  dt_02 <- shiny::reactive({
    plotData_tab <- calculateSM()
    plotData_tab <- plotData_tab %>% 
      dplyr::select(Study, Dose, NOAEL, Cmax, AUC, SM) %>% 
             dplyr::filter(NOAEL == TRUE) %>% 
             dplyr::select(-NOAEL) %>%
             dplyr::arrange(Study, Dose)
    plotdata_finding <- calculateSM()
    greater_than_noeal <- plotdata_finding[which(plotdata_finding$Dose>plotdata_finding$noael_value),]
    greater_than_noeal <- greater_than_noeal %>% 
      dplyr::select(Study, Findings) %>% 
      dplyr::distinct() 
    cmax_unit <- paste0("Cmax (", input$cmax_unit, ")")
    auc_unit <- paste0("AUC (", input$auc_unit, ")")
    plotData_tab <- dplyr::full_join(plotData_tab, greater_than_noeal, by="Study") %>% 
      dplyr::arrange(Study,Dose,Cmax,AUC,SM,Findings) %>% 
      dplyr::rename(
        "NOAEL (mg/kg/day)" = Dose,
        "Safety Margin" = SM,
        "Findings at Greater than NOAEL for the Study" = Findings
      ) %>% 
      dplyr::mutate(Study = as.factor(Study))
    
    names(plotData_tab)[names(plotData_tab)=="Cmax"] <- cmax_unit
    names(plotData_tab)[names(plotData_tab)=="AUC"] <- auc_unit
    plotData_tab$Study <- factor(plotData_tab$Study,levels= input$displayStudies)
    plotData_tab <- plotData_tab %>%
      dplyr::arrange(Study)
    plotData_tab
  })
  
# make column name same as flextable (add unit in DT table)
  output$table_02 <- DT::renderDT({
    data <- getData()
    clin_dose <- clin_data(data)
    if (clin_dose>0) {
    plotData_tab <- dt_02()
    plotData_tab <- DT::datatable(plotData_tab, rownames = FALSE, class = "cell-border stripe",
                              filter = list(position = 'top'),
                              extensions = list("Buttons" = NULL),
                              caption = htmltools::tags$caption(
                                style = "caption-side: top; text-align: center; font-size: 20px; color: black",
                                "Table :", htmltools::strong("Key Study Findings")
                              ),
                              options = list(
                                scrollY = TRUE,
                                pageLength = 100,
                                dom = "lfrtipB",
                                buttons = c("csv", "excel", "copy", "print"),
                                columnDefs = list(list(className = "dt-center", targets = "_all")),
                                initComplete = DT::JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                  "}"),
                                rowsGroup = list(0,1,2,3,4,5))) %>%
      DT::formatStyle(columns = colnames(plotData_tab), `font-size` = "18px")
    
    path <- dt_extension # folder containing dataTables.rowsGroup.js
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    plotData_tab$dependencies <- c(plotData_tab$dependencies, list(dep))
    plotData_tab
  }})
  
  
  # get data from DT for flextable
  filtered_tab_02 <- shiny::reactive({
    shiny::req(input$table_02_rows_all)
    data <- dt_02()
    data[input$table_02_rows_all, ]
  })
  
  # flextable 02
  dt_to_flex_02 <- shiny::reactive({
    cmax_unit <- paste0("Cmax (", input$cmax_unit, ")")
    auc_unit <- paste0("AUC (", input$auc_unit, ")")
    plotData_tab <- filtered_tab_02()
    plotData_tab <- plotData_tab %>% 
      dplyr::rename(
         "Dose" ="NOAEL (mg/kg/day)",
         "SM"= "Safety Margin",
         "Findings" = "Findings at Greater than NOAEL for the Study"
      )
    colnames(plotData_tab)[3] <- "Cmax"
    colnames(plotData_tab)[4] <- "AUC"
    plotData_tab <- plotData_tab %>%
      flextable::flextable() %>% 
          flextable::merge_v(j = ~ Study + Dose + Cmax+ AUC +SM+Findings) %>%
          flextable::autofit() %>% 
          flextable::set_header_labels("Dose" = "NOAEL (mg/kg/day)",
                        "Cmax" = cmax_unit,
                        "AUC" = auc_unit,
                        "Findings" = "Findings at Greater than NOAEL for the Study",
                        "SM" = "Safety Margin") %>% 
      flextable::add_header_row(values = c("Key Study Findings"), colwidths = c(6)) %>%
          flextable::theme_box()
    plotData_tab
    
  })
  
  # download table 02
  
  output$down_02_doc <- shiny::downloadHandler(
    filename = function() {
      paste0("key_findings", ".docx")
    },
    content = function(file) {
      flextable::save_as_docx(dt_to_flex_02(), path = paste0(user(), "/key_findings.docx"))
      file.copy(paste0(user(), "/key_findings.docx"), file)
    }
  )

  ## table 03 ----
  
  dt_03 <- shiny::reactive({
    cmax_unit <- paste0("Cmax (", input$cmax_unit, ")")
    auc_unit <- paste0("AUC (", input$auc_unit, ")")
    plotData_03 <- calculateSM()
    plotData_03 <- plotData_03 %>% 
      dplyr::select( Study,NOAEL, Dose, HED_value, Cmax, AUC , SM_start_dose, SM_MRHD) %>% 
      dplyr::mutate(Study = as.factor(Study)) %>% 
      unique() %>% 
      dplyr::filter(NOAEL == TRUE) %>% 
      dplyr::select(-NOAEL) %>% 
      dplyr::rename("NOAEL (mg/kg/day)" = Dose,
                     "Safety Margin at Starting Dose" = SM_start_dose,
                     "Safety Margin at MRHD" = SM_MRHD)
  
    names(plotData_03)[names(plotData_03)=="Cmax"] <- cmax_unit
    names(plotData_03)[names(plotData_03)=="AUC"] <- auc_unit
    Data <- getData()
    if (Data[["Clinical Information"]][["MgKg"]]=="FALSE") {
      plotData_03 <- plotData_03 %>% 
        dplyr::rename("HED (mg/day)" = HED_value)
    } else {plotData_03 <- plotData_03 %>% 
      dplyr::rename("HED (mg/kg/day)" = HED_value)
    }
   ## 
    plotData_03$Study <- factor(plotData_03$Study,levels= input$displayStudies)
    plotData_03 <- plotData_03 %>%
      dplyr::arrange(Study)
    plotData_03
  })
  
  # table 03 DT
  output$table_03 <- DT::renderDT({
    data <- getData()
    clin_dose <- clin_data(data)
    if (clin_dose>0) {
    plotData_03 <- dt_03()
    plotData_03 <- DT::datatable(plotData_03,rownames = FALSE, 
                             extensions = list("Buttons" = NULL,
                                               "ColReorder" = NULL),
                             class = "cell-border stripe",
                             filter = list(position = 'top'),
                             caption = htmltools::tags$caption(
                               style = "caption-side: top; text-align: center; font-size: 20px; color: black",
                               "Table :", htmltools::strong("Safety Margins Based on NOAEL from Pivotal Toxicology Studies")
                             ),
                             options = list(
                               dom = "lfrtipB",
                               buttons = c("csv", "excel", "copy", "print"),
                               colReorder = TRUE,
                               pageLength = 10,
                               columnDefs = list(list(className = "dt-center", targets = "_all")),
                               scrollY = TRUE,
                               initComplete = DT::JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                 "}"))) %>% 
      DT::formatStyle(columns = colnames(plotData_03), `font-size` = "18px")

    plotData_03
  }})
  
  # get data from DT table
  filtered_tab_03 <- shiny::reactive({
    shiny::req(input$table_03_rows_all)
    data <- dt_03()
    data[input$table_03_rows_all, ]
  })
  
  # flextable for docx file
  dt_to_flex_03 <- shiny::reactive({
    plotData_tab <- filtered_tab_03() %>% 
      flextable::flextable() %>%
      flextable::add_header_row(values = c("Nonclinical", "Clinical Exposure Margins"), colwidths = c(5,2)) %>%
      flextable::add_header_row(values = c("Safety Margins Based on NOAEL from Pivotal Toxicology Studies"), colwidths = c(7)) %>%
      flextable::theme_box()
    plotData_tab
  })
  

# download table 03
  output$down_03_doc <- shiny::downloadHandler(
    filename = function() {
      paste0("safety_margin", ".docx")
    },
    content = function(file) {
      flextable::save_as_docx(dt_to_flex_03(), path = paste0(user(), "/safety_margin.docx") )
      file.copy(paste0(user(), "/safety_margin.docx"), file)
    }
  )
  
  
  ## download all table
   download_all <- shiny::reactive({
     doc <- officer::read_docx()
     doc_02 <-  flextable::body_add_flextable(doc, dt_to_flex_01()) %>%
       officer::body_add_par("   ") %>%
       officer::body_add_par("   ") %>%
       officer::body_add_par("   ") %>%
       flextable::body_add_flextable( dt_to_flex_02()) %>%
       officer::body_add_par("   ") %>%
       officer::body_add_par("   ") %>%
       officer::body_add_par("   ") %>%
       flextable::body_add_flextable(dt_to_flex_03())

     doc_02
   })
##
   output$down_all <- shiny::downloadHandler(
     filename = function() {
       paste0("table_all", ".docx")
     },
     content = function(file) {
       print(download_all() , target = paste0(user(), "/table_all.docx"))
       file.copy(paste0(user(), "/table_all.docx"), file)


     }
   )
  
  # craete notes table ----
   all_study_notes <- shiny::reactive({
     plotData_tab <- calculateSM()
     plotData_tab <- plotData_tab %>%
       dplyr::select(Study_note, Study) %>%
       dplyr::rename(Notes = Study_note)
     plotData_tab$Study <- factor(plotData_tab$Study,levels= input$displayStudies)
     plotData_tab <- plotData_tab %>%
       dplyr::distinct() %>%
       dplyr::arrange(Study)
     plotData_tab
   })
   
 
# output table for notes  ----

  output$table_note <- DT::renderDT({
    data <- getData()
    clin_dose <- clin_data(data)
    if (clin_dose>0) {
    note_table <- all_study_notes()
    note_table <- DT::datatable(note_table,rownames = FALSE, 
                             extensions = list("Buttons" = NULL,
                                               "ColReorder" = NULL),
                             class = "cell-border stripe",
                             filter = list(position = 'top'),
                             caption = htmltools::tags$caption(
                               style = "caption-side: top; text-align: center; font-size: 20px; color: black",
                               "Table :", htmltools::strong("Notes for study")
                             ),
                             options = list(
                               dom = "lfrtipB",
                               buttons = c("csv", "excel", "copy", "print"),
                               colReorder = TRUE,
                               pageLength = 10,
                               columnDefs = list(list(className = "dt-center", targets = "_all")),
                               scrollY = TRUE,
                               initComplete = DT::JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                 "}"))) %>% 
      DT::formatStyle(columns = colnames(note_table), `font-size` = "18px")

    note_table
  }})
   
 
## download notes table
   table_note_to_flex <- shiny::reactive({
     note_table <- all_study_notes() %>%
       flextable::flextable() %>%
       flextable::add_header_row(values = c("Note for Studies"), colwidths = c(2)) %>%
       flextable::theme_box()
     note_table
   })
   
 # download notes table
   output$down_notes <- shiny::downloadHandler(
     filename = function() {
       paste0("note_table", ".docx")
     },
     content = function(file) {
       flextable::save_as_docx(table_note_to_flex(),
	    path = paste0(user(), "/note_table.docx"))
       file.copy(paste0(user(), "/note_table.docx"), file)
     }
   )
   
## filter NOAEL reactive ----
   
  filtered_plot <- shiny::reactive({
    if (input$NOAEL_choices == "ALL") {
      plot_data <- calculateSM()
    } else if (input$NOAEL_choices == "Less than or equal to NOAEL") {
        
        plot_data <- calculateSM()
        plot_data <- plot_data %>% 
          dplyr::filter(Dose <= noael_value)
 } else {
   plot_data <- calculateSM()
   plot_data <- plot_data %>% 
     dplyr::filter(Dose > noael_value)
    }
    
    plot_data
  })

  # plotheight ----
   plotHeight <- shiny::reactive({
     plotData <- calculateSM()
     nStudies <- length(unique(plotData$Study))
     plot_height <- (input$plotheight) * (nStudies)
     plot_height
   })


  
  
## figure -----
  
  output$figure <- ggiraph::renderGirafe({
    # shiny::req(input$clinDosing)
    input$selectData
    data <- getData()
    clin_dose <- clin_data(data)
    if (clin_dose>0) {
    plotData <- filtered_plot()
    plotData <- plotData[which(plotData$Findings %in% input$displayFindings),]
    plotData$Dose <- as.numeric(plotData$Dose)
    axis_limit <- calculateSM()
    suppressWarnings(SM_max <- max(axis_limit$SM))
    suppressWarnings(y_max <- as.numeric(max(axis_limit$Value_order)) +1)
    suppressWarnings(q_y_max <- as.numeric(max(axis_limit$Value_order)))
    finding_count <- length(unique(plotData$Findings))
# column width   
      if (finding_count < 4) {
        q_col_width <- 0.2* finding_count
      } else {
        q_col_width <- 0.9
      }
      
# text size of finding plot
      
      if (finding_count < 6) {
        q_text_size <- 6
      } else {
        q_text_size <- 4
      }
      
 ## plotdata for p plot (changed) ----
    plotData_p <- plotData
    plotData_p <- plotData_p %>% 
      dplyr::select(Study, Dose, SM, Value, NOAEL, Value_order, Study_note) %>% 
      unique()
    plotData_p$SM <- lapply(plotData_p$SM, roundSigfigs)
    plotData_p$SM <- as.numeric(plotData_p$SM)
    
#note 
    # plotData_note <- plotData_p %>% 
    #   select(Study, Study_note, SM, Value_order) %>% 
    #   unique()
    
    if (nrow(plotData)>0) {
      plotData$Study <- factor(plotData$Study,levels= input$displayStudies)
      plotData_p$Study <- factor(plotData_p$Study,levels= input$displayStudies)
      #plotData_note$Study <- factor(plotData_note$Study, levels = input$displayStudies)
      plotData$Findings <- factor(plotData$Findings, levels = input$displayFindings)
      plotData$DoseLabel <- factor(paste(plotData$Dose,'mg/kg/day'),
                                   levels=unique(paste(plotData$Dose,'mg/kg/day'))[order(unique(as.numeric(plotData$Dose),
								   decreasing=F))])
      maxFindings <- 1
      for (doseFinding in plotData$doseFindings) {
        nFindings <- stringr::str_count(doseFinding,'\n')
        if (nFindings > maxFindings) {
          maxFindings <- nFindings
        }
      }
      maxFindings <- maxFindings + 1
      #plotData$Findings <- as.factor(plotData$Findings)
      plotData$Severity <- as.factor(plotData$Severity)
      # make severity ordered factor
      plotData$Severity <- factor(plotData$Severity, 
                                  levels= c('Absent','Present','Minimal', 'Mild',
                                            'Moderate', 'Marked', 'Severe'), ordered = TRUE)
    
      #color_manual <- c('transparent','grey','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')
      color_manual <- c('Absent' = 'transparent',
                        'Present' = 'grey',
                        'Minimal' = '#feb24c',
                        'Mild' = '#fd8d3c',
                        'Moderate' = '#fc4e2a',
                        'Marked' = '#e31a1c',
                        'Severe' = '#b10026')

# # safety margin plot ----
      color_NOAEL <- c("TRUE" = "#239B56", "FALSE" = "black")
      tooltip_css <- "background-color:#3DE3D8;color:black;padding:2px;border-radius:5px;"
      if (input$dose_sm==1) {
        
          plot_p_label <- ggplot2::ggplot(plotData_p)+
          ggiraph::geom_label_interactive(ggplot2::aes(x = SM, y = Value_order,
                                     label = paste0(Dose, " mg/kg/day"),
                                     
                                     tooltip =paste0(SM, "x")), #DoseLabel changed
                                 color = "white",
                                 fontface = "bold",
                                 size = 6,
                                 fill= ifelse(plotData_p$NOAEL == TRUE, "#239B56", "black"),
                                 label.padding = ggplot2::unit(0.6, "lines")
          )
      } else if (input$dose_sm==2) {
        plot_p_label <- ggplot2::ggplot(plotData_p)+
          ggiraph::geom_label_interactive(ggplot2::aes(x = SM, y = Value_order,
                                     label = paste0(Dose, " mg/kg/day", "\n", SM, "x"),
                                     tooltip =paste0(Study_note)), #DoseLabel changed
                                 color = "white",
                                 fontface = "bold",
                                 size = 6,
                                 fill= ifelse(plotData_p$NOAEL == TRUE, "#239B56", "black"),
                                 label.padding = ggplot2::unit(0.6, "lines"))
      } else {
        plot_p_label <- ggplot2::ggplot(plotData_p)+
          ggiraph::geom_label_interactive(ggplot2::aes(x = SM, y = Value_order,
                                     label = paste0(Dose, " mg/kg/day", "\n", SM, "x"),
                                     tooltip =paste0(Study_note)), #DoseLabel changed
                                 color = "white",
                                 fontface = "bold",
                                 size = 6,
                                 fill= ifelse(plotData_p$NOAEL == TRUE, "#239B56", "black"),
                                 label.padding = ggplot2::unit(0.6, "lines")
          )+
          ggplot2::geom_text(data=plotData_p ,ggplot2::aes(x = 0.5*(SM_max), y=0.3 , label= Study_note),
                    color = "black",
                    size= 6)
      }
      
      p <- plot_p_label +
        ggplot2::scale_x_log10(limits = c(min(axis_limit$SM/2), max(axis_limit$SM*2)))+
        #scale_fill_manual(values = color_NOAEL)+
        ggplot2::ylim(0,y_max)+
        ggplot2::facet_grid( Study ~ ., labeller = ggplot2::label_wrap_gen(width = input$text_width))+
        ggplot2::labs( title = "      Summary of Toxicology Studies", x = "Exposure Margin")+
        ggplot2::theme_bw(base_size=12)+
        ggplot2::theme(
          axis.title.y = ggplot2::element_blank(),
              axis.ticks.y= ggplot2::element_blank(),
              axis.text.y = ggplot2::element_blank(),
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank(),
              plot.title = ggplot2::element_text(size= 20, hjust = 1),
              axis.title.x = ggplot2::element_text(size = 18, vjust = -0.9),
              axis.text.x = ggplot2::element_text(size = 16),
              legend.position = "none",
              strip.text.y = ggplot2::element_text(size=14, color="#000000", hjust = 0),
              strip.background = ggplot2::element_rect( fill = "white"))
      
# findings plot ----
      
      q <- ggplot2::ggplot(plotData)+
        ggiraph::geom_col_interactive(ggplot2::aes(x= Findings, y = Value, fill = Severity, group = Dose,  tooltip = Findings),
                 position = ggplot2::position_stack(reverse = TRUE),
                 color = 'transparent',
                 width = q_col_width)+
        ggiraph::geom_text_interactive(ggplot2::aes(x = Findings, y = Value, label = Dose, group = Dose,  tooltip = Findings),
                  size = q_text_size,
                  color = 'white',
                  fontface = 'bold',
                  position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE))+
        #scale_y_discrete(position = 'right')+
        ggplot2::ylim(0, q_y_max)+
        ggplot2::scale_fill_manual(values = color_manual)+
        ggplot2::facet_grid(Study ~ ., scales = 'free')+
        ggplot2::theme_bw(base_size=12)+
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
              strip.text.y = ggplot2::element_blank(),
              axis.ticks.y = ggplot2::element_blank(),
              axis.text.y = ggplot2::element_blank(),
              axis.title.x = ggplot2::element_blank(),
              axis.text.x = ggplot2::element_text(size= 16, angle = 90), #need to work
              #plot.title = element_text(size=20,hjust = 0.5),
              panel.grid.major.y = ggplot2::element_blank(),
              panel.grid.minor.y = ggplot2::element_blank(),
              panel.grid.major.x = ggplot2::element_line(),
              panel.grid.minor.x = ggplot2::element_blank(),
              legend.text  = ggplot2::element_text(size = 14),
              legend.title = ggplot2::element_text(size = 16),
              legend.justification = "top")+
        ggplot2::guides(fill = ggplot2::guide_legend(override.aes = ggplot2::aes(label = "")))

      ggiraph::girafe(code = print(p+q+ patchwork::plot_layout(ncol = 2, widths = c(3,1))),
             options = list(ggiraph::opts_tooltip(css = tooltip_css)),
             fonts = list(sans= "Roboto"),
             width_svg = 18, height_svg = plotHeight())
    }
	}
  })

  shiny::observe({
    shiny::req(input$selectData)
    values$selectData <- input$selectData
  })
  
  ## download rds file
  output$download_rds <- shiny::renderUI({
    datasets <- c(grep('.rds',list.files(user(),full.names = T),value=T))
    names(datasets) <- basename(unlist(strsplit(datasets,'.rds')))
    shiny::selectInput("downloadRDS", "Select to Download an Application:", choices = datasets, selected = NULL)
  })
  
  output$down_btn <- shiny::downloadHandler(
    filename = function() {
      app_name <- basename(input$downloadRDS)
      app_name
    },
    content = function(file) {
      file.copy(input$downloadRDS, file)
    }
  )
  
  ## upload file rds
  
  shiny::observe({
    if (is.null(input$upload_rds)) return()
    file.copy(input$upload_rds$datapath,   paste0(user(), "/",  input$upload_rds$name))
    datasets <- c('blankData.rds',grep('.rds',list.files(user(),full.names = T),value=T))
    names(datasets) <- basename(unlist(strsplit(datasets,'.rds')))
    names(datasets)[which(datasets=='blankData.rds')] <- 'New Application'
    shiny::selectInput('selectData','Select Application:',datasets)
    shiny::updateSelectInput(session,'selectData',choices=datasets,selected=values$Application)
  })
  
  # download tar file ----

  output$tar_file <- shiny::downloadHandler(
    filename = function() {
      "all_file.tar"
    },
    content = function(file) {
      all_file <- utils::tar("all_file.tar", files = paths$save_file_path)
      file.copy("all_file.tar", file)
    }
  )
####
  output$Admin_toggle <- shiny::renderUI({
    if (basename(user()) == "md_ali") {
"Admin"
    }
  })
  ###
  output$download_tar_file <- shinydashboard::renderMenu({
    if (input$pass_admin == "HeLLo_aDMiN_PT") {
      shiny::downloadButton("tar_file", "Download all file")
    }
  })
  ####
  output$show_file_table <- shinydashboard::renderMenu({
    if (input$pass_admin == "HeLLo_aDMiN_PT") {
      DT::DTOutput("dir_list")
    }
  })
  
  
  #####
  dir_to_df <- shiny::reactive({
    
    df_files <- data.frame(matrix(ncol = 2))
    colnames(df_files) <- c("user", "files")
    folder_list <- basename(list.dirs(paths$save_file_path))
    folder_list <- utils::tail(folder_list, -1)
    count <- 1
    for (folder in folder_list) {
      
        file_list <- grep(".rds", list.files(fs::path(paths$save_file_path, folder)), value = T)
        for (file in file_list) {
          df_files[count, "user"] <- folder
          file <- unlist(strsplit(file, ".rds"))
          df_files[count, "files"] <- file
          count <- count+1
        }
    }
    df_files <- df_files %>% 
      dplyr::arrange(user, files)
    df_files
  })
  
###
  
  output$dir_list <- DT::renderDT({
    dir_tab <- dir_to_df()
    dir_tab <- DT::datatable(dir_tab, rownames = FALSE, class = "cell-border stripe",
                              filter = list(position = 'top'),
                              extensions = list("Buttons" = NULL),
                              caption = htmltools::tags$caption(
                                style = "caption-side: top; text-align: center; font-size: 20px; color: black",
                                "Table :", htmltools::strong("All the RDS Files")
                              ),
                              options = list(
                                scrollY = TRUE,
                                pageLength = 100,
                                dom = "lfrtipB",
                                buttons = c("csv", "excel", "copy", "print"),
                                columnDefs = list(list(className = "dt-center", targets = "_all")),
                                initComplete = DT::JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                  "}"),
                                rowsGroup = list(0))) %>%
      DT::formatStyle(columns = colnames(dir_tab), `font-size` = "18px")
    
    path <- dt_extension # folder containing dataTables.rowsGroup.js
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    dir_tab$dependencies <- c(dir_tab$dependencies, list(dep))
    dir_tab
  })
  
  
  ## save units for Cmax and AUC ----
  
  shiny::observeEvent(input$save_units, {
    Data <- getData()
    Data[["CmaxUnit"]] <- input$cmax_unit
    Data[["AUCUnit"]] <- input$auc_unit
    saveRDS(Data,values$Application)
    shiny::showNotification("saved", duration = 3)
  })
  
  five_space <- paste0(
      htmltools::HTML("&nbsp;"),
      htmltools::HTML("&nbsp;"),
      htmltools::HTML("&nbsp;"),
      htmltools::HTML("&nbsp;"),
      htmltools::HTML("&nbsp;")
  )
  ## start dose cmax and auc untis
  output$start_cmax <- shiny::renderUI({
    cmax <- paste0("Start Dose Cmax ", "(", input$cmax_unit, "):")
    htmltools::HTML(paste0(five_space, htmltools::strong(cmax)))
  })
  
  output$start_auc <- shiny::renderUI({
    auc <- paste0("Start Dose AUC ", "(", input$auc_unit, "):")
    htmltools::HTML(paste0(five_space, htmltools::strong(auc)))
  })
  
 ## MRHD dose cmax and auc unit
  output$MRHD_cmax <- shiny::renderUI({
    cmax <- paste0("MRHD Dose Cmax ", "(", input$cmax_unit, "):")
    htmltools::HTML(paste0(five_space, htmltools::strong(cmax)))
  })
  
  output$MRHD_auc <- shiny::renderUI({
    auc <- paste0("MRHD Dose AUC ", "(", input$auc_unit, "):")
    htmltools::HTML(paste0(five_space, htmltools::strong(auc)))
  })
  
  ## custom dose 
  output$custom_cmax <- shiny::renderUI({
    cmax <- paste0("Custom Dose Cmax ", "(", input$cmax_unit, "):")
    htmltools::HTML(paste0(five_space, htmltools::strong(cmax)))
  })
  
  output$custom_auc <- shiny::renderUI({
    auc <- paste0("Custom Dose AUC ", "(", input$auc_unit, "):")
    htmltools::HTML(paste0(five_space, htmltools::strong(auc)))
  })
  
# clinical data modal function ----
#   clinical_data_modal <- function() {
#      shiny::modalDialog(

# 		 size = "l",

# 		 htmltools::h4("Edit Clinical Data"),
# 		 htmltools::tags$hr(style = "border-top: 1px solid#1e9acd;"),
# 		 htmltools::br(),
        
#          shiny::checkboxGroupInput(
#              "clinDosing", "Clinical Dosing:",
#              clinDosingOptions
#          ),
#          shiny::conditionalPanel(
#              "condition=input.MgKg==false",
#              shiny::numericInput("HumanWeight", "*Human Weight (kg):",
#                  value = 60, min = 0
#              )
#          ),
#          shiny::checkboxInput("MgKg", "Dosing in mg/kg?", value = F),
#          shiny::conditionalPanel(
#              condition = 'input.clinDosing.includes("Start Dose")',
#              htmltools::tags$hr(style = "border-top: 1px solid#1e9acd;"),
#              htmltools::h4("Start Dose Information:"),
#              shiny::conditionalPanel(
#                  condition = "input.MgKg==true",
#                  shiny::numericInput("StartDoseMgKg", "*Start Dose (mg/kg/day):",
#                      value = NULL, min = 0
#                  )
#              ),
#              shiny::conditionalPanel(
#                  condition = "input.MgKg==false",
#                  shiny::numericInput("StartDose", "*Start Dose (mg/day):",
#                      value = NULL, min = 0
#                  )
#              ),
#              shiny::uiOutput("start_cmax"),
#              shiny::numericInput("StartDoseCmax", NULL, value = NULL, min = 0),
#              shiny::uiOutput("start_auc"),
#              shiny::numericInput("StartDoseAUC", NULL, value = NULL, min = 0)
#          ),
#          shiny::conditionalPanel(
#              condition = 'input.clinDosing.includes("MRHD")',
#             htmltools::tags$hr(style = "border-top: 1px solid#1e9acd;"),
#              htmltools::h4("MRHD Information:"),
#              shiny::conditionalPanel(
#                  condition = "input.MgKg==true",
#                  shiny::numericInput("MRHDMgKg", "*MRHD (mg/kg):",
#                      value = NULL, min = 0
#                  )
#              ),
#              shiny::conditionalPanel(
#                  condition = "input.MgKg==false",
#                  shiny::numericInput("MRHD", "*MRHD (mg):", value = NULL, min = 0)
#              ),
#              shiny::uiOutput("MRHD_cmax"),
#              shiny::numericInput("MRHDCmax", NULL, value = NULL, min = 0),
#              shiny::uiOutput("MRHD_auc"),
#              shiny::numericInput("MRHDAUC", NULL, value = NULL, min = 0)
#          ),
#          shiny::conditionalPanel(
#              condition = 'input.clinDosing.includes("Custom Dose")',
#              htmltools::tags$hr(style = "border-top: 1px solid#1e9acd;"),
#              htmltools::h4("Custom Dose Information:"),
#              shiny::conditionalPanel(
#                  condition = "input.MgKg==true",
#                  shiny::numericInput("CustomDoseMgKg", "*Custom Dose (mg/kg):",
#                      value = NULL, min = 0
#                  )
#              ),
#              shiny::conditionalPanel(
#                  condition = "input.MgKg==false",
#                  shiny::numericInput("CustomDose", "*Custom Dose (mg):",
#                      value = NULL, min = 0
#                  )
#              ),
#              shiny::uiOutput("custom_cmax"),
#              shiny::numericInput("CustomDoseCmax", NULL, value = NULL, min = 0),
#              shiny::uiOutput("custom_auc"),
#              shiny::numericInput("CustomDoseAUC", NULL, value = NULL, min = 0)
#          ),
#          shiny::actionButton("saveClinicalInfo", "Save Clinical Information",
#              icon = shiny::icon("plus-circle"),
# 			 style = "background-color: white;
#             border: 2px solid #4CAF50;"

#          ),
#          htmltools::br(),
# 		 htmltools::br(),
#          footer = htmltools::tagList(
# 			htmltools::tags$h4("* Indicates Required Fields ", align = "left"),
#              htmltools::tags$h4("Please save the clinical data before hit the close button",
#                  style = "color:#E31616;"
#              ),
#              shiny::modalButton("Close")
#          )
#      )
#  }
  
 
   #### get studyID from IND selection
  

  get_studyid <- shiny::reactive({
      # req(input$ind_id)
      df <- ind_table
      df <- df[IND_num == input$ind_id, studyID]
      df
  })
  
  #### get information about studyid
  
  studyid_info <- shiny::reactive({
	  shiny::req(input$ind_id)
	  st_id <- get_studyid()
	  st_id_df <- DBI::dbGetQuery(conn=conn,
	'SELECT STUDYID,  TSPARMCD,TSPARM, TSVAL 
FROM TS WHERE 
STUDYID IN (:x)
AND
TSPARMCD IN ("SDESIGN",
"SPECIES",
"SSTYP",
"STITLE",
"STRAIN", 
"RECSAC", 
"DOSDUR", 
"ROUTE")',
	 params=list(x=st_id))
	  st_id_df <- data.table::as.data.table(st_id_df)
	  st_id_df
  })
  
  #### studyid options
  
  studyid_option <- shiny::reactive({
	  df <- studyid_info()

	  df <- df[TSPARMCD ==  "STITLE", .(STUDYID,TSPARMCD,TSVAL)][!duplicated(STUDYID)]
	  df <- df[, st_title := paste0(STUDYID, ": ", TSVAL)]
	  df
	  
  })

  # 
output$studyid_ui  <- shiny::renderUI({
     shiny::req(input$ind_id)
	#  if (!is.null(input$ind_id)) {
	  st_id_option <- studyid_option()
	  st_id_option <- st_id_option$st_title
	  names(st_id_option) <- paste0("\U25FC ", st_id_option)
	  
	 
	 shiny::selectizeInput(
            inputId = "study_id",
            label = htmltools::tags$div(htmltools::HTML('<i class="fa fa-database"
           style = "color:#000000;font-size:18px;"></i> Select StudyID')),
            selected = NULL,
            # choices =  setNames(st_id_option, paste0("\U25FC ", st_id_option))
            choices =  c(Choose = "", st_id_option)
        )
	#  }
})
  
  # update studyID list
  
  shiny::observeEvent(input$ind_id, {
      st_id_option <- studyid_option()
      st_id_option <- st_id_option$st_title
      names(st_id_option) <- paste0("\U25FC ", st_id_option)
	   if (input$selectStudy=='New Study') {
      shiny::updateSelectizeInput(session = session,
          inputId = "study_id",
          selected = NULL,
          choices = c(Choose = "", st_id_option)
          #  choices =  setNames(st_id_option, paste0("\U25FC ", st_id_option))
      )
	   } else {
		   Data <- getData()
      studyData <- Data[["Nonclinical Information"]][[input$selectStudy]]
		   select_study <- studyData$studyid_name
		   if(select_study %ni% st_id_option) {
			   select_study <- NULL 

		   }
		    shiny::updateSelectizeInput(session = session,
          inputId = "study_id",
          selected = select_study,
          choices = c(Choose = "", st_id_option)
          #  choices =  setNames(st_id_option, paste0("\U25FC ", st_id_option))
      )

	   }
  })
  
  studyid_selected <- shiny::eventReactive(input$study_id, {
	   if(!is.null(input$study_id) & (input$study_id != "")) {
	  df <- studyid_option()
	  df <- df[st_title==input$study_id, STUDYID]
	  df
	  }
  })
  

    # select sex 
  # update from database
  get_sex_from_studyid <- shiny::reactive({
      st_selected <- studyid_selected()
      df <- DBI::dbGetQuery(
          conn = conn,
          "SELECT DISTINCT SEX FROM DM WHERE STUDYID==:x",
          params = list(x = st_selected)
      )
      df$SEX
  })

  shiny::observeEvent(input$study_id, {

	  if(!is.null(input$study_id) & (input$study_id != "")) {
	  sex <- get_sex_from_studyid()
	  names(sex) <- sex
	  sex <- sort(sex)
	  shiny::updateCheckboxGroupInput(session = session, inputId = "which_sex",
	  choices = sex,
	  selected = sex,
	  inline = TRUE
	  )
	  }
  })
  # get species information
  
  shiny::observeEvent(input$study_id, {
	   if(!is.null(input$study_id) & (input$study_id != "")) {

	  st_selected <- studyid_selected()
	  df <- studyid_info()
	  df_species <- df[STUDYID==st_selected, ][TSPARMCD=="SPECIES"][!duplicated(STUDYID)][, TSVAL]
	  df_species <- tools::toTitleCase(tolower(df_species))
	  df_duration <- df[STUDYID==st_selected, ][TSPARMCD=="STITLE"][!duplicated(STUDYID)][,  TSVAL]


		if (input$selectStudy == "New Study" ) {
			shiny::updateSelectInput(session = session, inputId = "Species", selected = df_species)
			shiny::updateTextInput(session = session, inputId = "Duration", value = df_duration)
		} else {
			Data <- getData()
      		studyData <- Data[["Nonclinical Information"]][[input$selectStudy]]
		   select_study <- studyData$studyid_name
		   if (input$study_id != select_study  ) {
			   shiny::updateSelectInput(session = session, inputId = "Species", selected = df_species)
			   shiny::updateTextInput(session = session, inputId = "Duration", value = df_duration)

		   }
		   
		}
		


	   }
  })
  
# 



  # observeEvent(input$get_from_database, {
  #   updateNumericInput(session = session, inputId = 'nDoses')
  # })

# nonclinical modal dialog ----

# data_modal <- function() {

# 	htmltools::tags$div(id = "nonclinicalModal",
#     shiny::modalDialog(
		
		
# 		size = "l",
# 		htmltools::h4("Edit Nonclinical Data"),
# 		htmltools::tags$hr(style = "border-top: 1px solid#1e9acd;"),
# 		htmltools::br(),
        
#         shiny::uiOutput("selectStudy"),
#         htmltools::br(),
#         shiny::actionButton("saveStudy", "Save Study",
#             icon = shiny::icon("plus-circle"),
#             style = "background-color: white;
#             border: 2px solid #4CAF50;"
#         ),
#         shiny::actionButton("deleteStudy", "Delete Study",
#             icon = shiny::icon("minus-circle"),
#             style = "background-color: white;
#                     border: 2px solid #FF0000;"
#         ),
#         htmltools::br(),
#         htmltools::br(),
#         htmltools::tags$hr(style = "border-top: 3px solid#1e9acd;"),
		
#         shiny::selectizeInput(
#             inputId = "ind_id",
#             label = htmltools::tags$div(
#                 htmltools::HTML('<i class="fa fa-folder-open"
#              style = "color:#000000;font-size:18px;"></i> Select IND')
#             ),
#             selected = NULL,
#             choices = NULL
#                         # ind_number_list
                        
#             # options = list(maxOptions = 2500)
#         ),
		
#         htmltools::br(),
#         htmltools::br(),
# 		shiny::uiOutput("studyid_ui"),
#         htmltools::br(),
#         htmltools::br(),
#         shiny::selectInput("Species",
#             label = htmltools::tags$div(htmltools::HTML('<i class="fa fa-dog"
#             style = "color:#724028d9;font-size:18px;"></i> *Select Species:')),
#             choices = names(speciesConversion)
#         ),
#         htmltools::br(),
#         htmltools::br(),
# 		shiny::checkboxGroupInput("which_sex",
#             label = htmltools::tags$div(htmltools::HTML('<i class="fa fa-venus"
#             style = "color:#943488d9;font-size:18px;"></i> *Select Sex:')),
#             choices = choices_sex,
# 			selected = choices_sex,
# 			inline  = TRUE
#             # choices = c("ALL", "M", "F")
#         ),
#         shiny::textAreaInput("Duration", "*Study Duration/Description:",
#          height = "100px"),
#         htmltools::h4("Study Name:"),
#         shiny::verbatimTextOutput("studyTitle"),
#         htmltools::hr(style = "border-top: 3px solid#1e9acd;"),
#         shiny::uiOutput("choose_auc"),
# 		shiny::uiOutput("Choose_visit_day"),
# 		shiny::actionButton("get_from_db", 
# 		 "Click me to populate dose and pk from  database", 
# 		icon  = shiny::icon("mouse-pointer"),
# 		 style = "background-color:skyblue"),
# 		 htmltools::br(),
# 		 htmltools::br(),
#         # checkboxInput(
#         #     inputId = "get_from_database",
#         #     label = "Populate from Database", value = FALSE
#         # ),

#         #   shiny::actionButton(inputId = "get_from_database",
#         #   label = "Populate From Database",
#         #   style = "background-color: white;
#         #   border: 2px solid #4CAF50;"),


#         shiny::numericInput("nDoses",
#             label = htmltools::tags$div(htmltools::HTML('<i class="fa fa-syringe"
# 			style = "color:#169abbd9;font-size:18px;"></i> *Number of Dose Levels:')),
#             value = 1, step = 1, min = 1
#         ),
#         # numericInput('nDoses','*Number of Dose Levels:',value=1,step=1,min=1),
#         shiny::uiOutput("Doses"),
#         htmltools::hr(style = "border-top: 3px solid#1e9acd;"),
#         shiny::numericInput("nFindings",
#             label = htmltools::tags$div(htmltools::HTML('<i class="fa fa-microscope"
# 			style = "color:#940aebd9;font-size:18px;"></i> *Number of Findings:')),
#             value = 1, step = 1, min = 1
#         ),
#         shiny::uiOutput("Findings"),
#         htmltools::tags$hr(style = "border-top: 2px solid#1e9acd;"),
#         shiny::checkboxInput("notes", "Notes for Study?", value = FALSE),
#         shiny::uiOutput("study_note"),
#         shiny::actionButton("saveStudy_02", "Save Study",
#             icon = shiny::icon("plus-circle"),
#             style = "
#                    background-color: white;
#                    border: 2px solid #4CAF50;"
#         )
# 		,
		
#         footer = htmltools::tagList(
#             htmltools::tags$h4("Please save the study before close",
#                 style = "color:#E31616;"
#             ),
#             shiny::modalButton("Close")
#         )

#     )
# 	)
# }
  
#   shiny::observeEvent(eventExpr = input$edit_nonclinical, {
#       shiny::showModal(
#           data_modal()
#       )
#   })

  shiny::observeEvent(input$selectData, ignoreNULL = TRUE, {
	#   shiny::req(input$selectData)
    #   input$selectData
      Data <- getData()
      studyList <- names(Data[["Nonclinical Information"]])
	shiny::updateSelectInput(session, "selectStudy", "Select Study:", choices = studyList )
	shiny::updateSelectizeInput(session, inputId =  "ind_id",
	                     choices = c(Choose = "", ind_number_list), server = TRUE)

  })
  



  
  
  #### choose AUC from database
  output$choose_auc <- shiny::renderUI({
	  study <- studyid_selected()
    
    auc_list <- DBI::dbGetQuery(conn=conn,
	 'SELECT DISTINCT PPTESTCD,PPTEST FROM PP WHERE STUDYID=:x AND PPTESTCD LIKE "%auc%"',
	 params=list(x=study))
  
    auc_list <- data.table::as.data.table(auc_list)
	auc_list[, choice_option := paste0(PPTESTCD, " (", PPTEST, ")")]
	
	if ("AUCLST" %in% auc_list$PPTESTCD) {
		shiny::selectizeInput(inputId = "auc_db", 
                          label="Select AUC parameter",
                          selected= "AUCLST",
                          choices= stats::setNames(auc_list$PPTESTCD, auc_list$choice_option))

	} else {
    
    shiny::selectizeInput(inputId = "auc_db", 
                          label="Select AUC parameter",
                          choices= stats::setNames(auc_list$PPTESTCD, auc_list$choice_option))
	}

	# if(length(auc_list$PPTESTCD) < 1) {
	# 	  shiny::selectizeInput(inputId = "auc_db", 
    #                       label="NO AUC parameter available",
    #                       choices= c(choose = ""))

	# } else if ("AUCLST" %in% auc_list$PPTESTCD) {
	# 	shiny::selectizeInput(inputId = "auc_db", 
    #                       label="Select AUC parameter",
    #                       selected= "AUCLST",
    #                       choices= setNames(auc_list$PPTESTCD, auc_list$choice_option))

	# } else {
    
    # shiny::selectizeInput(inputId = "auc_db", 
    #                       label="Select AUC parameter",
    #                       choices= setNames(auc_list$PPTESTCD, auc_list$choice_option))
	# }
  }) 
  
  # 

  #### group by visit day ----
output$Choose_visit_day <- shiny::renderUI({
    study <- studyid_selected()
    pp_df <- DBI::dbGetQuery(
        conn = conn,
        "SELECT * FROM PP WHERE STUDYID=:x",
        params = list(x = study)
    )

	if(!all(is.na(pp_df[["PPNOMDY"]]))) {
		auc_list <- unique(pp_df[["PPNOMDY"]])

	} else {
		auc_list <- unique(pp_df[["VISITDY"]])

	}
    # auc_list <- RSQLite::dbGetQuery(
    #     conn = conn,
    #     "SELECT DISTINCT VISITDY FROM PP WHERE STUDYID=:x",
    #     params = list(x = study)
    # )
    # auc_list <- auc_list[["VISITDY"]]
    names(auc_list) <- as.character(auc_list)

    shiny::selectizeInput(
        inputId = "pp_visitday",
        label = "Select PPNOMDY/VISITDY Day",
        choices = c(auc_list),
        selected = auc_list,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
    )
})


# help_menu_item <- function() {
#     shinydashboard::menuItem(
#         "Questions/Feedback",
#         shinydashboard::menuSubItem(
#             text = "Email for help",
#             icon = shiny::icon("envelope-square"),
#             href = "mailto:kevin.snyder@fda.hhs.gov"
#         )
#     )
# }

shiny::observeEvent(eventExpr = input$reload_app, {
	session$reload()
})

output$show_unit_in_nonclinical  <- shiny::renderUI({
	shiny::req(input$selectData)
	if(input$MgKg == FALSE) {
		dose_unit <- "mg or (mg/day)"
	} else{
		dose_unit <- "mg/kg/day or mg/kg"
	}
	cmax_unit <- input$cmax_unit
	auc_unit <- input$auc_unit
	htmltools::HTML(paste0(
		"<h5>", "Dose Unit: ",dose_unit, "<br>",
		"Cmax Unit: ",cmax_unit, "<br>","AUC Unit: ",  auc_unit, "</h5>"
	))

})

output$clin_page_application  <- shiny::renderText({
	shiny::req(input$selectData)
	if(input$selectData != "blankdData.rds") {
	text <- basename(unlist(strsplit(input$selectData, ".rds")))
	if(text =="blankData") {
		text <- paste0("Selected Application: ", "New Application")
	} else {
	text <- paste0("Selected Application: ", text)}
	} else { text <- paste0("Selected Application: ", "New Application")
	}
	text

})


output$non_clin_page_application  <- shiny::renderText({
	shiny::req(input$selectData)
	if(input$selectData != "blankdData.rds") {
	text <- basename(unlist(strsplit(input$selectData, ".rds")))
	if(text =="blankData") {
		text <- paste0("Selected Application: ", "New Application")
	} else {
	text <- paste0("Selected Application: ", text)}
	} else { text <- paste0("Selected Application: ", "New Application")
	}
	text

})


shiny::observeEvent(eventExpr = input$selectData, ignoreNULL = FALSE, ignoreInit = TRUE, {

    
	 Data <- getData()

    # update clinical information
    clinData <- Data[['Clinical Information']]
    if (clinData$MgKg==F) {
      shiny::updateNumericInput(session,'HumanWeight',value = clinData$HumanWeight)
    } else { shiny::updateCheckboxInput(session, "MgKg", value = T)}
    
    clinDosing <- NULL
    for (dose in clinDosingOptions) {
      clin_dose <- clinData[[dose]][[gsub(' ','',dose)]]
      clin_dose_mgkg <- clinData[[dose]][[paste0(gsub(' ','',dose), 'MgKg')]]
      if ((!is.null(clin_dose)) | (!is.null(clin_dose_mgkg))) {
        clinDosing <- c(clinDosing,dose)
      }
    }
	
    shiny::updateCheckboxGroupInput(session, inputId = 'clinDosing',
	choices = clinDosingOptions,
	 selected=clinDosing)

    for (dose in clinDosing) {
      doseName <- gsub(' ','',dose)
      if (clinData$MgKg==F) {
        shiny::updateNumericInput(session,doseName,value = clinData[[dose]][[doseName]])
      } else {
        shiny::updateNumericInput(session,paste0(doseName,'MgKg'),value = clinData[[dose]][[paste0(doseName,'MgKg')]])
      }
      shiny::updateNumericInput(session,paste0(doseName,'Cmax'),value = clinData[[dose]][[paste0(doseName,'Cmax')]])
      shiny::updateNumericInput(session,paste0(doseName,'AUC'),value = clinData[[dose]][[paste0(doseName,'AUC')]])
    }
  })
  


 output$select_Data <- shiny::renderUI({
      datasets <- c("blankData.rds", grep(".rds", list.files(user(),
          full.names = T
      ), value = T))
      names(datasets) <- basename(unlist(strsplit(datasets, ".rds")))
      names(datasets)[which(datasets == "blankData.rds")] <- "New Application"
      if (is.null(values$selectData)) {
          shiny::selectInput("selectData", "Select Application:",
              datasets,
              selected = "blankData.rds"
          )
      } else {
          shiny::selectInput("selectData", "Select Application:",
              datasets,
              selected = values$selectData
          )
      }
  })
  
  # output$menu function -----
  
  output$menu <- shiny::renderUI({

    if (!is.null(input$selectData)) {
      if (input$selectData=='blankData.rds') {

htmltools::tagList(
	shiny::actionButton('reload_app','Reload App',icon=shiny::icon('rotate-right'),
			style = "background-color: white;
            border: 2px solid #bcbf0a;"
			),
			htmltools::br(),
			htmltools::tags$hr(style = "border-top: 1px solid#337ab7;"),
			htmltools::br(),

		shiny::uiOutput("select_Data"),
		shiny::conditionalPanel('input.selectData=="blankData.rds"',
							shiny::textInput('newApplication','Enter New Application Number:')
							),
			shiny::actionButton('saveData','Submit',icon=shiny::icon('plus-circle'),
			style = "background-color: white;
            border: 2px solid #4CAF50;"
			),
			htmltools::br(),
			htmltools::br()
			

			)
      } else {
		htmltools::tagList(
			shiny::actionButton('reload_app','Reload App',icon=shiny::icon('rotate-right'),
			style = "background-color: white;
            border: 2px solid #bcbf0a;"
			),
			htmltools::br(),
			htmltools::tags$hr(style = "border-top: 1px solid#337ab7;"),
			htmltools::br(),
        
			shiny::uiOutput('select_Data'),
			shiny::conditionalPanel('input.selectData=="blankData.rds"',
							shiny::textInput('newApplication','Enter New Application Number:')
			),
			shiny::actionButton('deleteData','Delete',icon=shiny::icon('minus-circle'),
			style = "background-color: white;
                    border: 2px solid #FF0000;"),
			htmltools::br(),
			htmltools::tags$hr(style = "border-top: 1px solid#337ab7;"),
			shiny::uiOutput('studyName')
		)
                  
      }
    } else {
		htmltools::tagList(
			shiny::actionButton('reload_app','Reload App',icon=shiny::icon('rotate-right'),
			style = "background-color: white;
            border: 2px solid #bcbf0a;"
			),
			
		htmltools::br(),
			htmltools::tags$hr(style = "border-top: 1px solid#337ab7;"),
			htmltools::br(),
   
		shiny::uiOutput('select_Data'),
		shiny::conditionalPanel('input.selectData=="blankData.rds"',
						shiny::textInput('newApplication','Enter New Application Number:')
		),
		shiny::actionButton('saveData','Submit',icon=shiny::icon('plus-circle'),
		style = "background-color: white;
            border: 2px solid #4CAF50;")
		)
                  
       
    }
  })
  


    output$renderFigure <- shiny::renderUI({
    shinycssloaders::withSpinner(
		ggiraph::girafeOutput('figure',width='100%',height=paste0(100*plotHeight(),'px')))
  })
    
    # shinyjs::runcodeServer()
}


ui <- shiny::navbarPage("Nonclinical Summary",
shiny::tabPanel("Application",
shiny::sidebarLayout(
	shiny::sidebarPanel(width = 2,
		shiny::uiOutput("menu")
	),
	shiny::mainPanel(
	htmltools::includeCSS(paste0(www_path,"/www/modal_dialog.css")),
	  htmltools::includeScript(paste0(www_path, "/www/button.js")),
	#   tags$head(tags$script(src = "button.js")),
    # shinyjs::useShinyjs(),
    # shinyjs::runcodeUI(),
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    # ),

    shiny::fluidRow(
      shiny::column(2,
             shiny::uiOutput('humanDosing')
      ),
      shiny::column(2,
            #  shiny::conditionalPanel(
            #    'input.clinDosing != null && input.clinDosing != ""',
               shiny::selectInput('SMbasis','Base Exposure Margin on:',c('HED','Cmax','AUC'))
            #  )
      ),
      shiny::column(4,
             shiny::uiOutput('displayStudies')
      ),
      
      shiny::column(4, 
             shiny::uiOutput('displayFindings'))
    ),
    # shiny::conditionalPanel(
    #   condition='input.selectData!="blankData.rds" && input.clinDosing != null && input.clinDosing != ""',
      shiny::tabsetPanel(
        shiny::tabPanel('Figure',
                 
                 shiny::fluidRow(
                   
                   shiny::column(2,
                          shiny::actionButton('refreshPlot','Refresh Plot')),
                  shiny::column(2, 
                         shiny::selectInput("NOAEL_choices", "Filter NOAEL:",
						  choices = c("ALL", "Less than or equal to NOAEL", "Greater than NOAEL"),
                             selected = "ALL")),
                  shiny::column(2, 
                         shiny::radioButtons("dose_sm", "Display Dose/Exposure Margin/Notes:",
						 choices = list(
						"Show Dose Only"=1,
						 "Show Dose with Exposure Margin"= 2,
						 "Show Notes" =3))),
                 shiny::column(3, 
                        shiny::sliderInput("plotheight", "Adjust Plot Height:",
						 min = 1, max = 15, value = 6)),
				shiny::column(2,
				shiny::sliderInput("text_width", "Adjust Text", 
				min = 10, max = 80, value = 60 ))),
                 htmltools::br(),
                 #withSpinner(girafeOutput('figure')),
				 shiny::uiOutput('renderFigure'),
                 htmltools::br(),
                 htmltools::hr(style = "border-top: 1px dashed black")
						  ),
        
      shiny::tabPanel("Clinical Relevance Table",
               DT::DTOutput('table_01'),
               htmltools::br(),
               htmltools::hr(style = "border-top: 1px dashed black"),
               htmltools::h4("Click on button below to export the table in a docx file"),
               shiny::downloadButton("down_01_doc", "Docx file download"),
               htmltools::br()
      ),
      shiny::tabPanel("Key Findings Table",
               DT::DTOutput('table_02'),
               htmltools::br(),
               htmltools::hr(style = "border-top: 1px dashed black"),
               htmltools::h4("Click on button below to export the table in a docx file"),
               shiny::downloadButton("down_02_doc", "Docx file download"),
               htmltools::br()
      ),
      shiny::tabPanel("Safety Margin Table",
               DT::DTOutput('table_03'),
               htmltools::br(),
               htmltools::hr(style = "border-top: 1px dashed black"),
               htmltools::h4("Click on button below to export the table in a docx file"),
               shiny::downloadButton("down_03_doc", "Docx file download"),
               htmltools::br()
      ),
      shiny::tabPanel("All Table", 
               htmltools::br(),
               htmltools::p("All three table (Clinical Relevance Table, Key Findings Table, Safety Margin Table) can be downloaded in single docx file. Click button below to download."),
               shiny::downloadButton("down_all", "Docx file download")),
	shiny::tabPanel("Notes Table",
				htmltools::br(),
	   			DT::DTOutput("table_note"),
				htmltools::h4("Click on button below to export the table in a docx file"),
				shiny::downloadButton("down_notes", "Docx file download")),
      
      shiny::tabPanel("Download Application",
               htmltools::br(),
               htmltools::h4("Download Application in RDS format:"),
               htmltools::br(),
               htmltools::p("Application can be downloaded in RDS format to share with others"),
               
               shiny::uiOutput("download_rds"),
               shiny::downloadButton("down_btn", "Download Application"),
               htmltools::br(),
               htmltools::hr(style = "border-top: 1px dashed black"),
               
               htmltools::h4("Upload Application in RDS format:"),
               shiny::fileInput("upload_rds", "Upload", accept = c(".rds"), multiple = F)),
      shiny::tabPanel(shiny::uiOutput("Admin_toggle"),
               htmltools::br(),
               shiny::passwordInput("pass_admin", "Password:", placeholder = "Restricted for Admin"),
               shiny::uiOutput("download_tar_file"),
               htmltools::br(),
               htmltools::hr(),
               htmltools::br(),
               shiny::uiOutput("show_file_table"))
			   )

	)
)),
shiny::tabPanel("Edit Clinical",
shiny::fluidPage(
shiny::fluidRow(
	shiny::column(width = 4,offset = 1, style = "background-color:#ffffff",
htmltools::h4("Edit Clinical Data", style= "text-align:center"),
		 htmltools::tags$hr(style = "border-top: 1px solid#1e9acd;"),
		shiny::textOutput("clin_page_application"),
	htmltools::br(),
	htmltools::br(),
         shiny::checkboxGroupInput(
            inputId = "clinDosing", label = "Clinical Dosing:",
             choices = clinDosingOptions
         ),

         shiny::conditionalPanel(
             "condition=input.MgKg==false",
             shiny::numericInput("HumanWeight", "*Human Weight (kg):",
                 value = 60, min = 0
             )
         ),
         shiny::checkboxInput("MgKg", "Dosing in mg/kg?", value = F),
         shiny::conditionalPanel(
             condition = 'input.clinDosing.includes("Start Dose")',
             htmltools::tags$hr(style = "border-top: 1px solid#1e9acd;"),
             htmltools::h4("Start Dose Information:"),
             shiny::conditionalPanel(
                 condition = "input.MgKg==true",
                 shiny::numericInput("StartDoseMgKg", "*Start Dose (mg/kg/day):",
                     value = NULL, min = 0
                 )
             ),
             shiny::conditionalPanel(
                 condition = "input.MgKg==false",
                 shiny::numericInput("StartDose", "*Start Dose (mg/day):",
                     value = NULL, min = 0
                 )
             ),
             shiny::uiOutput("start_cmax"),
             shiny::numericInput("StartDoseCmax", NULL, value = NULL, min = 0),
             shiny::uiOutput("start_auc"),
             shiny::numericInput("StartDoseAUC", NULL, value = NULL, min = 0)
         ),
         shiny::conditionalPanel(
             condition = 'input.clinDosing.includes("MRHD")',
            htmltools::tags$hr(style = "border-top: 1px solid#1e9acd;"),
             htmltools::h4("MRHD Information:"),
             shiny::conditionalPanel(
                 condition = "input.MgKg==true",
                 shiny::numericInput("MRHDMgKg", "*MRHD (mg/kg):",
                     value = NULL, min = 0
                 )
             ),
             shiny::conditionalPanel(
                 condition = "input.MgKg==false",
                 shiny::numericInput("MRHD", "*MRHD (mg):", value = NULL, min = 0)
             ),
             shiny::uiOutput("MRHD_cmax"),
             shiny::numericInput("MRHDCmax", NULL, value = NULL, min = 0),
             shiny::uiOutput("MRHD_auc"),
             shiny::numericInput("MRHDAUC", NULL, value = NULL, min = 0)
         ),
         shiny::conditionalPanel(
             condition = 'input.clinDosing.includes("Custom Dose")',
             htmltools::tags$hr(style = "border-top: 1px solid#1e9acd;"),
             htmltools::h4("Custom Dose Information:"),
             shiny::conditionalPanel(
                 condition = "input.MgKg==true",
                 shiny::numericInput("CustomDoseMgKg", "*Custom Dose (mg/kg):",
                     value = NULL, min = 0
                 )
             ),
             shiny::conditionalPanel(
                 condition = "input.MgKg==false",
                 shiny::numericInput("CustomDose", "*Custom Dose (mg):",
                     value = NULL, min = 0
                 )
             ),
             shiny::uiOutput("custom_cmax"),
             shiny::numericInput("CustomDoseCmax", NULL, value = NULL, min = 0),
             shiny::uiOutput("custom_auc"),
             shiny::numericInput("CustomDoseAUC", NULL, value = NULL, min = 0)
         ),
         shiny::actionButton("saveClinicalInfo", "Save Clinical Information",
             icon = shiny::icon("plus-circle"),
			 style = "background-color: white;
            border: 2px solid #4CAF50;"

         ),
		 htmltools::tags$div(style= " padding-bottom: 40px")

), 
shiny::column(offset = 1, width=3, style = "background-color:#ffffff",
htmltools::h4("Insert Custom Units", style = "text-align:center"),
htmltools::tags$hr(style = "border-top: 1px solid#1e9acd;"),
htmltools::br(),
# htmltools::tags$div(style= " padding-bottom: 40px"),
shiny::textInput("cmax_unit", "*Insert Unit for Cmax:", value = "ng/mL"),
shiny::textInput("auc_unit", "*Insert Unit for AUC:", value = "ng*h/mL"),
shiny::actionButton('save_units','Save Units',icon= shiny::icon('plus-circle'),
style = "background-color: white;
            border: 2px solid #4CAF50;"
),
htmltools::tags$div(style= " padding-bottom: 20px")
)
))),



shiny::tabPanel("Edit Nonclinical",
shiny::fluidPage(

htmltools::h4("Edit Nonclinical Data", style = "text-align:center;"),
		htmltools::tags$hr(style = "border-top: 1px solid#1e9acd;"),
		htmltools::br(),
        shiny::fluidRow( style= "margin-right:50px;",
			shiny::column(width = 3, offset = 1, style = "background-color:#ffffff",
			htmltools::tags$div(style= " padding-bottom: 10px"),
			shiny::textOutput("non_clin_page_application"),
			htmltools::br(),
        shiny::uiOutput("selectStudy"),
        htmltools::br(),
        shiny::actionButton("saveStudy", "Save Study",
            icon = shiny::icon("plus-circle"),
            style = "background-color: white;
            border: 2px solid #4CAF50;"
        ),
        shiny::actionButton("deleteStudy", "Delete Study",
            icon = shiny::icon("minus-circle"),
            style = "background-color: white;
                    border: 2px solid #FF0000;"
        ),
        htmltools::br(),
        htmltools::br(),
        htmltools::tags$hr(style = "border-top: 2px solid#1e9acd;"),
		
        shiny::selectizeInput(
            inputId = "ind_id",
            label = htmltools::tags$div(
                htmltools::HTML('<i class="fa fa-folder-open"
             style = "color:#000000;font-size:18px;"></i> Select IND')
            ),
            selected = NULL,
            choices = NULL
                        # ind_number_list
                        
            # options = list(maxOptions = 2500)
        ),
		
        htmltools::br(),
        htmltools::br(),
		shiny::uiOutput("studyid_ui"),
        htmltools::br(),
		htmltools::tags$hr(style = "border-top: 2px solid#1e9acd;"),
        htmltools::br(),
        shiny::selectInput("Species",
            label = htmltools::tags$div(htmltools::HTML('<i class="fa fa-dog"
            style = "color:#724028d9;font-size:18px;"></i> *Select Species:')),
            choices = names(speciesConversion)
        ),
        htmltools::br(),
        htmltools::br(),
		shiny::checkboxGroupInput("which_sex",
            label = htmltools::tags$div(htmltools::HTML('<i class="fa fa-venus"
            style = "color:#943488d9;font-size:18px;"></i> *Select Sex:')),
            choices = choices_sex,
			selected = choices_sex,
			inline  = TRUE
            # choices = c("ALL", "M", "F")
        ),
		htmltools::br(),
        shiny::textAreaInput("Duration", "*Study Duration/Description:",
         height = "100px"),
        htmltools::h4("Study Name:"),
        shiny::textOutput("studyTitle"),
		htmltools::br(),
		htmltools::br(),
		shiny::uiOutput("choose_auc"),
		shiny::uiOutput("Choose_visit_day"),
		shiny::actionButton("get_from_db", 
		 "Click me to populate dose and pk from  database", 
		icon  = shiny::icon("mouse-pointer"),
		 style = "background-color:skyblue"),
		htmltools::tags$div(style= " padding-bottom: 40px")

        # htmltools::hr(style = "border-top: 3px solid#1e9acd;")
		),
		shiny::column(width = 3, offset = 1,style = "background-color:#ffffff",
		htmltools::tags$div(style= " padding-bottom: 10px"),
		#  htmltools::br(),
		#  htmltools::br(),
        # checkboxInput(
        #     inputId = "get_from_database",
        #     label = "Populate from Database", value = FALSE
        # ),

        #   shiny::actionButton(inputId = "get_from_database",
        #   label = "Populate From Database",
        #   style = "background-color: white;
        #   border: 2px solid #4CAF50;"),


        shiny::numericInput("nDoses",
            label = htmltools::tags$div(htmltools::HTML('<i class="fa fa-syringe"
			style = "color:#169abbd9;font-size:18px;"></i> *Number of Dose Levels:')),
            value = 1, step = 1, min = 1
        ),
        # numericInput('nDoses','*Number of Dose Levels:',value=1,step=1,min=1),
        shiny::uiOutput("Doses"),
		htmltools::tags$div(style= " padding-bottom: 40px"),
		htmltools::tags$hr(style = "border-top: 1px solid#1e9acd;"),
		htmltools::tags$h4("Units used in Clinical:"),
		shiny::uiOutput("show_unit_in_nonclinical")
        # htmltools::hr(style = "border-top: 3px solid#1e9acd;")
		),
		shiny::column(width = 3, offset = 1,style = "background-color:#ffffff",
		htmltools::tags$div(style= " padding-bottom: 10px"),
        shiny::numericInput("nFindings",
            label = htmltools::tags$div(htmltools::HTML('<i class="fa fa-microscope"
			style = "color:#940aebd9;font-size:18px;"></i> *Number of Findings:')),
            value = 1, step = 1, min = 1
        ),
        shiny::uiOutput("Findings"),
        htmltools::tags$hr(style = "border-top: 2px solid#1e9acd;"),
        shiny::checkboxInput("notes", "Notes for Study?", value = FALSE),
        shiny::uiOutput("study_note"),
        shiny::actionButton("saveStudy_02", "Save Study",
            icon = shiny::icon("plus-circle"),
            style = "
                   background-color: white;
                   border: 2px solid #4CAF50;"
        ),
		htmltools::tags$div(style= " padding-bottom: 40px")
		)


),
htmltools::tags$hr(style = "border-top: 1px solid#1e9acd;")
)
),

shiny::tabPanel("Help")


)


# app running function ----

shiny::shinyApp(ui = ui, server = server)

}
