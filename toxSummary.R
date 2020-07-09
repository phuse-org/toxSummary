library(shiny) #test
library(ggplot2)
library(stringr)
library(htmltools)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(ggstance)
library(ggrepel)
library(RColorBrewer)
library(DT)
library(plotly)
library(officer)
library(flextable)
library(magrittr)
library(ggiraph)
library(patchwork)
# Bugs ####

# Notes from 6/29: ################################

# Data Selection:
#### - Change Enter Tox Program to Enter Application Number # done

# - Automatically open new application after entering it rather than having user select from list

# Clinical Data:
# - Set default to check Start Dose and MRHD
# - Fix that need to enter both a Start Dose and MRHD
# pop up delete button to confirm delete # added by yousuf

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


'%ni%' <- Negate('%in%')

# # Save configuration of blankData.rds below for later: ####

# Data <- list(
#   INDnumber = NULL,
#   'Clinical Information'= list(
#     HumanWeight = 60,
#     MgKg = F,
#     'Start Dose' = list(
#       StartDose = NULL,
#       StartDoseMgKg = NULL,
#       StartDoseCmax = NULL,
#       StartDoseAUC = NULL
#     ),
#     'MRHD' = list(
#       MRHDDose = NULL,
#       MRHDDoseMgKg = NULL,
#       MRHDCmax = NULL,
#       MRHDAUC = NULL
#     ),
#     'Custom Dose' = list(
#       CustomDose = NULL,
#       CustomDoseMgKg = NULL,
#       CustomDoseCmax = NULL,
#       CustomDoseAUC = NULL
#     )
#   ),
#   'Nonclinical Information' = list(
#     'New Study' = list(
#       Species = NULL,
#       Duration = NULL,
#       Doses = list(
#         Dose = NULL,
#         NOAEL = F,
#         Cmax = NULL,
#         AUC = NULL
#       ),
#       Findings = list(
#         Finding = NULL,
#         Reversibility = F,
#         FindingDoses = NULL
#       )
#     ),
#     'Rat Study' = list(
  #       Species = NULL,
  #       Duration = NULL,
  #       Doses = list(
  #         Dose = NULL,
  #         NOAEL = F,
  #         Cmax = NULL,
  #         AUC = NULL
  #       ),
  #       Findings = list(
  #         Finding = NULL,
  #         Reversibility = F,
  #         FindingDoses = NULL
  #       )
  #     )
#     'Dog Study' = list(
#       Species = NULL,
#       Duration = NULL,
#       Doses = list(
#         Dose = NULL,
#         NOAEL = F,
#         Cmax = NULL,
#         AUC = NULL
#       ),
#       Findings = list(
#         Finding = NULL,
#         Reversibility = F,
#         FindingDoses = NULL
#       )
#     )
#   )
# )
# 
# saveRDS(Data,'blankData.rds')

addUIDep <- function(x) {
  jqueryUIDep <- htmlDependency("jqueryui", "1.10.4", c(href="shared/jqueryui/1.10.4"),
                                script = "jquery-ui.min.js",
                                stylesheet = "jquery-ui.min.css")
  
  attachDependencies(x, c(htmlDependencies(x), list(jqueryUIDep)))
}


values <- reactiveValues()
values$Application <- NULL
values$SM <- NULL
values$selectData <- NULL

# Species Conversion ----

speciesConversion <- c(6.2,1.8,3.1,3.1)
names(speciesConversion) <- c('Rat','Dog','Monkey','Rabbit')

clinDosingOptions <- c('Start Dose','MRHD','Custom Dose')


## significant figure

sigfigs <- function(x){
  orig_scipen <- getOption("scipen")
  options(scipen = 999)
  on.exit(options(scipen = orig_scipen))
  
  x <- as.character(x)
  x <- sub("\\.", "", x)
  x <- gsub("(^0+|0+$)", "", x)
  nchar(x)
}

roundSigfigs <- function(x,N=2) {
  roundNumber <- round(x,digits=0)
  if (sigfigs(roundNumber)<=N) {
    roundNumber <- signif(x,digits=N)
  }
  return(roundNumber)
}

# Server function started here (selectData) ----

server <- function(input,output,session) {

  output$selectData <- renderUI({
    datasets <- c('blankData.rds',grep('.rds',list.files('Applications/',full.names = T),value=T))
    names(datasets) <- basename(unlist(strsplit(datasets,'.rds')))
    names(datasets)[which(datasets=='blankData.rds')] <- 'New Application'
    if (is.null(values$selectData)) {
      selectInput('selectData','Select Application:',datasets,selected='blankData.rds')
    } else {
      selectInput('selectData','Select Application:',datasets,selected=values$selectData)
    }
  })
  
  output$studyName <- renderUI({
    req(input$selectData)
    if (input$selectData!='blankData.rds') {
      HTML(paste(
        p(HTML(paste0('<h4>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<u>Selected Study</u></h4><h4>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;',
                      (basename(unlist(strsplit(input$selectData,'.rds')))),'</h4>')
        ))
      ))
    }
  })
  
# getData ------
  
  getData <- reactive({
    input$refreshPlot
    req(input$selectData)
    input$selectStudy
    Data <- readRDS(input$selectData)
  })
  
  observe({
    req(input$selectData)
    if (input$selectData == 'blankData.rds') {
      values$Application <- paste0('Applications/',input$newApplication,'.rds')
    } else {
      values$Application <- input$selectData
    }
  })
  
  observeEvent(input$saveData,{
    Data <- getData()
    saveRDS(Data,values$Application)
    datasets <- c('blankData.rds',grep('.rds',list.files('Applications/',full.names = T),value=T))
    names(datasets) <- basename(unlist(strsplit(datasets,'.rds')))
    names(datasets)[which(datasets=='blankData.rds')] <- 'New Application'
    selectInput('selectData','Select Application:',datasets)
    updateSelectInput(session,'selectData',choices=datasets,selected=values$Application)
  })
  
  
  
  
  # observeEvent(input$deleteData,{
  #   file.remove(values$Application)
  #   datasets <- c('blankData.rds',grep('.rds',list.files('Applications/',full.names = T),value=T))
  #   names(datasets) <- basename(unlist(strsplit(datasets,'.rds')))
  #   names(datasets)[which(datasets=='blankData.rds')] <- 'New Application'
  #   selectInput('selectData','Select Application:',datasets)
  #   updateSelectInput(session,'selectData',choices=datasets,selected='blankData.rds')
  # })
  
  
  observeEvent(input$deleteData, {
    showModal(modalDialog(
      title="Delete Application?",
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirmDelete", "Delete")
                       
      )
    ))
  })
  
  
  observeEvent(input$confirmDelete, {

    file.remove(values$Application)
    datasets <- c('blankData.rds',grep('.rds',list.files('Applications/',full.names = T),value=T))
    names(datasets) <- basename(unlist(strsplit(datasets,'.rds')))
    names(datasets)[which(datasets=='blankData.rds')] <- 'New Application'
    selectInput('selectData','Select Application:',datasets)
    updateSelectInput(session,'selectData',choices=datasets,selected='blankData.rds')
    
    removeModal()
  })
  
  
  output$selectStudy <- renderUI({
    req(input$selectData)
    input$selectData
    isolate(Data <- getData())
    studyList <- names(Data[['Nonclinical Information']])
    selectInput('selectStudy','Select Study:',choices=studyList)
  })
  
  # Clinical information -----
  
  observeEvent(input$selectData,ignoreNULL = T,{
    Data <- getData()
    clinData <- Data[['Clinical Information']]
    if (clinData$MgKg==F) {
      updateNumericInput(session,'HumanWeight',value = clinData$HumanWeight)
    }
    clinDosing <- NULL
    for (dose in clinDosingOptions) {
      if (!is.null(clinData[[dose]][[gsub(' ','',dose)]])) {
        clinDosing <- c(clinDosing,dose)
      }
    }
    updateCheckboxGroupInput(session,'clinDosing',selected=clinDosing)
    for (dose in clinDosing) {
      doseName <- gsub(' ','',dose)
      if (clinData$MgKg==F) {
        updateNumericInput(session,doseName,value = clinData[[dose]][[doseName]])
      } else {
        updateNumericInput(session,paste0(doseName,'MgKg'),value = clinData[[dose]][[paste0(doseName,'MgKg')]])
      }
      updateNumericInput(session,paste0(doseName,'Cmax'),value = clinData[[dose]][[paste0(doseName,'Cmax')]])
      updateNumericInput(session,paste0(doseName,'AUC'),value = clinData[[dose]][[paste0(doseName,'AUC')]])
    }
  })
  
# Nonclinical data update ------
  
  observeEvent(input$selectStudy,ignoreNULL = T,{
    Data <- getData()
    studyData <- Data[['Nonclinical Information']][[input$selectStudy]]
    updateSelectInput(session,'Species',selected=studyData$Species)
    updateTextInput(session,'Duration',value=studyData$Duration)
    updateNumericInput(session,'nDoses',value=studyData$nDoses)
    updateNumericInput(session,'nFindings',value=studyData$nFindings)
    
  })
  
  observeEvent(eventExpr = input$saveStudy, {
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
        findingList[[i]] <- list(Finding=input[[paste0('Finding',i)]],
                                 Reversibility = input[[paste0('Reversibility',i)]],
                                 # FindingDoses = input[[paste0('FindingDoses',i)]],
                                 Severity = severity
        )
      }
    } else {
      findingList[[1]] <- NULL
    }
    
    # Severity data update -----
    
    
    
    # studyName and data -----
    
    Data <- getData()
   
    studyName <- paste(input$Species,input$Duration,sep=': ')
    Data[['Nonclinical Information']][[studyName]] <- list(
      Species = input$Species,
      Duration = input$Duration,
      nDoses = input$nDoses,
      Doses = doseList,
      nFindings = input$nFindings,
      Findings = findingList
    )
    
    saveRDS(Data,values$Application)
    
    studyList <- names(Data[['Nonclinical Information']])
    updateSelectInput(session,'selectStudy',choices=studyList,selected=studyName)
    input$refreshPlot
  })
  

  observeEvent(input$saveClinicalInfo, {
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
  })
  
  # 
  # observeEvent(input$deleteStudy,{
  #   Data <- getData()
  #   studyIndex <- which(names(Data[['Nonclinical Information']])==input$selectStudy)
  #   restIndex <- seq(length(names(Data[['Nonclinical Information']])))[-studyIndex]
  #   restNames <- names(Data[['Nonclinical Information']])[restIndex]
  #   Data[['Nonclinical Information']] <- Data[['Nonclinical Information']][restNames]
  #   saveRDS(Data,values$Application)
  #   studyList <- names(Data[['Nonclinical Information']])
  #   updateSelectInput(session,'selectStudy',choices=studyList,selected='New Study')
  # })
  
  observeEvent(input$deleteStudy, {
    showModal(modalDialog(

      title="Delete Study?",
      footer = tagList(modalButton("Cancel"),
                       
                       actionButton("confirmRemove", "Delete")
                       
      )
    ))
  })
  
  
  observeEvent(input$confirmRemove, {
    
    Data <- getData()
    studyIndex <- which(names(Data[['Nonclinical Information']])==input$selectStudy)
    restIndex <- seq(length(names(Data[['Nonclinical Information']])))[-studyIndex]
    restNames <- names(Data[['Nonclinical Information']])[restIndex]
    Data[['Nonclinical Information']] <- Data[['Nonclinical Information']][restNames]
    saveRDS(Data,values$Application)
    studyList <- names(Data[['Nonclinical Information']])
    updateSelectInput(session,'selectStudy',choices=studyList,selected='New Study')


    removeModal()
  })
  
  
  
  
  
  output$studyTitle <- renderText({
    paste(input$Species,input$Duration,sep=': ')
  })
  
  output$displayStudies <- renderUI({
    req(input$clinDosing)
    input$selectData
    input$selectStudy
    isolate(Data <- getData())
    studyList <- names(Data[['Nonclinical Information']])
    studyList <- studyList[-which(studyList=='New Study')]
    addUIDep(selectizeInput('displayStudies',label='Select Studies to Display:',choices=studyList,
                            selected=studyList,
                            multiple=TRUE,width='100%',options=list(plugins=list('drag_drop','remove_button'))))
  })
  
  ## output$Doses -----
  
  output$Doses <- renderUI({
    req(input$selectStudy)
    if (input$selectStudy=='New Study') {
      lapply(1:(4*input$nDoses), function(i) {
        I <- ceiling(i/4)
        if (i %% 4 == 1) {
          div(
            hr(style = "border-top: 1px dashed skyblue"),
          numericInput(paste0('dose',I),paste0('Dose ',I,' (mg/kg/day):'), min = 0,NULL))
        } else if (i %% 4 == 2) {
          div(style="display: inline-block;vertical-align:top; width: 115px;",
              numericInput(paste0('Cmax',I),paste0('Dose ',I,' Cmax (ng/mL):'), min = 0, NULL))
          
          
        }
        else if (i %% 4 == 3) {
          div(style="display: inline-block;vertical-align:top; width: 115px;",
              numericInput(paste0('AUC',I),paste0('Dose ',I,' AUC (ng*h/mL):'),min = 0, NULL))
          
        } else {
          div(checkboxInput(paste0('NOAEL',I),'NOAEL?',value=F))
        }
        
        
        
        
      })
    } else {
      Data <- getData()
      studyData <- Data[['Nonclinical Information']][[input$selectStudy]]
      lapply(1:(4*input$nDoses), function(i) {
        I <- ceiling(i/4)
        doseName <- names(studyData$Doses)[I]
        if (i %% 4 == 1) {
          div(hr(style = "border-top: 1px dashed skyblue"),
          textInput(paste0('dose',I),paste0('Dose ',I,' (mg/kg/day):'),studyData$Doses[[doseName]][['Dose']]))
        } else if (i %% 4 == 2) {
          div(style="display: inline-block;vertical-align:top; width: 115px;",
              numericInput(paste0('Cmax',I),paste0('Dose ',I,' Cmax (ng/mL):'),studyData$Doses[[doseName]][['Cmax']]))
          
          
          
        }
        else if (i %% 4 == 3) {
          div(style="display: inline-block;vertical-align:top; width: 115px;",
              numericInput(paste0('AUC',I),paste0('Dose ',I,' AUC (ng*h/mL):'),studyData$Doses[[doseName]][['AUC']]))
          
        } else {
         div(checkboxInput(paste0('NOAEL',I),'NOAEL?',value=studyData$Doses[[doseName]][['NOAEL']]))
        }
       
        
      })

    }
  })
  
  # findings with severity -----

  output$Findings <- renderUI({
    req(input$selectStudy)
    if (input$selectStudy=='New Study') {
      if (input$nFindings>0) {
        numerator <- 2 + input$nDoses
        lapply(1:(numerator*input$nFindings), function(i) {
          
          
          I <- ceiling(i/numerator)
          if (i %% numerator == 1) {
            div(
              hr(style = "border-top: 1px dashed skyblue"),
            textInput(paste0('Finding',I),paste0('Finding ',I,':')))
            
          } else if (i %% numerator == 2) {
            radioButtons(paste0('Reversibility',I),'Reversibility:',
                         choiceNames=c('Reversible [Rev]','Not Reversible [NR]',
                                       'Partially Reversible [PR]','Not Assessed'),
                         choiceValues=c('[Rev]','[NR]','[PR]',''))
          } else {
            lapply(1:input$nDoses, function(j) {
              if ((i %% numerator == 2+j)|((i %% numerator == 0)&(j==input$nDoses))) {
               selectInput(inputId = paste0('Severity',I,'_',j),label = paste0('Select Severity at Dose ',j,' (',input[[paste0('dose',j)]],' mg/kg/day)'),
                            choices = c('Absent','Present','Minimal','Mild','Moderate','Marked','Severe'))
                
              }
            })
          }
        })
      }
    } else {
      Data <- getData()
      studyData <- Data[['Nonclinical Information']][[input$selectStudy]]
      #print(studyData)
      if (input$nFindings>0) {
        numerator <- 2 + input$nDoses
        lapply(1:(numerator*input$nFindings), function(i) {
          I <- ceiling(i/numerator)
          if (i %% numerator == 1) {
            
            
            
            div(
              hr(style = "border-top: 1px dashed skyblue"),
                textInput(paste0('Finding',I),paste0('Finding ',I,':'),
                      studyData$Findings[[paste0('Finding',I)]]$Finding))
          } else if (i %% numerator == 2) {
            radioButtons(paste0('Reversibility',I),'Reversibility:',
                         choiceNames=c('Reversible [Rev]','Not Reversible [NR]',
                                       'Partially Reversible [PR]','Not Assessed'),
                         choiceValues=c('[Rev]','[NR]','[PR]',''),
                         selected=studyData$Findings[[paste0('Finding',I)]]$Reversibility)
          } else {
            
            lapply(1:input$nDoses, function(j) {
              if ((i %% numerator == 2+j)|((i %% numerator == 0)&(j==input$nDoses))) {
                
                selectInput(inputId = paste0('Severity',I,'_',j),label = paste0('Select Severity at Dose ',j,' (',input[[paste0('dose',j)]],' mg/kg/day)'),
                            choices = c('Absent','Present','Minimal','Mild','Moderate','Marked','Severe'),
                            selected=studyData$Findings[[paste0('Finding',I)]]$Severity[[paste0('Dose',j)]])
                
                
              }
              
              
              
            })
          }
          
        
        })
      }
    }
    
  })
  
  # output$Findings <- renderUI({
  #   req(input$selectStudy)
  #   if (input$selectStudy=='New Study') {
  #     if (input$nFindings>0) {
  #       numerator <- 2 + input$nDoses
  #       lapply(1:(numerator*input$nFindings), function(i) {
  #         I <- ceiling(i/numerator)
  #         if (i %% numerator == 1) {
  #           textInput(paste0('Finding',I),paste0('Finding ',I,':'))
  #         } else if (i %% numerator == 2) {
  #           radioButtons(paste0('Reversibility',I),'Reversibility:',
  #                        choiceNames=c('Reversible [Rev]','Not Reversible [NR]',
  #                                      'Partially Reversible [PR]','Not Assessed'),
  #                        choiceValues=c('[Rev]','[NR]','[PR]',''))
  #         } else {
  #           lapply(1:input$nDoses, function(j) {
  #             if ((i %% numerator == 2+j)|((i %% numerator == 0)&(j==input$nDoses))) {
  #               selectInput(inputId = paste0('Severity',I,'_',j),label = paste0('Select Severity at Dose ',j,' (',input[[paste0('dose',j)]],' mg/kg/day)'),
  #                           choices = c('Absent','Present','Minimal','Mild','Moderate','Marked','Severe'))
  #             }
  #           })
  #         }
  #       })
  #     }
  #   } else {
  #     Data <- getData()
  #     studyData <- Data[['Nonclinical Information']][[input$selectStudy]]
  #     if (input$nFindings>0) {
  #       numerator <- 2 + input$nDoses
  #       lapply(1:(3*input$nFindings), function(i) {
  #         I <- ceiling(i/numerator)
  #         if (i %% numerator == 1) {
  #           textInput(paste0('Finding',I),paste0('Finding ',I,':'),
  #                     studyData$Findings[[paste0('Finding',I)]]$Finding)
  #         } else if (i %% numerator == 2) {
  #           radioButtons(paste0('Reversibility',I),'Reversibility:',
  #                        choiceNames=c('Reversible [Rev]','Not Reversible [NR]',
  #                                      'Partially Reversible [PR]','Not Assessed'),
  #                        choiceValues=c('[Rev]','[NR]','[PR]',''),
  #                        selected=studyData$Findings[[paste0('Finding',I)]]$Reversibility)
  #         } else {
  #           lapply(1:input$nDoses, function(j) {
  #             if ((i %% numerator == 2+j)|((i %% numerator == 0)&(j==input$nDoses))) {
  #               selectInput(inputId = paste0('Severity',I,'_',j),label = paste0('Select Severity at Dose ',j,' (',input[[paste0('dose',j)]],' mg/kg/day)'),
  #                           choices = c('Absent','Present','Minimal','Mild','Moderate','Marked','Severe'))
  #             }
  #           })
  #         }
  #       })
  #     }
  #   }
  # })
  
  
  
  # Create PlotData (changed) -----
  

  
 getPlotData <- reactive({
  Data <- getData()
  plotData <- data.frame(matrix(ncol = 18 ))
  column_names <- c("Study", "Species", "Months", "Dose_num", "Dose", 
                    "NOAEL", "Cmax", "AUC", "Findings",
                    "Reversibility", "Severity", "Value", "Value_order", "SM", "HED_value", "SM_start_dose", "SM_MRHD", "noael_value")
  colnames(plotData) <- column_names
  
  
  count <- 1
  
  for (Study in names(Data[["Nonclinical Information"]])) {
    if (Study != "New Study") {
      studyData <- Data[["Nonclinical Information"]][[Study]]
      
      for (i in seq(studyData$nFindings)){
        for (j in seq(studyData$nDoses)){
          
          plotData[count, "Study"] <- Study
          plotData[count, "Species"] <- studyData[["Species"]]
          plotData[count, "Months"] <- studyData[["Duration"]]
          plotData[count, "Dose_num"] <- names(studyData[["Doses"]][j])
          plotData[count, "Dose"] <- studyData[["Doses"]][[paste0("Dose", j)]][["Dose"]]
          plotData[count, "NOAEL"] <- studyData[["Doses"]][[paste0("Dose",j)]][["NOAEL"]]
          plotData[count, "Cmax"] <- studyData[["Doses"]][[paste0("Dose", j)]][["Cmax"]]
          plotData[count, "AUC"] <- studyData[["Doses"]][[paste0("Dose", j)]][["AUC"]]
          plotData[count, "Findings"] <- studyData[["Findings"]][[paste0("Finding", i)]][["Finding"]]
          plotData[count, "Reversibility"] <- studyData[["Findings"]][[paste0("Finding", i)]][["Reversibility"]]
          plotData[count, "Severity"] <- studyData[["Findings"]][[paste0("Finding", i)]][["Severity"]][[paste0("Dose", j)]]
          plotData[count, "Value"] <- 1
          plotData[count, "Value_order"] <- j
          plotData[count, "SM"] <- NA
          plotData[count, "HED_value"] <- NA
          plotData[count, "SM_start_dose"] <- NA
          plotData[count, "SM_MRHD"] <- NA
          plotData[count, "noael_value"] <- NA
          
          count <- count+1
          
          
        }
      }
    }
  }
  
  plotData$Rev <- gsub("\\[|\\]", "", plotData$Reversibility)
  plotData$finding_rev <- paste0(plotData$Findings,"_", plotData$Rev)
  plotData$find_rev_b <- paste0(plotData$Findings, plotData$Reversibility)
  # plotData$Study <- str_to_lower(plotData$Study)
  # plotData$Study <- str_to_title(plotData$Study)
  plotData$Findings <- tolower(plotData$Findings)
  plotData$Findings <- str_to_title(plotData$Findings)
  plotData$Dose <- as.numeric(plotData$Dose)
  
  
  plotData$Rev[plotData$Rev == ""] <- "Not Assessed"
  plotData$Rev[plotData$Rev == "Rev"] <- "Reversible"
  plotData$Rev[plotData$Rev == "NR"] <- "Not Reversible"
  plotData$Rev[plotData$Rev == "PR"] <- "Partially Reversible"
  
  plotData <- plotData[which(plotData$Study %in% input$displayStudies),]

  return(plotData)
  
})
 
 
    

  
  output$humanDosing <- renderUI({
    req(input$clinDosing)
    Data <- getData()
    clinDosingNames <- input$clinDosing
    names(clinDosingNames) <- clinDosingNames
    if (length(clinDosingNames)>0) {
      for (clinDose in input$clinDosing) {
        if (Data[['Clinical Information']][['MgKg']]==F) {
          names(clinDosingNames)[which(clinDosingNames==clinDose)] <- paste0(clinDose,
                                                                             ': (',Data[['Clinical Information']][[clinDose]][[paste0(unlist(strsplit(clinDose,' ')),
                                                                                                                                      collapse='')]],' mg)')
        } else {
          names(clinDosingNames)[which(clinDosingNames==clinDose)] <- paste0(clinDose,': (',Data[['Clinical Information']][[clinDose]][[paste0(unlist(strsplit(clinDose,' ')),'MgKg',collapse='')]],' mg/kg)')
        }
      }
    }
    selectInput('humanDosing','Select Human Dose:',choices=clinDosingNames)
  })
  
  
  
  ## 
  
  filter_NOAEL <- reactive({
    
    df_plot <- getPlotData()
   
    count <- 0
    for (i in unique(df_plot$Study)){
      
      ind <- which(df_plot$Study == i)
      study <- df_plot[ind,]
      row_num <- nrow(study)
      
      
      for (j in seq(nrow(study))) {
        if (any(study$NOAEL == TRUE)) {
          dose <- study$Dose[which(study$NOAEL == TRUE)]
          dose <- unique(dose)
          k <- count+j
          df_plot[k, "noael_value"] <- as.numeric(dose)
          
        } else {
          dose <- min(study$Dose)
          dose <- as.numeric(dose) - 1
          k <- count + j
          df_plot[k, "noael_value"] <- as.numeric(dose)
          
        }
        
        
      }
      
      count <- count +row_num
    }
 
    
    df_plot

  })
  
   #observeEvent(filter_NOAEL(), {print(filter_NOAEL())})
  
  # 
  # for (i in unique(df_plot$Study)){
  #   
  #   ind <- which(df_plot$Study == i)
  #   study <- df_plot[ind,]
  #   row_num <- nrow(study)
  #   
  #   
  #   for (j in seq(nrow(study))) {
  #     
  #     dose <- study$Dose[which(study$NOAEL == TRUE)]
  #     dose <- unique(dose)
  #     k <- count+j
  #     df_plot[k, "noael_value"] <- dose
  #     
  #   }
  #   
  #   count <- count +row_num
  # }
  
  
# ## calculate safety margin (SM) ------
#
  calculateSM <- reactive({
    Data <- getData()
    plotData <- filter_NOAEL()
    # HED_value <- NULL
    # SM <- NULL
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
        # humanDose <- input[[humanDoseName]]
        if (input$SMbasis=='HED') {
          humanDose <- Data[['Clinical Information']][[input$humanDosing]][[humanDoseName]]
          HED <- Dose/speciesConversion[[Species]]
          SM_start <- HED/(Data[["Clinical Information"]][["Start Dose"]][["StartDoseMgKg"]])
          SM_MRHD <- HED/(Data[["Clinical Information"]][["MRHD"]][["MRHDMgKg"]])
          
          if (input$MgKg==F) {
            HED <- HED*Data[['Clinical Information']][['HumanWeight']]
            SM_start <- HED/(Data[["Clinical Information"]][["Start Dose"]][["StartDose"]])
            SM_MRHD <- HED/(Data[["Clinical Information"]][["MRHD"]][["MRHD"]])
            
          }
        } else if (input$SMbasis=='Cmax') {
          humanDose <- Data[['Clinical Information']][[input$humanDosing]][[paste0(humanDoseName,input$SMbasis)]]
          HED <- Dose
          SM_start <- HED/(Data[["Clinical Information"]][["Start Dose"]][["StartDoseCmax"]])
          SM_MRHD <- HED/(Data[["Clinical Information"]][["MRHD"]][["MRHDCmax"]])
          
        } else {
          humanDose <- Data[['Clinical Information']][[input$humanDosing]][[paste0(humanDoseName,input$SMbasis)]]
          HED <- Dose
          SM_start <- HED/(Data[["Clinical Information"]][["Start Dose"]][["StartDoseAUC"]])
          SM_MRHD <- HED/(Data[["Clinical Information"]][["MRHD"]][["MRHDAUC"]])
          
        }
        
        
        
        
        plotData[i, "HED_value"]<- round(HED, digits = 2) ##for table 03
        plotData[i, "SM"] <- round(HED/humanDose, 2)
        plotData[i, "SM_start_dose"] <- round(SM_start, digits = 2)
        plotData[i, "SM_MRHD"] <- round(SM_MRHD, digits = 2)
      }
    }
    
    
    #plotData <- cbind(plotData,SM, HED_value)
    return(plotData)
  })

  
  
  #observeEvent(calculateSM(), {print(str(calculateSM()))})
  ### output table ----
  
 ## {{{{{{{{{{{{{{{}}}}}}}}}}}}}}} -----
  
  dt_01 <- reactive({
    
    plotData_tab <- calculateSM()
    plotData_tab <- plotData_tab %>% 
      mutate(Findings = as.factor(Findings),
             Rev = as.factor(Rev),
             Study = as.factor(Study),
             Dose = as.numeric(Dose),
             SM = as.numeric(SM),
             Severity = as.factor(Severity))
    
    plotData_tab <- plotData_tab %>% 
      select( Findings,Rev, Study, Dose, SM, Severity) %>%
      filter(Severity != "Absent") %>% 
      select(-Severity) %>% 
      arrange(Findings, Rev) %>% 
      rename(Reversibility = Rev,
             "Clinical Safety Margin" = SM,
             "Dose (mg/kg/day)" = Dose)
    plotData_tab
 
  })
  


  
  output$table_01 <- renderDT({
    plotData_tab <- dt_01()
  
    plotData_tab <- datatable(plotData_tab, rownames = FALSE,
                              class = "cell-border stripe",
                              filter = list(position = 'top'),
                              extensions = list("Buttons" = NULL,
                                                "ColReorder" = NULL),

                              options = list(
                                dom = "lfrtipB",
                                buttons = c("csv", "excel", "copy", "print"),
                                colReorder = TRUE,
                                scrollY = TRUE,
                                pageLength = 25,
                                columnDefs = list(list(className = "dt-center", targets = "_all")),
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                  "}"),

                                rowsGroup = list(0,1,2))) %>%
      formatStyle(columns = colnames(plotData_tab), `font-size` = "18px")
    
    path <- "DT_extension" # folder containing dataTables.rowsGroup.js
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0",
      path, script = "dataTables.rowsGroup.js")
    plotData_tab$dependencies <- c(plotData_tab$dependencies, list(dep))
    plotData_tab
  })
  
  
  filtered_tab_01 <- reactive({
    req(input$table_01_rows_all)
    data <- dt_01()
    data[input$table_01_rows_all, ]

  })



  dt_to_flex_01 <- reactive({
    plotData_tab <- filtered_tab_01()

    plotData_tab <- plotData_tab %>%

      #select( Findings,Rev, Study, Dose, SM) %>%
      #group_by(Findings, Rev, Study) %>%
      dplyr::arrange(Findings, Reversibility, Study) %>%
      flextable() %>%
      merge_v(j = ~ Findings + Reversibility + Study) %>%

      flextable::autofit() %>%
      add_header_row(values = c("Nonclinical Findings of Potential Clinical Relevance"), colwidths = c(5)) %>%
      theme_box()
    #fontsize(size = 18, part = "all") %>%
    plotData_tab

  })



  observeEvent(dt_to_flex_01(), {save_as_docx(dt_to_flex_01(), path = "table_01.docx")})



  output$down_01_doc <- downloadHandler(
    filename = function() {
      paste("table_01", ".docx")
    },
    content = function(file) {
      file.copy("table_01.docx", file)


    }
  )

  #### table 02
  
  dt_02 <- reactive({
    
    plotData_tab <- calculateSM()
    plotData_tab <- plotData_tab %>% 
      dplyr::select(Study, Dose, NOAEL, Cmax, AUC, SM, Findings, Severity) %>% 
      mutate(Findings = as.factor(Findings),
             Study = as.factor(Study)) %>% 
             filter(NOAEL == TRUE) %>% 
             filter(Severity != "Absent") %>% 
             dplyr::select(-NOAEL) %>%
          #group_by(Findings, Rev, Study) %>%
             dplyr::arrange(Study, Dose)
    plotData_tab
    
  })
  
# make column name same as flextable (add unit in DT table)
  output$table_02 <- renderDT({
    plotData_tab <- dt_02()
    plotData_tab <- datatable(plotData_tab, rownames = FALSE, class = "cell-border stripe",
                              filter = list(position = 'top'),
                              extensions = list("Buttons" = NULL),
                            
                              
                              options = list(
                                scrollY = TRUE,
                                pageLength = 25,
                                dom = "lfrtipB",
                                buttons = c("csv", "excel", "copy", "print"),
                                
                                columnDefs = list(list(className = "dt-center", targets = "_all")),
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                  "}"),
                                rowsGroup = list(0,1,2,3,4))) %>%
      formatStyle(columns = colnames(plotData_tab), `font-size` = "18px")
    
    path <- "DT_extension" # folder containing dataTables.rowsGroup.js
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    plotData_tab$dependencies <- c(plotData_tab$dependencies, list(dep))
    plotData_tab
  })
  
  
  
  filtered_tab_02 <- reactive({
    req(input$table_02_rows_all)
    data <- dt_02()
    data[input$table_02_rows_all, ]
    
  })
  
  
  dt_to_flex_02 <- reactive({
    plotData_tab <- filtered_tab_02()
    plotData_tab <- plotData_tab %>%
      flextable() %>% 
          merge_v(j = ~ Study + Dose + Cmax+ AUC +SM) %>%
          flextable::autofit() %>% 
      
          set_header_labels("Dose" = "Dose (mg/kg/day)",
                        "Cmax" = "Cmax (ng/ml)",
                        "AUC" = "AUC (ng*h/ml)",
                        "SM" = "Safety Margin") %>% 
          theme_box()
    plotData_tab
    
  })
  
  
  
  observeEvent(dt_to_flex_02(), {save_as_docx(dt_to_flex_02(), path = "table_02.docx")})
  
  
  
  output$down_02_doc <- downloadHandler(
    filename = function() {
      paste("table_02", ".docx")
    },
    content = function(file) {
      file.copy("table_02.docx", file)
      
      
    }
  )
  
  
  
  
  
  ## table 04 ----
  
  dt_03 <- reactive({
    
    plotData_03 <- calculateSM()
    plotData_03 <- plotData_03 %>% 
      select( Study,NOAEL, Dose, SM , HED_value, Cmax, AUC , SM_start_dose, SM_MRHD) %>% 
      mutate(Study = as.factor(Study)) %>% 
      unique() %>% 
      filter(NOAEL == TRUE) %>% 
      select(-NOAEL) %>% 
      dplyr::rename( HED = HED_value, "NOAEL (mg/kg/day)" = Dose,
                     "Cmax (ng/ml)" = Cmax, "AUC (ng*h/ml)" = AUC, 
                     "Safety Margin" = SM,
                     "Safety Margin at Starting Dose" = SM_start_dose,
                     "Safety Margin at MRHD" = SM_MRHD)
      #dplyr::mutate('Starting Dose' = NA, MRHD = NA) # have to change
    plotData_03
    
  })
  
  output$table_03 <- renderDT({
    plotData_03 <- dt_03()
    plotData_03 <- datatable(plotData_03,rownames = FALSE, 
                             extensions = list("Buttons" = NULL,
                                               "ColReorder" = NULL), 
                          
                             
                             class = "cell-border stripe",
                             filter = list(position = 'top'),
                             caption = htmltools::tags$caption(
                               style = "caption-side: top; text-align: center; font-size: 20px; color: black",
                               "Table :", htmltools::strong("Safety Margins Based on NOAEL from Pivotal Toxicology Studies")
                             ),
                             options = list(
                               
                               #autoWidth = TRUE,
                               #columnDefs = list(list(width = "150px", targets = "_all")),
                               dom = "lfrtipB",
                               buttons = c("csv", "excel", "copy", "print"),
                               colReorder = TRUE,
                               pageLength = 10,
                               columnDefs = list(list(className = "dt-center", targets = "_all")),
                               
                               scrollY = TRUE,
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                 "}"))) %>% 
      formatStyle(columns = colnames(plotData_03), `font-size` = "18px")

    plotData_03
  })
  
  
  filtered_tab_03 <- reactive({
    req(input$table_03_rows_all)
    data <- dt_03()
    data[input$table_03_rows_all, ]
    
  })
  
  
  dt_to_flex_03 <- reactive({
    plotData_tab <- filtered_tab_03() %>% 
      flextable() %>%
      add_header_row(values = c("Nonclinical", "Clinical Safety Margins"), colwidths = c(6,2)) %>%
      add_header_row(values = c("Safety Margins Based on NOAEL from Pivotal Toxicology Studies"), colwidths = c(8)) %>%
      theme_box()
    
    plotData_tab
    
    
  })
  
  
  
  observeEvent(dt_to_flex_03(), {save_as_docx(dt_to_flex_03(), path = "table_03.docx")})
  
  
  
  output$down_03_doc <- downloadHandler(
    filename = function() {
      paste("table_03", ".docx")
    },
    content = function(file) {
      file.copy("table_03.docx", file)
      
      
    }
  )
  
  
  ## download all table 
  
  
  
   download_all <- reactive({
     doc <- read_docx()
     doc_02 <-  body_add_flextable(doc, dt_to_flex_01()) %>%
       body_add_par("   ") %>%
       body_add_par("   ") %>%
       body_add_par("   ") %>%
       body_add_flextable( dt_to_flex_02()) %>%
       body_add_par("   ") %>%
       body_add_par("   ") %>%
       body_add_par("   ") %>%
       body_add_flextable(dt_to_flex_03())

     doc_02
   })

  observeEvent(download_all(), {print(download_all() , target = "table_all.docx")})



   output$down_all <- downloadHandler(
     filename = function() {
       paste("table_all", ".docx")
     },
     content = function(file) {
       file.copy("table_all.docx", file)


     }
   )
  
  
 
 #### plotheight ----

  # plotHeight <- function() {
  #   plotData <- calculateSM()
  #   nStudies <- length(unique(plotData$Study))
  #   if (nStudies < 2){
  #     plotHeight <- as.numeric(8*nStudies)
  #   } else {
  #     plotHeight <- as.numeric(5*nStudies)
  #     }
  # }
  
  # y_limit <- function() {
  #   y_axis <- calculateSM()
  #   y_max <-as.numeric(max(y_axis$Value_order))
  #   y_min <- as.numeric(min(y_axis$Value_order))
  #   
  # }

   

## Figure in UI
  filtered_plot <- reactive({
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
  

  
   plotHeight <- reactive({
     plotData <- calculateSM()
     nStudies <- length(unique(plotData$Study))
     plot_height <- (input$plotheight) * (nStudies)
     plot_height
   })
   # 
   # 
   # p_tile_width <- reactive({
   #   width <- input$textbox
   #   width
   # })
  
  
  
  
  output$figure <- renderGirafe({
    
    
    plotData <- filtered_plot()
    plotData$Dose <- as.numeric(plotData$Dose)
    
    axis_limit <- calculateSM()
    suppressWarnings(SM_max <- max(axis_limit$SM))
    
    suppressWarnings(y_max <- as.numeric(max(axis_limit$Value_order)) +1)
    suppressWarnings(q_y_max <- as.numeric(max(axis_limit$Value_order)))
      #y_min <- as.numeric(min(axis_limit$Value_order)) -1
      
      # p plot tile height and weight
      
      # if (SM_max <= 3) {
      #   p_tile_width <- 0.17
      #   p_tile_height <- 0.65
      # } else if (SM_max > 5 & SM_max <= 100) {
      #   p_tile_width <- 0.45
      #   p_tile_height <- 0.65
      # } else if (SM_max > 5 & SM_max <= 100) {
      #   p_tile_width <- 0.45
      #   p_tile_height <- 0.65
      # } else if (SM_max > 100 & SM_max <= 500) {
      #   p_tile_width <- 0.60
      #   p_tile_height <- 0.65
      # } else if (SM_max > 500 & SM_max <= 1000) {
      #   p_tile_width <- 0.65
      #   p_tile_height <- 0.65
      # } else {
      #   p_tile_width <- log10(SM_max)*(0.158)
      #   p_tile_height <- 0.7
      # }
      
      # SM_max_log <- log10(SM_max)
      # p_tile_width <- 0.11641234+ (SM_max_log * 0.23974182) - ((SM_max_log)^2 * 0.04421635) + ((SM_max_log)^3 * 0.00547356)
      
    
    
      # if (SM_max < 2) {
      #   p_tile_width <- 0.81
      # } else {
      #   x <- log10(SM_max)
      #   p_tile_width <- 0.118141 + (x *0.177231) + ( (x)^2 * 0.000962 )- ((x)^3 *0.012639) + ((x)^4 * 0.002044 )
      # }

      
  
      p_tile_height <- 0.70
      
      finding_count <- length(unique(plotData$Findings))
      
      
      if (finding_count < 4) {
        q_col_width <- 0.2* finding_count
      } else {
        q_col_width <- 0.9
      }
      # q_col_width <- 0.9
      
      

    ## plotdata for p plot (changed) ----
    plotData_p <- plotData
  
    plotData_p <- plotData_p %>% 
      select(Study, Species, Months, Dose, SM, Value, NOAEL, Value_order) %>% 
      #group_by(Study, Dose, SM) %>% 
      unique()
    plotData_p$SM <- lapply(plotData_p$SM, roundSigfigs)
    plotData_p$SM <- as.numeric(plotData_p$SM)
      
    
    if (nrow(plotData)>0) {
      plotData$Study <- factor(plotData$Study,levels= input$displayStudies)
      plotData_p$Study <- factor(plotData_p$Study,levels= input$displayStudies)
      plotData$DoseLabel <- factor(paste(plotData$Dose,'mg/kg/day'),levels=unique(paste(plotData$Dose,'mg/kg/day'))[order(unique(as.numeric(plotData$Dose),decreasing=F))])
      maxFindings <- 1
      for (doseFinding in plotData$doseFindings) {
        nFindings <- str_count(doseFinding,'\n')
        if (nFindings > maxFindings) {
          maxFindings <- nFindings
        }
      }
      maxFindings <- maxFindings + 1

      
      
      

      
      plotData$Findings <- as.factor(plotData$Findings)
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

  
      
# # Study vs safety margin plot  (changed) -------
      color_NOAEL <- c("TRUE" = "#239B56", "FALSE" = "black")
      
      tooltip_css <- "background-color:#3DE3D8;color:black;padding:2px;border-radius:5px;"
      
      
      p <- ggplot(plotData_p)+
      # suppressWarnings(geom_tile(aes (x = SM, y = Value_order, fill = NOAEL, text =paste("SM: ", SM)), 
      #             color = "transparent", width = p_tile_width(), height = p_tile_height))+
        
        
       geom_label_interactive(aes(x = SM, y = Value_order, label = paste(Dose, " mg/kg/day"), tooltip =paste0("SM: ", SM)), #DoseLabel changed
                  color = "white",
                  fontface = "bold",
                  size = 6,
                  fill= ifelse(plotData_p$NOAEL == TRUE, "#239B56", "black"),
                  label.padding = unit(0.6, "lines")
                  )+
        scale_x_log10(limits = c(min(axis_limit$SM/2), max(axis_limit$SM*2)))+
        #scale_fill_manual(values = color_NOAEL)+
        ylim(0,y_max)+
        facet_grid( Study ~ .)+
        labs( title = "      Summary of Toxicology Studies", x = "Safety Margin")+
        theme_bw(base_size=12)+
        theme(
          axis.title.y = element_blank(),
              axis.ticks.y= element_blank(),
              axis.text.y = element_blank(),
           
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(size= 20, hjust = 1),
              
              axis.title.x = element_text(size = 18, vjust = -0.9),
              axis.text.x = element_text(size = 16),
              legend.position = "none",
              strip.text.y = element_text(size=14, color="black"),
              strip.background = element_rect( fill = "white"))
      
      
      q <- ggplot(plotData)+
        geom_col_interactive(aes(x= Findings, y = Value, fill = Severity, group = Dose,  tooltip = Findings),
                 position = position_stack(reverse = TRUE),
                 color = 'transparent',
                 width = q_col_width)+
        geom_text_interactive(aes(x = Findings, y = Value, label = Dose, group = Dose),
                  size = 6,
                  color = 'white',
                  fontface = 'bold',
                  position = position_stack(vjust = 0.5, reverse = TRUE))+
        #scale_y_discrete(position = 'right')+
        ylim(0, q_y_max)+
        scale_fill_manual(values = color_manual)+
        facet_grid(Study ~ ., scales = 'free')+
        theme_bw(base_size=12)+
        theme(axis.title.y = element_blank(),
              strip.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(size= 16, angle = 90), #need to work
              #plot.title = element_text(size=20,hjust = 0.5),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_line(),
              panel.grid.minor.x = element_blank(),
              legend.text = element_text(size = 14),
              legend.title=element_text(size=16),
              legend.justification = "top")+
        #labs(title = '' )+
        guides(fill = guide_legend(override.aes = aes(label = "")))
      
      
      
      girafe(code = print(p+q+plot_layout(ncol = 2, widths = c(3,1))),
             options = list(opts_tooltip(css = tooltip_css)),
             fonts = list(sans= "Roboto"),
             width_svg = 18, height_svg = plotHeight())
      
      #q <- girafe(ggobj = q)
      # p + q + plot_layout(ncol=2,widths=c(3,1))
      
      #ggplotly(p, tooltip = "x")
      
      # p <- ggplotly(p, tooltip = c("text","text"), height = plotHeight()) %>% 
      #   plotly::style(showlegend = FALSE)
      # q <- ggplotly(q, tooltip = "text",  height = plotHeight()) #show warning though
      # 
      # subplot(p, q, nrows = 1, widths = c(0.7, 0.3), titleX = TRUE, titleY = TRUE) %>% 
      #   layout(title= "Summary of Toxicology Studies",
      #          xaxis = list(title = "Safety Margin"), 
      #          xaxis2 = list(title = ""))
      
    }
  })

  observe({
    req(input$selectData)
    values$selectData <- input$selectData
  })
  
  # output$menu function -----
  
  output$menu <- renderMenu({
    if (!is.null(input$selectData)) {
      if (input$selectData=='blankData.rds') {
        sidebarMenu(id='menu',
                    menuItem('Data Selection',icon=icon('database'),startExpanded = T,
                             uiOutput('selectData'),
                             conditionalPanel('input.selectData=="blankData.rds"',
                                              textInput('newApplication','Enter Application Number:')
                             ),
                             actionButton('saveData','Open New Application',icon=icon('plus-circle')),
                             br()
                    ),
                    br(),
                    uiOutput('studyName'),
                    br(),
                    br()
        )
      } else {
        sidebarMenu(id='menu',
                    menuItem('Data Selection',icon=icon('database'),startExpanded = T,
                             uiOutput('selectData'),
                             conditionalPanel('input.selectData=="blankData.rds"',
                                              textInput('newApplication','Enter Application Number:')
                             ),
                             actionButton('deleteData','Delete Application',icon=icon('minus-circle')),
                             br()
                    ),
                    hr(),
                    uiOutput('studyName'),
                    hr(),
                    menuItem('Clinical Data',icon=icon('user'),
                             checkboxGroupInput('clinDosing','Clinical Dosing:',clinDosingOptions),
                             conditionalPanel('condition=input.MgKg==false',
                                              numericInput('HumanWeight','*Human Weight (kg):',value=60)
                             ),
                             checkboxInput('MgKg','Dosing in mg/kg?',value=F),
                             conditionalPanel(
                               condition='input.clinDosing.includes("Start Dose")',
                               hr(),
              # hr line         
             
                               #tags$hr(style="height:3px;border-width:0;color:white;background-color:green"),
                               
                               h4('Start Dose Information:'),
                               conditionalPanel(condition='input.MgKg==true',
                                                numericInput('StartDoseMgKg','*Start Dose (mg/kg/day):',value=NULL)
                               ),
                               conditionalPanel(condition='input.MgKg==false',
                                                numericInput('StartDose','*Start Dose (mg/day):',value = NULL)
                               ),
                               numericInput('StartDoseCmax','Start Dose Cmax (ng/mL):',value=NULL),
                               numericInput('StartDoseAUC','Start Dose AUC (ng*h/mL):',value=NULL)
                             ),
                             conditionalPanel(
                               condition='input.clinDosing.includes("MRHD")',
                               hr(),
                               #tags$hr(style="height:3px;border-width:0;color:white;background-color:skyblue"),
                               
                               h4('MRHD Information:'),
                               conditionalPanel(condition='input.MgKg==true',
                                                numericInput('MRHDMgKG','*MRHD (mg/kg):',value=NULL)
                               ),
                               conditionalPanel(condition='input.MgKg==false',
                                                numericInput('MRHD','*MRHD (mg):',value = NULL)
                               ),
                               numericInput('MRHDCmax','MRHD Cmax (ng/mL):',value=NULL),
                               numericInput('MRHDAUC','MRHD AUC (ng*h/mL):',value=NULL)
                             ),
                             conditionalPanel(
                               condition='input.clinDosing.includes("Custom Dose")',
                               
                               
                            hr(),
                            #tags$hr(style="height:3px;border-width:0;color:white;background-color:white"),
                               
                               h4('Custom Dose Information:'),
                               conditionalPanel(condition='input.MgKg==true',
                                                numericInput('CustomDoseMgKG','*Custom Dose (mg/kg):',value=NULL)
                               ),
                               conditionalPanel(condition='input.MgKg==false',
                                                numericInput('CustomDose','*Custom Dose (mg):',value = NULL)
                               ),
                               numericInput('CustomDoseCmax','Custom Dose Cmax (ng/mL):',value=NULL),
                               numericInput('CustomDoseAUC','Custom Dose AUC (ng*h/mL):',value=NULL)
                             ),
                             actionButton('saveClinicalInfo','Save Clinical Information',icon=icon('plus-circle')),
                             br()
                    ),                   
                    menuItem('Nonclinical Data',icon=icon('flask'),tabName = 'Nonclinical Info',
                             uiOutput('selectStudy'),
                             actionButton('saveStudy','Save Study',icon=icon('plus-circle')),
                             actionButton('deleteStudy','Delete Study',icon=icon('minus-circle')),
                             
                           
                             
                             selectInput('Species','*Select Species:',choices=names(speciesConversion)),
                             textInput('Duration','*Study Duration/Description:'),
                             
                             h4('Study Name:'),
                             verbatimTextOutput('studyTitle'),
                             
                             hr(),
                             #tags$hr(style="height:3px;border-width:0;color:white;background-color:green"),
                             
                             numericInput('nDoses','Number of Dose Levels:',value=3,step=1,min=1),
                            
                             uiOutput('Doses'),
                             
                             hr(),
                             #tags$hr(style="height:3px;border-width:0;color:white;background-color:green"),
                             
                             numericInput('nFindings','Number of Findings:',value=0,step=1,min=0),
                            
                             uiOutput('Findings'),
                             br()
                    ),
                    hr(),
                    h6('* Indicates Required Fields')
                    
        )
      }
    } else {
      sidebarMenu(id='menu',
                  menuItem('Data Selection',icon=icon('database'),startExpanded = T,
                           uiOutput('selectData'),
                           conditionalPanel('input.selectData=="blankData.rds"',
                                            textInput('newApplication','Enter Application Number:')
                           ),
                           actionButton('saveData','Open New Application',icon=icon('plus-circle')),
                           br()
                  ),
                  br(),
                  uiOutput('studyName'),
                  br(),
                  br()
      )
    }
  })
}


# ui function ------
ui <- dashboardPage(
  
  

  dashboardHeader(title="Nonclinical Summary Tool",titleWidth = 250),
 

  
  
  dashboardSidebar(width = 250,
                   sidebarMenuOutput('menu'),
                   tags$head(
                     tags$style(
                       HTML(".sidebar {height: 94vh; overflow-y: auto;}")
                     )
                   )
                   
  ),
  
  
  
  dashboardBody(
    

    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    # ),

    fluidRow(
      column(4,
             uiOutput('humanDosing')
      ),
      column(4,
             conditionalPanel(
               'input.clinDosing != null && input.clinDosing != ""',
               selectInput('SMbasis','Base Safety Margin on:',c('HED','Cmax','AUC'))
             )
      ),
      column(4,
             uiOutput('displayStudies')
      )
    ),
    conditionalPanel(
      condition='input.selectData!="blankData.rds"',
      tabsetPanel(
        
        
        tabPanel('Figure',
                 actionButton('refreshPlot','Refresh Plot'),
                 br(),
                
                 fluidRow(
                   column(4,
                 div(style = "display:inline-block;vertical-align:top; width: 215px;", selectInput("NOAEL_choices", "NOAEL", choices = c("ALL", "Less than or equal to NOAEL", "Greater than NOAEL"),
                             selected = "ALL"))),
            
                 # column(4,
                 # div(style = "display:inline-block;vertical-align:top; width: 215px;", sliderInput("textbox", h5("Adjust Text Box"), min = 0.2, max = 0.9, value = 0.2))),
                 
                 column(4, 
                 div(style = "display:inline-block;vertical-align:top; width: 215px;", sliderInput("plotheight", h5("Adjust Plot Height"), min = 2, max = 20, value = 8)))),
                
                 br(),
                 withSpinner(girafeOutput('figure'))
        ),
        
        
  
        
      tabPanel("Table_01",
               DT::dataTableOutput('table_01'),
               h4("For Downloading in docx file click link below"),
               downloadButton("down_01_doc", "Docx file download")
      ),
      tabPanel("Table_02",
               DT::dataTableOutput('table_02'),
               h4("For Downloading in docx file click link below"),
               downloadButton("down_02_doc", "Docx file download")
               
               
      ),
      
      tabPanel("Table_03",
               DT::dataTableOutput('table_03'),
               h4("For Downloading in docx file click link below"),
               downloadButton("down_03_doc", "Docx file download")
               
      ),
      
      tabPanel("Table_All",
               downloadButton("down_all", "Docx file download"))
  ))))



# app running function ----

shinyApp(ui = ui, server = server)
