
# clinical data modal
clinical_data_modal <- function() {
     modalDialog(
         checkboxGroupInput(
             "clinDosing", "Clinical Dosing:",
             clinDosingOptions
         ),
         conditionalPanel(
             "condition=input.MgKg==false",
             numericInput("HumanWeight", "*Human Weight (kg):",
                 value = 60, min = 0
             )
         ),
         checkboxInput("MgKg", "Dosing in mg/kg?", value = F),
         conditionalPanel(
             condition = 'input.clinDosing.includes("Start Dose")',
             hr(),
             h4("Start Dose Information:"),
             conditionalPanel(
                 condition = "input.MgKg==true",
                 numericInput("StartDoseMgKg", "*Start Dose (mg/kg/day):",
                     value = NULL, min = 0
                 )
             ),
             conditionalPanel(
                 condition = "input.MgKg==false",
                 numericInput("StartDose", "*Start Dose (mg/day):",
                     value = NULL, min = 0
                 )
             ),
             uiOutput("start_cmax"),
             numericInput("StartDoseCmax", NULL, value = NULL, min = 0),
             uiOutput("start_auc"),
             numericInput("StartDoseAUC", NULL, value = NULL, min = 0)
         ),
         conditionalPanel(
             condition = 'input.clinDosing.includes("MRHD")',
             hr(),
             h4("MRHD Information:"),
             conditionalPanel(
                 condition = "input.MgKg==true",
                 numericInput("MRHDMgKg", "*MRHD (mg/kg):",
                     value = NULL, min = 0
                 )
             ),
             conditionalPanel(
                 condition = "input.MgKg==false",
                 numericInput("MRHD", "*MRHD (mg):", value = NULL, min = 0)
             ),
             uiOutput("MRHD_cmax"),
             numericInput("MRHDCmax", NULL, value = NULL, min = 0),
             uiOutput("MRHD_auc"),
             numericInput("MRHDAUC", NULL, value = NULL, min = 0)
         ),
         conditionalPanel(
             condition = 'input.clinDosing.includes("Custom Dose")',
             hr(),
             h4("Custom Dose Information:"),
             conditionalPanel(
                 condition = "input.MgKg==true",
                 numericInput("CustomDoseMgKg", "*Custom Dose (mg/kg):",
                     value = NULL, min = 0
                 )
             ),
             conditionalPanel(
                 condition = "input.MgKg==false",
                 numericInput("CustomDose", "*Custom Dose (mg):",
                     value = NULL, min = 0
                 )
             ),
             uiOutput("custom_cmax"),
             numericInput("CustomDoseCmax", NULL, value = NULL, min = 0),
             uiOutput("custom_auc"),
             numericInput("CustomDoseAUC", NULL, value = NULL, min = 0)
         ),
         actionButton("saveClinicalInfo", "Save Clinical Information",
             icon = icon("plus-circle")
         ),
         br(),
         footer = tagList(
             tags$h4("Please save the Clinical Data before close",
                 style = "color:#E31616;"
             ),
             modalButton("Close")
         )
     )
 }


# nonclinical

data_modal <- function() {
    
    modalDialog(
      uiOutput('selectStudy'),
      br(),
      actionButton('saveStudy','Save Study',icon=icon('plus-circle'),
                   style = "
                   background-color: white;
                   border: 2px solid #4CAF50;"),
      
      actionButton('deleteStudy','Delete Study',icon=icon('minus-circle'),
                   style = "
                   background-color: white;
                   border: 2px solid #FF0000;"),
      br(),
      br(),
	  htmltools::tags$hr(style="border-top: 3px solid#1e9acd;"),
	      
      shiny::selectizeInput(inputId = "ind_id",
                            label = tags$div(HTML('<i class="fa fa-folder-open" style = "color:#000000;font-size:18px;"></i> Select IND')),
                            selected = NULL,
                            choices = c(Choose = '', ind_number_list),
                            options = list(maxOptions = 1500)),
	    br(),
       br(),
	  
	        shiny::selectizeInput(inputId = "studyid",
                            # label= "Select StudyID",
                            label = tags$div(HTML('<i class="fa fa-database" style = "color:#000000;font-size:18px;"></i> Select StudyID')),
                            selected = NULL,
                            choices = c(Choose = '', sd_id),
                            options = list(maxOptions = 5000)), 
	  
	  br(), 
      br(),
      selectInput('Species',
                  label = tags$div(HTML('<i class="fa fa-dog" style = "color:#724028d9;font-size:18px;"></i> *Select Species:')),
                  choices=names(speciesConversion)),
				  br(), 
				  br(),
				  selectInput('which_sex',
                  label = tags$div(HTML('<i class="fa fa-venus" style = "color:#943488d9;font-size:18px;"></i> *Select Sex:')),
                  choices=c("ALL", "M", "F")),
      textAreaInput('Duration','*Study Duration/Description:', height="100px"),
      h4('Study Name:'),
      verbatimTextOutput('studyTitle'),
      hr(style="border-top: 3px solid#1e9acd;"),
      

      uiOutput('choose_auc'),
      checkboxInput(inputId = 'get_from_database', label = 'Populate from Database', value = FALSE),
    #   shiny::actionButton(inputId = "get_from_database",
    #   label = "Populate From Database",
	#   style = "background-color: white; 
	#   border: 2px solid #4CAF50;"),
	  
      
      numericInput('nDoses',
                   label = tags$div(HTML('<i class="fa fa-syringe" style = "color:#169abbd9;font-size:18px;"></i> *Number of Dose Levels:')),
                   value=1,step=1,min=1),
      #numericInput('nDoses','*Number of Dose Levels:',value=1,step=1,min=1),
      uiOutput('Doses'),
      hr(style="border-top: 3px solid#1e9acd;"),
      
      numericInput('nFindings',
                   label = tags$div(HTML('<i class="fa fa-microscope" style = "color:#940aebd9;font-size:18px;"></i> *Number of Findings:')),
                   value=1,step=1,min=1),
      uiOutput('Findings'),
	  htmltools::tags$hr(style="border-top: 2px solid#1e9acd;"),
      checkboxInput("notes", "Notes for Study?", value = FALSE),
      uiOutput("study_note"),
      actionButton('saveStudy_02','Save Study',icon=icon('plus-circle'),
                   style = "
                   background-color: white;
                   border: 2px solid #4CAF50;"
                   ),
      
      
      

      
      footer = tagList(
        tags$h4("Please save the study before close", style="color:#E31616;"),
        modalButton("Close")
        
      )
      
    )
    
    
  }