library(shiny)
library(DBI)
library(data.table)
library(RSQLite)
source("get_dose_pp.R")


db_path <- "C:/Users/Md.Ali/not_in_onedrive/CDER_SEND.db"

conn <-RSQLite::dbConnect(drv = SQLite(),db_path)
sd_id <- RSQLite::dbGetQuery(conn=conn, 'SELECT DISTINCT STUDYID FROM TX')
sd_id <- data.table::as.data.table(sd_id)
#names(sd_id) <- ""
#SELECT DISTINCT PPTESTCD FROM PP WHERE STUDYID=='8001964' AND PPTESTCD LIKE '%auc%'
#RSQLite::dbGetQuery(conn = conn, 'SELECT * FROM POOLDEF WHERE STUDYID==:x', params = list(x=studyid))


ui <- shiny::fluidPage(
  title = "Dose and PK automation",
  shiny::titlePanel("dose and pk value extraction"),
  
  shiny::sidebarLayout(
    
    shiny::sidebarPanel(
      shiny::selectizeInput(inputId = "studyid",
                         label= "Select StudyID",
                         selected = NULL,
                         choices = NULL), 
      shiny::uiOutput("choose_auc"),

      
      shiny::uiOutput("dose")
    ),
    shiny::mainPanel(
      
      shiny::tableOutput("doseTable")
      
    )
  )
)


server <- function(input, output, session) {
  
 
  
  updateSelectizeInput(session = session, inputId = 'studyid', choices = c(Choose = '', sd_id),
                       server = TRUE,
                       options = list(maxOptions = 5000))
  

  output$choose_auc <- shiny::renderUI({
    
    auc_list <- RSQLite::dbGetQuery(conn=conn, 'SELECT DISTINCT PPTESTCD FROM PP WHERE STUDYID=:x AND PPTESTCD LIKE "%auc%"',
                                    params=list(x=input$studyid))
    # print(input$studyid)
    # print("----")
   
    auc_list <- data.table::as.data.table(auc_list)
    #print(auc_list)
 

    shiny::selectizeInput(inputId = "auc", 
                          label="Select AUC parameter",
                          selected= NULL,
                          choices= c(Choose="", auc_list))
  })
  

  
  
  
  get_dose_pk_for_study <- shiny::reactive({
    
    if (!is.null(input$studyid) & !is.null(input$auc)) {
    df <- get_pk_param(conn=conn, input$studyid, pk_param = input$auc )
    df
    }
    
    
  })
  
  
  
  output$dose <- renderUI({
    df <- get_dose_pk_for_study()
    n_dose <-   length(unique(df[,TRTDOS]))

    cmax <- df[PPTESTCD=="CMAX"]
    auc <- df[PPTESTCD!="CMAX"]
    # 
    # for (i in seq_along(1:3)) {
    #   dose <- cmax[i,.(TRTDOS)]
    #   cmax_value <- cmax[i,  .(mean)]
    #   auc_value <- auc[i,.(mean)]
    #   print(dose)
    #   print(cmax_value)
    #   print((auc_value))
    # }
    
    # lapply(1:n_dose, function(i) {
    #   div(style="display: inline-block;vertical-align:top; width: 115px;",
    #   shiny::numericInput(inputId = paste0(input$auc,i), label = paste0(input$auc,i), value = auc[i,.(mean)]))
    #   
    # })
    
    lapply(1:(4*n_dose), function(i) {
      I <- ceiling(i/4)
      #doseName <- names(studyData$Doses)[I]
      if (i %% 4 == 1) {
        div(hr(style = "border-top: 1px dashed skyblue"),
            numericInput(paste0('dose',I),paste0('*Dose ',I,  " ",cmax[I, .(TRTDOSU)], ":"), min=0, value = cmax[I, .(TRTDOS)]))
      } else if (i %% 4 == 2) {
        div(style="display: inline-block;vertical-align:top; width: 115px;",
            numericInput(paste0('Cmax',I),paste0('Cmax ',I, " ", cmax[I, .(PPSTRESU)], ":"), min=0, value=cmax[I, .(mean)]))
      }
      else if (i %% 4 == 3) {
        div(style="display: inline-block;vertical-align:top; width: 115px;",
            numericInput(paste0("AUC",I),paste0(input$auc, " ",I, " ",auc[I, PPSTRESU], ":"), min=0, value=auc[I, .(mean)]))
        
      }
      # else {
      #   div(checkboxInput(paste0('NOAEL',I),'NOAEL?',value= T))
      # }
    })
    
    
    
  })
  
output$doseTable <- shiny::renderTable({
  req(input$studyid)
  req(input$auc)
  df <- get_dose_pk_for_study()
  df
  
  
})
  
  

shiny::onSessionEnded(function() {
  RSQLite::dbDisconnect(conn = conn)
})

}

shiny::shinyApp(ui,server)


# https://stackoverflow.com/questions/38438920/shiny-selectinput-very-slow-on-larger-data-15-000-entries-in-browser