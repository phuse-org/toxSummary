
guide_01 <- cicerone::Cicerone$new()$step(
    el = "help_application",
    title = "Read Carefully",
    description = "Follow the step to create New Application, Now Hit Next"
)$step(
    el = "select_Data",
    title = "Step: 01",
    description = "To create a new application, make sure 'New Application' selected here",
    position = "right"
)$step(
    el = "newApplication",
    title = "Step: 2",
    description = "Type an application number in the text box",
    position = "right"
)$step(
    el = "saveData",
    title = "Step: 3",
    description = "Hit the submit button to create the application",
    position = "right"
)$step(
    ".nav",
    title = "Step: 4",
    description = "Click Edit Clinical Tab \U1F446 and go to edit clinical page. You will enter clinical info there",
    is_id = FALSE,
    position = "bottom"
)$step(
    el = "reload_app",
    title = "Some tips",
    description = paste0(
        "General workflow of the app: 01. You create an application in this page. ",
        "02. Enter  clinical information in Edit clinical page (don't forget to hit save button). ",
        "03. Enter Nonclinical information in Nonclinical Page (don't forget to hit save button). "  ,
        "about Relaod Button: ",
        "There are situations when you might need to hit reload button. ",
        "If you decide to reload the app, make sure you saved you data \U1F64F.",
        " All unsaved data will be lost \U1F61F. You have to enter again. ",
        "App might get disconnected after some idle time. ",
        "You will need to hit reload button to restart the app. ",
        "If reload does not work, restart your browser. ",
        "Google Chrome is the preferred browser. "
        
    )
)

guide_02 <- cicerone::Cicerone$new()$step(
    el = "clin_page_application",
    title = "Current Application",
    description = paste0("This is the application you selected to work on. ",
    "If this is New Application, go back to previous page (Application Tab) and ",
    "create your application first"),
    position = "bottom"

)$step(
    el = "clinDosing",
    title = "Clinical Dosing",
    description = paste0("There are three checkbox. When you  click on any rectangular box that  would be",
    " selected and more option to enter  relevant data will be shown below. ", 
    ""),
    position = "right"

)$step(
    el = "HumanWeight",
    title = "Human Weight",
    description = paste0("Default clinical dose in mg/day ",
    "and default human weight is 60 kg.  App convert mg/day  to mg/kg/day.", 
    " You can change default human weight by clicking  up and down a arrow. ",
    "If dose in mg/kg/day already then check the Dosing in mg/kg? checkbox"),
    position = "right"
)$step(
    el = "saveClinicalInfo",
    title = "Save before you move",
    description = paste0("Please save before you go to next section"),
    position = "right"

)$step(
     el = "cmax_unit-label",
    title = "Default Cmax and AUC unit",
    description = paste0("This is default Cmax and AUC unit for clinical dose. ",
    "You need to  click Save Units  button if  you change  any of them"),
    position = "right"
)$step(
       ".nav",
    title = "Next Page",
    description = paste0("Go to nonclinical page by clicking \U1F446 edit nonclinical tab. ",
     "You will enter nonclinical info there."),
    is_id = FALSE,
    position = "bottom"

)