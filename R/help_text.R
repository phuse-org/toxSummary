
guide_01 <- cicerone::Cicerone$new()$step(
    el = "help_application",
    title = "Read Carefully",
    description = "Follow the step to create New Application, Now Hit Next"
)$step(
    el = "select_Data",
    title = "Step: 01",
    description = "To create a new application, make sure <strong>New Application</strong> selected here",
    position = "right"
)$step(
    el = "newApplication",
    title = "Step: 2",
    description = "Type an application number in the text box",
    position = "right"
)$step(
    el = "saveData",
    title = "Step: 3",
    description = "Hit the <strong>Submit</strong> button to create the application",
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
    " selected and more option to enter  relevant data will be shown below. "),
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

guide_03 <- cicerone::Cicerone$new(allow_close = FALSE)$step(
  el = "non_clin_page_application",
    title = "Current Application",
    description = paste0("This is the application you selected to work on. ",
    "If this is New Application, go back to Application Tab and ",
    "create your application first"),
    position = "right"
)$step(
     el = "selectStudy-label",
    title = "Select Study",
    description = paste0("To add a new study to the application, select ",
    "<strong> New Study.</strong> There are two workflow: <strong>01.</strong> ",
    "Automated Data Entry <strong>02.</strong> Manual Data Entry. <br>",
    "Let's start with automated data entry. Hit Next"),
    position = "right"

)$step(
     el = "ind_id-selectized",
    title = "Select IND",
    description = paste0("If you click on <strong>Choose</strong> it will show a long list",
    " of IND number. Best way to find the ind number by typing the number here. You don't need",
    " to type <strong>IND</strong> keyword. Just typing the number will show you the search",
     " result. If list contain IND number it  should be on top. Click the number to select. When you",
     " select IND number, another option <strong>Select StudyID</strong> will appear below. You",
     " need to select one study from the list",
     "For the next step, let's assume you selected IND number and StudyID. ",
      "In case IND not availabe  in database",
     " it will not show up. In that case you need to enter data manually. I will show how to enter data manually later. Now hit next"),
    position = "right"

)$step(
     el = "Species-label",
    title = "Species",
    description = paste0("When  you select StudyID, this species will updated from study data"),
    position = "right"

)$step(
     el = "which_sex",
    title = "Select Sex",
    description = paste0("This checkbox  will be updated from study data.",
    "If study have Male and Female animals then both option here will be checked.",
    " But you only want to include  Female animals. To exclude  Male  animal you can just uncheck the <strong>M</strong> box",
    " If study have only Female  or Male animals, only that checkbox will be available"
    ),
    position = "right"

)$step(
     el = "Duration",
    title = "Keep it short",
    description = paste0("This text box will be filled out with the study title by default.",
    " But you can edit the text. Sometime the title are very long. ",
    "We suggest that you edit to keep it short and meaningful to you.  ",
    " For example, you can include animal sex, visit Day, AUC parameter and recovery period "),
    position = "right"

)$step(
     el = "studyTitle",
    title = "Name",
    description = paste0("This will be final name of the study. This is generated by adding species",
    " to the  study description you entered in previous box."),
    position = "right"

)$step(
     el = "auc_db-label",
    title = "AUC",
    description = paste0("This will show list of available AUC parameter. If AUCLST available then AUCLST",
    " will be selected by default. If AUCLST not availabel then very first one will be selected",
    " by default. If this is empty after you selected study, that mean this study does not have PP domain"),
    position = "right"

)$step(
     el = "pp_visitday-label",
    title = "Visit Day",
    description = paste0("All the available days will be shown here. By default, all days are included.",
    "You can remove any day by clicking <strong>X</strong> sing beside number.",
    " If this is empty after you selected study, that mean this study does not have PP domain"),
    position = "right"

)$step(
     el = "get_from_db",
    title = "Get the Data",
    description = paste0("When you click this button, it will get all the data from database.",
    "This  will also update Number",
    "If AUC and Visit dat empty, it will only get  dose  information",
     "It may take few ",
    "seconds for first time but it will be lot faster in next query"),
    position = "right"

)


#$step(
#      el = "",
#     title = "",
#     description = paste0(""),
#     position = "right"

# )$step(
#      el = "",
#     title = "",
#     description = paste0(""),
#     position = "right"

# )$step(
#      el = "",
#     title = "",
#     description = paste0(""),
#     position = "right"

# )$step(
#      el = "",
#     title = "",
#     description = paste0(""),
#     position = "right"

# )$step(
#      el = "",
#     title = "",
#     description = paste0(""),
#     position = "right"

# )$step(
#      el = "",
#     title = "",
#     description = paste0(""),
#     position = "right"

# )$step(
#      el = "",
#     title = "",
#     description = paste0(""),
#     position = "right"

# )$step(
#      el = "",
#     title = "",
#     description = paste0(""),
#     position = "right"

# )$step(
#      el = "",
#     title = "",
#     description = paste0(""),
#     position = "right"

# )$step(
#      el = "",
#     title = "",
#     description = paste0(""),
#     position = "right"

# )$step(
#      el = "",
#     title = "",
#     description = paste0(""),
#     position = "right"

# )

