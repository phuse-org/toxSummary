
guide_01 <- cicerone::Cicerone$new(allow_close = FALSE)$step(
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
        "General workflow of the app: <br> 01. You create an application in this page. <br>",
        "02. Enter  clinical information in Edit clinical page (don't forget to hit save button). <br> ",
        "03. Enter Nonclinical information in Nonclinical Page (don't forget to hit save button). <br>"  ,
        "about Relaod Button: ",
        "There are situations when you might need to hit reload button. ",
        "If you decide to reload the app, make sure you saved you data \U1F64F.",
        " All unsaved data will be lost \U1F61F. You have to enter again. ",
        "App might get disconnected after some idle time. ",
        "You will need to hit reload button to restart the app. ",
        "If reload does not work, restart your browser. <br>",
        "Google Chrome is the preferred browser for the app. "
        
    )
)$step(
    el = "refreshPlot",
    title = "Plot Options",
    description = paste("Let's assume you already created an application and added a study. That will create plot on this page.",
    "Next few options will help you to customize the plot."),
    position = "right"
)$step(
    el = "humanDosing-label",
    title = "Plot Options",
    description = paste("You can change Clinical Dosing options here."),
    position = "left"
)$step(
    el = "SMbasis-label",
    title = "SM Basis",
    description = paste("You can change how exposure margin will be calculated."),
    position = "left"
)$step(
    el = "displayStudies-label",
    title = "Display Studies",
    description = paste("you can select or remove study from the plot here.  ", 
    "You can also drag and rearrange the order of the study in the plot."),
    position = "left"
)$step(
    el = "displayFindings-label",
    title = "Display Findings",
    description = paste("you can select or remove findings from the plot here.", 
    "You can also drag and rearrange the order of the findings in the plot."),
    position = "left"
)$step(
    el = "NOAEL_choices-label",
    title = "NOAEL Choices",
    description = paste("You can filter NOAEL dose or greater than NOAEL dose from the plot here. "),
    position = "left"
)$step(
    el= "dose_sm-label",
    title = "Dose SM",
    description = paste("You can change the option here to see dose with exposure margin", 
     "or notes on the plot."),
    position = "left"
)$step(
    el = "plotheight-label",
    title = "Plot Height",
    description = paste("You can change the plot height here. "),
    position = "left"
)$step(
    el = "text_width-label",
    title = "Text Width",
    description = paste("You can change the text width here. sometimes text is too long to fit in the right side of the plot."),
    position = "left"
)


guide_02 <- cicerone::Cicerone$new(allow_close = FALSE)$step(
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
    description = paste0("This is the application you selected to work on. <br> ",
    "If, selected application: <strong>New Application</strong> , <br> go back to Application tab and ",
    "create your application first."),
    position = "right"
)$step(
     el = "selectStudy-label",
    title = "Select Study",
    description = paste0("To add a new study to the application, select ",
    "<strong> New Study.</strong> There are two workflow for nonclinical data entry: <br> <strong>01.</strong> ",
    "Automated Data Entry <br> <strong>02.</strong> Manual Data Entry. <br>",
    "Let's start with <br>  <strong>workflow 01: automated data entry.</strong> <br> Hit Next"),
    position = "right"

)$step(
     el = "ind_id-label",
    title = "Select IND",
    description = paste0("If you click on dropdown menu it will show a long list",
    " of IND number. Best way to find the ind number by typing the number here. You don't need",
    " to type <strong>IND</strong> keyword. Just typing the number will show you the search",
     " result. If this list contain IND number you are searching, it  should be on top. Click the number to select. When you",
     " select IND number, another option <strong>Select StudyID</strong> will appear below. You",
     " need to select one study from the list. ",
     "For the next step, let's assume you selected IND number and StudyID. ",
      "In case IND not availabe  in database",
     " it will not show up in search. In that case you need to enter data manually. I will show how to enter data manually later. ",
     "<br> Now hit next"),
    position = "right"

)$step(
     el = "Species-label",
    title = "Species",
    description = paste0("After you select Study, this species will be updated from study data"),
    position = "right"

)$step(
     el = "which_sex",
    title = "Select Sex",
    description = paste0("This checkbox  will be updated from study data.",
    "If study have Male and Female animals, then both option here will be checked.",
    "If study have only Female  or Male animals, only that checkbox will be available. <br>",
    "<strong>Check</strong> box to <strong> include </strong> <br>",
    "<strong>Uncheck </strong> box to <strong> exclude </strong>"

    ),
    position = "right"

)$step(
     el = "Duration",
    title = "Keep it short",
    description = paste0("This text box will be filled out with the study title by default.",
    " But you can edit the text. Sometime the title are very long. ",
    "We suggest that you edit to keep it short and meaningful to you.  ",
    " For example, you can include animal sex, visit Day, AUC parameter and recovery period. "),
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
    " will be selected by default. If AUCLST not available, then very first one will be selected",
    " by default. <br> If this is empty after you selected study, that mean this study does not have PP domain"),
    position = "top"

)$step(
     el = "pp_visitday-label",
    title = "Visit Day",
    description = paste0("All the available days will be shown in below box. By default, all days are included.",
    " You can remove any day by clicking <strong>X</strong> sign beside number.",
    " If this is empty after you selected study, that mean this study does not have PP domain"),
    position = "top"

)$step(
     el = "get_from_db",
    title = "Get the Data",
    description = paste0("When you click this button, it will get all the data from database.",
    "This  will also update Number of Doses in next column and will update all the Doses, Cmax and AUC  values.",
    " If AUC and Visit day empty, it will only get  dose  information. ",
    "This button label right now <strong>Click to Update</strong>. When you click the button, label will",
     " be changed to <strong>Updated</strong>. ",
     " If you change anything from IND, StudyID, Sex, AUC, Visit Day then button label will be changed to ",
     "<strong>Click to Update</strong> and you need to click again to update the data. <br> ",
     " It may take few ",
    "seconds to load the data for first time but it will be lot faster in next query."),
    position = "top"

)$step(
     el = "nDoses-label",
    title = "Doses",
    description = paste0("Number of Doses will be updated from study data."),
    position = "right"

)$step(
     el = "Doses",
    title = "About Each Dose",
    description = paste0(
    " Dose, Cmax and AUC value will be  updated",
    " from study data if available "),
    position = "right"

)$step(
     el = "NOAEL1",
    title = "NOAEL?",
    description = paste0("NOAEL information not in the database. So You need to check manually",
    " if this dose is NOAEL. <br>", 
    " <strong> checked: </strong> means <strong> NOAEL dose</strong>",
    " <strong> unchecked: </strong> means <strong> not NOAEL dose</strong>"),
    position = "right"

)$step(
     el = "show_unit_in_nonclinical",
    title = " Units",
    description = paste0("These are units for clinical dose entered in previous page."),
    position = "right"

)$step(
     el = "nFindings-label",
    title = "Number of Findings",
    description = paste0("You need to enter how many findings are there for the study. <br> ",
    " You can type or use",
     " up and down arrow to change the number. <br> Note: Findings information not in database so it will not be",
     " updated automatically"),
    position = "left"

)$step(
     el = "Findings",
    title = "Findings",
    description = paste0("Here you need to enter finding information. <br>",
    "Hit next to see how to complete this section. "),
    position = "left"

)$step(
     el = "Finding1-label",
    title = "Finding",
    description = paste0("Type the finding in the text box. After you type, <br> you need to click ",
    "<strong>Add</strong> appeared below or hit <strong>Enter</strong> in keyboard. "),
    position = "left"

)$step(
     el = "Reversibility1",
    title = "Choose One",
    description = paste0("Click on term that match with the  finding"),
    position = "left"

)$step(
     el = "Severity1_1-label",
    title = "Severity",
    description = paste0("Select severity from dropdown list. Default is Absent."),
    position = "left"

)$step(
     el = "notes",
    title = "note?",
    description = paste0("This is Optional: <br> If you check this box, there will be an text input area.",
    " You can type particular note for this study. There will be a table generated from all study notes ",
    " for this application.", " Notes also can be displayed on the plot."),
    position = "left"

)$step(
     el = "saveStudy_02",
    title = "Don't Forget to SAVE",
    description = paste0("Hit the <strong>Save Study</strong> button to save your study. ",
    "There are two button to save the study. Two button does the same thing. <br> Hit next to see other save button"),
    position = "left"

)$step(
     el = "saveStudy",
    title = "Don't Forget to SAVE",
    description = paste0("Here is other Save button."),
    position = "right"

)$step(
     el = "deleteStudy",
    title = "Delete the Study",
    description = paste0("First select the study you want to delete. Then click <strong>Delete Study</strong> button.",
    "There will be a popup dialogbox just to confirm you really want to delete this study. <br>",
    " On popup dialogbox: <br> If you want to delete then hit <strong>Delete</strong>, <br> if you  don't want to delete hit <strong>Cancel</strong>"),
    position = "right"

)$step(
    el = "help_nonclinical_02",
    title = "Workflow 02: Manual Data Entry",
    description = paste0("If your study not available in database and you want to add study manually, click", 
    " this button to see the steps.")
)

guide_04 <- cicerone::Cicerone$new(allow_close = FALSE)$step(
  el = "non_clin_page_application",
    title = "Current Application",
    description = paste0("This is the application you selected to work on. ",
    "If selected application: <strong>New Application</strong> , <br> go back to Application tab and ",
    "create your application first"),
    position = "right"
)$step(
     el = "selectStudy-label",
    title = "Select Study",
    description = paste0("To add a new study to the application, select ",
    "<strong> New Study.</strong>"),
    position = "right"

)$step(
     el = "Species-label",
    title = "Species",
    description = paste0("From the dropdown menu,  select the species, <br>",
    "Default is Rat. "),
    position = "right"

)$step(
     el = "which_sex",
    title = "Select Sex",
    description = paste0("<strong>Check</strong> box to <strong> include </strong> <br>",
    "<strong>Uncheck </strong> box to <strong> exclude </strong>"

    ),
    position = "right"

)$step(
     el = "Duration",
    title = "Keep it short",
    description = paste0("You can type short description for the study. This will be use to create your study name"),
    position = "right"

)$step(
     el = "studyTitle",
    title = "Name",
    description = paste0("This will be final name of the study. This is generated by adding species",
    " to the  study description you entered in previous box."),
    position = "right"

)$step(
     el = "nDoses-label",
    title = "Doses",
    description = paste0("You can type or use up and down arrow to change the value"),
    position = "right"

)$step(
     el = "Doses",
    title = "About Each Dose",
    description = paste0(
    " You need to enter the dose, Cmax and AUC value manually. <br>",
    "Dose is mandatory, but Cmax and AUC value are optional. "),
    position = "right"

)$step(
     el = "NOAEL1",
    title = "NOAEL?",
  description = paste0("NOAEL information not in the database. So You need to check manually",
    " if this dose is NOAEL. <br>", 
    " <strong> checked: </strong> means <strong> NOAEL dose</strong>",
    " <strong> unchecked: </strong> means <strong> not NOAEL dose</strong>"),
    position = "right"

)$step(
     el = "show_unit_in_nonclinical",
    title = " Units",
    description = paste0("These are units for clinical dose entered in previous page."),
    position = "right"

)$step(
     el = "nFindings-label",
    title = "Number of Findings",
    description = paste0("You need to enter how many findings are there for the study. <br> ",
    " You can type or use",
     " up and down arrow to change the number. <br> "),
    position = "left"

)$step(
     el = "Findings",
    title = "Findings",
    description = paste0("Here you need to enter finding information. <br>",
    "Hit next to see how to complete this section. "),
    position = "left"

)$step(
     el = "Finding1-label",
    title = "Finding",
      description = paste0("Type the finding in the text box. After you type, <br> you need to click ",
    "<strong>Add</strong> appeared below or hit <strong>Enter</strong> in keyboard. "),
    position = "left"

)$step(
     el = "Reversibility1",
    title = "Choose One",
    description = paste0("Click on term that match with the  finding"),
    position = "left"

)$step(
     el = "Severity1_1-label",
    title = "Severity",
    description = paste0("Select severity from dropdown list. Default is Absent."),
    position = "left"

)$step(
     el = "notes",
    title = "note?",
    description = paste0("This is Optional: <br> If you check this box, there will be an text input area.",
    " You can type particular note for this study. There will be a table generated from all study notes ",
    " for this application.", " Notes also can be displayed on the plot."),
    position = "left"

)$step(
     el = "saveStudy_02",
    title = "Don't Forget to SAVE",
    description = paste0("Hit the <strong>Save Study</strong> button to save your study. ",
    "There are two button to save the study. Two button does the same thing. <br> Hit next to see other save button"),
    position = "left"

)$step(
     el = "saveStudy",
    title = "Don't Forget to SAVE",
    description = paste0("Here is other Save button."),
    position = "right"

)$step(
     el = "deleteStudy",
    title = "Delete the Study",
    description = paste0("First select the study you want to delete. Then click <strong>Delete Study</strong> button.",
    "There will be a popup dialogbox just to confirm you really want to delete this study. <br>",
    " On popup dialogbox: <br> If you want to delete then hit <strong>Delete</strong>, <br> if you  don't want to delete hit <strong>Cancel</strong>"),
    position = "right"

)


# guide_05 <- cicerone::Cicerone$new(allow_close = FALSE)$step(
#     el = "humanDosing-label",
#     title = "Human Dosing",
#     description = paste("You can change Clinical Dosing options here. <br> "),
#     position = "left"
# )$step(
#     el = "SMbasis-label",
#     title = "SM Basis",
#     description = paste("You can change SM Basis options here. <br> "),
#     position = "left"
# )$step(
#     el = "displayStudies-label",
#     title = "Display Studies",
#     description = paste("you can select or remove strudy from the plot here.  ", 
#     "You can also drag and rearrange the order of the study in the plot."),
#     position = "left"
# )$step(
#     el = "displayFindings-label",
#     title = "Display Findings",
#     description = paste("you can select or remove findings from the plot here.", 
#     "You can also drag and rearrange the order of the findings in the plot."),
#     position = "left"
# )$step(
#     el = "NOAEL_choices-label",
#     title = "NOAEL Choices",
#     description = paste("You can filter NOAEL dose or greater than NOAEL dose from the plot here. "),
#     position = "left"
# )$step(
#     el= "dose_sm-label",
#     title = "Dose SM",
#     description = paste("You can change the option here to see dose with exposure margin", 
#      "or notes on the plot."),
#     position = "left"
# )$step(
#     el = "plotheight-label",
#     title = "Plot Height",
#     description = paste("You can change the plot height here. "),
#     position = "left"
# )$step(
#     el = "text_width-label",
#     title = "Text Width",
#     description = paste("You can change the text width here. sometimes text is too long to fit in the right side of the plot."),
#     position = "left"
# )
