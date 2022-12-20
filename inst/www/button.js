/*
################################################################################
## some helper js code for toxSummary shiny app      
## 
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2021-04-28   Yousuf Ali           Initial version
################################################################################
*/
Shiny.addCustomMessageHandler("mymessage", function (message) {
  document.getElementById(message).click();
});

Shiny.addCustomMessageHandler("save_clin_info", function(message) {
	document.getElementById(message).click()
})


Shiny.addCustomMessageHandler("confirm_study_remove", function(message) {
	document.getElementById(message).click()
})

Shiny.addCustomMessageHandler("toggle_help", function(data){

if(data) {
    $("#help_nonclinical").css("display","block")
    $("#help_nonclinical_02").css("display","block")
    $("#help_button_space").css("padding-bottom", "15px")
} else{
    $("#help_nonclinical").css("display","none")
    $("#help_nonclinical_02").css("display","none")
    $("#help_button_space").css("padding-bottom", "1px")
}
}
)
