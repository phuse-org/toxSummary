
Shiny.addCustomMessageHandler("mymessage", function (message) {
  document.getElementById(message).click();
});

Shiny.addCustomMessageHandler("save_clin_info", function(message) {
	document.getElementById(message).click()
})


Shiny.addCustomMessageHandler("confirm_study_remove", function(message) {
	document.getElementById(message).click()
})
