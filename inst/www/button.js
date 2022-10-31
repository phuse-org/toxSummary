
Shiny.addCustomMessageHandler("mymessage", function (message) {
  document.getElementById(message).click();
});

Shiny.addCustomMessageHandler("save_clin_info", function(message) {
	document.getElementById(message).click()
})


Shiny.addCustomMessageHandler("confirm_study_remove", function(message) {
	document.getElementById(message).click()
})

// Shiny.addCustomMessageHandler("change_auc", function(message) {


// 	let auc_value = document.getElementById("choose_auc").innerText
// 	if (auc_value == "PP domain empty or AUC not available for this study") {
// 		document.getElementById("get_from_db").style.visibility = "hidden"
// 	} else {
// 		document.getElementById("get_from_db").style.visibility = "visible"

// 	}

// })