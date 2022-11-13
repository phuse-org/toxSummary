
Shiny.addCustomMessageHandler("mymessage", function (message) {
  document.getElementById(message).click();
});

Shiny.addCustomMessageHandler("save_clin_info", function(message) {
	document.getElementById(message).click()
})


Shiny.addCustomMessageHandler("confirm_study_remove", function(message) {
	document.getElementById(message).click()
})

// Shiny.addCustomMessageHandler("dose_value", function(message) {
// 	let el_dose = document.getElementById(message);
// 	console.log(message)
	

	
// 	el_dose.addEventListener("blur", function(event){
// 		// event.preventDefault()
		
// 		Shiny.setInputValue("save_now", "blur", {priority : "event"})
// 		console.log(event.target)

// 	})

// 	el_dose.addEventListener("focus", function(event){
		
// 		Shiny.setInputValue("save_now", "focus", {priority : "event"})

// 	})


// })

// Shiny.addCustomMessageHandler("change_auc", function(message) {


// 	let auc_value = document.getElementById("choose_auc").innerText
// 	if (auc_value == "PP domain empty or AUC not available for this study") {
// 		document.getElementById("get_from_db").style.visibility = "hidden"
// 	} else {
// 		document.getElementById("get_from_db").style.visibility = "visible"

// 	}

// })




// var dose1 = document.getElementById("dose1")

// if(dose1) {
// 	let rnd = math.random()

// 	dose1.addEventListener("blur", function(event){
// 		let random_num1 = Math.random()
// 		Shiny.setInputValue("save_now", "blur", {priority : "event"})
// 		dose1.style.backgroundColor = "yellow";
// 	  })

// 	  	dose1.addEventListener("focus", function(event){
// 				let random_num2 = Math.random()
// 				Shiny.setInputValue("save_now", "focus", {priority : "event"})
// 				dose1.style.backgroundColor = "red";
		
// 			})

// 			console.log(rnd)


// 		}

// document.addEventListener('DOMContentLoaded', function (event) {
// 	// let el_dose = document.getElementById("nDoses");
// 	// console.log(el_dose)
// 	var el_dose = document.getElementById('dose1');
// 	if (typeof (el_dose) != 'undefined' && el_dose != null) {
// 		// Exists.


// 		el_dose.addEventListener("blur", function (event) {

// 			Shiny.setInputValue("save_now", "blur", { priority: "event" })


// 		})

// 		el_dose.addEventListener("focus", function (event) {

// 			Shiny.setInputValue("save_now", "focus", { priority: "event" })
			

// 		})
// 	}
// 	//the event occurred
// })

