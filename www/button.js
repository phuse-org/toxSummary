
Shiny.addCustomMessageHandler("mymessage", function (message) {
  document.getElementById(message).click();
});
