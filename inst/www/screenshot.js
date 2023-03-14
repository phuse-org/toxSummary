



// Shiny.addCustomMessageHandler("shot", function(message) {

// var mes = message;
//   console.log(mes)
// var node = document.getElementById("table_shot");
//   console.log(node)
// domtoimage.toPng(node)
//     .then(function (dataUrl) {
//         var img = new Image();
//         img.src = dataUrl;
//         document.body.appendChild(img);
//     })
//     .catch(function (error) {
//         console.error('oops, something went wrong!', error);
//     });


// })




Shiny.addCustomMessageHandler("shot", function(message) {

var mes = message;
  // console.log(mes["file_name"])
  domtoimage.toJpeg(document.getElementById('table_01'),  {bgcolor: "#ffffff"})
    .then(function (dataUrl) {
        var link = document.createElement('a');
      link.download = mes["file_name"];
        link.href = dataUrl;
        link.click();
    });
})



Shiny.addCustomMessageHandler("shot_02", function(message) {

var mes = message;
  domtoimage.toJpeg(document.getElementById('table_02'),  {bgcolor: "#ffffff"})
    .then(function (dataUrl) {
        var link = document.createElement('a');
      link.download = mes["file_name"];
        link.href = dataUrl;
        link.click();
    });
})
// for table 03

Shiny.addCustomMessageHandler("shot_03", function(message) {

var mes = message;
  domtoimage.toJpeg(document.getElementById('table_03'),  {bgcolor: "#ffffff"})
    .then(function (dataUrl) {
        var link = document.createElement('a');
      link.download = mes["file_name"];
        link.href = dataUrl;
        link.click();
    });
})





// for table 04

Shiny.addCustomMessageHandler("shot_04", function(message) {

var mes = message;
  domtoimage.toJpeg(document.getElementById('table_note'),  {bgcolor: "#ffffff"})
    .then(function (dataUrl) {
        var link = document.createElement('a');
      link.download = mes["file_name"];
        link.href = dataUrl;
        link.click();
    });
})
