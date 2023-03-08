



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
  console.log(mes)
  domtoimage.toJpeg(document.getElementById('table_01'),  {bgcolor: "#ffffff"})
    .then(function (dataUrl) {
        var link = document.createElement('a');
        link.download = 'my-image-name.jpeg';
        link.href = dataUrl;
        link.click();
    });
})
