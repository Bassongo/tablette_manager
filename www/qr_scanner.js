var currentStream = null;
function stopQrScanner() {
  if (currentStream) {
    currentStream.getTracks().forEach(function(t){ t.stop(); });
    currentStream = null;
  }
}
function startQrScanner(targetInput) {
  var container = document.getElementById('qr-reader');
  if (!container) return;
  container.innerHTML = '<video id="qr-video" playsinline autoplay muted style="width:100%;"></video>';
  var video = document.getElementById('qr-video');

  if (!(navigator.mediaDevices && navigator.mediaDevices.getUserMedia)) {
    container.innerHTML = 'Acc\xC3\xA8s \xC3\xA0 la cam\xC3\xA9ra non disponible. Utilisez HTTPS ou localhost.';
    return;
  }

  navigator.mediaDevices.getUserMedia({video: {facingMode: 'environment'}})
    .then(function(stream){
      currentStream = stream;
      video.srcObject = stream;
      return video.play();
    })
    .then(function(){
      var startDetection = function(){
        if ('BarcodeDetector' in window) {
          var detector = new BarcodeDetector({formats:['qr_code']});
          var detect = function(){
            if (!currentStream) return;
            detector.detect(video).then(function(barcodes){
              if (barcodes.length > 0) {
                Shiny.setInputValue(targetInput, barcodes[0].rawValue, {priority:'event'});
                stopQrScanner();
                $("#shiny-modal").modal('hide');
              } else {
                requestAnimationFrame(detect);
              }
            }).catch(function(){ requestAnimationFrame(detect); });
          };
          requestAnimationFrame(detect);
        } else if (window.jsQR) {
          var canvas = document.createElement('canvas');
          var context = canvas.getContext('2d');
          var scan = function(){
            if (!currentStream) return;
            canvas.width = video.videoWidth;
            canvas.height = video.videoHeight;
            context.drawImage(video, 0, 0, canvas.width, canvas.height);
            var imageData = context.getImageData(0, 0, canvas.width, canvas.height);
            var code = jsQR(imageData.data, canvas.width, canvas.height);
            if (code) {
              Shiny.setInputValue(targetInput, code.data, {priority:'event'});
              stopQrScanner();
              $("#shiny-modal").modal('hide');
            } else {
              requestAnimationFrame(scan);
            }
          };
          requestAnimationFrame(scan);
        } else {
          container.innerHTML = 'BarcodeDetector API non support\xC3\xA9e';
        }
      };
      startDetection();
    })
    .catch(function(err){
      console.error(err);
      container.innerHTML = 'Erreur d\'acc\xC3\xA8s \xC3\xA0 la cam\xC3\xA9ra';
    });
}
Shiny.addCustomMessageHandler('start-scan', function(message){
  startQrScanner(message.target);
});
$(document).on('hidden.bs.modal', '#shiny-modal', function(){
  stopQrScanner();
});
