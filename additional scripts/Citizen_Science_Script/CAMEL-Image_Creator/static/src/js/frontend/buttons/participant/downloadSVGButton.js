/* add button: */
const dlButton = `<button id="saveCAMpicture" class="material-icons" onclick="onDownloadSVGfile()" title="Save CAM as picture" style="margin-right: 5px;">crop_original</button>`;
var target = document.getElementById("rightButton");
target.innerHTML += dlButton;


// language file
$(function () {
    document.getElementById("saveCAMpicture").title = languageFileOut.btr_03; // buttons top right (btr)
  });

function onDownloadSVGfile() {
    console.log("CAM picture (svg) has been saved");
    if(CAM.creator != null){
        downloadCAMsvg(CAMSVG, "CAMsvg-" + CAM.creator + ".svg");
    }else{
        downloadCAMsvg(CAMSVG, "CAMsvg-" + CAM.idCAM + ".svg");
    }


    if(!config.surpressSaveCAMpopup){
        toastr.info(
            languageFileOut.popSavePicture_CAM,
            {
              closeButton: true,
              timeOut: 4000,
              positionClass: "toast-top",
              preventDuplicates: true,
            }
          );
    }



}

function downloadCAMsvg(svgEl, fileName) {
    svgEl.setAttribute("xmlns", svgns);

    /* adjust CAM picture if negative coordinates / svg to small */
    document.getElementById('CAMSVG').setAttribute("height", "1400px");
    document.getElementById('CAMSVG').setAttribute("width", "2400px");

    var condHitX = false;
    var condHitY = false;

    var arrayPosX = [];
    CAM.nodes.forEach(element => {
        arrayPosX.push(element.position.x);
    });

    if (arrayPosX.some(element => element < 100)) {
        condHitX = true;
        CAM.nodes.forEach(element => {
            element.position.x = element.position.x + (Math.abs(Math.min(...arrayPosX)) + 100);
        });

        CAM.draw()
    } else {
        CAM.nodes.forEach(element => {
            element.position.x = element.position.x - (Math.abs(Math.min(...arrayPosX)) - 100);
        });

        CAM.draw()
    }

    var arrayPosY = [];
    CAM.nodes.forEach(element => {
        arrayPosY.push(element.position.y);
    });

    if (arrayPosY.some(element => element < 100)) {
        condHitY = true;
        CAM.nodes.forEach(element => {
            element.position.y = element.position.y + (Math.abs(Math.min(...arrayPosY)) + 100);
        });

        CAM.draw()
    } else {
        CAM.nodes.forEach(element => {
            element.position.y = element.position.y - (Math.abs(Math.min(...arrayPosY)) - 100);
        });

        CAM.draw()
    }

    var svgData = svgEl.outerHTML;
    var preface = '<?xml version="1.0" standalone="no"?>\r\n';
    var svgBlob = new Blob([preface, svgData], {
        type: "image/svg+xml;charset=utf-8"
    });
    const a = document.createElement("a");
    a.href = URL.createObjectURL(svgBlob);
    a.download = fileName;

    var img = document.createElement("img");
    img.src = a.href;
    a.click();

    /* REDO adjustments of CAM picture if negative coordinates / svg to small */
    document.getElementById('CAMSVG').setAttribute("height", "800px");
    document.getElementById('CAMSVG').setAttribute("width", "1300px");
    if (condHitX) {
        CAM.nodes.forEach(element => {
            element.position.x = element.position.x - (Math.abs(Math.min(...arrayPosX)) + 100);
        });

        CAM.draw()
    } else {
        CAM.nodes.forEach(element => {
            element.position.x = element.position.x + (Math.abs(Math.min(...arrayPosX)) - 100);
        });

        CAM.draw()
    }

    if (condHitY) {
        CAM.nodes.forEach(element => {
            element.position.y = element.position.y - (Math.abs(Math.min(...arrayPosY)) + 100);
        });

        CAM.draw()
    } else {
        CAM.nodes.forEach(element => {
            element.position.y = element.position.y + (Math.abs(Math.min(...arrayPosY)) - 100);
        });

        CAM.draw()
    }
    /*
    > https://stackoverflow.com/questions/3975499/convert-svg-to-image-jpeg-png-etc-in-the-browser
    canvg('canvas', $("#editor").html());
    */
}
