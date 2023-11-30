/* add button: */
const downloadJSONButton = `<button class="material-icons" onclick="onDownloadCAMdata()" title="Save CAM as file" style="margin-left: 10px; margin-right: 5px;">vertical_align_bottom</button>`;
var target = document.getElementById("hideResearcherButtonsTop");
target.innerHTML += downloadJSONButton;



function downloadCAMdata(content, fileName, contentType) {
    const a = document.createElement("a");
    const file = new Blob([content], {
        type: contentType
    });
    a.href = URL.createObjectURL(file);
    a.download = fileName;
    a.click();
}

function onDownloadCAMdata() {
    console.log("CAM data has been saved");
    downloadCAMdata(JSON.stringify(CAM), "CAMdataJSON-" + CAM.idCAM + ".txt", "text/plain");

    toastr.info('You can save your CAM as a data file (JSON file).');
}