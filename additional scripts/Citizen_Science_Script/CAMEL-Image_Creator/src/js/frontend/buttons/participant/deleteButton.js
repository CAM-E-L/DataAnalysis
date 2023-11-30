/* add button: */
const buttonDelete = `<button id="deleteCAM" class="material-icons" style="color:red;" title="Delete CAM" onclick="deleteCam()"> delete</button>`;
var target = document.getElementById("rightButton");
target.innerHTML += buttonDelete;

// language file
$(function () {
    document.getElementById("deleteCAM").title = languageFileOut.btr_04; // buttons top right (btr)
});

function deleteCam() {
    let confirmdel = confirm(languageFileOut.confirmDeleting_01text);
    if (confirmdel == true) {
        CAM.connectors = [];
        CAM.nodes = [];
        toastr.error(languageFileOut.confirmDeleting_02message);
        console.log("complete CAM has been deleted");

        defaultCAM();
    }
}
