const confirmSaveCAM = `
<div class="reference">
<div id="informationDefault" class="confirm-save">
${languageFileOut.confirmSaving_02_text}
</div>
</div>
`;

var target = document.getElementById("dialogConfirmSave");
target.innerHTML += confirmSaveCAM;