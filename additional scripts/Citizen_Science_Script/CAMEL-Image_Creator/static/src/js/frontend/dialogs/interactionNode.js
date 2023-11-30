const interaction = `
<div class="properties" id="interactionNode">
<!-- delete node: -->
<div style="padding-bottom: 30px;">
    <button id="deleteNode" class="material-icons deleteButton" style="color:red;" title="Delete Concept">
        delete </button>
</div>

<!-- > adjust text -->
<div class="properties-align">
    <div class="properties-size-naming">
    ${languageFileOut.nd_01}
    </div>
    <input id="inptextnode" type="text"
        style="width: 99%; text-align: left;   margin: auto; display: block;" autofocus>
</div>


<!-- > adjust type and strength of concept -->
<div class="properties-align">
    <div class="properties-size-naming">
    ${languageFileOut.nd_02}
    </div>
</div>

<div class="outerNodeSlider" style="margin-bottom: 25px;">
    <div class="greenColorNodeSlider">
        <div class="yellowColorNodeSlider">
            <div class="redColorNodeSlider">
                <input type="range" min="1" max="7" step="1" id="nodeSlider" list="steplist">
            </div>
        </div>
    </div>
    <div class="labelsNodeSlider">
        <span>-3</span>
        <span>-2</span>
        <span>-1</span>
        <span>0</span>
        <span>1</span>
        <span>2</span>
        <span>3</span>
    </div>
</div>


<div id="hideAmvivalentNode" class="spacing-node">

    <input type="checkbox" id="checkboxAmbivalent" style="font-size: 20px;">
    <label for="checkboxAmbivalent" style="font-size: 16px; margin-left: 5px;">${languageFileOut.nd_03} </label>

    <image type="image"
        src="./img/ambivalent_node.svg#svgView(viewBox(5,5,250,120))"
        style="width: 85px; height: 65px; margin-left: 5px; margin-top: 5px;"></image>

</div>




<!-- > adjust comment -->
<div class="properties-align">
    <div class="properties-size-naming">
    ${languageFileOut.nd_04}
    </div>
    <textarea id="inpcommentnode"
        style="width: 97%; text-align: left; margin: auto; display: block;"></textarea>
</div>

<!-- for researcher only -->
<div id="hideResearcherButtonsNode">
    <div style="margin-top: 20px; font-size:16px; font-style: italic;">
        The following functions are only available to researchers:
    </div>
    <div>
        <button id="ResErasabilityNode" type="button" class="typeResearcherButton">
            deletable
        </button>

        <button id="ResManoeuvrability" type="button" class="typeResearcherButton">
            movable
        </button>

        <button id="TextChangeableNode" type="button" class="typeResearcherButton">
         changeable
    </button>
    </div>
</div>
</div>`;

var target = document.getElementById("dialogInteractionNode");
target.innerHTML += interaction;

// language file
$(function () {
    document.getElementById("deleteNode").title = languageFileOut.nd_05buttonDelete;
  });


$(function () {

    $('#inptextnode').on("input", function () {
        var MaxLengthWords = config.MaxLengthWords; // allow not more than X words
        var MaxLengthChars = config.MaxLengthChars; // allow not more than X characters

        // console.log("CAM.currentNode.isTextChangeable:", CAM.currentNode.isTextChangeable)

        if(CAM.currentNode.isTextChangeable){
            var numWords = this.value.split(' ').filter(word => word != "");
            numWords = numWords.length;
            // console.log("length chars: ", this.value.length, this.value.length <= MaxLengthChars);
            // console.log("numWords: ", numWords, numWords <= MaxLengthWords);
    
            if (numWords <= MaxLengthWords && this.value.length <= MaxLengthChars) {
                //console.log("show me:", this.value);
                CAM.updateElement("Node", "text", this.value);
                CAM.draw();
            } else if (numWords > MaxLengthWords) {
                toastr.warning(languageFileOut.ndw_01tooManyWords, languageFileOut.ndw_02tooManyWords + MaxLengthWords + languageFileOut.ndw_03tooManyWords, {
                    closeButton: true,
                    timeOut: 2000,
                    positionClass: "toast-top-center",
                    preventDuplicates: true
                })
    
                
                // alert("Please do not use more than " + MaxLengthWords + " words for a single node!\nInstead, please draw several connected nodes.");
            } else if (this.value.length > MaxLengthChars) {
                toastr.warning(languageFileOut.ndw_01tooManyWords, languageFileOut.ndw_02tooManyWords + MaxLengthChars + languageFileOut.ndw_03tooManyWordsA, {
                    closeButton: true,
                    timeOut: 2000,
                    positionClass: "toast-top-center",
                    preventDuplicates: true
                })
            }
        }else{
            toastr.info(languageFileOut.ndw_01predefinedConcept, languageFileOut.ndw_02predefinedConcept, {
                closeButton: true,
                timeOut: 2000,
                positionClass: "toast-top-center",
                preventDuplicates: true
            })
        }

    });


    $('#nodeSlider').on("input", function () {
        var valenceValue = document.querySelector('#nodeSlider');

        var myGreenColorNodeSlider = document.querySelector('.greenColorNodeSlider');
        var myRedColorNodeSlider = document.querySelector('.redColorNodeSlider');

        switch (true) {
            case (valenceValue.value == 4):
                myRedColorNodeSlider.style.backgroundColor = COLOUR.red3;
                myGreenColorNodeSlider.style.backgroundColor = COLOUR.green3;
                CAM.updateElement("Node", "value", 0);
                break;

            case (valenceValue.value <= 3):
                const colourPaletteRed = ["white", COLOUR.red1, COLOUR.red2, COLOUR.red3];
                myRedColorNodeSlider.style.backgroundColor = colourPaletteRed[valenceValue.value];
                CAM.updateElement("Node", "value", valenceValue.value - 4);
                break;

            case (valenceValue.value >= 5):
                const colourPaletteGreen = ["white", COLOUR.green3, COLOUR.green2, COLOUR.green1];
                myGreenColorNodeSlider.style.backgroundColor = colourPaletteGreen[valenceValue.value - 4];
                CAM.updateElement("Node", "value", valenceValue.value - 4);
                break;
        }

        CAM.draw();
    });

    $('#checkboxAmbivalent').on("click", function (event) {
        var myValueCheckbox = document.querySelector('#checkboxAmbivalent').checked;

        if (myValueCheckbox === true) {
            toastr.info(languageFileOut.ndw_01ambivalentConcept);
            CounterChangeAmbiConcept++;
            if (CounterChangeAmbiConcept == 2) {
                $(this).off(event);
            }
        }
    });


    $('#checkboxAmbivalent').on("input", function () {

        var myValueCheckbox = document.querySelector('#checkboxAmbivalent').checked;
        document.getElementById("nodeSlider").value = 4;

        if (myValueCheckbox === true) {
            document.getElementById("nodeSlider").disabled = true;
            CAM.updateElement("Node", "value", 10);
        } else {
            document.getElementById("nodeSlider").disabled = false;
            CAM.updateElement("Node", "value", 0);
        }
        CAM.draw();
    });


    // > comment
    $('#inpcommentnode').on("input", function () {
        CAM.updateElement("Node", "comment", this.value);
        CAM.draw();
    });

    // > delete
    $("#deleteNode").on("click", (evt) => {
        console.log("Deleted using botton");
        CAM.currentNode.enterLog({
            type: "node was deleted",
            value: -99
        });
        CAM.deleteElement();

        $("#dialogInteractionNode").dialog('close');
    });

    $(document).keyup(function(e){
        if(e.keyCode == 46) {
            if(CAM.currentNode != null){
                console.log("Deleted using keypress");
                CAM.currentNode.enterLog({
                    type: "node was deleted",
                    value: -77
                });
                CAM.deleteElement();  
                $("#dialogInteractionNode").dialog('close');
            }else if(CAM.currentConnector != null){
                console.log("Deleted using keypress");
                CAM.currentConnector.enterLog({
                    type: "connector was deleted",
                    value: -77
                });
                CAM.deleteElement();
                $("#dialogInteractionEdge").dialog('close');
            }
        }
    });


    $("#ResErasabilityNode").on("click", (evt) => {
        if (CAM.currentNode != null) {
            if (CAM.currentNode.isDeletable == true) {
                CAM.currentNode.setIsDeletable(false);
                toastr.info('The node is now not deletable.');
            } else if (CAM.currentNode.isDeletable == false) {
                CAM.currentNode.setIsDeletable(true);
                toastr.info('The node is now deletable.');
            }
        }
    });

    $("#ResManoeuvrability").on("click", (evt) => {
        if (CAM.currentNode != null) {
            if (CAM.currentNode.isDraggable == true) {
                CAM.currentNode.setIsDraggable(false);
                toastr.info('The node is now not draggable.');
            } else if (CAM.currentNode.isDraggable == false) {
                CAM.currentNode.setIsDraggable(true);
                toastr.info('The node is now draggable.');
            }
        }
    });

    $("#TextChangeableNode").on("click", (evt) => {
        if (CAM.currentNode != null) {
            if (CAM.currentNode.isTextChangeable == true) {
                CAM.currentNode.setIsTextChangeable(false);
                toastr.info('The text of the node is now not changeable.');
            } else if (CAM.currentNode.isTextChangeable == false) {
                CAM.currentNode.setIsTextChangeable(true);
                toastr.info('The text of the node is now changeable.');
            }
        }
    });
})


// hide ambivalent node
if(config.hideAmbivalent){
    $('#hideAmvivalentNode').hide();
    $(function () {
        $('#hideAmvivalentNode').hide();
    });
}else{
    $('#hideAmvivalentNode').show();
    $(function () {
        $('#hideAmvivalentNode').show();
    });
}