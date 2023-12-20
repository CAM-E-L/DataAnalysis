/* add button: */
const createConfigSave = `
<span style="font-size: 12px; vertical-align: super;">to set up your study click:</span>
<button id="createConfigSave" title="set up your config file and cope & paste the resulting code to set up the experiment" class="material-icons" style="margin-left: -5px;">settings</button>
`;
var target = document.getElementById("hideResearcherButtonsTop");
target.innerHTML += createConfigSave;

/* add dialog window */
const interactionSetUpStudy = `
<div class="properties">
    <!-- > adjust text -->
    <div class="properties-align">
        <div class="properties-size-naming" style="font-size:18px;">
            Define the configuration of your C.A.M.E.L. study:
        </div>
        <div class="row" style="background-color:#aaa;">
            <div class="column1">
                Number of nodes necessary to draw (about 10 recommended):
            </div>
            <div class="column2">
            <input type="number" id="setConNumNodes" min="1" max="50" style="width: 60%; margin-top: 14px;" value="10">
            </div>
        </div>
        <div class="row" style="background-color:#bababa;">
            <div class="column1">
                Maximum number of words for each concept (2-3 recommended):
            </div>
            <div class="column2">
                <input type="number" id="setMaxLengthWords" min="1" max="5" style="width: 60%; margin-top: 10px;" value="3">
            </div>
        </div>
        <div class="row" style="background-color:#aaa;">
            <div class="column1">
                Maximum number of characters for each concept (at least 30 recommended):
            </div>
            <div class="column2">
                <input type="number" id="setMaxLengthChars" min="30" max="300" style="width: 60%; margin-top: 10px;" value="30">
            </div>
        </div>

        <div class="row" style="background-color:#bababa;">
            <div class="column1">
                Possibility to draw arrows / directed connections:
            </div>
            <div class="column2">
                <label class="switch" style="margin-top: 8px;">
                <input type="checkbox" id="sethideArrows" checked>
                <div class="slider round">
                </div>
                </label>
            </div>
        </div>

        <div class="row" style="background-color:#aaa;">
            <div class="column1">
                As default the drawn connection is bidirectional:
            </div>
            <div class="column2">
                <label class="switch" style="margin-top: 8px;">
                <input type="checkbox" id="setBidirectionalDefault" checked>
                <div class="slider round">
                </div>
                </label>
            </div>
        </div>


        

        <div class="row" style="background-color:#bababa;">
            <div class="column1">
                Possibility to draw only supporting connections (no recommendation):
            </div>
            <div class="column2">
                <label class="switch" style="margin-top: 8px;">
                <input type="checkbox" id="setshowOnlyPosSlid">
                <div class="slider round">
                </div>
                </label>
            </div>
        </div>

        <div class="row" style="background-color:#aaa;">
            <div class="column1">
                Possibility to to draw ambivalent nodes (no recommendation):
            </div>
            <div class="column2">
                <label class="switch" style="margin-top: 8px;">
                <input type="checkbox" id="sethideAmbivalent" checked>
                <div class="slider round">
                </div>
                </label>
            </div>
        </div>
        
        <div class="row" style="background-color:#bababa;">
            <div class="column1">
                Include splotlight feature to move screen (only recommended if large CAMs are expected):
            </div>
            <div class="column2">
                <label class="switch" style="margin-top: 8px;">
                <input type="checkbox" id="setcameraFeature">
                <div class="slider round">
                </div>
                </label>
            </div>
        </div>

        <div class="row" style="background-color:#aaa;">
        <div class="column1">
            Set study to fullscreen mode and collect paradata (recommended):
        </div>
        <div class="column2">
            <label class="switch" style="margin-top: 8px;">
            <input type="checkbox" id="setfullScreen" checked>
            <div class="slider round">
            </div>
            </label>
        </div>
    </div>

    <div class="row" style="background-color:#bababa;">
        <div class="column1">
            Set the language of the C.A.M.E.L. interface:
        </div>
    <div class="column2">
        <label class="switch" style="margin-top: 8px;">
            <select name="setLanguage" id="setLanguage">
            <option value="English">English</option>
            <option value="German">German</option>
            <option value="Spanish">Spanish</option>
            <option value="Chinese">Chinese</option>
            </select> 
        </label>
    </div>
    </div>
    <br>
    <div class="properties-align" style="font-size: 15px;">
       After you have set the configuration click button and copy & paste the code (JSON file) to the administrative:
    </div>
    <div class="centerCopyPaste">
    <button onclick="copyText()" style="height: 45px; width: 75px;">Copy text</button>
    </div>

    <br>
    <div class="properties-align" style="font-size: 15px; font-style: italic;">
    You have generated the following code (! do not change the text except you really know what you are doing):
<textarea id="createdConfigPlusCAM"
    style="width: 97%; text-align: left; margin: auto; display: block;"></textarea>
</div>
</div>
</div>`;

var target = document.getElementById("dialogSetUpStudy");
target.innerHTML += interactionSetUpStudy;

/* function to copy text */
function copyText() {
    /* Get the text field */
    var copyText = document.getElementById("createdConfigPlusCAM");

    /* Select the text field */
    copyText.select();
    copyText.setSelectionRange(0, 99999); /* For mobile devices */

    /* Copy the text inside the text field */
    navigator.clipboard.writeText(copyText.value);

    /* Alert the copied text */
    alert("Copied the text: " + copyText.value);
}

/* function to set text file*/
function setConfigCAMfile() {
    var setCAMConfig = {
        config: {
            ConNumNodes: $("#setConNumNodes").val(), // number of nodes necessary to draw
            MaxLengthWords: $("#setMaxLengthWords").val(), // maximum number of words for each concept
            MaxLengthChars: $("#setMaxLengthChars").val(), // maximum number of characters for each concept

            hideArrows: null, // if false = possible to draw arrows
            BidirectionalDefault: null, // if false = possible to draw arrows

            showOnlyPosSlid: null, // if true show only slider for agreement (+1 - +3)

            hideAmbivalent: null, // if false = possible to draw ambivalent node
            cameraFeature: null, // if true include camera / splotlight feature to move screen
            fullScreen: null, // if true = study in fullscreen mode + paradata

            setLanguage: $("#setLanguage").val(), // set language of your CAM study

            /* default: */
            LengthSentence: 16, // include breaklines if >= X characters
            LengthWords: 12, // include breaklines after each word with cumsum >= X characters
            ShowResearcherButtons: false, // if true = show researcher functionalities
        },
        CAM: {
            nodes: null,
            connectors: null,
        },
    };

    /* missing values
    var MISSING = {
        CAMproject: "proj_" + uuid.v4(), // necessary for server (see ERM)
        AdaptiveStudy: false, // run as adaptive study 
        ADAPTIVESTUDYurl: null // "https://studien.psychologie.uni-freiburg.de/publix/304/start?batchId=379&generalMultiple" // URL the CAM data should be append to
    }
     */

    /* set up the config */
    if ($("#sethideArrows").is(":checked")) {
        setCAMConfig.config.hideArrows = false;
    } else {
        setCAMConfig.config.hideArrows = true;
    }

    if ($("#setBidirectionalDefault").is(":checked")) {
        setCAMConfig.config.BidirectionalDefault = true;
    } else {
        setCAMConfig.config.BidirectionalDefault = false;
    }

    if ($("#sethideAmbivalent").is(":checked")) {
        setCAMConfig.config.hideAmbivalent = false;
    } else {
        setCAMConfig.config.hideAmbivalent = true;
    }

    if ($("#setshowOnlyPosSlid").is(":checked")) {
        setCAMConfig.config.showOnlyPosSlid = true;
    } else {
        setCAMConfig.config.showOnlyPosSlid = false;
    }

    if ($("#setcameraFeature").is(":checked")) {
        setCAMConfig.config.cameraFeature = true;
    } else {
        setCAMConfig.config.cameraFeature = false;
    }

    if ($("#setfullScreen").is(":checked")) {
        setCAMConfig.config.fullScreen = true;
    } else {
        setCAMConfig.config.fullScreen = false;
    }

    /* set up the CAM */
    // nodes
    saveNodes = [];
    for (var i = 0; i < CAM.nodes.length; i++) {
        var elementNode = CAM.nodes[i];
        var currentNode = {
            id: null,
            value: null,
            text: null,
            position: null,
            isDeletable: null,
            isDraggable: null,
            isTextChangeable: null,
        };
        // console.log(elementNode);

        if (elementNode.isActive) {
            currentNode.id = elementNode.getId();
            currentNode.value = elementNode.getValue();
            currentNode.text = elementNode.getText();
            currentNode.position = elementNode.getPosition();
            currentNode.isDeletable = elementNode.getIsDeletable();
            currentNode.isDraggable = elementNode.getIsDraggable();
            currentNode.isTextChangeable = elementNode.getIsTextChangeable();
            saveNodes.push(currentNode);
        }
    }
    //console.log(saveNodes)
    setCAMConfig.CAM.nodes = saveNodes;

    // connectors
    saveConnectors = [];
    for (var i = 0; i < CAM.connectors.length; i++) {
        var elementConnector = CAM.connectors[i];
        var currentConnector = {
            id: null,
            intensity: null,
            agreement: null,
            isBidirectional: null,
            source: null,
            target: null,
            isDeletable: null,
        };
        // console.log(elementNode);

        if (elementConnector.isActive) {
            currentConnector.id = elementConnector.getId();
            currentConnector.intensity = elementConnector.getIntensity();
            currentConnector.agreement = elementConnector.agreement;
            currentConnector.isBidirectional = elementConnector.isBidirectional;
            currentConnector.source = elementConnector.source;
            currentConnector.target = elementConnector.target;
            currentConnector.isDeletable = elementConnector.getIsDeletable();

            saveConnectors.push(currentConnector);
        }
    }
    //console.log(saveConnectors)
    setCAMConfig.CAM.connectors = saveConnectors;

    $("#createdConfigPlusCAM").text(JSON.stringify(setCAMConfig, null, 1));
    // console.log(JSON.parse(JSON.stringify(setConfig, null, 1)))
}

$(function () {
    /* set up dialog */
    $("#dialogSetUpStudy").dialog({
        autoOpen: false,
        modal: true,
        show: "fade",
        hide: false,
        resizable: false,
        draggable: true,
        width: 460,
        maxWidth: 460,
        open: function (event, ui) {
            $(".ui-dialog-titlebar").hide(); // hide titlebar
            $(this)
                .dialog({
                    draggable: false,
                })
                .parent()
                .draggable();

            console.log("dialog got open");

            //setTimeout("$('#dialogSetUpStudy').dialog('close')",5000);

            $(".ui-widget-overlay").on("click", function () {
                $("#dialogSetUpStudy").dialog("close");
            });
        },
        close: function (event, ui) {
            console.log("dialog got closed");
        },
        position: {
            my: "center", // add percentage offsets
            at: "center",
            of: $(".boxCAMSVG"),
        },
    });

    $("#createConfigSave").on("click", (evt) => {
        $("#dialogSetUpStudy").dialog("open");
        setConfigCAMfile();
    });

    $(
        "#sethideArrows, #setBidirectionalDefault, #setfullScreen, #sethideAmbivalent, #setshowOnlyPosSlid, #setcameraFeature"
    ).click(function () {
        setConfigCAMfile();
    });

    $(
        "#setConNumNodes,#setMaxLengthWords, #setMaxLengthChars, #setLanguage"
    ).change(function () {
        setConfigCAMfile();
    });
});
