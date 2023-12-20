
var showDialogOnce = (function() {
    var executed = config.showNotPopupStart;
    return function() {
        if (!executed) {
            executed = true;
            //alert(123);
            $("#dialogStart").dialog("open");

            /* if software is on JATOS */
            if (typeof jatos.jQuery === "function") {
                var resultJson = CAM;
                console.log("my result data sent to JATOS first time");
                jatos.submitResultData(resultJson)
                    .then(() => console.log('success'))
                    .catch(() => console.log('error'));
        
            }
        }
    };
})();

/* !!! RENAME within code !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
// necessary conditions to save CAM
//var ConNumNodes = config.ConNumNodes; // # of nodes

// stop / breaklines in text:
// > maximum number of characters:
//var MaxLengthWords = config.MaxLengthWords; // allow not more than X characters


/* DEFAULT values */
// if no arrows closeness to node
if (config.hideArrows) {
    var DistanceArrows = 20;
} else {
    var DistanceArrows = 40;
}

// hide all researcher functionalities
/*
$(function () {
    $('#hideResearcherButtonsNode').hide(); // hide
    $('#hideResearcherButtonsConnector').hide();
    $('#hideResearcherButtonsTop').hide();
});

// hide reference slider agreement only
$(function () {
    $('#showSliderAgreeOnlyRef').hide();
});

// hide camera functionality
$(function () {
    $('#showCameraFeature').hide();
});
*/


/* start url parameters */
// provide precheck IF partcipants changing URL
// !!! on server possible to send object via pst request
const urlSearchParams = new URLSearchParams(window.location.search);
const params = Object.fromEntries(urlSearchParams.entries());


console.log("url params: ", params);


// hide researcher buttons
if ((urlSearchParams.has('ShowResearcherButtons') && urlSearchParams.get('ShowResearcherButtons') === "false")) {
    config.ShowResearcherButtons = false;
}else if((urlSearchParams.has('ShowResearcherButtons') && urlSearchParams.get('ShowResearcherButtons') === "true")){
    config.ShowResearcherButtons = true;
}






// show agreement only slider
if ((urlSearchParams.has('showOnlyPosSlid') && urlSearchParams.get('showOnlyPosSlid') === "true")) {
    config.showOnlyPosSlid = true;
}else if((urlSearchParams.has('showOnlyPosSlid') && urlSearchParams.get('showOnlyPosSlid') === "false")){
    config.showOnlyPosSlid = false;
}




// hide connector: direction of influence + reference
if ((urlSearchParams.has('hideArrows') && urlSearchParams.get('hideArrows') === "true")) {
    config.hideArrows = true;
    DistanceArrows = 20;
}else if((urlSearchParams.has('hideArrows') && urlSearchParams.get('hideArrows') === "false")){
    config.hideArrows = false;
}


// hide ambivalent node + reference
if ((urlSearchParams.has('hideAmbivalent') && urlSearchParams.get('hideAmbivalent') === "true")) {
    config.hideAmbivalent = true;
}else if((urlSearchParams.has('hideAmbivalent') && urlSearchParams.get('hideAmbivalent') === "false")){
    config.hideAmbivalent = false;
}


// necessary conditions to save CAM
if (urlSearchParams.has('ConNumNodes')) {
    config.ConNumNodes = parseInt(urlSearchParams.get('ConNumNodes'), 10);
    //ConNumNodes = parseInt(urlSearchParams.get('ConNumNodes'), 10);
}

// stop / breaklines in text:
if (urlSearchParams.has('MaxLengthWords')) {
    config.MaxLengthWords = parseInt(urlSearchParams.get('MaxLengthWords'), 10);
    //MaxLengthWords = parseInt(urlSearchParams.get('MaxLengthWords'), 10);
}


// enable camera functionality
if ((urlSearchParams.has('cameraFeature') && urlSearchParams.get('cameraFeature') === "false")) {
    config.cameraFeature = false;
    $(function () {
        $('#showCameraFeature').hide();
    });
}else if((urlSearchParams.has('cameraFeature') && urlSearchParams.get('cameraFeature') === "true")){
    config.cameraFeature = true;
    $(function () {
        $('#showCameraFeature').show();
    });
}

// set on fullscreen mode
if ((urlSearchParams.has('fullScreen') && urlSearchParams.get('fullScreen') === "false")) {
    config.fullScreen = false;
}else if((urlSearchParams.has('fullScreen') && urlSearchParams.get('fullScreen') === "true")){
    config.fullScreen = true;
}


console.log("config URL params: ", config);