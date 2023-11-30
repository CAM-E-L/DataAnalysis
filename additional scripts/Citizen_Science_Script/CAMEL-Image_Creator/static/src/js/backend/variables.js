/* initiate CAM */
var CAM = new Elements();
const svgns = "http://www.w3.org/2000/svg";
const WEBSOCKET = false;

/* camera option -> move SVG within broader frame*/
// speed of CAM movement
const moveCAMSpeed = 10;
// stopping condition X coordinates
var stopConX = 0;
var stopConY = 0;

/* !!! partly RENAME within code !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
//var LengthSentence = config.LengthSentence; // if >= X characters
//var LengthWords = config.LengthWords; // after each word with cumsum >= X characters

/* GLOBAL variables */
// hide connector: direction of influence

//working env variables
var camMother = null;
var linkRedirect = null;
var token = null;

// save only part of positions in eventLog when moving node
var arrayPositions;

// show researcher buttons
//var ShowResearcherButtons;

// after 2x show information regarding changing ambivalent node turn off
var CounterChangeAmbiConcept = 0;

/* DEFAULT values */
// change zoom level of svg elements
var zoomScaleNode = 0.45;
//var zoomScaleConnector = 1;



// increase slider intensity by X (thicker lines)
const IncreaseSliderIntensity = 3;


