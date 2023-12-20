let arrow = document.createElementNS(svgns, "defs");

let arrowRight = document.createElementNS(svgns, "marker");
arrowRight.setAttribute("id", "arrowRight");
arrowRight.setAttribute("markerWidth", "30");
arrowRight.setAttribute("markerHeight", "15");
arrowRight.setAttribute("refX", "30");
arrowRight.setAttribute("refY", "7.5");
arrowRight.setAttribute("orient", "auto");
arrowRight.setAttribute("markerUnits", "userSpaceOnUse");

let polyR = document.createElementNS(svgns, "polygon");
if (config.hideArrows == true) {
    polyR.setAttribute("points", "0 0, 0 0, 0 0");
}else{
    polyR.setAttribute("points", "30 0, 30 15, 0 7.5");
}

polyR.setAttribute("fill", "#8c8c8c");
arrowRight.appendChild(polyR);
arrow.appendChild(arrowRight);

let arrowLeft = document.createElementNS(svgns, "marker");
arrowLeft.setAttribute("id", "arrowLeft");
arrowLeft.setAttribute("markerWidth", "30");
arrowLeft.setAttribute("markerHeight", "15");
arrowLeft.setAttribute("refX", "0");
arrowLeft.setAttribute("refY", "7.5");
arrowLeft.setAttribute("orient", "auto");
arrowLeft.setAttribute("markerUnits", "userSpaceOnUse");

let polyL = document.createElementNS(svgns, "polygon");
if (config.hideArrows == true) {
    polyL.setAttribute("points", "0 0, 0 0, 0 0");
}else{
    polyL.setAttribute("points", "0 0, 30 7.5, 0 15");
}
polyL.setAttribute("fill", "#8c8c8c");
arrowLeft.appendChild(polyL);
arrow.appendChild(arrowLeft);