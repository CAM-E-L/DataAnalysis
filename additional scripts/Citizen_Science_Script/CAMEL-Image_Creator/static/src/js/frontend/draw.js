



function draw(CAM) {



    const board = document.querySelector("#CAMSVG");
    board.innerHTML = "";

    board.appendChild(this.drawBackground());

    board.appendChild(arrowRight);
    board.appendChild(arrowLeft);


    /*
    var newLine = document.createElementNS(svgns,'line');
    newLine.setAttribute('id','line2');
    newLine.setAttribute('x1','650');
    newLine.setAttribute('y1','0');
    newLine.setAttribute('x2','650');
    newLine.setAttribute('y2','1300');
    newLine.setAttribute("stroke", "black")
    newLine.setAttribute("stroke-dasharray", "10")
    
    board.appendChild(newLine);
    */


    CAM.connectors.forEach(connector => {
        if (connector.getIsActive()) {
            const mother = CAM.getNodeById(connector.source);
            const daughter = CAM.getNodeById(connector.target);
            const currentConnector = drawConnector(connector, mother, daughter);
            board.appendChild(currentConnector);
        }
    });




    CAM.nodes.forEach(node => {
        if (node.getIsActive()) {
            const currentNode = drawNode(node);
            board.appendChild(currentNode);
        }
    });




}

function drawBackground() {
    let background = document.createElementNS(svgns, "rect");
    background.setAttribute("id", "background");
    background.setAttribute("x", 0);
    background.setAttribute("y", 0);
    background.setAttribute("width", "100%");
    background.setAttribute("height", "100%");
    background.setAttribute("fill", COLOUR.background);



    return background;
}


function drawNode(node) {

    let group = document.createElementNS(svgns, "g");
    group.setAttribute("transform", `translate(${node.position.x},${node.position.y}) scale(${zoomScaleNode})`)
    group.appendChild(getShapeSVG(node));
    group.appendChild(getTextSVG(node));

    return group;
}

function getShapeSVG(node) {
    switch (true) {

        case (node.value == 10): // ambivalent concept
            return drawAmbivalent(node);

        case (node.value == 0): // neutral concept
            return drawNeutral(node);

        case (node.value < 0): // negative concept
            return drawNegativeNode(node);
        
        case (node.value > 0): // positive concept
            return drawPositiveNode(node);
    }
}

function getTextSVG(node) {

    let nodeText = document.createElementNS(svgns, "text");
    nodeText.setAttribute("id", node.id);
    nodeText.setAttribute("class", "noselect node");
    nodeText.setAttribute("x", 0);
    nodeText.setAttribute("fill", TEXT.colour);
    nodeText.setAttribute("alignment-baseline", "center");
    nodeText.setAttribute("text-anchor", "middle")
    nodeText.setAttribute("font-size", TEXT.size)

    if (node.text.length >= config.LengthSentence) {
        const cumulativeSum = (sum => value => sum += value)(0);
        var LengthCumWords = config.LengthWords;
        var LengthText = [];
        var ArrayText = node.text.split(' ');

        ArrayText.forEach(element => LengthText.push(element.length));

        LengthText = LengthText.map(cumulativeSum);

        for (var i = 0; i <= LengthText.length; i++) {
            if (LengthText[i] > LengthCumWords) {
                ArrayText[i] = " <tspan dy='1.1em' x='0'>" + ArrayText[i] + "</tspan>";
                LengthCumWords += config.LengthWords;
            }
        }

        nodeText.setAttribute("y", -20);

        nodeText.innerHTML = ArrayText.join(" ");
    } else {
        nodeText.innerHTML = node.text;
        nodeText.setAttribute("y", 0);
    }
    return nodeText;
}

function drawPositiveNode(node) {
    let positiveNode = document.createElementNS(svgns, "ellipse");
    positiveNode.setAttribute("id", node.id);
    positiveNode.setAttribute("class", "node");
    positiveNode.setAttribute(null, "cx", 0);
    positiveNode.setAttribute(null, "cy", 0);
    positiveNode.setAttribute("rx", "100");
    positiveNode.setAttribute("ry", "70");
    positiveNode.setAttribute("transform", "translate(0,0)"); // scale(0.1)
    positiveNode.setAttribute("fill", COLOUR.positiveNode);
    positiveNode.setAttribute("stroke", COLOUR.positiveLine);
    positiveNode.setAttribute("stroke-width", Math.abs(node.value) * 3);

    positiveNode.setAttribute("opacity", 1);


    if (node.isSelected === true) {
        positiveNode.setAttribute("fill", COLOUR.selected);
    }
    if (node.isConnectorSelected === true) {
        positiveNode.setAttribute("fill", COLOUR.adjacent);
    }

    return positiveNode;
}

function drawNegativeNode(node) {
    let negativeNode = document.createElementNS(svgns, "polygon");
    negativeNode.setAttribute("id", node.id);
    negativeNode.setAttribute("class", "node");
    negativeNode.setAttribute("points", "-100,0 -60,-60 60,-60 100,0 60,60 -60,60");
    negativeNode.setAttribute("transform", "translate(0,0)")
    negativeNode.setAttribute("fill", COLOUR.negativeNode);
    negativeNode.setAttribute("stroke", COLOUR.negativeLine);
    negativeNode.setAttribute("stroke-width", Math.abs(node.value) * 3);

    if (node.isSelected === true) {
        negativeNode.setAttribute("fill", COLOUR.selected);
    }
    if (node.isConnectorSelected === true) {
        negativeNode.setAttribute("fill", COLOUR.adjacent);
    }

    return negativeNode;
}

function drawNeutral(node) {
    let neutralNode = document.createElementNS(svgns, "rect");
    neutralNode.setAttribute("id", node.id);
    neutralNode.setAttribute("class", "node");
    neutralNode.setAttribute("x", -100);
    neutralNode.setAttribute("y", -60);
    neutralNode.setAttributeNS(null, 'width', '200');
    neutralNode.setAttributeNS(null, 'height', '120');
    neutralNode.setAttribute("transform", "translate(0,0)")

    neutralNode.setAttribute("fill", COLOUR.neutralNode);
    neutralNode.setAttribute("stroke", COLOUR.neutralLine);
    neutralNode.setAttribute("stroke-width", 5);

    if (node.isSelected === true) {
        neutralNode.setAttribute("fill", COLOUR.selected);
    }
    if (node.isConnectorSelected === true) {
        neutralNode.setAttribute("fill", COLOUR.adjacent);
    }

    return neutralNode;
}

function drawAmbivalent(node) {
    let ambivalentNode = document.createElementNS(svgns, "g");

    let innerEllipse = document.createElementNS(svgns, "ellipse");
    innerEllipse.setAttribute("id", node.id);
    innerEllipse.setAttribute("class", "node");
    innerEllipse.setAttribute(null, "cx", 0);
    innerEllipse.setAttribute(null, "cy", 0);
    innerEllipse.setAttribute("rx", "90");
    innerEllipse.setAttribute("ry", "55");
    innerEllipse.setAttribute("transform", "translate(0,0)")
    innerEllipse.setAttribute("fill", COLOUR.ambivalentNode);
    innerEllipse.setAttribute("stroke", COLOUR.ambivalentLine);
    innerEllipse.setAttribute("stroke-width", 3);

    let outsideShape = document.createElementNS(svgns, "polygon");
    outsideShape.setAttribute("id", node.id);
    outsideShape.setAttribute("class", "node");
    outsideShape.setAttribute("points", "-100,0 -60,-60 60,-60 100,0 60,60 -60,60");
    outsideShape.setAttribute("transform", "translate(0,0)")
    outsideShape.setAttribute("fill", COLOUR.ambivalentNode);
    outsideShape.setAttribute("stroke", COLOUR.ambivalentLine);
    outsideShape.setAttribute("stroke-width", 5);

    if (node.isSelected === true) {
        innerEllipse.setAttribute("fill", COLOUR.selected);
        outsideShape.setAttribute("fill", COLOUR.selected);
    }
    if (node.isConnectorSelected === true) {
        innerEllipse.setAttribute("fill", COLOUR.adjacent);
        outsideShape.setAttribute("fill", COLOUR.adjacent);
    }

    /*
    let positiveNode = document.createElementNS(svgns, "ellipse");
    positiveNode.setAttribute("id", node.id);
    positiveNode.setAttribute("class", "node");
    positiveNode.setAttribute(null, "cx", 0);
    positiveNode.setAttribute(null, "cy", 0);
    positiveNode.setAttribute("rx", "120");
    positiveNode.setAttribute("ry", "80");
    positiveNode.setAttribute("transform", "translate(0,0)"); // scale(0.1)
    positiveNode.setAttribute("fill", "white");
    positiveNode.setAttribute("stroke", "black");
    positiveNode.setAttribute("stroke-width", 7);

    ambivalentNode.appendChild(positiveNode);
    */
    ambivalentNode.appendChild(outsideShape);
    ambivalentNode.appendChild(innerEllipse);





    return ambivalentNode;
}

function distanceElement(value) {
    if (value < 0) return 90;
    if (value === 0) return 90;
    if (value === 10) return 90;
    if (value > 0) return 90;
}


function drawLine(connector, motherD, position, angle, dist, compensation) {
    let lineConnector = document.createElementNS(svgns, "line");
    lineConnector.setAttribute("class", "connector");
    lineConnector.setAttribute("id", connector.id);
    lineConnector.setAttribute("transform", `translate(${position.x},${position.y}) scale(1,1) `) // ${zoomScaleConnector}
    lineConnector.setAttribute("x1", (motherD + DistanceArrows) * Math.cos(angle) * compensation);
    lineConnector.setAttribute("y1", (motherD + DistanceArrows) * Math.sin(angle) * compensation);
    lineConnector.setAttribute("x2", (dist - DistanceArrows) * Math.cos(angle) * compensation);
    lineConnector.setAttribute("y2", (dist - DistanceArrows) * Math.sin(angle) * compensation);
    lineConnector.setAttribute("stroke", COLOUR.line);
    lineConnector.setAttribute("stroke-width", connector.intensity);

    lineConnector.setAttribute("marker-start", "url(#arrowRight)");

    
    if (connector.isBidirectional){
        lineConnector.removeAttribute("marker-start");
        lineConnector.setAttribute("x1", (motherD + 20) * Math.cos(angle) * compensation);
        lineConnector.setAttribute("y1", (motherD + 20) * Math.sin(angle) * compensation);
        lineConnector.setAttribute("x2", (dist - 20) * Math.cos(angle) * compensation);
        lineConnector.setAttribute("y2", (dist - 20) * Math.sin(angle) * compensation);
    }
    // if (connector.isBidirectional) lineConnector.setAttribute("marker-end", "url(#arrowLeft)");

    if (!connector.isBidirectional) { // adjust distance to element if uni-directional
        lineConnector.setAttribute("x2", (dist - 20) * Math.cos(angle) * compensation);
        lineConnector.setAttribute("y2", (dist - 20) * Math.sin(angle) * compensation);
    }

    if (connector.isSelected === true) {
        lineConnector.setAttribute("stroke", COLOUR.selected);

    }

    if (connector.agreement === true) {
        lineConnector.setAttribute("stroke-linecap", "butt");
    }

    if (connector.agreement === false) {
        lineConnector.setAttribute("stroke-dasharray", "20,10");
    }

    return lineConnector;
}


function drawCross(connector, motherD, position, angle, dist, compensation, mother, daughter) {

    /*
let lineConnector = document.createElementNS(svgns, "line");
lineConnector.setAttribute("class", "connector");
lineConnector.setAttribute("id", connector.id);
lineConnector.setAttribute("transform", `translate(${position.x},${position.y}) scale(1,1) `) // ${zoomScaleConnector}
lineConnector.setAttribute("x1", (motherD + dist * .5 + 30 * compensation * Math.cos(angle)) * Math.cos(angle) * compensation);
lineConnector.setAttribute("y1", (motherD + dist * .5 + 0) * Math.sin(angle) * compensation);
lineConnector.setAttribute("x2", (dist - dist * .5 + 0) * Math.cos(angle) * compensation);
lineConnector.setAttribute("y2", (dist - dist * .5 + 0) * Math.sin(angle) * compensation);
lineConnector.setAttribute("stroke", "red");
lineConnector.setAttribute("stroke-width", "5");
*/


/*
const vec = {
    x: (mother.position.x - daughter.position.x),
    y: (mother.position.y - daughter.position.y),
};

var x1 = (motherD + DistanceArrows) * Math.cos(angle) * compensation;
var y1 = (motherD + DistanceArrows) * Math.sin(angle) * compensation;
var x2 = (dist - DistanceArrows) * Math.cos(angle) * compensation;
var y2 = (dist - DistanceArrows) * Math.sin(angle) * compensation;

let drawCross = document.createElementNS(svgns, "circle");
drawCross.setAttribute("cx", vec.x + DistanceArrows * Math.cos(angle) * compensation );
drawCross.setAttribute("cy", vec.y  + DistanceArrows * Math.sin(angle) * compensation);
drawCross.setAttribute("r", "22");
drawCross.setAttribute("fill", "red");
*/


    return drawCross;
}

function drawOuter(connector, daughter, dist, angle, compensation) {
    let outerConnector = document.createElementNS(svgns, "line");
    outerConnector.setAttribute("transform", `translate(${daughter.position.x},${daughter.position.y}) `)
    outerConnector.setAttribute("class", "outer-connector");
    outerConnector.setAttribute("id", connector.id);
    outerConnector.setAttribute("x1", 0);
    outerConnector.setAttribute("y1", 0);
    outerConnector.setAttribute("x2", (dist) * Math.cos(angle) * compensation);
    outerConnector.setAttribute("y2", (dist) * Math.sin(angle) * compensation);
    outerConnector.setAttribute("stroke", COLOUR.outerLine);
    outerConnector.setAttribute("stroke-width", 30);

    return outerConnector;
}

function drawSelected(daughter, dist, angle, compensation) {
    let highlight = document.createElementNS(svgns, "line");
    highlight.setAttribute("transform", `translate(${daughter.position.x},${daughter.position.y}) `)
    highlight.setAttribute("x1", 0);
    highlight.setAttribute("y1", 0);
    highlight.setAttribute("x2", (dist) * Math.cos(angle) * compensation);
    highlight.setAttribute("y2", (dist) * Math.sin(angle) * compensation);
    highlight.setAttribute("stroke", COLOUR.selectedLine);
    highlight.setAttribute("stroke-width", 30);
    return highlight;
}

function drawConnector(connector, mother, daughter) {


    const vec = {
        x: (mother.position.x - daughter.position.x),
        y: (mother.position.y - daughter.position.y),
    };

    const dir = {
        x: vec.x / Math.sqrt(vec.x ** 2 + vec.y ** 2),
        y: vec.y / Math.sqrt(vec.x ** 2 + vec.y ** 2)
    };
    const angle = dir.x === 0 ? Math.atan(dir.y / 0.001) : Math.atan(dir.y / dir.x);
    const motherD = Math.sqrt((Math.cos(angle) * 40) ** 2 + (Math.sin(angle) * 20) ** 2);
    const dist = Math.sqrt(vec.x ** 2 + vec.y ** 2) - motherD;
    const compensation = dir.x >= 0 ? 1 : -1;
    const position = {
        "x": daughter.position.x,
        "y": daughter.position.y
    };

    let group = document.createElementNS(svgns, "g");

    /* change ordering to enable click on ability "node selected -> connector"*/
    if (mother.isSelected || daughter.isSelected) {
        const selectedDraw = this.drawSelected(daughter, dist, angle, compensation);
        group.appendChild(selectedDraw);
    }

    const line = this.drawLine(connector, motherD, position, angle, dist, compensation);
    group.appendChild(line);

    const outer = this.drawOuter(connector, daughter, dist, angle, compensation);
    group.appendChild(outer);

    //const drawCross = this.drawCross(connector, motherD, position, angle, dist, compensation, mother, daughter);
    //group.appendChild(drawCross);

    

    return group;
}


function setOverlay() {
    let background = document.createElementNS(svgns, "rect");
    background.setAttribute("id", "overlay");
    background.setAttribute("x", 0);
    background.setAttribute("y", 0);
    background.setAttribute("width", "100%");
    background.setAttribute("height", "100%");
    background.setAttribute("fill", "#aaaaaa");
    background.setAttribute("opacity", .1);
    return background;
}

function drawOverlay() {
    const svg = document.querySelector("#CAMSVG");
    svg.innerHTML = "";

    svg.appendChild(setOverlay());
}