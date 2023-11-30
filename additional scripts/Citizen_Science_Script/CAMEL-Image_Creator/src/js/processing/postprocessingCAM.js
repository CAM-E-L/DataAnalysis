/* 
Class for post-processing CAM data used to
- adaptive study designs
applying in the future: https://js.cytoscape.org/#collection/centrality
*/
// get array with active nodes
function getActiveListNodes() {
    var listOfNodes = [];
    CAM.nodes.forEach((elt) => {
        //(new Node(elt))
        if (elt.getIsActive()) {
            listOfNodes.push({
                id: elt.id,
                text: elt.getText(),
                value: elt.getValue(),
                comment: elt.getComment(),
                isDeletable: elt.getIsDeletable(),
                connectedTo: elt.connectedTo,
            }); // adjust connectedTo using daughter / mother
        }
    });
    return listOfNodes;
}

// get array with nodes of certain valence
function getAllNodesOfValence(listOfNodes, valence) {
    const ARGSVALENCE = ["positive", "negative", "ambivalent", "neutral"];
    if (!ARGSVALENCE.includes(valence)) {
        throw "wrong argument for valence! Please use 'positive', 'negative', 'ambivalent', 'neutral'";
    }

    if (valence === "positive") {
        var listNodes = listOfNodes.filter(
            (elt) => elt.value >= 1 && elt.value < 5
        );
    }
    if (valence === "negative") {
        var listNodes = listOfNodes.filter((elt) => elt.value < 0);
    }
    if (valence === "ambivalent") {
        var listNodes = listOfNodes.filter((elt) => elt.value === 10);
    }
    if (valence === "neutral") {
        var listNodes = listOfNodes.filter((elt) => elt.value === 0);
    }

    return listNodes;
}

// get array with node(s) with highest degree UNDIRECTED
function getHighestDegree(listOfNodes) {
    // array with Degree of all nodes
    var degreeNodes = [];
    listOfNodes.forEach((elt) => {
        degreeNodes.push(elt.connectedTo.length);
    });
    // array with index of node(s) with maximum degree
    var indexDegreeNodes = [];
    degreeNodes.forEach((item, index) =>
        item === Math.max(...degreeNodes) ? indexDegreeNodes.push(index) : null
    );

    // output node(s) with maximum degree
    const HighestdegreeNodes = [];
    for (let i = 0; i < indexDegreeNodes.length; i++) {
        HighestdegreeNodes.push(listOfNodes[indexDegreeNodes[i]]);
    }

    return HighestdegreeNodes;
}

// get array with node(s) with longest / shortest text:
function getHighestLowestText(listOfNodes, type) {
    const ArgsType = ["min", "max"];
    if (!ArgsType.includes(type)) {
        throw "wrong argument for type! Please use 'min', 'max'";
    }

    // array with Degree of all nodes
    const LengthTextNodes = [];
    listOfNodes.forEach((elt) => {
        LengthTextNodes.push(elt.text.toString().length);
    });

    // array with index of node(s) with maximum degree
    const IndexTextNodes = [];
    if (type === "max") {
        LengthTextNodes.forEach((item, index) =>
            item === Math.max(...LengthTextNodes)
                ? IndexTextNodes.push(index)
                : null
        );
    }
    if (type === "min") {
        LengthTextNodes.forEach((item, index) =>
            item === Math.min(...LengthTextNodes)
                ? IndexTextNodes.push(index)
                : null
        );
    }

    // output node(s) with maximum degree
    const HighestLowestTextNodes = [];
    for (let i = 0; i < IndexTextNodes.length; i++) {
        HighestLowestTextNodes.push(listOfNodes[IndexTextNodes[i]]);
    }

    return HighestLowestTextNodes;
}

// mean valence
function getMeanValenceNodes(listOfNodes) {
    // array with value of all nodes
    const ValueNodes = [];
    listOfNodes.forEach((elt) => {
        if (elt.value === 10) {
            ValueNodes.push(0);
        } else {
            ValueNodes.push(elt.value);
        }
    });

    const sum = ValueNodes.reduce((a, b) => a + b, 0);
    const avg = sum / ValueNodes.length || 0;

    return avg;
}
