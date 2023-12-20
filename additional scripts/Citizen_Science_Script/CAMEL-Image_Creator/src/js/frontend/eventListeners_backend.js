$(document).on("mousedown", ".node", function (event) {
    arrayPositions = [];
    /* if double click */
    if (event.detail == 2) {
        CAM.selecteNode($(this)[0].id);

        if (CAM.currentNode != null) {
            // get text of current node
            document.getElementById("inptextnode").value =
                CAM.currentNode.getText();
            // get comment of current node
            document.getElementById("inpcommentnode").value =
                CAM.currentNode.getComment();
            // get value slider, hide / show graphics / change colors
            var backendGreenColorNodeSlider = document.querySelector(
                ".greenColorNodeSlider"
            );
            var backendRedColorNodeSlider = document.querySelector(
                ".redColorNodeSlider"
            );

            if (CAM.currentNode.value == 0) {
                document.getElementById("nodeSlider").value = 4;

                document.getElementById("checkboxAmbivalent").checked = false;
                document.getElementById("nodeSlider").disabled = false;

                backendRedColorNodeSlider.style.backgroundColor =
                    "hsl(0, 50%, 60%)";
                backendGreenColorNodeSlider.style.backgroundColor =
                    "hsl(110, 50%, 60%)";
            } else if (CAM.currentNode.value == 10) {
                document.getElementById("nodeSlider").value = 4;
                backendRedColorNodeSlider.style.backgroundColor =
                    "hsl(0, 50%, 60%)";
                backendGreenColorNodeSlider.style.backgroundColor =
                    "hsl(110, 50%, 60%)";

                document.getElementById("checkboxAmbivalent").checked = true;
                document.getElementById("nodeSlider").disabled = true;
            } else if (CAM.currentNode.value < 0) {
                document.getElementById("checkboxAmbivalent").checked = false;
                document.getElementById("nodeSlider").disabled = false;
                if (CAM.currentNode.value == -1) {
                    document.getElementById("nodeSlider").value = 3;
                    backendRedColorNodeSlider.style.backgroundColor =
                        "hsl(0, 50%, 60%)";
                } else if (CAM.currentNode.value == -2) {
                    document.getElementById("nodeSlider").value = 2;
                    backendRedColorNodeSlider.style.backgroundColor =
                        "hsl(0, 50%, 50%)";
                } else if (CAM.currentNode.value == -3) {
                    document.getElementById("nodeSlider").value = 1;
                    backendRedColorNodeSlider.style.backgroundColor =
                        "hsl(0, 50%, 40%)";
                }
            } else if (
                CAM.currentNode.value > 0 &&
                CAM.currentNode.value <= 4
            ) {
                document.getElementById("checkboxAmbivalent").checked = false;
                document.getElementById("nodeSlider").disabled = false;
                if (CAM.currentNode.value == 1) {
                    document.getElementById("nodeSlider").value = 5;
                    backendGreenColorNodeSlider.style.backgroundColor =
                        "hsl(110, 50%, 60%)";
                } else if (CAM.currentNode.value == 2) {
                    document.getElementById("nodeSlider").value = 6;
                    backendGreenColorNodeSlider.style.backgroundColor =
                        "hsl(110, 50%, 50%)";
                } else if (CAM.currentNode.value == 3) {
                    document.getElementById("nodeSlider").value = 7;
                    backendGreenColorNodeSlider.style.backgroundColor =
                        "hsl(110, 50%, 40%)";
                }
            }

            /* change position of pop up */
            if (CAM.currentNode.position.x - 380 < 0) {
                var changeAtLeft = "left+" + (CAM.currentNode.position.x + 70); // to far left position to right
            } else {
                var changeAtLeft = "left+" + (CAM.currentNode.position.x - 360); // position to left
            }
            var changeAtTop = "top+" + (CAM.currentNode.position.y - 10);

            $("#dialogInteractionNode").dialog("open");
        }
    } else {
        CAM.readyToMove = true;
        resetConnectorSelection();
        CAM.selecteNode($(this)[0].id);
    }

    CAM.draw();
});

$(document).on("mouseup", ".node", function (event) {
    // save position data for only every 150px difference of X or Y
    var newArrX = [];
    var tmpArrayPosX = arrayPositions[0];
    for (var i = 1; i < arrayPositions.length; i++) {
        if (Math.abs(arrayPositions[i].value.x - tmpArrayPosX.value.x) >= 150) {
            newArrX.push(1);
            tmpArrayPosX = arrayPositions[i];
        } else {
            newArrX.push(0);
        }
    }
    newArrX.unshift(0);

    var newArrY = [];
    var tmpArrayPosY = arrayPositions[0];
    for (var i = 1; i < arrayPositions.length; i++) {
        if (Math.abs(arrayPositions[i].value.y - tmpArrayPosY.value.y) >= 150) {
            newArrY.push(1);
            tmpArrayPosY = arrayPositions[i];
        } else {
            newArrY.push(0);
        }
    }
    newArrY.unshift(0);

    var newArrayPositions = [];
    newArrayPositions.unshift(arrayPositions[0]);

    // simple check that no 2 undefined entries are included (non-moved element)
    if (arrayPositions.length > 2) {
        for (var i = 1; i < arrayPositions.length; i++) {
            if (newArrX[i] == 1 || newArrY[i] == 1) {
                newArrayPositions.push(arrayPositions[i]);
            }
        }

        newArrayPositions.forEach((element) => {
            CAM.currentNode.eventLog.push(element);
        });
    }

    newArrayPositions.push(arrayPositions[arrayPositions.length - 1]);

    CAM.readyToMove = false;
    if (CAM.hasElementMoved) {
        resetConnectorSelection();
        resetNodeSelection();
        CAM.hasElementMoved = false;
        CAM.draw();
    }
});

/* for what these two event handlers??? */

$(document).on("click", ".connector", function (event) {
    resetConnectorSelection();
    resetNodeSelection();
    CAM.selectConnection($(this)[0].id);

    CAM.draw();
});

$(document).on("click", ".outer-connector", function (event) {
    resetConnectorSelection();
    resetNodeSelection();
    CAM.selectConnection($(this)[0].id);

    CAM.draw();
});

$(document).on("mousedown", ".connector, .outer-connector", function (event) {
    /* if double click */

    // console.log($(this)[0].id);

    if (event.detail == 2) {
        resetConnectorSelection();
        resetNodeSelection();
        CAM.selectConnection($(this)[0].id);

        if (CAM.currentConnector != null) {
            var backendGreenColorSlider = document.querySelector(
                ".greenConnectorColorSlider"
            );
            var backendGreenColorTick =
                document.querySelector(".greenColorTick");

            var backendRedColorSlider = document.querySelector(
                ".redColorConnectorSlider"
            );
            var backendRedColorTick = document.querySelector(".redColorTick");

            if (CAM.currentConnector.agreement) {
                backendRedColorSlider.style.backgroundColor = "white";
                backendRedColorTick.style.backgroundColor = "white";

                document.getElementById("edgeSlider").value =
                    CAM.currentConnector.getIntensity() /
                        IncreaseSliderIntensity +
                    3;
                if (document.getElementById("edgeSlider").value == 4) {
                    backendGreenColorSlider.style.backgroundColor =
                        "hsl(110, 100%, 70%)";
                    backendGreenColorTick.style.backgroundColor =
                        "hsl(110, 100%, 70%)";
                } else if (document.getElementById("edgeSlider").value == 5) {
                    backendGreenColorSlider.style.backgroundColor =
                        "hsl(110, 100%, 50%)";
                    backendGreenColorTick.style.backgroundColor =
                        "hsl(110, 100%, 50%)";
                }
                if (document.getElementById("edgeSlider").value == 6) {
                    backendGreenColorSlider.style.backgroundColor =
                        "hsl(110, 100%, 40%)";
                    backendGreenColorTick.style.backgroundColor =
                        "hsl(110, 100%, 40%)";
                }
            } else if (!CAM.currentConnector.agreement) {
                backendGreenColorSlider.style.backgroundColor = "white";
                backendGreenColorTick.style.backgroundColor = "white";

                if (
                    CAM.currentConnector.getIntensity() ==
                    IncreaseSliderIntensity
                ) {
                    document.getElementById("edgeSlider").value = 3;
                    backendRedColorSlider.style.backgroundColor =
                        "hsl(0, 100%, 70%)";
                    backendRedColorTick.style.backgroundColor =
                        "hsl(0, 100%, 70%)";
                } else if (
                    CAM.currentConnector.getIntensity() ==
                    IncreaseSliderIntensity * 2
                ) {
                    document.getElementById("edgeSlider").value = 2;
                    backendRedColorSlider.style.backgroundColor =
                        "hsl(0, 100%, 50%)";
                    backendRedColorTick.style.backgroundColor =
                        "hsl(0, 100%, 50%)";
                } else if (
                    CAM.currentConnector.getIntensity() ==
                    IncreaseSliderIntensity * 3
                ) {
                    document.getElementById("edgeSlider").value = 1;
                    backendRedColorSlider.style.backgroundColor =
                        "hsl(0, 100%, 40%)";
                    backendRedColorTick.style.backgroundColor =
                        "hsl(0, 100%, 40%)";
                }
            }

            /* change position of pop up */
            // > get current mother / daugther
            var currentMotherNode = CAM.nodes.filter(
                (el) => el.id === CAM.currentConnector.source
            )[0];
            var currentDaughterNode = CAM.nodes.filter(
                (el) => el.id === CAM.currentConnector.target
            )[0];
            // > get midpoint of connector
            var MeanPositionX =
                (currentMotherNode.position.x +
                    currentDaughterNode.position.x) /
                2;
            var MeanPositionY =
                (currentMotherNode.position.y +
                    currentDaughterNode.position.y) /
                2;

            if (MeanPositionX - 380 < 0) {
                var changeAtLeft = "left+" + (MeanPositionX + 40); // to far left position to right
            } else {
                var changeAtLeft = "left+" + (MeanPositionX - 340); // position to left
            }

            if (
                Math.abs(
                    currentMotherNode.position.x -
                        currentDaughterNode.position.x
                ) < 300
            ) {
                var changeAtTop = "top+" + (MeanPositionY - 130); // x = horizontal spacing less than 300
            } else {
                var changeAtTop = "top+" + MeanPositionY;
            }

            $("#dialogInteractionEdge").dialog("open");
        }

        CAM.draw();
    }
});

$(document).on("click", "#background", function (event) {
    if (!(resetConnectorSelection() || resetNodeSelection())) {
        const positionClick = {
            x: event.clientX - $("#CAMSVG").position().left, // / zoomScale,
            y: event.clientY - $("#CAMSVG").position().top, // / zoomScale
        };

        CAM.addElement(new NodeCAM(0, "", positionClick, 1, 1, 1));
    }

    CAM.draw();
});

$(document).on("mousemove", "#CAMSVG", function (event) {
    const positionClick = {
        x: event.clientX - $("#CAMSVG").position().left, // / zoomScale,
        y: event.clientY - $("#CAMSVG").position().top, // / zoomScale
    };

    if (CAM.readyToMove) {
        CAM.hasElementMoved = true;
        CAM.updateElement("Node", "position", positionClick);

        arrayPositions.push({
            time: new Date(),
            type: "position",
            value: positionClick,
        });
    }

    CAM.draw();
});

$(document).on("mouseup", "#CAMSVG", function (event) {
    if (CAM.readyToMove) {
        CAM.readyToMove = false;
        resetNodeSelection();
        CAM.draw();
    }
});

function resetConnectorSelection() {
    if (CAM.hasSelectedConnector) {
        CAM.unselectConnection();
        return true;
    }
    return false;
}

function resetNodeSelection() {
    if (CAM.hasSelectedNode) {
        CAM.unselectNode();
        return true;
    }
    return false;
}

/* Add camera feature */
if (config.cameraFeature) {
    $(document).on("mouseover", "#background", function (event) {
        var positionMouse = {
            x: event.clientX - $("#CAMSVG").position().left, // / zoomScale,
            y: event.clientY - $("#CAMSVG").position().top, // / zoomScale
        };

        /*
       var arrayPosX = [];
        CAM.nodes.forEach(element => {
            arrayPosX.push(element.position.x)
        });
        /arrayPosX = arrayPosX.filter(element => element > 1900 || element < -300);
        */

        //console.log("positionMouse - X: ", positionMouse.x, "positionMouse - Y: ", positionMouse.y);
        //console.log("positionMouse.y: ", positionMouse.y);
        if (
            positionMouse.x < 20 ||
            positionMouse.x > 1280 ||
            positionMouse.y < 20 ||
            positionMouse.y > 740
        ) {
            //console.log("stopConY: ", stopConY);
            // $("body").css("cursor", "move");

            if (stopConX > -500 && positionMouse.x > 1290) {
                CAM.nodes.forEach((element) => {
                    element.position.x -= moveCAMSpeed;
                });
                stopConX -= moveCAMSpeed;
            } else if (stopConX < 500 && positionMouse.x < 10) {
                CAM.nodes.forEach((element) => {
                    element.position.x += moveCAMSpeed;
                });
                stopConX += moveCAMSpeed;
            }

            if (stopConY > -250 && positionMouse.y > 755) {
                CAM.nodes.forEach((element) => {
                    element.position.y -= moveCAMSpeed;
                });
                stopConY -= moveCAMSpeed;
            } else if (stopConY < 250 && positionMouse.y < 10) {
                CAM.nodes.forEach((element) => {
                    element.position.y += moveCAMSpeed;
                });
                stopConY += moveCAMSpeed;
            }

            CAM.draw();
        } else {
            $("body").css("cursor", "default");
        }
    });
}
