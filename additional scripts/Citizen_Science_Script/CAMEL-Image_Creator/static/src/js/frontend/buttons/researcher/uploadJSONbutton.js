/* add button: */
const uploadJSONButton = `<button class="material-icons" onclick="document.getElementById('fileToLoad').click();" title="Upload CAM from file">vertical_align_top</button>
<input type='file' id="fileToLoad" style="display:none">`;
var target = document.getElementById("hideResearcherButtonsTop");
target.innerHTML += uploadJSONButton;



/* > upload CAM as JSON file
adjusted: https://stackoverflow.com/questions/36127648/uploading-a-json-file-and-using-it
https://stackoverflow.com/questions/23344776/how-to-access-data-of-uploaded-json-file
https://stackoverflow.com/questions/31746837/reading-uploaded-text-file-contents-in-html */
$(function () {
    $('#fileToLoad').on('change', async (event) => {
        // delete former CAM
        CAM.connectors = [];
        CAM.nodes = [];
        CAM.draw();
        console.log("complete CAM has been deleted");

        /* get file list */
        var fileToLoad = document.getElementById("fileToLoad").files; // [0]
        //console.log("file to load: ", fileToLoad)
        // console.log("fileToLoad:", fileToLoad);
        /* parse to JSON file */
        var jsonObj = await fileToJSON(fileToLoad);
        console.log("file to load parsed: ", jsonObj);
        //console.log("file to load parsed length nodes: ", jsonObj.nodes.length);

        // add CAM information: 
        CAM.idCAM = jsonObj.idCAM
        CAM.creator = jsonObj.creator
        CAM.projectCAM = jsonObj.projectCAM

        
        /* draw CAM */
        arrayIDs = [];
        for (var i = 0; i < jsonObj.nodes.length; i++) {
            var elementNode = jsonObj.nodes[i];
            //console.log(elementNode);

            if(elementNode.isActive){
                CAM.addElement(new NodeCAM(elementNode.value, elementNode.text, {
                    x: elementNode.position.x,
                    y: elementNode.position.y
                }, elementNode.isDraggable, elementNode.isDeletable, elementNode.isTextChangeable));
    
         
                // CAM.nodes[i].id = elementNode.id; // add ID of former node
                // CAM.nodes[i].isDraggable = true; // moveable
                arrayIDs.push(elementNode.id);
            }

        }

        // draw connectors
        for (var i = 0; i < jsonObj.connectors.length; i++) {
            //CAM.nodes.match(elt => elt.id ===     jsonObj.connectors[0].source)
            var elementConnector = jsonObj.connectors[i];
            //console.log(elementConnector);

            if(elementConnector.isActive){
            var connector1 = new ConnectorCAM();

            connector1.establishConnection(CAM.nodes[arrayIDs.indexOf(elementConnector.source)], CAM.nodes[arrayIDs.indexOf(elementConnector.target)],
                elementConnector.intensity, elementConnector.agreement);
            connector1.isBidirectional = elementConnector.isBidirectional;
            connector1.isDeletable = elementConnector.isDeletable;
            CAM.addElement(connector1);
            }
        }
        // draw CAM
        CAM.draw();
    });
});





function fileToJSON(file) {
    return new Promise((resolve, reject) => {
        const fileReader = new FileReader()
        fileReader.onload = event => resolve(JSON.parse(event.target.result))
        fileReader.onerror = error => reject(error)
        fileReader.readAsText(file.item(0))
    });
}
