const ws = new WebSocket("ws://localhost:8080");

ws.addEventListener("open", () => {
    ws.send("Connection established.");
});


ws.onmessage = function (msg) {

    const data = JSON.parse(msg.data);
    
    CAM.isIncoming = true;

    if (data.type == "Add") {
            CAM.importElement(data.value);
    }

    if (data.type == "Update") {

        switch (data.kind) {
            case "Node":
                console.log(CAM.getNodeById(data.id));
                CAM.currentNode = CAM.getNodeById(data.id);
                break;
            case "Connector":
                CAM.currentConnector = CAM.getConnectorById(data.id);
                break;
        }
        CAM.updateElement(data.kind, data.field, data.value);
    } 

    CAM.isIncoming = false;
};


function sendMessage(type, id, kind, field, value){
    console.log(type);
    if (CAM.isIncoming == false) {
        ws.send(JSON.stringify({
            "type": type,
            "id": id,
            "kind":kind,
            "field":field,
            "value":value
        }));
    }
}