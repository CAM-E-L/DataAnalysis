if(usingMangoDB){
    async function fetchData(URL) {
        const dataRaw = await fetch(URL);
        if(dataRaw.status != 200){
console.log(dataRaw.status)
            defaultCAM() 
            return;
        }
        const data = await dataRaw.json()
    
        camMother = JSON.parse(data.cam);
        config = JSON.parse(data.config);
        linkRedirect = data.link;
        token = data.token
    
        console.log(linkRedirect);
        camMother.nodes.forEach(element => {
            element.kind = "Node";
            element.comment = "";
            element.eventLog = [];
            element.isActive = true;
            element.isConnectorSelected = false;
            element.isSelected = false;
            CAM.importElement(element);
        });
    
        camMother.connectors.forEach(element => {
            element.kind = "Connector";
            element.eventLog = "";
            CAM.importElement(element);
        });
        CAM.draw();
    
    }
    
    const queryString = window.location.search;
    const urlParams = new URLSearchParams(queryString);
    const link = urlParams.get('link');
    const participantID = urlParams.get('participantID');
    CAM.creator = participantID;
    
    
    
    fetchData(link + "&participantID=" + participantID);
}else{
    console.log("default CAM drawn - MongoDB is not used")

    async function getDefaultCAM() {
        defaultCAM();
    }

    getDefaultCAM();
}




