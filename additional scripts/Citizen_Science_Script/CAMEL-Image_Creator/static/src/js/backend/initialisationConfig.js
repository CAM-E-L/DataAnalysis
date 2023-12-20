if(usingMangoDB){
    async function fetchData(URL) {
        const dataRaw = await fetch(URL);
        if(dataRaw.status != 200){
console.log(dataRaw.status)
            return;
        }

        usingMangoDB = true;
        const data = await dataRaw.json()
    
        config = JSON.parse(data.config);
        console.log('config within: ', config)
        /*
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
        */
    
    }
    
    const queryString2 = window.location.search;
    const urlParams2 = new URLSearchParams(queryString2);
    console.log('urlParams2: ', urlParams2);
    const link2 = urlParams2.get('link');

    
    
    
    fetchData(link2);

    console.log('config outer: ', config)
}
    
    