if(config.fullScreen == true){
    var paradefocuscount = 0;
    var arraydefocusevent = [];
    var lastBlurTimestamp;
    
    function enterFullscreen() {
        if (document.documentElement.requestFullscreen) {
            document.documentElement.requestFullscreen();
        } else if (document.documentElement.msRequestFullscreen) { // for IE11 (remove June 15, 2022)
            document.documentElement.msRequestFullscreen();
        } else if (document.documentElement.webkitRequestFullscreen) { // iOS Safari
            document.documentElement.webkitRequestFullscreen();
        }
    }
    
    
    window.addEventListener('load', (e) => {
        document.getElementById('alert').style.visibility = "visible";
        document.getElementById('hideall').style.visibility = "hidden";
    });
    
    
    window.addEventListener('blur', (e) => {
        paradefocuscount++;
        document.getElementById('alert').style.visibility = "visible";
        document.getElementById('hideall').style.visibility = "hidden";
        lastBlurTimestamp = e.timeStamp;
    });
    
    
    window.addEventListener('focus', (e) => {
        var durDefocus = e.timeStamp - lastBlurTimestamp;
        //console.log('duration:', duration)
        //console.log('sender', this.state.sender)
            arraydefocusevent.push(durDefocus)
            console.log("durDefocus: ", durDefocus, "arraydefocusevent: ", arraydefocusevent);
    })
}

