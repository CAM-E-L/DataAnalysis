
const contentFirstReminder =  `
Sie haben nur noch <b>3 Minuten</b> Zeit Ihre <i>kognitiv-affektive Karte</i> (CAM) zu zeichnen. Bitte schließen Sie diese langsam ab und denken Sie an folgendes:
<ul  style="font-size: 14px;">
  <li>Verwenden Sie nicht mehr als drei Wörter pro Begriff und lassen Sie kein gezeichnetes Konzept leer.</li>
  <li>Verbinden Sie alle gezeichneten Begriffe miteinander.</li>
     </ul>
     <br>
     Klicken Sie bitte auf dem Hintergrund um fortzufahren.
`;


const contentFinalReminder =  `
Bitte schließen Sie das Zeichnen der <i>kognitiv-affektive Karte</i> (CAM) nun bitte ab und bedenken Sie folgendes:
<ul  style="font-size: 14px;">
  <li>Lassen Sie kein gezeichnetes Konzept leer.</li>
  <li>Verbinden Sie alle gezeichneten Begriffe miteinander.</li>
     </ul>
     <br>
     Klicken Sie bitte auf dem Hintergrund um fortzufahren und auf das Diskettensymbol oben rechts, um zu speichern.
`;



var startTimeMS = 0;  // EPOCH Time of event count started
var timerStepFirst = 5000;   // Time first reminder
var timerStepFinal = 10000;   // Time final reminder


function firstReminder() {
   $("#dialogReminder").dialog("open");
   $("#textDialogReminder")[0].innerHTML = contentFirstReminder;
 }
   
 function finalReminder() {
   $("#dialogReminder").dialog("open");
   $("#textDialogReminder")[0].innerHTML = contentFinalReminder;
 }
   


// This function starts the two reminder
function startTimer(){
   startTimeMS = (new Date()).getTime();
   setTimeout(firstReminder,timerStepFirst);
   setTimeout(finalReminder,timerStepFinal);
}


// Gets the number of ms remaining to execute the eventRaised Function
function getRemainingTime(){
    var remainingTimeFirst =  timerStepFirst - ( (new Date()).getTime() - startTimeMS );
    var remainingTimeFinal =  timerStepFinal - ( (new Date()).getTime() - startTimeMS );

    console.log("remaining time first reminder:", remainingTimeFirst);
    console.log("remaining time final reminder:", remainingTimeFinal);
}


if(config.setReminder){
  startTimer();
}



