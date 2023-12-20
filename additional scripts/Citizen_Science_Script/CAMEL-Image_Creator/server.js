const WebSocket = require('ws');
const EventEmitter = require("events");

const eventEmitter = new EventEmitter();
const wss = new WebSocket.Server({ port: 8080 });
 


// Creating connection using websocket
wss.on("connection", ws => {
    console.log("new client connected");
    // sending message


    ws.on('message', function message(data, isBinary) {
        const message = isBinary ? data : data.toString();
        console.log(message);
        wss.clients.forEach(function each(client) {
          if (client !== ws && client.readyState === WebSocket.OPEN) {
            client.send(message);
          }
        });
      });


    // handling what to do when clients disconnects from server
    ws.on("close", () => {
        console.log("the client has connected");
    });
    // handling client connection error
    ws.onerror = function () {
        console.log("Some Error occurred")
    }
});
console.log("The WebSocket server is running on port 8080");