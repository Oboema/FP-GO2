var ipadres = "127.0.0.1";
var port = 9161;
var addOnUrl = "";
var ws;
var timeoutId = 0;

function openWebSocket() {
    if ( "WebSocket" in window) {
		console.log("Attempting to Connect");
		clearTimeout(timeoutId);
		timeoutId = -1;
        ws = new WebSocket("ws://"+ipadres+":"+port+"/"+addOnUrl);
		console.log("Socket made");
        ws.onopen =     function() {
                            onOpen();
                        };
        ws.onmessage =  function(event) {
                            onMessage(event.data);
                        };
        ws.onerror =    function(error) {
                            onError(error);
                        };
        ws.onclose =    function() {
                            onClose();
                        };
						
						
    } else {
        alert("Websocket is not supported in this browser!");
    }
}

function onOpen() {
    //Connection may be opened but interrupted during page load
    if(ws.readyState == 1) {
		console.log("Socket opened");
        setupConnection();
    }
}

function onMessage(message) {
    console.log("incoming: " + message);
    fromJsonObject(message);
}

function onError(error) {
	console.log("error");	
}

function onClose() {
	console.log("closing");
	if(timeoutId == -1) {
		timeoutId = setTimeout(openWebSocket, 100);	
	}
}

function sendMessage(message) {
    ws.send(message);
    console.log("outgoing: "+ message);
}

function closeWebSocket() {
	console.log(ws);
	ws.close();
	if(timeoutId == -1) {
		timeoutId = setTimeout(openWebSocket, 100);
	}
}