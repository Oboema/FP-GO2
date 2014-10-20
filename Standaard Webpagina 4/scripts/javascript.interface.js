$(window).load(function () {
    setupPage();
    setupCanvas();
    openWebSocket();
 });

var canvasName = "IOcanvas";
var wrapperName = canvasName + "Wrapper";
var canvasId = "#" + canvasName;
var interval = 15; // in milliseconds

var onMouseClickFunc = function(layer) {
                    console.log("click!");
                    mouseClick(layer.name, layer.eventX, layer.eventY, layer.event.which);
                  }

var onMouseUpFunc = function(layer) {
						mouseUp(layer.name, layer.eventX, layer.eventY, layer.event.which);
					}					  
				  
var onMouseDownFunc = function(layer) {
						mouseDown(layer.name, layer.eventX, layer.eventY, layer.event.which);
                      }

var standardProperties = {
	layer: true,
	click: onMouseClickFunc,
	mouseup: onMouseUpFunc,
	mousedown: onMouseDownFunc
};					  
					  
function setupCanvas() {
    $(document).keydown(
                    function(event) {
                        keyPressed(event.keyCode);
                    }
                    );              
}

function setupPage() {
    var text = "A connection to " + ipadres + " is being made...";
    document.getElementById(wrapperName).innerHTML = "<span> " + text + "</span>";  
}

function setupConnection() {
    $(canvasId).removeLayers();
    sendSystemMessage("setup");
    sendSystemMessage("background"); 
}

/* Draw functions */
function drawCanvasRectangle(name, edgeColor, edgeThickness, color, position, w, h, groupname) {
	var rect = {
        name: name,
		strokeStyle: edgeColor,
		strokeWidth: edgeThickness,
        fillStyle: color,
        x: position.x, y: position.y,
        width: w,
        height: h,
        fromCenter: false,
		groups: [groupname]
    };
    
    $.extend(rect, standardProperties);
	
    $(canvasId).drawRect(rect);
}

function drawCanvasLine(name, edgeColor, edgeThickness, positions, groupname) {
    var line = {
		name: name,
		strokeStyle: edgeColor,
		strokeWidth: edgeThickness,
		rounded: false,
		groups:[groupname]
	};
	
    $.extend(line, standardProperties);
    
	for(var i = 0; i < positions.length; i++) {
		line['x'+(i+1)] = positions[i].x;
		line['y'+(i+1)] = positions[i].y;
	}
	
	$(canvasId).drawLine(line);
}

function drawCanvasText(name, edgeColor, edgeThickness, color, position, size, font, text, fromCenter, groupname) {
    var text = {
		name: name,
		strokeStyle: color,
		strokeWidth: edgeThickness,
		fillStyle: edgeColor,
		x: position.x, y: position.y,
		fontSize: size,
		fontFamily: font,
		text: text,
		fromCenter: fromCenter,
		groups: [groupname]
	}
    
    $.extend(text, standardProperties);

    $(canvasId).drawText(text);
}

function drawCanvasArc(name, edgeColor, edgeThickness, color, position, rad, startAng, endAng, groupname) {
	var arc = {
        name: name,
        strokeStyle: edgeColor,
		strokeWidth: edgeThickness,
		fillStyle: color,
        x: position.x, y: position.y,
        radius: rad,
		start: startAng, end: endAng, //0,360 is top of arc
		groups: [groupname],
		inDegrees: true
    }
    
    $.extend(arc, standardProperties);
    
    $(canvasId).drawArc(arc);
}

/*Manipulate functions */
function moveElement(name, xPos, yPos, relative) {
    var x = 0;
	var y = 0;
	
	if(relative) {
		var layer = $(canvasId).getLayer(name);
        if(layer) {
            x = layer.x;
            y = layer.y;
        }
	}
    
	$(canvasId).setLayer(name, {
		x: x + xPos, y: y + yPos
	});
    $(canvasId).drawLayers();
}

function moveGroup(name, xPos, yPos, relative) {
	var list = $(canvasId).getLayerGroup(name);
	if(list) {
        for(var i = 0; i < list.length; i++) {
            moveElement(list[i].name, xPos, yPos, relative);
        };
    };
}

function removeElement(name) {
   	$(canvasId).removeLayer(name);
	$(canvasId).drawLayers();
}

function removeGroup(name) {
	$(canvasId).removeLayerGroup(name);
	$(canvasId).drawLayers();
}

function clearAll() {
	var h = $(canvasId).height();
	var w = $(canvasId).width();
	console.log(h);
	console.log(w);
	
	$(canvasId).removeLayers();
	$(canvasId).clearCanvas();
	
	setCanvas(h, w);
}

/* IO Events */
/* Decode functions Server -> Client */

function error(message) {
	alert(message);
}

function fromJsonObject (jsonMessage) {
    var jsonObject = jQuery.parseJSON(jsonMessage);
 
    if(jsonObject.mode == "systemmessageanswers") {
        fromJsonSystemMessageResponses(jsonObject.answers);
    } else {
        fromJsonGObject(jsonObject.command);
    }
}

function fromJsonSystemMessageResponses(list) {

    for(var i = 0; i < list.length; i++) { 
        var answer = list[i].systemmessageanswer;
        
        switch (answer) {
            case "canvassetup" :
                setCanvas(list[i].dimension.h, list[i].dimension.w);
                break;
            case "timer" :
                timer(list[i].timedata);
                break;
			case "close" :
				closeWebSocket();
				break;
        }
    }
}

function setCanvas(h, w) {
    var canvas = "<canvas id=\"" + canvasName + "\" width=\"" + w + "px\" height=\"" + h + "px\"></canvas>"
    document.getElementById(wrapperName).innerHTML = canvas;
    document.getElementById(wrapperName).style.height = h + "px";
    document.getElementById(wrapperName).style.width = w + "px";
    
    var rect = {
        name: "canvas",
        fillStyle: 'rgb(255, 255, 255)',
        x: 0, y: 0,
        width: w,
        height: h,
        fromCenter: false
    };
    
    $.extend(rect, standardProperties);
    
    $(canvasId).drawRect(rect);
}

var timer;

function timer(timedata) {
    console.log("Time: " + timedata);
    if(timedata.use) {
        timer = window.setInterval(function() {onTime();}, timedata.i);
    } else {
        window.clearInterval(timer);
    }
}

function onTime() {
    sendSystemMessage("time");
}

function fromJsonGObject(jsonObject) {
	if(jsonObject.mode == "draw") {
		drawJsonGObject(jsonObject.gobject, jsonObject.groupname);
	} else if (jsonObject.mode == "movegroup") {
		moveGroup(jsonObject.groupname, jsonObject.position.x, jsonObject.position.y, jsonObject.relative);
	} else if (jsonObject.mode == "moveelement") {
		moveElement(jsonObject.name, jsonObject.position.x, jsonObject.position.y, jsonObject.relative);
	} else if (jsonObject.mode == "removegroup") {
		removeGroup(jsonObject.groupname);
	} else if (jsonObject.mode == "removeelement") {
		removeElement(jsonObject.name);	
	} else if (jsonObject.mode == "clearall") {
		clearAll();
	} else {
		error("Found a message with a non-supported mode: " + jsonObject.mode);
	}
}

function drawJsonGObject(gObject, groupname) {
    if(gObject.prim) {
        var type = gObject.prim.type;
        var edgeColor = toColor(gObject.prim.edgecolor);

        switch (type) {
            case "rect": 
                    var color = toColor(gObject.prim.color); 
                    drawCanvasRectangle(gObject.name, edgeColor, gObject.prim.edgethickness, color, gObject.prim.position, gObject.prim.dimension.width, gObject.prim.dimension.height, groupname);
                break;
            case "line":
                    drawCanvasLine(gObject.name, edgeColor, gObject.prim.edgethickness, gObject.prim.positions, groupname);
                break;
            case "text":
                    var color = toColor(gObject.prim.color); 
                    drawCanvasText(gObject.name, edgeColor, gObject.prim.edgethickness, color, gObject.prim.position, gObject.prim.size, gObject.prim.font, gObject.prim.text, gObject.prim.fromcenter, groupname);
                break;
            case "arc":
                    var color = toColor(gObject.prim.color); 
                    drawCanvasArc(gObject.name, edgeColor, gObject.prim.edgethickness, color, gObject.prim.position, gObject.prim.radius, gObject.prim.startang, gObject.prim.endang, groupname);
                break;
            default:
                error("Found a primitive that is not supported: " + type);
                break;
        }
    } else {
        
        // Container
    }
	for(var i = 0; i < gObject.children.length; i++) {
		drawJsonGObject(gObject.children[i], groupname);
	}
}

function toColor(color) {
    return "rgb(" + color.r + ", " + color.g + ", " + color.b + ")";
}

/* Encode Functions Client -> Server */
function toJsonVarString(left, right) {
	return "\"" + left + "\":\"" + right + "\"";
}

function toJsonVarInt(left, right) {
	return "\"" + left + "\":" + right;
}

function keyPressed(key) {
	button = keyValueToText(key);
	sendMessage("{" + toJsonVarString("type", "keyboard") + "," + toJsonVarString("button", button) + "}");
}

function mouseClick(name, x, y, buttonNumber) {
	mouseMessage(name, x, y, buttonNumber, "mouseclick");
}

function mouseUp(name, x, y, buttonNumber) {
	mouseMessage(name, x, y, buttonNumber, "mouseup");
}

function mouseDown(name, x, y, buttonNumber) {
	mouseMessage(name, x, y, buttonNumber, "mousedown");
}

function buttonNumberToButton(buttonNumber) {
	var buttonText;
	
	switch(buttonNumber) {
		case 1  : 
		default : buttonText = "left"; 
				  break;
		case 2  : buttonText = "middle";
				  break;
		case 3  : buttonText = "right";
				  break;
		
	}
	
	return buttonText;
}

function mouseMessage(name, x, y, buttonNumber, mouseType) {
	var type = toJsonVarString("type", "mouse");
	var mouseType_ = toJsonVarString("mousetype", mouseType);
	var button_ = toJsonVarString("button", buttonNumberToButton(buttonNumber));
	var x_ = toJsonVarInt("x", x);
	var y_ = toJsonVarInt("y", y);
	var element = toJsonVarString("element", name);
    
    sendMessage("{" + type + "," + mouseType_ + "," + button_ + "," + x_ + "," + y_ + "," + element + "}");
}

function sendSystemMessage(message) {
    sendMessage("{" + toJsonVarString("type", "systemmessage") + "," + toJsonVarString("message", message) + "}");
}

function keyValueToText(value) {
	var r;
	switch (value) {
		//MAIN section
		case 8 : r ="backspace"; break;
		case 9 : r ="tab"; break;
		case 13 : r ="enter"; break;
		case 16 : r ="shift"; break;
		case 17 : r ="ctrl"; break;
		case 18 : r ="alt"; break;
		case 20 : r ="caps"; break;
		case 23 : r ="esc"; break;
		case 32 : r ="space"; break;
		case 33 : r ="pgup"; break;
		case 34 : r ="pgdn"; break;
		case 35 : r ="end"; break;
		case 36 : r ="home"; break;
		case 45 : r ="insert"; break;
		case 46 : r ="delete"; break;
		
		case 48 : r ="0"; break;
		case 49 : r ="1"; break;
		case 50 : r ="2"; break;
		case 51 : r ="3"; break;
		case 52 : r ="4"; break;
		case 53 : r ="5"; break;
		case 54 : r ="6"; break;
		case 55 : r ="7"; break;
		case 56 : r ="8"; break;
		case 57 : r ="9"; break;
	
		case 59 : r =";/:"; break;
		case 61 : r ="=/+"; break;
		
		case 65 : r ="a"; break;
		case 66 : r ="b"; break;
		case 67 : r ="c"; break;
		case 68 : r ="d"; break;
		case 69 : r ="e"; break;
		case 70 : r ="f"; break;
		case 71 : r ="g"; break;
		case 72 : r ="h"; break;
		case 73 : r ="i"; break;
		case 74 : r ="j"; break;
		case 75 : r ="k"; break;
		case 76 : r ="l"; break;
		case 77 : r ="m"; break;
		case 78 : r ="n"; break;
		case 79 : r ="o"; break;
		case 80 : r ="p"; break;
		case 81 : r ="q"; break;
		case 82 : r ="r"; break;
		case 83 : r ="s"; break;
		case 84 : r ="t"; break;
		case 85 : r ="u"; break;
		case 86 : r ="v"; break;
		case 87 : r ="w"; break;
		case 88 : r ="x"; break;
		case 89 : r ="y"; break;
		case 90 : r ="z"; break;
		
		case 91 : r ="winkey"; break;
		case 93 : r ="dirkey"; break;
		
		case 112 : r ="F1"; break;
		case 113 : r ="F2"; break;
		case 114 : r ="F3"; break;
		case 115 : r ="F4"; break;
		case 116 : r ="F5"; break;
		case 117 : r ="F6"; break;
		case 118 : r ="F7"; break;
		case 119 : r ="F8"; break;
		case 120 : r ="F9"; break;
		case 121 : r ="F10"; break;
		case 122 : r ="F11"; break;
		case 123 : r ="F12"; break;
		
		case 173 : r ="-/_"; break;
		case 188 : r =",/<"; break;
		case 190 : r ="./>"; break;
		case 191 : r ="//?"; break;
		case 192 : r ="`/~"; break;
		case 219 : r ="[/{"; break;
		case 220 : r ="\/|"; break;
		case 221 : r ="]/}"; break;
		case 222 : r ="'/\""; break;
		
		
		// NUMPAD section
		case 96 : r ="num0"; break;
		case 97 : r ="num1"; break;
		case 98 : r ="num2"; break;
		case 99 : r ="num3"; break;
		case 100 : r ="num4"; break;
		case 101 : r ="num5"; break;
		case 102 : r ="num6"; break;
		case 103 : r ="num7"; break;
		case 104 : r ="num8"; break;
		case 105 : r ="num9"; break;
		case 106 : r ="num*"; break;
		case 107 : r ="num+"; break;
		case 109 : r ="num-"; break;
		case 110 : r ="numDEL/."; break;
		case 111 : r ="num/"; break;
		case 114 : r ="numlock"; break;
		
		
		default : r ="undefined";
	}
	return r;
}