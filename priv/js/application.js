/*
    the code is taken from Beebole.com and adapted to Websockets
*/
var zIndex = 0;
var colors = ["pink", "yellow", "green", "blue"];

var _port = window.location.href.split('/')[2].split(':')[1];
port = (_port == undefined) ? 80 :_port;

var refresh = {
                "action" : "read_all",
            }

function rand(upper){
	return Math.floor(Math.random()*upper) + 50
}

    
function encode_json(json){
    return JSON.stringify(json);
}

function submit_json(json, ws){
    var msg = encode_json(json);
    console.log("sending: ", msg);
    ws.send(msg);
}

function renderNotes(json){
    $("div#board").html($p.render("notes", json));
}

function renderNote(json){
    $("div#board").after($p.render("notes", json));
    $("div#note_" + json[0].id).children("textarea").focus();
}

function zTop(note) {
    zIndex += 1; 
    note.css('z-index', zIndex); 
    note.attr("z", zIndex); 
}

$(document).ready(function(){
    const ws = new WebSocket('ws://localhost:' 
     + port + '/websocket/note_handler');

    ws.onclose = function() {
        console.log("WS CLOSED");
        $("#main").hide();
        $("#oops").show();
    };


    ws.onopen = function() {
        $("#oops").hide();
        $("#main").show();
        console.log("WS OPEN !!!");
           
        cbAdd = function(json){
            console.log("create returned: ", json);
            if(json == "ok") return;
            renderNote(json);
            attachEvent($("div#note_" + json[0].id));
        }

        function cbAll(json){
            if(json == "ok") return;
            console.log("All: ", json);
            $(".ui-draggable").remove();
            renderNotes(json);	
            attachEvent($("div.note"));
        }
        
        function set_status(_x){}

    ws.onmessage = function(msg) {
        json = JSON.parse(msg.data);
        console.log("incoming: ", msg.data);
        if(json == "refresh" ){
            submit_json(refresh, ws);
            return;
        }
        if(json["message"] == "ok" ){
            set_status("ok");
            return;
        }
        if(json.length > 0 && json[0]["action"] == "create") {
            cbAdd(json);
        } else {
            cbAll(json);
        }
      };
 
      function init(){
        compile();

        $("div.trash").droppable({
            accept: "div.note",
            activeClass: 'trash-active',
            hoverClass: "trash-hover",
            tolerance: "touch",
            drop: function(e, ui) {
                $(ui.draggable).draggable("destroy").fadeOut().remove();
                del($(ui.draggable));
            }	
        });
        
        var json = {
            "action" : "read_all",
        }

        submit_json(json, ws);
    }

    upd = function(note){
            var elTextarea = note.children("textarea")[0];
            if(elTextarea != undefined){
                var json = {
                "action": "update",
                "id": parseInt(note.attr("noteid")),
                "doc": {
                    "text": elTextarea.value, 
                    "x": parseInt(note.attr('x')),
                    "y": parseInt(note.attr('y')),
                    "z": parseInt(note.attr('z')),
                    "color": note.attr('color')
                }
            }
            submit_json(json, ws);
            $("div#note_" + json.id).attr("saved", "true");
        }
    };


    doUpd = function(el){
        var id = parseInt($(el).attr("noteid"));
        var note = $("div#note_" + id);
        upd(note);
    };


    del = function(note){
        var json = {
            "action": "delete",
            "id": parseInt(note.attr("noteid"))
        }
        submit_json(json, ws);
    };


    compile = function(){
        var notesDirectives = {
            "." : "note <-",
            "[id]": function(arg){
                arg.item.noteid = "note_" + arg.item.id;
                return "note_" + arg.item.id;
            },
            "[noteid]": "note.id",
            "[class]": function(arg){ return "note " + arg.item.doc.color},
            "[style]": function(arg){
                return "top:" + arg.item.doc.y + "px; left:" + arg.item.doc.x + "px;z-index:" + arg.item.doc.z ;  
            },
            "[saved]": "'true'",
            "[x]": function(arg){ return arg.item.doc.x; },
            "[y]": function(arg){ return arg.item.doc.y; },
            "[z]": function(arg){ 
                if(arg.item.doc.z > zIndex) zIndex = arg.item.doc.z; 
                return arg.item.doc.z;
            },
            "[color]": function(arg){ return arg.item.doc.color; },
            "div.picker[noteid]": "note.id",
            "div.picker[onclick]": "'pickColor(this)'",
            "textarea": "note.doc.text",
            "textarea[noteid]": "note.id",
            "textarea[onblur]": "'doUpd(this)'"
        }

        $("div#templates div.note").clone().compile("notes", notesDirectives);
    };


    $('div.add').click(function(){
        zIndex += 1;
        var json = {
            "action": "create",
            "doc": {
                "text": "",
                "x": rand(50),
                "y": rand(50),
                "z": zIndex,
                "color": "yellow"
            }
        }
        submit_json(json, ws);
    });

    attachEvent = function(note){
        note.children("textarea").focus(function(){
            zTop($(this).parent("div.note"));	
        });
        note.mouseup(function(){
            $(this).children("textarea").focus();
            zTop($(this));
            upd($(this));
            
            }).keyup(function(){
                $(this).attr("saved", "false");
                upd($(this));

            }).mouseout(function(){
                if($(this).attr("saved") == "false") upd($(this));

            }).draggable({
            opacity: 0.75,
            stop:function(e, ui){ 
                var id =  $(this).attr("id");
                if($("div#" + id).length > 0) {
                    $(this).attr("x", ui.position.left).attr("y", ui.position.top);
                    upd($(this))}}});
    }

    pickColor = function(el){
        var id = parseInt($(el).attr("noteid"));
        var note = $("div#note_" + id);
        var color = note.attr("color");
        var nextIndex = colors.indexOf(color) + 1;
        if(nextIndex >= colors.length) nextIndex = 0;
        var nextColor = colors[nextIndex];
        note.attr("color", nextColor).removeClass(color).addClass(nextColor);
        console.log(nextColor);
        upd(note);
    }
    
    init();
}
});
