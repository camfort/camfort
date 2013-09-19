var th = new Array();
var x = 0;
var y = 0;

function toggle(n) {
    //evt = (evt || event);
    //x = evt.clientX;
    //y = evt.clientY;
    
    if (document.getElementById("a" + n.toString()) != null) {
       if (th[n]) {
	   document.getElementById("a" + n.toString()).style.display = ""
	   //document.getElementById("a" + n.toString()).style.position = "relative"
	   //document.getElementById("a" + n.toString()).style.left = x;
	   //document.getElementById("a" + n.toString()).style.top = y;
	   th[n] = false;
       } else {
	   document.getElementById("a" + n.toString()).style.display = "none"
	   th[n] = true;
       }      
    }
}
