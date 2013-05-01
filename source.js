var th = new Array();

function toggle(n) {
    alert("a" + n.toString);
    if (th[n]) {
      document.getElementById("a" + n.toString).style.visible = ""
      th[n] = false;
    } else {
      document.getElementById("a" + n.toString).style.visible = "none"
	  th[n] = true;
    }      
}