window.onbeforeunload = function(a){
  return message="You cannot refresh this page. Please open another tab",
  a.returnValue=message,message
},
$("#removeoutliers").prepend(
  '<img id="theImg" src="beta2.png" style="position: absolute; top: 0px; right: 0px;" />'
),
$("#spanpop").popover({
  html:!0,title:"Input data structure:",
  content:"Table format:<br> <img src='images/exampleTab.png' width='300' height='200' />
  <br><br>
  Matrix format:<br>
  <img src='images/exampleMat.png'  width='300' height='200'/>
  <br><br>
  For more information about input file format please check the 
  <b style='color:#2fa4e7;'>USER GUIDE</b>.",
  trigger:"hover",placement:"auto",container:"body",animation:!0
});
