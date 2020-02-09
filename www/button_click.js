//$(document).keyup(function(event) {
//  if ($("question-div").is(":focus") && (event.key == "Enter")) {
//    $("next_question").click();
//  }
//});

//$(document).keyup(function(event) {
//  if (event.key == "Enter") {
//    $("next_question").click();
//  }
//});

document.onkeydown = function (e) {
  e = e || window.event;
  var key = (e.which || e.keyCode),
  pressed = {13:'next_question', 78:'show_answer', 66:'back_to_question'};
				
  if( typeof pressed[ key ] === 'undefined' )
    return;
				
	button = document.getElementById(pressed[ key ]);
	if (button.classList.contains("shinyjs-hide"))
	  return;
	  
  button.click();
 }
 