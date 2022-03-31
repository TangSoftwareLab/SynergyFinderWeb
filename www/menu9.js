$(document).ready(function() {
	function style_change(item){
		var links = document.getElementsByTagName("a");
		for(var i=0;i<links.length;i++)
		{
			links[i].style.color = "black";  
			
		}
		const note = document.querySelector(item);
		note.style.color = '#0477bd';
	}
	function check(link){
		console.log(window.location.href);
		if (window.location.href.indexOf("userGuide") > -1) {
			console.log("userGuide");
			style_change('.item3');
			document.querySelector('.item2').style.color = 'black';
			document.querySelector('.item4').style.color = 'black';
			document.querySelector('.item5').style.color = 'black';
			document.querySelector('.item6').style.color = 'black';
			document.querySelector('.item7').style.color = 'black';
			document.querySelector('.item1').style.color = 'black';
		}
		else if (window.location.href.indexOf("dashboard") > -1) {
			console.log("dashboard");
			//const note = document.querySelector('.item2');
			//note.style.color = 'blue';
			style_change('.item2');
			document.querySelector('.item3').style.color = 'black';
			document.querySelector('.item4').style.color = 'black';
			document.querySelector('.item5').style.color = 'black';
			document.querySelector('.item6').style.color = 'black';
			document.querySelector('.item7').style.color = 'black';
			document.querySelector('.item1').style.color = 'black';

		}
		else if (window.location.href.indexOf("cite") > -1) {
			console.log("cite");
			//const note = document.querySelector('.item5');
			//note.style.color = 'blue';
			style_change('.item5');
			document.querySelector('.item2').style.color = 'black';
			document.querySelector('.item4').style.color = 'black';
			document.querySelector('.item3').style.color = 'black';
			document.querySelector('.item6').style.color = 'black';
			document.querySelector('.item7').style.color = 'black';
			document.querySelector('.item1').style.color = 'black';

		}
		else if (window.location.href.indexOf("aboutUs") > -1) {
			console.log("aboutUs");
			const note = document.querySelector('.item6');
			note.style.color = '#0477bd';
			document.querySelector('.item2').style.color = 'black';
			document.querySelector('.item4').style.color = 'black';
			document.querySelector('.item5').style.color = 'black';
			document.querySelector('.item3').style.color = 'black';
			document.querySelector('.item7').style.color = 'black';
			document.querySelector('.item1').style.color = 'black';

		}
		else if (window.location.href.indexOf("contact") > -1) {
			console.log("contact");
			const note = document.querySelector('.item7');
			note.style.color = '#0477bd';
			document.querySelector('.item2').style.color = 'black';
			document.querySelector('.item4').style.color = 'black';
			document.querySelector('.item5').style.color = 'black';
			document.querySelector('.item6').style.color = 'black';
			document.querySelector('.item3').style.color = 'black';
			document.querySelector('.item1').style.color = 'black';

		}
		else if (window.location.href.indexOf("faq") > -1) {
			console.log("faq");
			const note = document.querySelector('.item4');
			note.style.color = '#0477bd';
			document.querySelector('.item2').style.color = 'black';
			document.querySelector('.item3').style.color = 'black';
			document.querySelector('.item5').style.color = 'black';
			document.querySelector('.item6').style.color = 'black';
			document.querySelector('.item7').style.color = 'black';
			document.querySelector('.item1').style.color = 'black';

		}
		else{
			console.log("home");
			const note = document.querySelector('.item1');
			note.style.color = '#0477bd';
			document.querySelector('.item2').style.color = 'black';
			document.querySelector('.item4').style.color = 'black';
			document.querySelector('.item5').style.color = 'black';
			document.querySelector('.item6').style.color = 'black';
			document.querySelector('.item7').style.color = 'black';
			document.querySelector('.item3').style.color = 'black';

		}
	}
	window.addEventListener('hashchange', function(e) {
		//alert("Hash Changed");
		//console.log("test",e.newURL);
		//console.log(window.location.href);
		check(e.newURL);
	});
	
	check(window.location.href);
});
