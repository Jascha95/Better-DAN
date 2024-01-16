const inputForm = document.querySelector("form");
const definitionList = document.querySelector("ol");

let wordEntered = document.querySelector("h3");

function cleardefinitionList() {
    for (const li of definitionList.querySelectorAll("li")) {
        li.remove();
    }
}

inputForm.addEventListener("submit", function(event) {
	console.log(event);
	
    event.preventDefault(); // no reload
    const wordInput = inputForm.wordInput.value;
    const request = new Request(
        "https://api.dictionaryapi.dev/api/v2/entries/en/"+wordInput, {method:'GET'}
		)

    fetch(request).then(function(response){
        response.json().then(function(data) {
			cleardefinitionList();
            wordEntered.textContent = "Word entered: " + wordInput;
			if (inputForm.phonetics.checked=== true){
				if (data["0"]["phonetic"]!== undefined){
				wordEntered.textContent = wordEntered.textContent + " (" + data["0"]["phonetic"] +")"
				} else {
				wordEntered.textContent = wordEntered.textContent + " (phonetics not available)"
				}
					
			}
			const meanings = data["0"]["meanings"];
			for (const meaning of meanings){
				const li = document.createElement("li");
				li.textContent = meaning["partOfSpeech"] + ": " + meaning["definitions"]["0"]["definition"]
				definitionList.appendChild(li)
			}
			
		})
	}).catch(function(error) {
		alert(error);
	});

});




