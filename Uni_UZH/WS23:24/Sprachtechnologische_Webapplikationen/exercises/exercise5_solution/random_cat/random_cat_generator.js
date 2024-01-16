const button = document.querySelector("button");

button.addEventListener("click", function (event) {
  const imageElem = document.querySelector("img");
  const request = new Request("https://api.thecatapi.com/v1/images/search");
  fetch(request)
    .then(function (response) {
      response.json().then(function (data) {
        const url = data[0].url;
        imageElem.src = url;
      });
    })
    .catch(function (error) {
      alert(error);
    });
});

