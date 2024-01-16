

// Lec 6     25.10.23
// copied from an screenshot 

function tokenizeText() {
let text = this. parentElement. querySelector(".text°).textContent.trim();
fetch (http://127.0.0.1:5801/tokenize'. f
method: "POST",
headers: {
•Content-Type*: 'application/json'
body: JSON. stringifylf text: text b)
• then (response = response. json())
•then (data
= {
this. parentElement. classList. add ( tokenized°)
this. parentElement. querySelector'.text).innerText = **;
da. forEach token = {
const newtoken = document. createElement (span'):
newToken.className = 'token';
newToken. innerText = token.tok;
newroken. title = token. tag;
this. parentElement. querySelector ('•text').appendChild (newToken);


function displayEntry (text) {
// console. log (text)
const entriesDiv = document. getElementById(entries');
const tokenButton = document. createElement (button"):
tokenButton. type = "button';
tokenButton.className = "token-button':
tokenButton. innerText = "Tokenize':
tokenBut
= tokenizeText

// 
const data = [
    "What a beautiful day!",
    "Dont forget to water the plants",
    "Take your time on the projecct"] 

// now in python for flask and server hard coded 
data.forEach(entry => {
    //console.log(entry)
    displayEntry(entry);
})

// now adding on the index.html an iput from the user 
// additionaly a python file, which stores it in memory if the python backendserver is killed all yield gets lost 
// example from a database 
//psql 
    //create database webapp 
// connecting application to the database // db host in an ".env" file 

