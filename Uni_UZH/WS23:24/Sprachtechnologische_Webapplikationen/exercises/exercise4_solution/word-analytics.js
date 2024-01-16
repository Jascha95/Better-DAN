// Task 2.1

function languageIdentification(word) {
  let language;
  if (word.endsWith("heit") || word.endsWith("keit") || word.endsWith("chen")) {
    language = "German";
  } else if (
    word.endsWith("ness") ||
    word.endsWith("nce") ||
    word.endsWith("ty")
  ) {
    language = "English";
  } else if (word.endsWith("que") || word.endsWith("age")) {
    language = "French";
  } else {
    language = "The language of the word cannot be determined.";
  }
  return language;
}

// Task 2.2

function wordLength(word) {
  return word.length;
}

function letterCapitalized(word) {
  return word[0] === word[0].toUpperCase();
}

function distinctLetters(word) {
  const charSet = new Set(word);
  return charSet.size;
}

function wordRatio(word) {
  const vowels = "aeiou";
  let numVowels = 0;
  for (const char of word.toLowerCase()) {
    if (vowels.indexOf(char) !== -1) {
      numVowels++;
    }
  }
  return (word.length - numVowels) / numVowels;
}

function invertedWord(word) {
  let output = "";
  for (const char of word) {
    if (letterCapitalized(char)) {
      output += char.toLowerCase();
    } else {
      output += char.toUpperCase();
    }
  }
  return output;
}

function wordWorth(word) {
  const alphabet = "abcdefghijklmnopqrstuvwxyz";
  let worth = 0;
  for (const char of word.toLowerCase()) {
    worth += alphabet.indexOf(char) + 1;
  }
  return worth;
}

// Task 2.3
function analyze(word) {
  document.querySelector("#word").textContent = word;
  document.querySelector("#language").textContent =
    languageIdentification(word);
  document.querySelector("#wordLength").textContent = wordLength(word);
  document.querySelector("#letterCapitalized").textContent =
    letterCapitalized(word);
  document.querySelector("#distinctLetters").textContent =
    distinctLetters(word);
  document.querySelector("#wordRatio").textContent = wordRatio(word).toFixed(2);
  document.querySelector("#invertedWord").textContent = invertedWord(word);
  document.querySelector("#wordWorth").textContent = wordWorth(word);
}

document.querySelector("div > button").addEventListener("click", () => {
  const userInput = document.querySelector("#wordInput").value;
  // alternative: const userInput = document.getElementById("wordInput").value;
  if (userInput) {
    analyze(userInput);
    palindrome(userInput);
  }
});

// Task 2.4

function palindrome(word) {
  const palindromeContainer = document.querySelector("#palindromeContainer");
  // remove old boxes from previous analysis
  palindromeContainer.replaceChildren();

  for (let i = 0; i < wordLength(word); i++) {
    const char = word[i];
    const opposingChar = word[wordLength(word) - i - 1];
    const isPalindrome = char.toLowerCase() === opposingChar.toLowerCase();

    const palindromeBox = document.createElement("span");
    palindromeBox.classList.add("palindromeBox");

    if (isPalindrome) {
      palindromeBox.classList.add("green");
    } else {
      palindromeBox.classList.add("red");
    }
    palindromeContainer.appendChild(palindromeBox);
  }
}
