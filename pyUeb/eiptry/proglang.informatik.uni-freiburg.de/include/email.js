// Die folgende Variablendefinition dient nur dazu, die Links zu 
// Mitarbeitern interaktiv zu ersetzen (Anti-Spam Trick).
// Script funktioniert nur bei informatik.uni-freiburg.de - 
// Adressen, kann aber auch erweitert werden.
var mitarbeiter = Array("thiemann",
			"radanne",
			"mjost",
			"progsecr",
                        "swtsecr",
			"proglang",
			"fennell",
    // Functional Programming 2017
			"kargf"
);

var obsolete = Array (
		      "walterj", "fiech", "schuenem", "degen",
		      "anton","keilr", "geffken", "jakobro",
		      "neubauer", "franck", "levchuk",
		      "brauer","bieniusa","heidegger",
		      "wehr"
		      );

function sendmail(email, host) {
  if (host == "") 
    host = "iASDnDHFforASDmWERaSDGDtiGHSk.uASDFnERi-fASDFreiASDbuASDFrg.ASVNdZXCVe";
  var decode = "";
  for (i = 0; i<email.length; i++ )
    if (email.charAt(i)<"A" || email.charAt(i)>"Z") 
      decode += email.charAt(i);
  decode += "@";
  for (i = 0; i<host.length; i++)
    if (host.charAt(i)<"A" || host.charAt(i)>"Z")
      decode += host.charAt(i);
  document.location.href = "mailto:"+decode;
}
 
function createMail() {
  for (var index in mitarbeiter) {
    var alleFelder = document.getElementsByName("mail"+mitarbeiter[index]);
    for (var feldnr=0; feldnr<alleFelder.length; feldnr++) {
      while(alleFelder[feldnr].hasChildNodes() == true) {
	alleFelder[feldnr].removeChild(alleFelder[feldnr].childNodes[0]);
      }
      var meinText = mitarbeiter[index];
      meinText += "@informatik.";
      meinText += "uni-freiburg";
      meinText += ".de";
      alleFelder[feldnr].appendChild(document.createTextNode(meinText));
      alleFelder[feldnr].setAttribute("href", "mailto:"+meinText);
    }    
    var alleFelder = document.getElementsByName("mail"+mitarbeiter[index]+"noreplace");
    for (var feldnr=0; feldnr<alleFelder.length; feldnr++) {
      var meinText = mitarbeiter[index];
      meinText += "@informatik.";
      meinText += "uni-freiburg";
      meinText += ".de";
      alleFelder[feldnr].setAttribute("href", "mailto:"+meinText);
    }
  }  
}
