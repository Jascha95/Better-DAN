function send() {
  var recipient = new Array("thiemann", "neubauer");
  var domainParts = new Array("informatik", "uni-freiburg", "de");
  var callstr = "mailto:";
  for (var index in recipient) {
    callstr += recipient[index]+"@"+domainParts.join(".")+";"
      }
  callstr += "?subject=Anmeldung zum Seminar Webprogrammierung";
  callstr += "&body=Hallo,\n\nhiermit melde ich mich verbindlich zum Seminar Webprogrammierung an.\n\nName:\nE-Mail:\n1. Lieblingsthema:\n2. Lieblingsthema:\n3.Lieblingsthema:\n";
  document.location = callstr;
}
