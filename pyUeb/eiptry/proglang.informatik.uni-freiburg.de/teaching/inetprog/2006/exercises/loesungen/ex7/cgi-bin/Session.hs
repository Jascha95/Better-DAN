import WASHHelper

step1 :: CGI ()
step1 = standardQuery "Step 1"
  <#> <p>Vorname <input type="text" name="vorname"/></p>
      <input type="submit" value="Nächster Schritt" 
             WASH:callback="step2" WASH:parms="vorname"/>
  </#>

step2 :: Text -> CGI ()
step2 vorname = standardQuery "Step 2"
  <#> <p>Nachname <input type="text" name="nachname"/></p>
      <input type="submit" value="Nächster Schritt" 
             WASH:callback="step3 vorname" WASH:parms="nachname"/>
  </#>

step3 :: Text -> Text -> CGI ()
step3 vorname nachname = standardQuery "Step 3"
  <#> <p>Alter <input type="text" name="alter"/></p>
      <input type="submit" value="Nächster Schritt" 
             WASH:callback="step4 vorname nachname" WASH:parms="alter"/>
  </#>

step4 :: Text -> Text -> Int -> CGI ()
step4 vorname nachname alter = standardQuery "Zusammenfassung"
  <table>
    <tr>
      <td>Vorname</td><td><%= vorname %></td>
    </tr>
    <tr>
      <td>Nachname</td><td><%= nachname %></td>
    </tr>
    <tr>
      <td>Alter</td><td><%= alter %></td>
    </tr>
  </table>

main = run step1
