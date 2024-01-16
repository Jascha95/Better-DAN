CREATE TABLE users_data(
       userid integer PRIMARY KEY,
       login varchar(10) UNIQUE NOT NULL,
       password varchar(32) NOT NULL, 
       anrede char(4) NOT NULL,
       name varchar(40) NOT NULL,
       vorname varchar(40) NOT NULL,
       email varchar(40) NOT NULL,
       geburtsdatum timestamp,
       homepageURL varchar(64),
       land varchar(32) default 'DE',
       ort varchar(40),
       strasse varchar(40),
       hausnum integer,
       postfach integer,
       selbstdar text,
       bild text);

-- Man sollte Bilder nicht in einer SQL Datenbank abspeichern, da relationale
-- Datenbanken nicht auf das Ausliefern groesserer Mengen von binaeren Daten
-- optimiert sind und dafuer gedacht, Daten in Relationen 
-- zusammen zu stellen und ueber diese Daten Suchabfragen zu machen. 
-- 
-- Erwuenscht ist, dass man nur Pfade zu Bilder in der RDBS speichert
-- und in dieser Verzeichnisse mittels Webserver auf die Bilder zugreift. 

CREATE TABLE friends(
       userid1 integer REFERENCES users_data(userid),
       userid2 integer REFERENCES users_data(userid));

CREATE TABLE comments(
       userid1 integer REFERENCES users_data(userid),
       userid2 integer REFERENCES users_data(userid),
       datum TIMESTAMP,
       comment text);    
