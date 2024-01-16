CREATE TABLE users_data(
       userid integer PRIMARY KEY,
       nickname VARCHAR(40) UNIQUE NOT NULL,
       password VARCHAR(40) NOT NULL, 
       anrede char(4) NOT NULL,
       name varchar(40) NOT NULL,
       vorname varchar(40) NOT NULL,
       email text NOT NULL,
       geburtsdatum timestamp,
       homepageURL text,
       land VARCHAR(4) REFERENCES country(code),
       ort VARCHAR(40),
       strasse varchar(40),
       hausnum integer CHECK (hausnum >= 1 AND hausnum <= 999),
       postfach integer CHECK (postfach >= 1 AND postfach <= 999999),
       plz integer,
       selbstdar VARCHAR(1000) NOT NULL,
       bild bytea);

;Beispiele, wie man mit dem Postgresql binaere Daten verarbeiten kann, sind auf der Seite:
;http://www.postgresql.org/docs/pg_handbuch/html/jdbc-binary-data.html

CREATE TABLE country(  
       name VARCHAR(32) NOT NULL UNIQUE,     
       code VARCHAR(4) PRIMARY KEY);

;sieh. country.sql, um die Relation aufzufuellen

CREATE TABLE friends(
       userid1 integer REFERENCES users_data(userid) NOT NULL,
       userid2 integer REFERENCES users_data(userid) NOT NULL);

CREATE TABLE comments(
       userid1 integer REFERENCES users_data(userid) NOT NULL,
       userid2 integer REFERENCES users_data(userid) NOT NULL,
       datum TIMESTAMP NOT NULL,
       comment text);

CREATE TABLE threads(
       tid               integer PRIMARY KEY,
       besitzer          integer REFERENCES users_data(userid) NOT NULL,
       titel             VARCHAR(40) NOT NULL
       access_control    boolean NOT NULL);

CREATE TABLE threadaccess(
       userid            integer REFERENCES users_data(userid) NOT NULL,
       tid               integer REFERENCES threads(tid) NOT NULL);

CREATE TABLE keywords (
       wid integer NOT NULL UNIQUE,
       word VARCHAR(40) PRIMARY KEY);

CREATE TABLE keythreads(
       wid               integer REFERENCES keywords(wid),
       tid               integer REFERENCES threads(tid),
       PRIMARY KEY(wid, tid));

CREATE TABLE postings(
       postid            integer PRIMARY KEY,
       author            integer REFERENCES users_data(userid) NOT NULL,
       datum             TIMESTAMP NOT NULL,
       message           VARCHAR(1000) NOT NULL,
       postid_father     integer REFERENCES postings(postid),
       tid               integer REFERENCES threads(tid) NOT NULL);


CREATE SEQUENCE postid_seq;
CREATE SEQUENCE tid_seq;
CREATE SEQUENCE wid_seq;
