CREATE TABLE canton (
    canton_id       integer NOT NULL,
    name            character varying NOT NULL, -- English canton name
    abbreviation    character(2) NOT NULL,      -- official abbreviation
    since           integer NOT NULL,           -- year of joining
    population      integer NOT NULL,           -- entire inhabitants
    area            integer NOT NULL,           -- in square kilometres
    alien_ratio     double precision NOT NULL   -- ratio of foreigners among population
);

CREATE TABLE language (
    language_id integer NOT NULL,
    name character varying NOT NULL -- English language name
);

CREATE TABLE language_in_canton (
    language_id integer NOT NULL,
    canton_id integer NOT NULL
);

INSERT INTO canton VALUES (1, 'Zürich', 'ZH', 1351, 1443436, 1729, 0.254);
INSERT INTO canton VALUES (2, 'Bern', 'BE', 1353, 1001281, 5959, 0.143);
INSERT INTO canton VALUES (4, 'Aargau', 'AG', 1803, 644830, 1404, 0.236);
INSERT INTO canton VALUES (5, 'St. Gallen', 'SG', 1803, 491699, 2026, 0.23);
INSERT INTO canton VALUES (7, 'Luzern', 'LU', 1332, 394604, 1493, 0.171);
INSERT INTO canton VALUES (11, 'Basel-Landschaft', 'BL', 1501, 278656, 518, 0.208);
INSERT INTO canton VALUES (12, 'Solothurn', 'SO', 1481, 261437, 791, 0.204);
INSERT INTO canton VALUES (13, 'Thurgau', 'TU', 1803, 260278, 991, 0.237);
INSERT INTO canton VALUES (14, 'Basel-Stadt', 'BS', 1501, 196668, 37, 0.348);
INSERT INTO canton VALUES (15, 'Graubünden', 'GR', 1803, 194959, 7105, 0.177);
INSERT INTO canton VALUES (16, 'Neuchâtel', 'NE', 1815, 176241, 803, 0.252);
INSERT INTO canton VALUES (17, 'Schwyz', 'SZ', 1291, 152759, 908, 0.196);
INSERT INTO canton VALUES (18, 'Zug', 'ZG', 1352, 120089, 239, 0.262);
INSERT INTO canton VALUES (19, 'Schaffhausen', 'SH', 1501, 78738, 298, 0.248);
INSERT INTO canton VALUES (20, 'Jura', 'JU', 1979, 71738, 838, 0.136);
INSERT INTO canton VALUES (21, 'Appenzell Ausserrhoden', 'AR', 1513, 53691, 243, 0.149);
INSERT INTO canton VALUES (22, 'Nidwalden', 'NW', 1291, 42080, 276, 0.132);
INSERT INTO canton VALUES (23, 'Glarus', 'GL', 1352, 39593, 685, 0.222);
INSERT INTO canton VALUES (24, 'Obwalden', 'OW', 1291, 36834, 491, 0.139);
INSERT INTO canton VALUES (25, 'Uri', 'UR', 1291, 36008, 1077, 0.112);
INSERT INTO canton VALUES (26, 'Appenzell Innerrhoden', 'AI', 1513, 15778, 173, 0.102);
INSERT INTO canton VALUES (10, 'Fribourg', 'FR', 1481, 297622, 1671, 0.213);
INSERT INTO canton VALUES (6, 'Geneva', 'GE', 1815, 469433, 282, 0.378);
INSERT INTO canton VALUES (8, 'Ticino', 'TI', 1803, 346539, 2812, 0.278);
INSERT INTO canton VALUES (3, 'Vaud', 'VD', 1803, 743317, 3212, 0.324);
INSERT INTO canton VALUES (9, 'Valais', 'VS', 1815, 327011, 5224, 0.224);

INSERT INTO language VALUES (1, 'German');
INSERT INTO language VALUES (2, 'French');
INSERT INTO language VALUES (3, 'Italian');
INSERT INTO language VALUES (4, 'Romansh');

INSERT INTO language_in_canton VALUES (1, 4);
INSERT INTO language_in_canton VALUES (1, 21);
INSERT INTO language_in_canton VALUES (1, 26);
INSERT INTO language_in_canton VALUES (1, 11);
INSERT INTO language_in_canton VALUES (1, 14);
INSERT INTO language_in_canton VALUES (1, 2);
INSERT INTO language_in_canton VALUES (1, 10);
INSERT INTO language_in_canton VALUES (1, 23);
INSERT INTO language_in_canton VALUES (1, 15);
INSERT INTO language_in_canton VALUES (1, 7);
INSERT INTO language_in_canton VALUES (1, 22);
INSERT INTO language_in_canton VALUES (1, 24);
INSERT INTO language_in_canton VALUES (1, 19);
INSERT INTO language_in_canton VALUES (1, 17);
INSERT INTO language_in_canton VALUES (1, 12);
INSERT INTO language_in_canton VALUES (1, 5);
INSERT INTO language_in_canton VALUES (1, 13);
INSERT INTO language_in_canton VALUES (1, 25);
INSERT INTO language_in_canton VALUES (1, 18);
INSERT INTO language_in_canton VALUES (1, 1);
INSERT INTO language_in_canton VALUES (2, 2);
INSERT INTO language_in_canton VALUES (2, 6);
INSERT INTO language_in_canton VALUES (2, 20);
INSERT INTO language_in_canton VALUES (2, 16);
INSERT INTO language_in_canton VALUES (2, 3);
INSERT INTO language_in_canton VALUES (2, 10);
INSERT INTO language_in_canton VALUES (3, 8);
INSERT INTO language_in_canton VALUES (3, 15);
INSERT INTO language_in_canton VALUES (4, 15);
INSERT INTO language_in_canton VALUES (2, 9);
INSERT INTO language_in_canton VALUES (1, 9);
