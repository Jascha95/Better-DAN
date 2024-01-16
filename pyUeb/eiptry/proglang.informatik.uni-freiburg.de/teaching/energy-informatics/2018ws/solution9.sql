-- ex1
select name, avg(readingvalue)
from city natural join building
natural join household natural join reading natural join fuel
where type ='gas'
group by name
having avg(readingvalue) >= 
all
(select avg(readingvalue) average_gas
from city natural join building
natural join household natural join reading natural join fuel
where type ='gas' group by name
);

-- Ex3:
create table T(A Number, B Number);
SELECT COUNT(*) AS NUM
FROM (
  SELECT * FROM T
  WHERE A NOT IN
  (SELECT B FROM T)
);
-- A)
delete from T;
insert into T (A,B) values (1,2);
insert into T (A,B) values (2,null);
insert into T (A,B) values (4,4);
insert into T (A,B) values (3,1);
-- B)
delete from T;
insert into T (A,B) values (1,2);
insert into T (A,B) values (2,2);
insert into T (A,B) values (null,4);
insert into T (A,B) values (3,1);

-- ex4
-- a 
SELECT name, count(*) AS building_count
FROM   city natural JOIN building 
GROUP BY name
ORDER BY building_count DESC;
-- b
select population, cityid
from city where population = (
SELECT max(population)
FROM  city );
