-- 1.
-- a)
select type from fuel;

-- b)
select f.type, r.readingdate, r.readingvalue
from Reading r, fuel f, household h, building b, city c
where r.fuelid = f.fuelid and r.householdid = h.householdid
and h.buildingid = b.buildingid and b.cityid = c.cityid;

-- 2. Use one of the join variants to find unique city names where at least on household doesn't have a reading.   
select distinct c.name 
from city c left outer join building b on c.cityid = b.cityid 
left outer join household h on b.buildingid = h.buildingid 
left outer join reading r on h.householdid = r.householdid 
where r.householdid is null;

-- 3.Use subqueries to find unique city names where at least one household doesn't have a reading.
select distinct c.name 
from city c 
left outer join building b on c.cityid = b.cityid 
left outer join household h on b.buildingid = h.buildingid 
where not exists
(select householdid from reading r where h.householdid = r.householdid );

select distinct c.name 
from city c 
left outer join building b on c.cityid = b.cityid 
left outer join household h on b.buildingid = h.buildingid 
left outer join reading r on h.householdid = r.householdid
where r.readingvalue is null;

-- 4. Find the complementary city set to the set of cities you found in the previous query.
select name from city
EXCEPT
select distinct c.name 
from city c 
left outer join building b on c.cityid = b.cityid 
left outer join household h on b.buildingid = h.buildingid 
left outer join reading r on h.householdid = r.householdid
where r.readingvalue is null;

-- 5.a. For each city, what is the average household's space
select name,  avg(space)
from city natural join building natural join household
group by name;

-- 5.b. For each city, what is the average number of households per building
select a.name , avg(a.householdcount)
from (
select name, buildingid,  count(householdid) as householdcount
from city natural join building natural join household
group by name, buildingid)a 
group by a.name;
-- Other solution without subquery- solution by Prashansa Shrestha and muhammad Faizan:
select c.name, count(distinct h.householdid)/ count(distinct b.buildingid)
from city c join building b on c.cityid = b.cityid
join household h on h.buildingid = b.buildingid
group by c.name;




