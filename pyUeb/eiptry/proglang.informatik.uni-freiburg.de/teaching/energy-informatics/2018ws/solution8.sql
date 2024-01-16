--1
select extract(month from readingdate), type, sum(readingvalue), avg(readingvalue)average
from reading natural join fuel
group by (extract(month from readingdate), type)
order by average asc;

--2
-- drop view readings_view;
create view readings_view as
select buildingid, fuelid, extract(year from readingdate) ye, 
extract(month from readingdate) mo, sum(readingvalue) total
from building natural join household natural join reading natural join fuel
group by buildingid, fuelid, extract(year from readingdate), 
extract(month from readingdate) 
order by buildingid, fuelid, ye, mo;

--3
-- drop table periodBuildingAnas
Create table periodBuildinganas(
pbID serial,
-- -- This is the standard way, but it's only supported in recent postgres versions.
-- pbID int generated always as identity,
buildingid int default 0,
fuelid int,
periodid int,
consumption int,
primary key(pbID), 
foreign key(buildingid) references building(buildingid) ON DELETE CASCADE,
foreign key(fuelid) references fuel(fuelid) on update cascade,
foreign key(periodid) references period(periodid) on delete cascade
);

