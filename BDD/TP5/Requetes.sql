

DROP VIEW if exists pilote;

CREATE VIEW pilote
AS select employes.*,avions.* FROM employes NATURAL JOIN certifications NATURAL JOIN avions order by eid,aid;

----------------------------------------

--1)
select eid as Pilote1,max(portee) as Portée from pilote p
group by eid
HAVING count(*) >= 2;

--2)
drop view if exists volCheapCdgNou;

create view volCheapCdgNou as
select min(prix) as prix2 from vols
where dep='CDG' and arr='NOU'
group by dep,arr;

select enom as Nom2 from pilote, volCheapCdgNou
where salaire < prix2;

drop view if exists volCheapCdgNou;

--3)
drop view if exists porteeMaxPilote;

<<<<<<< HEAD
create view minPorteedePilotedePleindeuro as
select min(portee) as portee from pilote
group by eid,salaire
having  salaire > 100000;

select dep,arr,distance from vols
where distance <= ALL (select portee FROM minPorteedePilotedePleindeuro);
=======
create view porteeMaxPilote as
select max(portee) as portee from pilote
where salaire > 100000
group by eid;

select dep,arr,distance from vols
where distance <= ALL
(select portee from PorteeMaxPilote);
>>>>>>> 68aa38f1e169cbe67f07d313479a4fe59a91ee26

drop view if exists porteeMaxPilote;

--4)

select enom from pilote
group by eid, enom
having EVERY(portee > 1500); 

--5)

select enom from pilote
group by eid, enom
having EVERY(portee > 1500) AND count(*) >=2;

--6)

select enom from pilote
group by eid, enom
having EVERY(portee > 1500) AND bool_or(anom LIKE '%Boeing%'); 

--7)

select enom,eid from employes e
ORDER BY salaire DESC
LIMIT 1 OFFSET 1;

--8)

select enom from pilote
group by eid, enom
having  bool_or(portee > 2000) and bool_and(anom not LIKE '%Boeing%');

--9)

drop view if exists moySalairePilote;
drop view if exists uniquePilote;

create view uniquePilote as
select eid,salaire from pilote
group by eid,salaire;

create view moySalairePilote as
select avg(salaire) as moySalaire from uniquePilote;

(select enom, salaire FROM employes
where salaire > ALL
 (select moySalaire from moySalairePilote)
)
EXCEPT (select enom, salaire FROM pilote);

--10)

drop view if exists moySalaireAll;

create view moySalaireAll as
select avg(salaire) as moyAll from employes;

select (moySalaire - moyAll) as diffMoy from moySalaireAll CROSS JOIN moySalairePilote;

drop view if exists moySalaireAll;
drop view if exists moySalairePilote;
drop view if exists uniquePilote;

