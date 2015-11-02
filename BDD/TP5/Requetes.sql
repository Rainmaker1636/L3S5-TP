

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
drop view if exists minPorteedePilotedePleindeuro;

create view minPorteedePilotedePleindeuro as
select min(portee) as portee2 from pilote
group by eid,salaire
having  salaire > 100000;

select dep,arr from vols, minPorteedePilotedePleindeuro as pv2
where not exists
(select * from pv2
where distance > portee2);

drop view if exists minPorteedePilotedePleindeuro;

--4)







