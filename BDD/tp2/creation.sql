drop table articles;
drop table fournisseurs;
drop table catalogue;


CREATE TYPE colors as  ENUM('rouge','noir','argente','opaque','cyan','magenta','vert','superjaune');

CREATE TABLE Articles(
aid int primary key,                     
anom varchar(30) not null, 
acoul colors
);

CREATE TABLE Catalogue(
fid int,   
aid int, 
prix numeric(8,2),
primary key(fid,aid)
);

CREATE TABLE Fournisseurs(
fid int primary key,
fnom varchar(30) not null,
fad varchar(70)
);




