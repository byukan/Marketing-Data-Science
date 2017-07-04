-- create schema retail_db;

create table retail_db.sales_tbl
(customer_id int null, 
date_day int null, 
prod_cat varchar(45) null,
region varchar(45) null,
sales decimal(10,2) null,
margin decimal(10,4) null)
;