library(tidyverse)
library(dplyr)
library(DBI)

# 
# URL:      http://142.93.20.188/phpmyadmin
# Username: admin
# Password: 2623360fe6582dccff16ab38d509253389444f4e6237a6d8
# 
# CREATE DATABASE matpuc;
# CREATE USER 'test'@'%' IDENTIFIED BY 'HFW9KYZBnEYr!';
# GRANT ALL PRIVILEGES ON matpuc.* TO 'test'@'%';
# FLUSH PRIVILEGES;
# 

con <- dbConnect(
  RMySQL::MySQL(),
  dbname = "matpuc",
  host = "142.93.20.188", 
  port = 3306,
  user = "test",
  password = "HFW9KYZBnEYr!"
)

DBI::dbListTables(con)

