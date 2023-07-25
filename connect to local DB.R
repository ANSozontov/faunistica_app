library(RPostgreSQL)
library(tidyverse)

# Definite connection arguments -------------------------------------------
dsn_database = "rdatabase"   # Specify the name of your Database

dsn_hostname = "localhost"  # Host name could also be :
                            # "aws-us-east-1-portal.4.dblayer.com" 
                            # or 190.23.25.112
dsn_port = "5432" #  port number
dsn_uid = "ruser" # username
dsn_pwd = list() # password; # better not to write in code directly:
dsn_pwd$mypass = scan("/var/mypass", what = "", nlines = 1, quiet = TRUE)

# Connect -----------------------------------------------------------------
tryCatch({
  drv <- dbDriver("PostgreSQL")
  cat(cli::bg_br_red("Connecting to Database…\n"))
  con <- dbConnect(drv, 
                      dbname = dsn_database,
                      host = dsn_hostname, 
                      port = dsn_port,
                      user = dsn_uid, 
                      password = dsn_pwd)
  cli::cli_alert_success("Database Connected!")
},
error=function(cond) {
  cli::cli_warn("Unable to connect to Database.")
})

# Get Querry --------------------------------------------------------------
DBI::dbGetQuery(con,"SELECT * FROM my_table")

# write records toward ----------------------------------------------------
a <- data.frame(name = c("Petir", "Max"), age = c(15, 45))
b <- data.frame(age = c(100, NA), name = c("Mina", "Malina"))
d <- data.frame(age = 50, name = 60)

DBI::dbWriteTable(con, "my_table", a, 
                  append = TRUE, row.names = FALSE)
DBI::dbGetQuery(con,"SELECT * FROM my_table")

DBI::dbWriteTable(con, "my_table", b, 
                  append = TRUE, row.names = FALSE)
DBI::dbGetQuery(con,"SELECT * FROM my_table")
# so here we can see that a column order of a dataframes doesn't matter

DBI::dbWriteTable(con, "my_table1", d, 
                  append = TRUE, row.names = FALSE)
DBI::dbGetQuery(con,"SELECT * FROM my_table1") %>% str 
# If I use new table name, it creates it newly with no asking a confirmation
# Now remove it back: 
DBI::dbRemoveTable(con, "my_table1")

#let's watch if it absents now 
dbListObjects(con) # option1
dbGetQuery(con, #option2
    "SELECT table_name FROM information_schema.tables WHERE table_schema='public'")

# now I'll try to mix column types (earlier it moved numeric to text)

e <- data.frame(age = "12", name = 60)
DBI::dbWriteTable(con, "my_table", e, 
                  append = TRUE, row.names = FALSE)
DBI::dbGetQuery(con,"SELECT * FROM my_table") # %>% str 
# nubers as test to numeric? Esily

f <- data.frame(age = "abc", name = 60)
DBI::dbWriteTable(con, "my_table", f, 
                  append = TRUE, row.names = FALSE)
# Error! Ok, I see 

# REMOVE a few rows 

dbSendQuery(con, "DELETE FROM my_table WHERE id BETWEEN 7 AND 10")
dbCommit(con)
# but there were 9 rows only. So it print a warning, but prosess 
DBI::dbGetQuery(con,"SELECT * FROM my_table") 

dbSendQuery(con, "DELETE FROM my_table WHERE id BETWEEN 4 AND 18")
dbCommit(con)
DBI::dbGetQuery(con,"SELECT * FROM my_table") 

# ok, each submit to tables creare new unique id, 
# and even I remove a psrt of table, psql create new id, not used before

# How to get what id were used already despite of removing a few rows?
dbSendQuery(con, "SELECT last_value FROM mytable_id_seq") # doesn't work
dbSendQuery(con, "SELECT generate_series(1, last_value) FROM mytable_id_seq;") 
dbSendQuery(con, "SELECT last_value FROM mytable_id_seq") 

# attempt 2
dbSendQuery(con, "SELECT relname 
FROM pg_class
WHERE relkind = 'S' AND relname LIKE my_table;")
dbSendQuery(con, "SELECT pg_get_serial_sequence('my_table', 'id');")
dbSendQuery(con, "SELECT last_value 
FROM my_actual_sequence_name;
") 
dbClearResult() 
dbSendQuery(con, ";") 

# test table  -------------------------------------------------------------





