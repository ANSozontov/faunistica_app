library(RPostgreSQL)

dsn_database = "rdatabase"   # Specify the name of your Database
# Specify host name e.g.:"aws-us-east-1-portal.4.dblayer.com"
dsn_hostname = "localhost"  
dsn_port = "5432"                # Specify your port number. e.g. 98939
dsn_uid = "ruser"         # Specify your username. e.g. "admin"
dsn_pwd = list() # Specify your password. e.g. "xxx" 
dsn_pwd$mypass = scan("/var/mypass", what = "", nlines = 1, quiet = TRUE)

tryCatch({
  drv <- dbDriver("PostgreSQL")
  cat(cli::bg_br_red("Connecting to Database…\n"))
  connec <- dbConnect(drv, 
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

DBI::dbGetQuery(connec,"SELECT * FROM my_table")
