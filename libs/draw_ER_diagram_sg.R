library(dplyr)
# ER Diagram libraries
library(dm)
# Database libraries
library(DBI)
library(RSQLite)


#################### Database connection
mydb <- dbConnect(RSQLite::SQLite(), file.path(path_db, "airdb.SQLite"))


#################### Getting table details
mydb_read <- dm_from_src(mydb)  

#################### Setting primary and foreign keys
mydb_w_key <- mydb_read %>%
  dm_add_pk(listing, id) %>%
  dm_add_pk(host_info, host_id) %>%
  
  dm_add_fk(listing, host_id, host_info) 

#################### Draw the schema
mydb_w_key %>% dm_draw(view_type = "all")
