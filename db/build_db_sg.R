path_root=".."
path_libs=file.path(path_root, "libs")
path_db=file.path(path_root, "db")
path_data=file.path(path_root, "data")
con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path_db, "airdb.SQLite"))
source(file.path(path_libs, "db_prep_sg.R"))
build_airbnb_database_sg(con, sg_listing_data, remove_old_database = T)
DBI::dbDisconnect(con)
