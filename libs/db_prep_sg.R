# Data cleaning libraries
library(readr)
library(tidyr)
library(stringr)
library(tibble)
library(dplyr) 
library(magrittr) 

# Database libraries
library(DBI)
library(RSQLite)


sg_listing_data <- read_csv(file.path(path_data, "listings.csv.gz"))

#################### Remove database if it exists
remove_live_database <- function(con){
  if(file.exists(file.path(path_db, "airdb.SQLite"))){
    if (exists("con")){
      dbDisconnect(con)   
    }
    file.remove(file.path(path_db, "airdb.SQLite"))
  }
}


#################### Function for inserting data into tables
insert_to_sql <- function(con, table, data){
  
  column_name <- paste(names(data), collapse = ", ")
  
  data_new <- data %>% 
    mutate_if(is.character, function(x) ifelse(is.na(x), NA,  x %>% 
                                                 str_replace_all('"', "'") %>% # Replace " with '
                                                 paste0('"', . , '"') # Add " before and after string
    )
    )
  
  value_data <- apply(data_new, MARGIN = 1,
                      function(x) x %>%
                        paste(collapse = ",") %>% # Join all column into single string
                        str_replace_all("\\bNA\\b", "NULL")  %>% # Create NULL from NA
                        str_trim() # remove unnecessary whitespace
  )
  
  query_value <- paste(value_data) %>% 
    paste0("(", ., ")") %>% # Add bracket before and after string
    paste(collapse = ", ") %>% # Join all values into single string
    str_remove_all("\\\\") %>% # Remove \\ from string
    paste0("INSERT INTO ", table, "(", column_name, ") VALUES ", .)
  
  dbSendQuery(con, query_value)
}

build_airbnb_database_sg <- function(con, listing_data, remove_old_database = FALSE){
  
  #################### Remove Existing database
  if(remove_old_database == TRUE){
    remove_live_database(con)
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path_db, "airdb.SQLite"))
  }
  
  #################### Deal with NA values 
  listing_data <- 
    listing_data %>% 
    
    # Convert dates to characters for NA values
    mutate(last_scraped = as.character(last_scraped),
           host_since = as.character(host_since),
           calendar_last_scraped = as.character(calendar_last_scraped),
           first_review = as.character(first_review),
           last_review = as.character(last_review),
    ) %>% 
    
    # Homogenize NA values
    #*# Taken from: https://rpubs.com/Argaadya/create_table_sql
    mutate_all(function(x) ifelse(x == "" | x == "None" | x == "N/A", NA, x)) %>%  #*#
    # mutate_all(function(x) ifelse(is.na(x), "NULL", x)) %>% 
    
    # Convert character strings back to date type
    mutate(last_scraped = as.Date(last_scraped),
           host_since = as.Date(host_since),
           calendar_last_scraped = as.Date(calendar_last_scraped),
           first_review = as.Date(first_review),
           last_review = as.Date(last_review)) 
  
  #################### Filter for SG data only 
  listing_data <- listing_data %>% filter(!grepl("malaysia", neighbourhood,ignore.case = TRUE), 
                                          !latitude>=1.45587, 
                                          !longitude<103.64828, 
                                          !(latitude>=1.40472 & longitude<103.6637))
  
  #################### Extract host data
  host_data <- listing_data %>% 
    select(host_id:host_identity_verified, 
           calculated_host_listings_count:calculated_host_listings_count_shared_rooms)
  
  
  #################### Remove duplicate values
  host_data <- host_data %>% distinct()
  
  
  #################### Convert dates
  # Note that this will need to converted back to type = date for analysis
  host_data  <- host_data %>% mutate(host_since = as.character(host_since)) 
  
  
  #################### Clean host verification column
  host_data <- 
    host_data %>% 
    mutate(host_verifications = str_remove_all(host_verifications, "[\\'\\[\\]]"))
  
  #dbCreateTable(con, "host_data", host_data)
  
  #################### Create table for host info
  query <- "CREATE TABLE host_info(
        host_id INT, 
        host_url VARCHAR(50), 
        host_name VARCHAR(100), 
        host_since VARCHAR(50),
        host_location VARCHAR(500), 
        host_about VARCHAR(10000),
        host_response_time VARCHAR(50),
        host_response_rate VARCHAR(50),
        host_acceptance_rate VARCHAR(50),
        host_is_superhost BOOLEAN,
        host_thumbnail_url VARCHAR(500),
        host_picture_url VARCHAR(500),
        host_neighbourhood VARCHAR(50),
        host_listings_count INT,
        host_total_listings_count INT,
        host_verifications VARCHAR(500),
        host_has_profile_pic BOOLEAN,
        host_identity_verified BOOLEAN,
        calculated_host_listings_count INT, 
        calculated_host_listings_count_entire_homes INT,
        calculated_host_listings_count_private_rooms INT,
        calculated_host_listings_count_shared_rooms INT,
        PRIMARY KEY(host_id)
        )"
  
  if (remove_old_database==FALSE) dbRemoveTable(con, "host_info")
  
  #################### Load host_info table
  dbSendQuery(con, query)
  
  #################### Check schema
  res <- dbSendQuery(con, "PRAGMA table_info([host_info]);")
  fetch(res)  
  dbClearResult(res)
  
  #################### Insert data into host_info table
  insert_to_sql(con, "host_info", host_data)


  ####################Listing table Processing####################
  
  # listing_data %>% view() 
  listing_data %>% glimpse()
  
  #################### Remove host_data columns
  listing_data <- listing_data %>% 
    select( - names(host_data)[-1])
  
  
  #################### Remove extraneous columns 
  listing_data <- listing_data %>% 
    select(-c(license, calendar_updated, bathrooms, scrape_id))
  
  
  #################### Remove dollar signs from price column
  listing_data <- listing_data %>% 
    mutate(price = str_remove_all(price, "[$,]") %>% 
             as.numeric()
    )
  
  
  #################### Transform amenities and host verification column
  listing_data <- listing_data %>% 
    mutate(amenities = str_remove_all(amenities, "[\"\\'\\[\\]]"))
  
  #################### Convert dates to character
  listing_data <- 
    listing_data %>% 
    mutate(last_scraped          = as.character(last_scraped), 
           calendar_last_scraped = as.character(calendar_last_scraped),
           first_review          = as.character(first_review),
           last_review           = as.character(last_review),
           id=as.character(id))
  
  
  #################### Create listing table
  query_2 <- "CREATE TABLE listing (
        id INT,
        listing_url VARCHAR(100),
        last_scraped VARCHAR(50),
        name VARCHAR(500),
        description VARCHAR(2000),
        neighborhood_overview VARCHAR(2000),
        picture_url VARCHAR(500),
        host_id INT,
        neighbourhood VARCHAR(100),
        neighbourhood_cleansed VARCHAR(100),
        neighbourhood_group_cleansed VARCHAR(100),
        latitude DECIMAL(25,18),
        longitude DECIMAL(25, 18),
        property_type VARCHAR(100),
        room_type VARCHAR(100),
        accommodates INT,
        bathrooms_text VARCHAR(100),
        bedrooms INT,
        beds INT,
        amenities VARCHAR(2000),
        price DECIMAL(15, 5),
        minimum_nights INT,
        maximum_nights INT,
        minimum_minimum_nights INT,
        maximum_minimum_nights INT,
        minimum_maximum_nights INT,
        maximum_maximum_nights INT,
        minimum_nights_avg_ntm DECIMAL(16, 5),
        maximum_nights_avg_ntm DECIMAL(16, 5),
        has_availability BOOLEAN,
        availability_30 INT,
        availability_60 INT,
        availability_90 INT,
        availability_365 INT,
        calendar_last_scraped VARCHAR(50),
        number_of_reviews INT,
        number_of_reviews_ltm INT,
        number_of_reviews_l30d INT,
        first_review VARCHAR(50),
        last_review VARCHAR(50),
        review_scores_rating DECIMAL(10, 5),
        review_scores_accuracy DECIMAL(10, 5),
        review_scores_cleanliness DECIMAL(10, 5),
        review_scores_checkin DECIMAL(10, 5),
        review_scores_communication DECIMAL(10, 5),
        review_scores_location DECIMAL(10, 5),
        review_scores_value DECIMAL(10, 5),
        instant_bookable BOOLEAN,
        reviews_per_month DECIMAL(10, 5),
        PRIMARY KEY(id),
        FOREIGN KEY(host_id) REFERENCES host_info(host_id)
    )"
  
  if (remove_old_database==FALSE) dbRemoveTable(con, "listing")
  
  #################### Insert listing table into database
  dbSendQuery(con, query_2)

  
  #################### Insert data into listing table
  insert_to_sql(con, "listing", listing_data)
  
  
}

#dbReadTable(con,"listing")
#res <- dbSendQuery(con, "SELECT * FROM listing LIMIT 10")
#out_db <- fetch(res)
#dbClearResult(res)

#rmarkdown::paged_table(out_db)


