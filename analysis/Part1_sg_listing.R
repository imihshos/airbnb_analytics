rm(list = ls(all = TRUE)); graphics.off() #; gc()

# Libraries
library(tidyr)
library(dplyr)
library(knitr)
library(scales)
library(ggplot2)
library(gridExtra)
library(tibble)
library(DBI) 
library(RSQLite)
library(tidyquant)


#################### Database Connection ####################  
con <- dbConnect(RSQLite::SQLite(), "../db/airdb.SQLite")

#################### Load database tables back into R
# load host_info table 
res_host_info <- dbSendQuery(con, "select * from host_info")  
host_info <- fetch(res_host_info) 
dbClearResult(res_host_info)

# load listing table
res_listing <- dbSendQuery(con, "select * from listing")
listing <- fetch(res_listing)
dbClearResult(res_listing)

#################### Queries ####################  


#################### 1. What is the most common room type available?
room_type_available <- listing %>% select(room_type, has_availability) %>% filter(has_availability==1) %>% group_by(room_type) %>% summarise(count=n()) %>% arrange(desc(count))

room_type_available_table <- room_type_available %>% kable(align = c("l", "c"))
room_type_available_table

room_type_available_plot<- room_type_available %>% ggplot(aes(x=room_type %>% reorder(-count), y=count)) +
  geom_bar(stat="identity", position = "dodge", fill="lightpink3") +
  labs(title = "Most Common Room Type Available", x="Room Type", y="Rooms Available") +
  geom_text(aes(label = count), vjust = -0.5, size = 3) +
  theme_minimal() +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 10), plot.title=element_text(size=16, hjust=0.5, face="bold"))

room_type_available_plot

availability_periods <- c('availability_30', 'availability_60', 
                          'availability_90', 'availability_365')

for(column in availability_periods){
  tables<- paste('q1',column, sep='_')
  assign(tables,listing%>%
           select(room_type,column)%>%
           group_by(room_type)%>%
           summarise(mean=mean(.data[[column]])))
}

q1_availability_30_plot <- q1_availability_30 %>% ggplot(aes(x=room_type %>% reorder(-mean), y=mean)) +
  geom_bar(stat="identity", position = "dodge", fill="lightpink3") +
  labs(title = "30 Days", x="Room Type", y="Rooms Available") +
  geom_text(aes(label = round(mean,2)), vjust = -0.25, size = 2) +
  theme_minimal() +
  theme(axis.text = element_text(size = 7 ), axis.title = element_text(size = 8), plot.title = element_text(size=10))

q1_availability_60_plot <- q1_availability_60 %>% ggplot(aes(x=room_type %>% reorder(-mean), y=mean)) +
  geom_bar(stat="identity", position = "dodge", fill="lightpink3") +
  labs(title = "60 Days", x="Room Type", y="Rooms Available") +
  geom_text(aes(label = round(mean,2)), vjust = -0.25, size = 2) +
  theme_minimal() +
  theme(axis.text = element_text(size = 7 ), axis.title = element_text(size = 8), plot.title = element_text(size=10))

q1_availability_90_plot <- q1_availability_90 %>% ggplot(aes(x=room_type %>% reorder(-mean), y=mean)) +
  geom_bar(stat="identity", position = "dodge", fill="lightpink3") +
  labs(title = "90 Days", x="Room Type", y="Rooms Available") +
  geom_text(aes(label = round(mean,2)), vjust = -0.25, size = 2) +
  theme_minimal() +
  theme(axis.text = element_text(size = 7 ), axis.title = element_text(size = 8), plot.title = element_text(size=10))

q1_availability_365_plot <- q1_availability_365 %>% ggplot(aes(x=room_type %>% reorder(-mean), y=mean)) +
  geom_bar(stat="identity", position = "dodge", fill="lightpink3") +
  labs(title = "365 Days", x="Room Type", y="Rooms Available") +
  geom_text(aes(label = round(mean,2)), vjust = -0.25, size = 2) +
  theme_minimal() +
  theme(axis.text = element_text(size = 7 ), axis.title = element_text(size = 8), plot.title = element_text(size=10))

grid.arrange(q1_availability_30_plot, 
             q1_availability_60_plot, 
             q1_availability_90_plot,
             q1_availability_365_plot, 
             ncol = 2, heights=c(3,3),
             top = textGrob("Most Common Room Type Available",gp=gpar(font=2, fontsize=16)))

#################### 2. What is the top and bottom 10 property types based on average price?
top10_property_type_price <- listing %>% select(property_type, price) %>% group_by(property_type) %>% summarise(price=mean(price)) %>% arrange(desc(price)) %>% top_n(10) 

top10_property_type_price_table<- top10_property_type_price%>% kable(align = c("l", "c"),format.args = list(big.mark = ","), digits = 2)

top10_property_type_price_table

top10_property_type_price_plot<- top10_property_type_price %>% ggplot(aes(y=property_type %>% reorder(price), x=price)) +
  geom_bar(stat="identity", position = "dodge", fill="lightpink3") +
  labs(title = "Top 10 Most Expensive Property Type ", y="Property Type", x="Average Price") +
  geom_text(aes(label = paste('$',formatC(price, big.mark=',',format="f", digits=2))), hjust = -0.1, size = 3)+
  theme_minimal() +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 10), plot.title=element_text(size=16, hjust=0.5, face="bold")) + 
  expand_limits(x = 1500)

top10_property_type_price_plot

bottom10_property_type_price <- listing %>% select(property_type, price) %>% group_by(property_type) %>% summarise(price=mean(price)) %>% arrange(price) %>% top_n(-10) 
bottom10_property_type_price_table <- bottom10_property_type_price %>% kable(align = c("l", "c"), format.args = list(big.mark = ","),digits = 2)

bottom10_property_type_price_table

bottom10_property_type_price_plot<- bottom10_property_type_price %>% ggplot(aes(y=property_type %>% reorder(price), x=price)) +
  geom_bar(stat="identity", position = "dodge", fill="lightpink3") +
  labs(title = "Top 10 Cheapest Property Type ", y="Property Type", x="Average Price") +
  geom_text(aes(label = paste('$',formatC(price, big.mark=',',format="f", digits=2))), hjust = -0.1, size = 3,) +
  theme_minimal() +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 10), plot.title=element_text(size=16, hjust=0.5, face="bold")) + 
  expand_limits(x = 70)

bottom10_property_type_price_plot

#################### 3. What is the top and bottom 10 property types based on review score?
top10_property_type_review<-listing %>% select(property_type, review_scores_rating) %>% group_by(property_type) %>% summarise(review_scores_rating=mean(review_scores_rating)) %>% arrange(desc(review_scores_rating)) %>% top_n(10) 

top10_property_type_review_table <- top10_property_type_review%>% kable(align = c("l", "c"),format.args = list(big.mark = ","),digits = 2)

top10_property_type_review_table

top10_property_type_review_plot<- top10_property_type_review %>% ggplot(aes(y=property_type %>% reorder(review_scores_rating), x=review_scores_rating)) +
  geom_bar(stat="identity", position = "dodge", fill="lightpink3") +
  labs(title = "Top 10 Property Type by Rating ", y="Property Type", x="Average Rating") +
  geom_text(aes(label = round(review_scores_rating,2)), hjust = -0.2, size = 3)+
  theme_minimal() +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 10), plot.title=element_text(size=16, hjust=0.5, face="bold"))+ 
  expand_limits(x = 5.5)

top10_property_type_review_plot

bottom10_property_type_review<-listing %>% select(property_type, review_scores_rating) %>% group_by(property_type) %>% summarise(review_scores_rating=mean(review_scores_rating)) %>% arrange(review_scores_rating) %>% top_n(10) 

bottom10_property_type_review_table <- bottom10_property_type_review %>% kable(align = c("l", "c"),format.args = list(big.mark = ","),digits = 2)

bottom10_property_type_review_table

bottom10_property_type_review_plot<- bottom10_property_type_review %>% ggplot(aes(y=property_type %>% reorder(review_scores_rating), x=review_scores_rating)) +
  geom_bar(stat="identity", position = "dodge", fill="lightpink3") +
  labs(title = "Bottom 10 Property Type by Rating ", y="Property Type", x="Average Rating") +
  geom_text(aes(label = round(review_scores_rating,2)), hjust = -0.2, size = 3)+
  theme_minimal() +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 10), plot.title=element_text(size=16, hjust=0.5, face="bold")) + 
  expand_limits(x = 5)

bottom10_property_type_review_plot

#################### 4. What is the most common amenities provided?

maxcols <- max(str_count(listing$amenities, ","))+1
newcols <- paste("col", 1:maxcols)
amenities_separated <- listing %>% select(amenities) %>% separate(col = amenities, sep= ",", into=newcols,remove = FALSE)
amenities_longformat <- pivot_longer(data=amenities_separated, cols = 'col 1':'col 68', names_to = "col_number", values_to = "separated_amenities")
amenities_count <- amenities_longformat %>% select(separated_amenities) %>% group_by(separated_amenities) %>% summarise(count=n()) %>% na.omit() %>% arrange(desc(count)) %>% paged_table()

amenities_count

#################### 5. Is there any correlation between room price and the review score?
reviewscores_price_table <- listing %>% 
  select(price, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, review_scores_communication, review_scores_location, review_scores_rating, review_scores_value) %>% 
  drop_na()

correlation_plot <- function(df, y_col, title){
  c_plot <- df %>% 
    ggplot(aes(x = price, y = y_col)) +
    geom_jitter(alpha = 0.5, color="lightpink3") + 
    scale_x_log10(label = number_format(big.mark = ",")) +
    labs(x = "Price", y = title, title = title) + 
    theme(axis.text = element_text(size = 8), axis.title = element_text(size = 8), plot.title = element_text(size=10))
  theme_minimal()
  
  return(c_plot)
}

c_plot_rating<- correlation_plot(reviewscores_price_table, reviewscores_price_table$review_scores_rating, "Rating")
c_plot_accuracy<- correlation_plot(reviewscores_price_table, reviewscores_price_table$review_scores_accuracy, "Accuracy")
c_plot_cleanliness<- correlation_plot(reviewscores_price_table, reviewscores_price_table$review_scores_cleanliness, "Cleanliness")
c_plot_checkin<- correlation_plot(reviewscores_price_table, reviewscores_price_table$review_scores_checkin, "Check-in")
c_plot_communication<- correlation_plot(reviewscores_price_table, reviewscores_price_table$review_scores_communication, "Communication")
c_plot_location<- correlation_plot(reviewscores_price_table, reviewscores_price_table$review_scores_location, "Location")
c_plot_value<- correlation_plot(reviewscores_price_table, reviewscores_price_table$review_scores_value, "Value")

c_plot_rating <- c_plot_rating + labs(title = "Rating vs Price") + theme(plot.title = element_text(size=16, hjust = 0.5, face="bold"))



c_plot_rating
grid.arrange(c_plot_accuracy, 
             c_plot_cleanliness,
             c_plot_checkin,
             ncol=3,
             top = textGrob("Review Score vs Price",gp=gpar(font=2, fontsize=16)))
grid.arrange(c_plot_communication, 
             c_plot_location,
             c_plot_value,
             ncol=3,
             top = textGrob("Review Score vs Price",gp=gpar(font=2, fontsize=16)))


#################### 6. Room listing geographical distribution

geog_data <- listing %>%left_join(host_info, by = "host_id") %>% select(id, host_name, listing_url, name, latitude, longitude, price,review_scores_rating, number_of_reviews, neighbourhood_cleansed) %>%
  replace_na(list(name = "No Name", host_name = "No Host Name"))

content<-paste0("<b><a href=", geog_data$listing_url, ">", geog_data$name,"</a></b><br>",
                "Listing ID: ", geog_data$id, "<br>",
                "Neighbourhood: ", geog_data$neighbourhood_cleansed, "<br>",
                "Host Name: ", geog_data$host_name,"<br>",
                "Price: ", geog_data$price, "<br>",
                "Review Score Rating: ", ifelse(is.na(geog_data$review_scores_rating), "No Rating Yet",geog_data$review_scores_rating), "<br>")

room_listing_map<- leaflet(data=geog_data) %>% addProviderTiles(providers$OpenStreetMap) %>% addMarkers(lng = ~longitude,lat = ~latitude, clusterOptions = markerClusterOptions(), popup=content)

room_listing_map

