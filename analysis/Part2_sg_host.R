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

#################### 7. Who are the top 10 host based on revenue?

q7 <- listing %>% 
  left_join(host_info, by = "host_id") %>% 
  select(host_id, host_name, price, 
         review_scores_rating, minimum_nights, number_of_reviews) %>%
  mutate(total_earnings = price * review_scores_rating * minimum_nights) %>% 
  drop_na() %>% 
  group_by(host_id, host_name) %>% 
  summarise(number_of_listing = n(), sum_total_earnings = sum(total_earnings)) %>% 
  arrange(desc(sum_total_earnings))

top_host_by_listing <- 
  q7 %>% 
  arrange(desc(number_of_listing)) %>% 
  select(host_name, number_of_listing) %>% 
  distinct() %>% 
  head(10) %>% 
  ggplot(aes(x = number_of_listing, y = host_name %>% reorder(number_of_listing))) + 
  geom_col(fill = "lightpink3") +
  labs(
    title    = "Top Host by # of Listings",
    x        = "Number of Listing",
    y        = "Host Name"
  ) + 
  theme_minimal() + 
  theme(axis.text = element_text(size = 9), 
        axis.title = element_text(size = 10), 
        plot.title=element_text(size=10, hjust=0.5, face="bold"))

top_host_by_earning <- 
  q7 %>% 
  select(host_name, sum_total_earnings) %>%
  arrange(desc(sum_total_earnings)) %>%
  head(10) %>% 
  ggplot(aes(x = sum_total_earnings, y = host_name %>% reorder(sum_total_earnings))) + 
  geom_col(fill = "lightpink3") + 
  scale_x_continuous(labels = scales::number_format(big.mark = ",")) + 
  labs(
    title    = "Top Host by Total Earning",
    x        = "Total Earning (in SGD)",
    y        = "Host Name"
  ) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(size=9, angle = 90, face = "bold"),
        axis.text.y = element_text(size=9,face = "bold"), 
        lot.title=element_text(size=10, hjust=0.5, face="bold"),
        axis.title = element_text(size = 10))

grid.arrange(top_host_by_listing, top_host_by_earning, ncol = 2)

#################### 8. Is there any difference in review score between superhost and normal host?

q8 <- listing %>% 
  left_join(host_info, by = "host_id") %>% 
  select(host_id, host_name, review_scores_rating, host_is_superhost) %>% 
  drop_na() %>% 
  mutate(host_is_superhost = as.logical(host_is_superhost)) %>% 
  select(review_scores_rating, host_is_superhost)

q8_1 <- 
  q8[q8$host_is_superhost == FALSE, ] %>% 
  ggplot(aes(y = review_scores_rating, group = host_is_superhost)) + 
  geom_boxplot(fill = "lightpink4") + 
  labs(
    title    = "Host Ratings",
    x        = "Host",
    y        = "Rating"
  ) + theme_minimal()  + 
  theme(axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(size=10)) 

q8_2 <- 
  q8[q8$host_is_superhost == TRUE, ] %>% 
  ggplot(aes(y = review_scores_rating, group = host_is_superhost)) + 
  geom_boxplot(fill = "lightpink1") + 
  labs(
    title    = "Superhost Ratings",
    x        = "Superhost",
    y        = "Rating"
  ) + theme_minimal() + 
  theme(axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(size=10))

grid.arrange(q8_1, 
             q8_2, 
             ncol = 2,
             top = textGrob("Ratings Distribution",gp=gpar(font=2, fontsize=16)))

#################### 9. Is there any difference in response rate between superhost and normal host?

q9 <- listing %>% 
  left_join(host_info, by = "host_id") %>% 
  select(host_id, host_name, host_response_rate, host_acceptance_rate, host_is_superhost) %>%
  drop_na() %>% 
  mutate(host_is_superhost = as.logical(host_is_superhost), 
         # Transform acceptance rate and response rate
         host_response_rate = host_response_rate %>% 
           str_remove("[%]") %>% 
           as.numeric(),
         host_acceptance_rate = host_acceptance_rate %>% 
           str_remove("[%]") %>% 
           as.numeric()
  )

q9_1 <- 
  q9[q9$host_is_superhost == FALSE, ] %>% 
  ggplot(aes(y = host_response_rate, group = host_is_superhost)) + 
  geom_boxplot(fill = "lightpink4") + 
  labs(
    title    = "Host Response Rate",
    x        = "Host",
    y        = "Response Rate"
  ) + theme_minimal()  + 
  theme(axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(size=10)) 

q9_2 <- 
  q9[q9$host_is_superhost == TRUE, ] %>% 
  ggplot(aes(y = host_response_rate, group = host_is_superhost)) + 
  geom_boxplot(fill = "lightpink1") + 
  labs(
    title    = "Superhost Response Rate",
    x        = "Superhost",
    y        = "Response Rate"
  ) + theme_minimal() + 
  theme(axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(size=10))

grid.arrange(q9_1, q9_2, 
             ncol = 2,
             top = textGrob("Response Rate Distribution",gp=gpar(font=2, fontsize=16)))

#################### 10. What is the most commonly verified host information?

#Max amount of verified informations per host
ncols_q10 <- max(str_count(na.omit(host_info$host_verifications), ",")) +1

#Create list of column names
colmn_q10 <- paste("col", 1:ncols_q10)


#Create columns filled with all the verified informations
q10<- host_info%>%
  select(host_verifications) %>% 
  filter(!host_verifications == "")%>%
  separate(
    col = host_verifications, 
    sep= ",", 
    into=colmn_q10,
    remove = FALSE)

#Pivot longer to prepare data into Data Base format
q10<- pivot_longer(data=q10,
                   cols = 'col 1':'col 3',
                   names_to = "col_number",
                   values_to = "separated_host_verifications")

q10$separated_host_verifications <- trimws(q10$separated_host_verifications)

#Pipe to answer the question
q10<- q10%>%
  select(separated_host_verifications)%>%
  group_by(separated_host_verifications)%>%
  summarise(host_verifications_count=n())%>%
  na.omit()%>%
  arrange(desc(host_verifications_count))

#ploting the answer for visual reference
q10_plot<-q10 %>%
  ggplot(aes(x = host_verifications_count, 
             y = separated_host_verifications %>% reorder(host_verifications_count))) + 
  geom_bar(stat="identity", position = "dodge", fill="lightpink3") + 
  scale_x_continuous(labels = scales::number_format(big.mark = ",")) + 
  labs(title    = "Most Common Verified Information",
       x        = "Count",
       y        = "Verified Information") + 
  geom_text(aes(label = host_verifications_count), hjust = -0.5, size = 4)+
  theme_minimal() + 
  theme(axis.text = element_text(size = 10 ), axis.title = element_text(size = 10), plot.title = element_text(size=16,hjust=0.5, face="bold")) +
  expand_limits(x = 900)

q10_plot

most_common_host_verifications<- q10%>%
  kable(align = c("l", "c"),
        format.args = list(big.mark = ","),
        digits = 2)

most_common_host_verifications


#################### 11. How has the number of hosts joining airbnb increased/decreased overtime?
q11 <- 
  host_info %>%                                                     # 1
  left_join(listing, by = "host_id") %>% 
  select(host_id, host_since) %>%                                   # 2
  mutate(host_since_date = as.Date(host_since)) %>%                 # 3
  separate("host_since", c("Year", "Month", "Day"), sep = "-") %>%  # 4
  select(-Day) %>%                                                  # 5
  group_by(Year, Month) %>%                                         # 6
  count(Year, Month) %>% 
  ungroup() %>% 
  mutate(year_month = paste0(Year, "-", Month, "-", "01"),          # 7
         year_month_2 = paste0(Year, "-", Month), 
         joined = n) %>% 
  select(year_month, year_month_2, joined) %>% 
  mutate(year_month = as.Date(year_month)) %>% 
  drop_na()                                                         # 8

q11 %>% arrange(desc(joined))

q11 %>% ggplot(aes(x = year_month, y = joined)) +
  geom_line(size = 1.2, colour = "lightpink3") + 
  scale_x_date(breaks = waiver(), date_breaks = "6 months") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, face = "bold", vjust = 0.65), axis.text.y = element_text(face = "bold"),plot.title=element_text(size=16, face="bold")) + 
  labs(
    title    = "Number of hosts joined",
    subtitle = "Shows the frequency rate at which new posts sign up for airbnb",
    y        = "Joined",
    x        = "Year/Month")

q11 %>% 
  select(-year_month) %>% 
  mutate(year_month = as.yearmon(year_month_2)) %>% 
  select(-year_month_2) %>% 
  select(year_month, joined) %>% 
  arrange(desc(joined)) %>% 
  head(10) %>% kable(align = c("c", "c"))

