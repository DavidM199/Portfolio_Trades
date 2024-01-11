library(tidyverse)
library(lubridate)

bondRating <- read.csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/FISD/FISD_rating_changes_only.csv")
bondFacts <- read.csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/FISD/FISD_issue_our_cusips.csv")
inquiryData <- read.csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/inquiries.csv")
cusips <- inquiryData["cusip"]

to_merge_Facts <- bondFacts %>% select(MATURITY, COMPLETE_CUSIP)



rating_type_rating_date_rating <- bondRating %>% select(COMPLETE_CUSIP, RATING_TYPE, RATING_DATE, RATING) %>%
  filter(RATING_TYPE=="SPR" | RATING_TYPE=="MR") %>% distinct(COMPLETE_CUSIP, RATING_DATE, .keep_all = TRUE) 

to_merge_Rating <- bondRating %>% filter(RATING_TYPE=="SPR" | RATING_TYPE=="MR")  %>% 
  group_by(COMPLETE_CUSIP) %>% summarise(RATING_DATE = max(RATING_DATE)) %>%  
  left_join(rating_type_rating_date_rating, by = c("COMPLETE_CUSIP" = "COMPLETE_CUSIP", "RATING_DATE" = "RATING_DATE"))

to_merge_Rating %>% n_distinct()

merged_inquiries <- inquiryData %>% left_join(to_merge_Facts, by = c("cusip" = "COMPLETE_CUSIP"), relationship = "many-to-one") 

merged_inquiries <- merged_inquiries %>%  left_join(to_merge_Rating, by = c("cusip" = "COMPLETE_CUSIP"), relationship = "many-to-one")



#pasted this here in case I need it later
func_days <- function(Date1 , Date2){
   D2 <- day(Date2)
   D1 <- day(Date1)
   
   M2 <- month(Date2)
   M1 <- month(Date1)
   
   Y2 <- year(Date2)
   Y1 <- year(Date1)
   
   D1 <- if_else(D1==31 , 30 , D1 , NA)
   D2 <- if_else(D2==31 & D1==30 , 30 , D2 , NA)
   D1 <- if_else(M1==2 & (D1==28 & D1==29) , 30 , D1 ,  NA)
   
   days <- 360*(Y2 - Y1) + 30*(M2 - M1) + (D2 - D1)
}
  

difference_in_years <- function(start_date, end_date){
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  interval <- func_days(start_date, end_date)
  years <- interval / 360
  
  return(years)
}

merged_inquiries$maturity_at_submittime <- difference_in_years(merged_inquiries$submittime, merged_inquiries$MATURITY)
  

merged_inquiries %>% select(RATING_TYPE, RATING)
  
  