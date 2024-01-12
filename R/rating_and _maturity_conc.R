library(tidyverse)
library(ggplot2)
library(lubridate)

bondRating <- read.csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/FISD/FISD_rating_changes_only.csv")
bondFacts <- read.csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/FISD/FISD_issue_our_cusips.csv")
inquiryData <- read.csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/inquiries_58_columns.csv")
cusips <- inquiryData["cusip"]

to_merge_Facts <- bondFacts %>% select(MATURITY, COMPLETE_CUSIP)

#creating a df to compare submittime to the ratingdate, taking only rating-date < submittime

to_join <- inquiryData %>% select(cusip, submittime) 

submittime_rating_date <- bondRating %>% select(RATING_DATE, COMPLETE_CUSIP, RATING_TYPE, RATING) %>% filter(RATING_TYPE=="SPR" | RATING_TYPE=="MR") %>%  
  left_join(to_join, by = c("COMPLETE_CUSIP"="cusip"), relationship = "many-to-many") %>% 
  filter(submittime > RATING_DATE) %>% select(-submittime)

#rating_type_rating_date_rating <- bondRating %>% select(COMPLETE_CUSIP, RATING_TYPE, RATING_DATE, RATING) %>%
#  filter(RATING_TYPE=="SPR" | RATING_TYPE=="MR") %>% distinct(COMPLETE_CUSIP, RATING_DATE, .keep_all = TRUE) 


#taking only the max(RATING_DATE)
to_merge_Rating <- submittime_rating_date  %>% 
  group_by(COMPLETE_CUSIP, RATING_TYPE) %>% summarise(RATING_DATE = max(RATING_DATE)) %>%  
  left_join(submittime_rating_date, by = c("COMPLETE_CUSIP" = "COMPLETE_CUSIP", "RATING_DATE" = "RATING_DATE", "RATING_TYPE"="RATING_TYPE"), relationship = "many-to-many") %>% ungroup() %>% distinct()

#checking that for most there is one rating per agency for the date
multiple_from_one_on_a_day <- to_merge_Rating %>% group_by(COMPLETE_CUSIP, RATING_TYPE) %>% summarise(num_rat = n()) %>% filter(num_rat == 1)
sum(multiple_from_one_on_a_day$num_rat)

merged_inquiries <- inquiryData %>% left_join(to_merge_Facts, by = c("cusip" = "COMPLETE_CUSIP"), relationship = "many-to-one") 

merged_inquiries <- merged_inquiries %>%  left_join(to_merge_Rating, by = c("cusip" = "COMPLETE_CUSIP"), relationship = "many-to-many")

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
  
#Here I find out what kind of ratings exict for both
ratings_SPR <- merged_inquiries %>% select(RATING_TYPE, RATING) %>% distinct() %>% filter(RATING_TYPE=="SPR")
ratings_MR <- merged_inquiries %>% select(RATING_TYPE, RATING) %>% distinct() %>% filter(RATING_TYPE=="MR")
ratings_SPR$RATING

ordered_ratings_SPR <- c("AAA", "AA+", "AA", "AA-", "A+", "A", "A-", 
                     "BBB+", "BBB", "BBB-", "BB+", "BB", "BB-", 
                     "B+", "B", "B-", "CCC+", "CCC", "CCC-", 
                     "CC", "C", "D")
ordered_ratings_MR <- c("Aaa", "Aa1", "Aa2", "Aa3", "A1", "A2", "A3",
                        "Baa1", "Baa2", "Baa3", "Ba1", "Ba2", "Ba3",
                        "B1", "B2", "B3", "Caa1", "Caa2", "Caa3",
                        "Ca", "C")
  
#Adding the numerical ratings column
merged_inquiries <- merged_inquiries %>% 
  mutate(RATING_num = ifelse(RATING_TYPE=="SPR", match(RATING, ordered_ratings_SPR), match(RATING, ordered_ratings_MR)))

#computing rating histograms

filtered_df <- merged_inquiries[merged_inquiries$number_assets >= 10, ]

# Compute the required statistics for each specified column
compute_stats <- function(column) {
  min_val <- min(column, na.rm = TRUE)
  q1 <- quantile(column, 0.25, na.rm = TRUE)
  q2 <- median(column, na.rm = TRUE)
  q3 <- quantile(column, 0.75, na.rm = TRUE)
  max_val <- max(column, na.rm = TRUE)
  
  mm <- max_val / min_val
  q31 <- q3 / q1
  q31_norm <- (q3 - q1) / q2
  mm_norm <- (max_val - min_val) / q2
  
  return(c(Min = min_val, Q1 = q1, Q2 = q2, Q3 = q3, Max = max_val, MM = mm, Q31 = q31, Q31_Norm = q31_norm, MM_Norm = mm_norm))
}
compute_mm <- function(x){
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  mm <- max_val / min_val
  return(mm)
}
compute_Q31 <- function(x){
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  q31 <- q3 / q1
  return(q31)
}
compute_Q31_Norm <- function(x){
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q2 <- median(x, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  q31_norm <- (q3 - q1) / q2
  return(q31_norm)
}
compute_MM_Norm <- function(x){
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  q2 <- median(x, na.rm = TRUE)
  mm_norm <- (max_val - min_val) / q2
}


#getting the maturity, s&p numerical, Moody's numerical dataframes ready
maturity_stats <- filtered_df %>% group_by(req_id, request_type) %>% summarise(MM = compute_mm(maturity_at_submittime), 
                                                                               Q31 = compute_Q31(maturity_at_submittime), 
                                                                               Q31_Norm = compute_Q31_Norm(maturity_at_submittime), 
                                                                               MM_Norm = compute_MM_Norm(maturity_at_submittime))
write.csv(maturity_stats, "~/Desktop/Portfolio_Trades_my_computer/data_minimizing/FISD/maturity_stats.csv")
maturity_stats <- read.csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/FISD/maturity_stats.csv")

SPR_num <- filtered_df %>% filter(RATING_TYPE=="SPR") %>% group_by(req_id, request_type) %>% summarise(MM = compute_mm(RATING_num), 
                                                                                                   Q31 = compute_Q31(RATING_num), 
                                                                                                   Q31_Norm = compute_Q31_Norm(RATING_num), 
                                                                                                   MM_Norm = compute_MM_Norm(RATING_num))
write.csv(SPR_num, "~/Desktop/Portfolio_Trades_my_computer/data_minimizing/FISD/S&P_stats.csv")

MR_num <- filtered_df %>% filter(RATING_TYPE=="MR") %>% group_by(req_id, request_type) %>% summarise(MM = compute_mm(RATING_num), 
                                                                                                          Q31 = compute_Q31(RATING_num), 
                                                                                                          Q31_Norm = compute_Q31_Norm(RATING_num),
                                                                                                     MM_Norm = compute_MM_Norm(RATING_num))
write.csv(MR_num, "~/Desktop/Portfolio_Trades_my_computer/data_minimizing/FISD/Moodys_stats.csv")

maturity_stats <- maturity_stats %>% filter(is.finite(MM), is.finite(Q31), is.finite(Q31_Norm), is.finite(MM_Norm))
maturity_stats <- maturity_stats %>%
  mutate(MM = as.numeric(MM),
         Q31 = as.numeric(Q31),
         Q31_Norm = as.numeric(Q31_Norm),
         MM_Norm = as.numeric(MM_Norm))

long_maturity_stats <- maturity_stats %>%
  gather(key = "metric", value = "value", MM, Q31, Q31_Norm, MM_Norm)

#PT maturity_stats
PT_long_maturity_stats <- long_maturity_stats %>% filter(request_type=="PT")

ggplot(PT_long_maturity_stats, aes(x = value, fill = factor(request_type))) +  
  geom_histogram(bins = 30, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 30))+
  facet_wrap(~metric, scales = "free_x") +
  scale_fill_manual(values = c("blue", "red")) +  
  labs(title = "Histograms of Maturity - PT", x = "Value", y = "Count")

#List maturity_stats
LIST_long_maturity_stats <- long_maturity_stats %>% filter(request_type=="List")

ggplot(LIST_long_maturity_stats, aes(x = value, fill = factor(request_type))) +  
  geom_histogram(bins = 40, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 40))+
  facet_wrap(~metric, scales = "free_x") +
  scale_fill_manual(values = c("blue", "red")) +  
  labs(title = "Histograms of Maturity - List", x = "Value", y = "Count")

#S&P ratings histograms-----------------------
SPR_num <- SPR_num %>% filter(is.finite(MM), is.finite(Q31), is.finite(Q31_Norm), is.finite(MM_Norm))
SPR_num <- SPR_num %>%
  mutate(MM = as.numeric(MM),
         Q31 = as.numeric(Q31),
         Q31_Norm = as.numeric(Q31_Norm),
         MM_Norm = as.numeric(MM_Norm))

long_SPR_num <- SPR_num %>%
  gather(key = "metric", value = "value", MM, Q31, Q31_Norm, MM_Norm)

#PT S&P ratings stats
PT_long_SPR_num <- long_SPR_num %>% filter(request_type=="PT")

ggplot(PT_long_SPR_num, aes(x = value, fill = factor(request_type))) +  
  geom_histogram(bins = 28, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 13))+
  facet_wrap(~metric, scales = "free_x") +
  scale_fill_manual(values = c("blue", "red")) +  
  labs(title = "Histograms of S&P ratings stats - PT", x = "Value", y = "Count")

#List S&P ratings stats
LIST_long_SPR_num <- long_SPR_num %>% filter(request_type=="List")

ggplot(LIST_long_SPR_num, aes(x = value, fill = factor(request_type))) +  
  geom_histogram(bins = 28, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 13))+
  facet_wrap(~metric, scales = "free_x") +
  scale_fill_manual(values = c("blue", "red")) +  
  labs(title = "Histograms of S&P ratings stats - List", x = "Value", y = "Count")

#Moody's ratings histograms-------------

MR_num <- MR_num %>% filter(is.finite(MM), is.finite(Q31), is.finite(Q31_Norm), is.finite(MM_Norm))
MR_num <- MR_num %>%
  mutate(MM = as.numeric(MM),
         Q31 = as.numeric(Q31),
         Q31_Norm = as.numeric(Q31_Norm),
         MM_Norm = as.numeric(MM_Norm))

long_MR_num <- MR_num %>%
  gather(key = "metric", value = "value", MM, Q31, Q31_Norm, MM_Norm)

#PT Moody's ratings stats
PT_long_MR_num <- long_MR_num %>% filter(request_type=="PT")

ggplot(PT_long_MR_num, aes(x = value, fill = factor(request_type))) +  
  geom_histogram(bins = 30, alpha = 0.7) +
  scale_x_continuous(limits = c(0, 15))+
  facet_wrap(~metric, scales = "free_x") +
  scale_fill_manual(values = c("blue", "red")) +  
  labs(title = "Histograms of Moody's rating stats - PT", x = "Value", y = "Count")

#List Moody's ratings stats
LIST_long_MR_num<- long_MR_num %>% filter(request_type=="List")

ggplot(LIST_long_MR_num, aes(x = value, fill = factor(request_type))) +  
  geom_histogram(bins = 30, alpha = 0.7) +
  scale_x_continuous(limits = c(0, 15))+
  facet_wrap(~metric, scales = "free_x") +
  scale_fill_manual(values = c("blue", "red")) +  
  labs(title = "Histograms of Moody's rating stats - List", x = "Value", y = "Count")


