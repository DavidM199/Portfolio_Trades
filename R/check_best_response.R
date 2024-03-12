library(haven)
library(tidyverse)
library(ggplot2)


#Importing the inquiries and reponses, I am keeping only the necessary columns

inquiries <- read_csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/inquiries.csv")
inquiries <-inquiries %>% select(inquiryid, trading_protocol, legside, req_quantity)
responses <- read_csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/responses_new_version.csv")
responses <- responses %>% select(inquiryid, response_level, response_quantity)


#Here I am comparing the initial best_responses of Me and Hamed
david_bestresponse <- read_csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/check_best_response/david_bestresponse.csv")
hamed_bestresponse <- read_dta("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/check_best_response/check_best_response.dta")

david_bestresponse <- david_bestresponse %>% rename(david_best = best_response)
hamed_bestresponse <- hamed_bestresponse %>% rename(hamed_best = best_response)


#The questions I want to answer in the next section:

#How many differ?
#By how much do they differ?

merged_df <- david_bestresponse %>%  left_join(hamed_bestresponse, by="inquiryid")

diff_df <- merged_df %>% filter(david_best != hamed_best) %>% mutate(diff = abs(david_best-hamed_best))

diff_summary <- diff_df %>%  summarise(
  Mean = mean(    diff,       na.rm = TRUE) %>% round(3),
  SD   = sd(      diff,       na.rm = TRUE) %>% round(3),
  p1   = quantile(diff, 0.01, na.rm = TRUE) %>% round(3),
  p5   = quantile(diff, 0.05, na.rm = TRUE) %>% round(3),
  p10  = quantile(diff, 0.10, na.rm = TRUE) %>% round(3),
  p50  = quantile(diff, 0.50, na.rm = TRUE) %>% round(3),
  p90  = quantile(diff, 0.90, na.rm = TRUE) %>% round(3),
  p95  = quantile(diff, 0.95, na.rm = TRUE) %>% round(3),
  p99  = quantile(diff, 0.99, na.rm = TRUE) %>% round(3))

diff_df_major <- diff_df %>% filter(diff > 0.1)

diff_df_major_summary <- diff_df_major %>%  summarise(
  Mean = mean(    diff,       na.rm = TRUE) %>% round(3),
  SD   = sd(      diff,       na.rm = TRUE) %>% round(3),
  p1   = quantile(diff, 0.01, na.rm = TRUE) %>% round(3),
  p5   = quantile(diff, 0.05, na.rm = TRUE) %>% round(3),
  p10  = quantile(diff, 0.10, na.rm = TRUE) %>% round(3),
  p50  = quantile(diff, 0.50, na.rm = TRUE) %>% round(3),
  p90  = quantile(diff, 0.90, na.rm = TRUE) %>% round(3),
  p95  = quantile(diff, 0.95, na.rm = TRUE) %>% round(3),
  p99  = quantile(diff, 0.99, na.rm = TRUE) %>% round(3))


diff_plot = diff_df %>%  ggplot(aes(diff)) + geom_histogram(binwidth = 1)
diff_plot 

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

differing <- diff_df_major %>% select(inquiryid)
#write_csv(differing, "~/Desktop/Portfolio_Trades_my_computer/data_minimizing/check_best_response/differing.csv")

#Getting the data necessary for the best_response calculation
data_for_calculation <- responses %>% left_join(inquiries, by="inquiryid")

#IN THIS SECTION I AM DOING THREE ROUNDS OF FILTERING ROWS FOR A CRITERIA 
# (response_quantity relation to req_quantity) AND 
#I AM EXCLUDING ALREADY CALCULATED INQUIRIES WITH filter(is.na(best_response))

round_1 <- data_for_calculation %>% mutate(response_level_adjusted = if_else(
  req_quantity == response_quantity, 
  response_level, NA)) %>% 
  group_by(inquiryid) %>%
  mutate(
    best_response = ifelse(
      sum(!is.na(response_level_adjusted)) > 0, case_when(
        (trading_protocol == "Price" & legside == "BWIC") ~ max(response_level_adjusted, na.rm = TRUE),
        (trading_protocol == "Price" & legside == "OWIC") ~ min(response_level_adjusted, na.rm = TRUE),
        ((trading_protocol == "Spread" | trading_protocol == "Yield") & legside == "BWIC") ~ min(response_level_adjusted, na.rm = TRUE),
        ((trading_protocol == "Spread" | trading_protocol == "Yield") & legside == "OWIC") ~ max(response_level_adjusted, na.rm = TRUE)
      ), NA
    )) %>% ungroup()

round_2 <- round_1 %>% filter(is.na(best_response))%>%
  mutate(response_level_adjusted = if_else(
    (req_quantity*0.5 < response_quantity) & (response_quantity < req_quantity*1.5), 
    response_level, 
    NA)) %>% 
  group_by(inquiryid) %>%
  mutate(
    best_response = ifelse(
      sum((req_quantity*0.5 < response_quantity) & (response_quantity < req_quantity*1.5)) > 0, case_when(
        (trading_protocol == "Price" & legside == "BWIC") ~ max(response_level_adjusted, na.rm = TRUE),
        (trading_protocol == "Price" & legside == "OWIC") ~ min(response_level_adjusted, na.rm = TRUE),
        ((trading_protocol == "Spread" | trading_protocol == "Yield") & legside == "BWIC") ~ min(response_level_adjusted, na.rm = TRUE),
        ((trading_protocol == "Spread" | trading_protocol == "Yield") & legside == "OWIC") ~ max(response_level_adjusted, na.rm = TRUE)
      ),NA
    )) %>% ungroup()

round_3 <- round_2 %>% filter(is.na(best_response))%>%
  group_by(inquiryid) %>%
  mutate(
    best_response = case_when(
        (trading_protocol == "Price" & legside == "BWIC") ~ max(response_level, na.rm = TRUE),
        (trading_protocol == "Price" & legside == "OWIC") ~ min(response_level, na.rm = TRUE),
        ((trading_protocol == "Spread" | trading_protocol == "Yield") & legside == "BWIC") ~ min(response_level, na.rm = TRUE),
        ((trading_protocol == "Spread" | trading_protocol == "Yield") & legside == "OWIC") ~ max(response_level, na.rm = TRUE),
        TRUE ~ NA
      )
    ) %>% ungroup()


#Now I only need one observation per inquiryid, and as every inquiryid has only one best_response, 
#I can just take any one of the responses by calling: distinct(inquiryid, .keep_all = TRUE)

round_1 <- round_1 %>% select(inquiryid, best_response) %>% distinct(inquiryid, .keep_all = TRUE)
round_2 <- round_2 %>% select(inquiryid, best_response) %>% distinct(inquiryid, .keep_all = TRUE)
round_3 <- round_3 %>% select(inquiryid, best_response) %>% distinct(inquiryid, .keep_all = TRUE)


#The corrected dataframe contains the newly calculated best_responses 
#2 COLUMNS: inquiryid, best_response
corrected <- round_1 %>% left_join(round_2, by="inquiryid") %>% 
                        mutate(best_response.x = ifelse(is.na(best_response.x), best_response.y, best_response.x)) %>% 
                        rename(best_response = best_response.x) %>% select(-best_response.y)

corrected <- corrected %>% left_join(round_3, by="inquiryid") %>% 
  mutate(best_response.x = ifelse(is.na(best_response.x), best_response.y, best_response.x)) %>% 
  rename(best_response = best_response.x) %>% select(-best_response.y)


#best_response.x - my newly calculated variable
#best_response.y - Hamed's best_response variable

#(I am only focusing on observations where the difference is bigger, than let's say 0.1,
#because smaller diffs can be attributed to rounding differences of software - I am using Rstudio, Hamed uses Stata)
FINAL_CHECK <- corrected %>% left_join(hamed_bestresponse, by="inquiryid") %>% mutate(best_response.y = as.numeric(best_response.y)) %>% 
                                filter(best_response.x != best_response.y)%>% 
                                mutate(diff = abs(best_response.x - best_response.y)) %>% 
                                filter(diff > 0.01)

#The 14 observations left in FINAL_CHECK  are practically the same with the usual stata and r format-discrepancy

write_csv(FINAL_CHECK, "~/Desktop/Portfolio_Trades_my_computer/data_minimizing/check_best_response/FINAL_CHECK.csv")

# Correcting old best_response data of mine in inquiries
inquiries <- inquiries %>% select(-best_response) %>% left_join(corrected, by="inquiryid")

corrected <- inquiries %>% select(inquiryid, best_response)

write.csv(inquiries, "~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/inquiries.csv")





