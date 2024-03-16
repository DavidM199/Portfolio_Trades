library(tidyverse)
library(tidyr)
library(dplyr)

combined_inquiries <- read.csv("~/Desktop/R/data_minimizing/working_files/inquiries.csv")

number_assets <- combined_inquiries  %>%  select(listid, cusip)
number_assets <- number_assets %>% group_by(listid) %>% summarise(number_assets=n_distinct(cusip)) %>% 
  arrange(listid) %>% na.omit(listid)

computations <- function(df){
  
  df <- df %>% left_join(number_assets, by=join_by(listid)) %>%  mutate(
                        
                      number_assets = if_else(!is.na(listid), number_assets, 1),
                      #setting the variables
                      cpp_bid_ps = case_when(
                        product_cd=="USHY" ~ cpp_bid_price,
                        product_cd=="USHG" ~ cpp_bid_spread
                      ),
                      cpp_offer_ps = case_when(
                        product_cd=="USHY" ~ cpp_offer_price,
                        product_cd=="USHG" ~ cpp_offer_spread
                      ),
                      trade_ps = case_when(
                        product_cd=="USHY" ~ tradeprice,
                        product_cd=="USHG" ~ tradespread
                      ),
                      t_b = case_when(
                        !is.na(trade_ps) ~ trade_ps,
                        is.na(trade_ps) ~ best_response
                      ),
                      #computations - START
                      cpp_mid_ps = (cpp_bid_ps + cpp_offer_ps)/2,
                      buy_or_sell = case_when(
                        legside == "OWIC" ~ 1,
                        legside == "BWIC" ~ -1
                      ),
                      price_or_spread = if_else(trading_protocol == "Price", 1, -1),
                      
                      spread_dollar = buy_or_sell * price_or_spread * (t_b - cpp_mid_ps),
                      spread_cpp = price_or_spread * (cpp_offer_ps - cpp_bid_ps)/2,
                      spread = spread_dollar / cpp_mid_ps,
                      req_dollar_value = case_when(
                        legside=="OWIC"~cpp_offer_ps * req_quantity ,
                        legside=="BWIC"~cpp_bid_ps * req_quantity
                      ),
                      best_dollar_value = t_b * req_quantity,
                      #Task 3.
                      request_type = case_when(
                        inquiry_type == "PT" ~ "PT",
                        (inquiry_type != "PT") & !is.na(listid) & (number_assets > 1) ~ "List",
                        is.na(listid) | (number_assets == 1   & inquiry_type != "PT") ~ "SRFQ" 
                      ),
                      req_id = ifelse(inquiry_type=="SRFQ", inquiryid, listid),
                      fill_rate = if_else(inquiry_outcome == "DNT" , 0 , trade_quantity/req_quantity),
                      Amgmt = ifelse(p_type=="Asset Manager", 1, 0),
                      BD = ifelse(p_type=="Broker-Dealer", 1, 0),
                      anyresp = ifelse(all_resps > 0, 1, 0),
                      asap = ifelse(asap=="Y", 1, 0),
                      OT_invited = ifelse(OT_invited=="Y", 1, 0),
                      revealdealers = ifelse(revealdealers=="Y", 1, 0) 
                      )
}

### REAL COMPUTATION 

combined_responses <- read.csv("~/Desktop/R/data_minimizing/working_files/responses_version_2.csv")


#adding missing 
to_join <- combined_responses %>% select(inquiryid, inquiry_outcome) %>% distinct(inquiryid, .keep_all = TRUE)
combined_inquiries <- combined_inquiries %>% left_join(to_join, by=join_by(inquiryid))

#This was necessary at the time of writing
#write.csv(combined_inquiries, "~/Desktop/R/data_minimizing/working_files/inquiries.csv")

computed_results <- combined_inquiries %>% computations() 

write.csv(computed_results, "~/Desktop/R/data_minimizing/working_files/newest_best_inquiries.csv")

filtered_results <- computed_results %>% select(-c(submittime, inquiryid, inquiry_type, listid, 
                                              tradeid, responsedate, tradetime, cusip, isin, 
                                              trading_protocol, legside, p_type, t_b, cpp_bid_ps, 
                                              cpp_offer_ps, trade_ps, n, n_extra, extra, n_minus_extra))

#Creating USHG and ASHY summary tables in the format required

USHG <- filtered_results %>% select(-number_assets) %>% filter(product_cd == "USHG", !is.infinite(spread)) %>% 
  group_by(request_type) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -request_type, names_to = "Variable", values_to = "Mean") %>% 
  pivot_wider(names_from = request_type, values_from = Mean) %>% 
  column_to_rownames('Variable')

USHY <- filtered_results %>% select(-number_assets) %>% filter(product_cd == "USHY", !is.infinite(spread)) %>% 
  group_by(request_type) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -request_type, names_to = "Variable", values_to = "Mean") %>% 
  pivot_wider(names_from = request_type, values_from = Mean) %>% 
  column_to_rownames('Variable')


#gathering means for number_assets  grouped by req_id

USHG_number_assets <- filtered_results %>% filter(product_cd == "USHG") %>%
                                          distinct(req_id, .keep_all = TRUE) %>% group_by(request_type) %>%  summarise(Mean = mean(number_assets)) %>% 
  pivot_wider(names_from = request_type, values_from = Mean) 
row.names(USHG_number_assets) <- c("number_assets")

USHY_number_assets <- filtered_results %>% filter(product_cd == "USHY") %>% 
                                            distinct(req_id, .keep_all = TRUE) %>% group_by(request_type) %>% summarise(Mean = mean(number_assets)) %>% 
  pivot_wider(names_from = request_type, values_from = Mean)
row.names(USHY_number_assets) <- c("number_assets")
 
#adding the number_asset mean observations to the table
USHG <- USHG %>% bind_rows(USHG_number_assets) 

USHY <- USHY %>% bind_rows(USHY_number_assets)

#renaming the rows appropriately
USHG <- USHG %>%
  rename(USHG_List = List, USHG_PT = PT, USHG_SRFQ = SRFQ)
USHY <- USHY %>%
  rename(USHY_List = List, USHY_PT = PT, USHY_SRFQ = SRFQ)

#ordering the rows in alphabetical order of the rownames
USHG <- USHG[order(rownames(USHG)), ]  
USHY <- USHY[order(rownames(USHY)), ]  

#rounding to 6 digits
USHG <- USHG %>% mutate(USHG_List = round(USHG_List, 6),
                        USHG_PT = round(USHG_PT, 6),
                        USHG_SRFQ = round(USHG_SRFQ, 6))

USHY <- USHY %>% mutate(USHY_List = round(USHY_List, 6),
                        USHY_PT = round(USHY_PT, 6),
                        USHY_SRFQ = round(USHY_SRFQ, 6))

#mean number of assets overall
number_assets_means <- filtered_results %>%
  distinct(req_id, .keep_all = TRUE) %>% group_by(request_type) %>%  summarise(Mean = mean(number_assets)) %>% 
  pivot_wider(names_from = request_type, values_from = Mean) 
row.names(number_assets_means) <- c("number_assets")

## REVISION

flagged_version <- filtered_results %>% group_by(req_id) %>% mutate(flag = ifelse(n_distinct(product_cd) > 1, 1, 0)) %>% filter(flag==0)
nrow(flagged_version[flagged_version$flag == 1,])

number_assets_means <- flagged_version %>%
  distinct(req_id, .keep_all = TRUE) %>% group_by(request_type) %>%  summarise(Mean = mean(number_assets)) %>% 
  pivot_wider(names_from = request_type, values_from = Mean) 
row.names(number_assets_means) <- c("number_assets")


#Saving the tables in pdf files
setwd("~/Desktop/R/PT/")
library("gridExtra")

pdf("ivc.pdf", height=nrow(USHG)/3, width = 13)
grid.table(USHG)
dev.off()

pdf("ivc.pdf2", height=nrow(USHG)/3, width = 13)
grid.table(USHY)
dev.off()






