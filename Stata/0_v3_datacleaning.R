library(data.table)
library(dplyr)
library(haven)

csv_files <- list.files("Data/RFQ/Original/" , full.names = T, pattern = ".csv")

df.inquiry <- list()
df.resp    <- list()

for(i in 1:length(csv_files)){
  
  df <- fread(csv_files[i])
  
  df <- df %>%
    mutate(extra             =  is.na(response_level),
           response_quantity =  if_else(extra , NA            , response_quantity),
           resp_price        =  if_else(extra , NA            , resp_price),
           resp_spread       =  if_else(extra , NA            , resp_spread),
           resp_yield        =  if_else(extra , NA            , resp_yield),
           response_quantity =  if_else(extra , NA            , response_quantity),
           response_via_OT   =  if_else(extra , "No Response" , response_via_OT )) %>%
    group_by(inquiryid) %>%
    mutate(n = n(),
           n_extra = sum(extra) ,
           n_minus_extra = n - n_extra) %>%
    ungroup()%>%
    filter(extra != 1 | n_minus_extra == 0)%>%
    distinct()
  
  
  df <- df %>%
    mutate(resp_spread = if_else(!is.na(response_level) & trading_protocol=="Spread" & is.na(resp_spread) , response_level , resp_spread ),
           resp_price  = if_else(!is.na(response_level) & trading_protocol=="Price"  & is.na(resp_price)  , response_level , resp_price ),
           resp_yield  = if_else(!is.na(response_level) & trading_protocol=="Yeild"  & is.na(resp_yield)  , response_level , resp_yield ))
  
  
  df.resp[[i]]    <- df %>%
    select(inquiryid , starts_with("resp"))
  
  df.inquiry[[i]] <- df %>%
    select(-starts_with("resp") , -c(extra , n , n_extra , n_minus_extra))%>%
    distinct()
  
  print(i)
    
  
  # write_dta(df.resp    , path = paste0("Data/RFQ/resp_" , substr(csv , 10 , 19) , ".dta"))
  # write_dta(df.inquiry , path = paste0("Data/RFQ/inqu_" , substr(csv , 10 , 19) , ".dta"))
  
}

df.inquiry_ <- rbindlist(df.inquiry)
df.resp_    <- rbindlist(df.resp)

write_dta(df.inquiry_    , path = "Data/RFQ/inquiry.dta")
write_dta(df.resp_       , path = "Data/RFQ/response.dta")

df_ <- df.inquiry_ %>%
    select(inquiryid , all_resps )

df_ <- left_join(df.resp_ , df_)

number_of_repsonses <- df_ %>%
  group_by(inquiryid)%>%
  summarise(n = n(),
            all_resp = unique(all_resps))

number_of_repsonses2 <- number_of_repsonses %>%
  filter(n != all_resp & all_resp >1)%>%
  mutate(diff = n - all_resp)

save(number_of_repsonses , file = "number_of_responses.RData")
