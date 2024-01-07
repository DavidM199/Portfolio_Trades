library(dplyr)


# Set the working directory to the folder containing the files


bigfunction <- function(df){
  df <- df %>% distinct() %>% 
    mutate(extra = ifelse(is.na(response_level), 1, 0),
           response_quantity =  if_else(extra == 1 , NA  , response_quantity),
           response_level	    = ifelse(all_resps == 0, NA, response_level),		
           response_quantity 	= 	ifelse(all_resps == 0, NA, response_quantity),
           resp_price 		    =  ifelse(all_resps == 0, NA, resp_price),
           resp_spread 		=  ifelse(all_resps == 0, NA, resp_spread),
           resp_yield 		    =  ifelse(all_resps == 0, NA, resp_yield),
           response_via_OT   =  if_else(extra == 1, "NO RESPONSE" , response_via_OT )) %>% 
    group_by(inquiryid) %>% mutate(n=n(),
                                   n_extra = sum(extra), 
                                   n_minus_extra = n - n_extra,
                                   all_extra = ifelse(n_extra == n, 1, 0) 
    )
  df <- df %>%  ungroup() %>% filter(extra != 1 | n_minus_extra == 0) %>% distinct()
  df <- df %>% mutate(
    resp_spread = ifelse(!is.na(response_level) & trading_protocol == "Spread", response_level, NA),
    resp_price = ifelse(!is.na(response_level) &  trading_protocol == "Price", response_level, NA),
    resp_yield = ifelse(!is.na(response_level) &  trading_protocol == "Yield", response_level, NA)
                      )
  df <- df %>% group_by(inquiryid) %>% mutate(best_response = case_when(
    sum(req_quantity == response_quantity) > 0 ~ case_when(
      (trading_protocol == "Price" & legside == "BWIC") ~ max(response_level),
      (trading_protocol == "Price" & legside == "OWIC") ~ min(response_level),
      ((trading_protocol == "Spread" | trading_protocol == "Yield") & legside == "BWIC") ~ min(response_level),
      ((trading_protocol == "Spread" | trading_protocol == "Yield") & legside == "OWIC") ~ max(response_level)
    ), 
    (sum(req_quantity*0.5 < response_quantity & response_quantity < req_quantity*1.5) > 0) ~ case_when(
      (trading_protocol == "Price" & legside == "BWIC") ~ max(response_level),
      (trading_protocol == "Price" & legside == "OWIC") ~ min(response_level),
      ((trading_protocol == "Spread" | trading_protocol == "Yield") & legside == "BWIC") ~ min(response_level),
      ((trading_protocol == "Spread" | trading_protocol == "Yield") & legside == "OWIC") ~ max(response_level)
    ),
    TRUE ~ case_when(
      (trading_protocol == "Price" & legside == "BWIC") ~ max(response_level),
      (trading_protocol == "Price" & legside == "OWIC") ~ min(response_level),
      ((trading_protocol == "Spread" | trading_protocol == "Yield") & legside == "BWIC") ~ min(response_level),
      ((trading_protocol == "Spread" | trading_protocol == "Yield") & legside == "OWIC") ~ max(response_level)
    )
  ))
  inquiries_df <- df[,c(1:25, 34:47)] %>% distinct(inquiryid, .keep_all = TRUE)
  responses_df <- df[, c(2, 26:33 )] 
  return(list(inquiries_df, responses_df))
}

#writing every inquiries and responses file individually, as opening
#and combining the original one was not realistic

setwd("~/Desktop/R/data_minimizing/RFQ/")
csv_files_original <- list.files(pattern = "^2022.*\\.csv$")

for (i in seq_along(csv_files_original)) {
  file <- read.csv(csv_files_original[i])
  inquiries_df <- bigfunction(file)[[1]]
  responses_df <- bigfunction(file)[[2]]
  write.csv(inquiries_df, 
            file.path("~/Desktop/R/data_minimizing/working_files/inquiries/", 
                      paste0("inquiries_", i, ".csv")), row.names = FALSE)
  write.csv(responses_df, 
            file.path("~/Desktop/R/data_minimizing/working_files/responses/", 
                      paste0("responses_", i, ".csv")), row.names = FALSE)
  
  gc()
}

#verification

setwd("~/Desktop/R/data_minimizing/working_files/inquiries/")
inquiries_csv <- list.files(pattern = "\\.csv$")
all_resps_inqu <- c()
for (inquiry in inquiries_csv){
  inquiry_df <- read.csv(inquiry) %>% arrange(inquiryid)
  all_resps_inqu <- c(all_resps_inqu, inquiry_df$all_resps)
  gc()
}

setwd("~/Desktop/R/data_minimizing/working_files/responses/")
responses_csvs <- list.files(pattern = "\\.csv$")
all_resps_res <- c()
for (response in responses_csvs){
  response_df <- read.csv(response)
  all_resps1 <- response_df %>% group_by(inquiryid) %>% arrange(inquiryid) %>% summarise(n = n())
  all_resps_res <- c(all_resps_res, all_resps1$n)
}
sum(!(all_resps_inqu == all_resps_res))
sum(all_resps_inqu == 0)
all_resps_inqu
all_resps_res

sum(!(all_resps_inqu == 0) & (all_resps_inqu > all_resps_res))
sum(!(all_resps_inqu == 0) & (all_resps_inqu != all_resps_res))
responses_csvs
inquiries_csv


#VERIFICATION 


setwd("~/Desktop/R/data_minimizing/working_files/responses/")
responses_csvs <- list.files(pattern = "\\.csv$")
resps_n <- 0
for (response in responses_csvs){
  response_df <- read.csv(response) %>% summarise(n=n())
  resps_n <- resps_n + response_df$n
}
setwd("~/Desktop/R/data_minimizing/working_files/inquiries/")
inquiries_csv <- list.files(pattern = "\\.csv$")
inqu_n <- 0
for (inquiry in inquiries_csv){
  inquiry_df <- read.csv(inquiry) %>% arrange(inquiryid) %>% summarise(n=n())
  inqu_n <- inqu_n + inquiry_df$n
  gc()
}
resps_n 
inqu_n



setwd("~/Desktop/R/data_minimizing/RFQ/")
csv_files_original <- list.files(pattern = "^2022.*\\.csv$")
all_resps_original <- c()
n_resps <- c()
for (csv in csv_files_original){
  df <- read.csv(csv)
  all_resps1 <- df %>% group_by(inquiryid) %>% arrange(inquiryid) %>% summarise(n = n())
  n_resps_df <- df %>% distinct(inquiryid, .keep_all = TRUE) %>% arrange(inquiryid) 
  n_resps <- c(n_resps, n_resps_df$all_resps)
  all_resps_original <- c(all_resps_original, all_resps1$n)
}
sum(!(n_resps == 0) & (n_resps < all_resps_original))


#Step to verify no rows with response_level == NA are left

setwd("~/Desktop/R/data_minimizing/working_files/responses/")
responses_csvs <- list.files(pattern = "\\.csv$")

fakes <- 0

for (response in responses_csvs){
  response_df <- read.csv(response)
  fakes = fakes + sum(is.na(response_df$response_level))
}

#further verification and comparing with Hamed
#Hamed's df is: number_of_repsonses

all_resps_res <- c()
inquiry_id <- c()
for (response in responses_csvs){
  response_df <- read.csv(response)
  all_resps1 <- response_df %>% distinct() %>% group_by(inquiryid) %>% summarise(n = n())  %>% 
    arrange(inquiryid)
  all_resps_res <- c(all_resps_res, all_resps1$n)
  inquiry_id <- c(inquiry_id, all_resps1$inquiryid)
}
number_of_repsonses <- number_of_repsonses %>% arrange(inquiryid)

my_data <- data.frame(inquiryid = inquiry_id, n = all_resps_res) %>% arrange(inquiryid)
sum(my_data$n)
dontmatch <- my_data %>% mutate(is_bad = (my_data$n != number_of_repsonses$n)) %>% filter(is_bad == TRUE) %>% select(-is_bad)
sum(number_of_repsonses$n)



#---------------------------------report -- correspondence with Hamed ----------------------------##
#response_quantity = ifelse(is.na(response_level) & n_minus_extra == 0, NA, response_quantity)


na <- responses_1 %>% filter(is.na(response_level))

small <- function(df){
  df <- df %>% distinct() %>% 
    mutate(extra = ifelse(is.na(response_level), 1, 0),
           response_quantity =  if_else(extra , NA , response_quantity),
           response_via_OT   =  if_else(extra == 1, "NO RESPONSE" , response_via_OT )) %>% 
    group_by(inquiryid) %>% mutate(n=n(),
                                    n_extra = sum(extra), 
                                   n_minus_extra = n - n_extra,
                                   all_extra = ifelse(n_extra == n, 1, 0) 
                                   )
  df <- df %>%  ungroup() %>% filter(extra != 1 | n_minus_extra == 0) %>% distinct()
}


hamed <- function(df){
  df <- df %>%
    mutate(extra             =  is.na(response_level),
           response_quantity =  if_else(extra , NA            , response_quantity),
           response_via_OT   =  if_else(extra , "NO RESPONSE" , response_via_OT )) %>%
    group_by(inquiryid) %>%
    mutate(n = n(),
           n_extra = sum(extra) ,
           n_minus_extra = n - n_extra) %>%
    ungroup()%>%
    filter(extra != 1 | n_minus_extra == 0)%>%
    distinct()
}

test_d <- X2022_01_11 %>% small() %>% group_by(inquiryid) %>% arrange(inquiryid) %>%  summarise(n=n())
test_h <- X2022_01_11 %>% hamed() %>% group_by(inquiryid) %>% arrange(inquiryid) %>%  summarise(n=n())

dontmatchsmall <- test_d %>% mutate(is_bad = (test_d$n !=   test_h$n)) %>% filter(is_bad == TRUE) %>% select(-is_bad)


#Taking a look at the original data, how many responses does it have 
#for each inquiry if I apply my small function?

setwd("~/Desktop/R/data_minimizing/RFQ/")
csv_files_original <- list.files(pattern = "^2022.*\\.csv$")
inquiry <- c()
num_responses <- c()

for (csv in csv_files_original){
  df <- read.csv(csv)
  all_resps1 <- df %>% small() %>% group_by(inquiryid) %>% summarise(n = n())%>% 
    arrange(inquiryid)
  num_responses <- c(num_responses, all_resps1$n)
  inquiry <- c(inquiry, all_resps1$inquiryid)

}

my_data2 <- data.frame(inquiryid = inquiry, n = num_responses) %>% arrange(inquiryid)
sum(my_data2$n)
dontmatch <- my_data2 %>% mutate(is_bad = (my_data2$n !=  number_of_repsonses$n)) %>% filter(is_bad == TRUE) %>% select(-is_bad)
sum(number_of_repsonses$n)

write.csv(dontmatch, file.path("~/Desktop/R/data_minimizing/", "doesntmatch.csv"))

##---------------------------------------------------------------------------###

#New tasks

#response_via_OT     = "No Response"	if	all_resps == 0
#response_level	    =  NA		    if	all_resps == 0
#response_quantity 	=  NA		    if	all_resps == 0
#resp_price 		    =  NA		    if	all_resps == 0
#resp_spread 		=  NA		    if	all_resps == 0
#resp_yield 		    =  NA		    if	all_resps == 0


new_thinking <- function(df) {
  df <- df %>% mutate( 
                      response_level	    = ifelse(inquiryid %in% all_resps_0, NA, response_level),
                      extra = ifelse(is.na(response_level), 1, 0),
                      response_quantity =  if_else(extra == 1 , NA  , response_quantity),
                      response_quantity 	= 	ifelse(inquiryid %in% all_resps_0, NA, response_quantity),
                      resp_price 		    =  ifelse(inquiryid %in% all_resps_0, NA, resp_price),
                      resp_spread 		=  ifelse(inquiryid %in% all_resps_0, NA, resp_spread),
                      resp_yield 		    =  ifelse(inquiryid %in% all_resps_0, NA, resp_yield),
                      response_via_OT   =  if_else(inquiryid %in% all_resps_0, "NO RESPONSE" , response_via_OT ))
}

#SETTIN R WORKSPACE AGAIN

setwd("~/Desktop/R/data_minimizing/working_files/inquiries/")
inquiries <- list.files(pattern = "\\.csv$")

list_of_inquiries <- lapply(inquiries, read.csv)
combined_inquiries <- bind_rows(list_of_inquiries)

setwd("~/Desktop/R/data_minimizing/working_files/responses/")
responses <- list.files(pattern = "\\.csv$")

list_of_responses <- lapply(responses, read.csv)
combined_responses <- bind_rows(list_of_responses)

#collecting the inquiryids for all_resps == 0
all_resps_0 <- combined_inquiries %>% filter(all_resps == 0) %>% select(inquiryid) %>% as.vector()
all_resps_0 <- all_resps_0[[1]]

#removing responses where all_reps == 0, and implementing new_thinking()
combined_responses <- combined_responses %>% new_thinking()
combined_responses_new <- combined_responses[!(combined_responses$inquiryid %in% all_resps_0), ]

write.csv(combined_responses_new, "responses_version_2.csv")

nrow(combined_responses_new)


##Discrepancy analysis with Andrew's data
inquiryData <- read.csv("inquiryData.csv")

David_inquiries <- combined_inquiries %>% select(inquiryid )%>% arrange(inquiryid)
Andrew_inquiries <- inquiryData %>% select(inquiryid) %>% arrange(inquiryid)
false <- David_inquiries$inquiryid[!David_inquiries$inquiryid %in% Andrew_inquiries$inquiryid]
Andrew_inquiries[Andrew_inquiries$inquiryid == "81908354",]


thefaulty <- combined_dataframe[combined_dataframe$inquiryid == "81908354",]


setwd("~/Desktop/R/data_minimizing/working_files/responses/")
responses <- list.files(pattern = "\\.csv$")

list_of_responses <- lapply(responses, read.csv)
combined_responses <- bind_rows(list_of_responses)

thefaulty_resp <- all_responses[all_responses$inquiryid == "81908354",]
