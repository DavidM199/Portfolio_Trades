library(haven)
library(tidyverse)

FISD_bond_issues <- read_dta("~/Desktop/Portfolio_trades_my_computer/data_minimizing/FISD/FISD_bond issues.dta")
FISD_bond_rating <- read_dta("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/FISD/FISD_bond rating.dta")
our_inquiries <- read_csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/inquiries.csv")

#gathering our cusips
our_cusips <- our_inquiries %>%  filter(!is.na(cusip)) %>% distinct(cusip)
our_cusips <- our_cusips$cusip

#EXTRACTING DATA THAT WE ACTUALLY NEED
FISD_rating_our_cusips <- FISD_bond_rating %>% filter(COMPLETE_CUSIP %in% our_cusips)

#keeping only the rows where the rating for a bond changes compared to the last rating
cusips_vector <- FISD_rating_our_cusips %>% distinct(COMPLETE_CUSIP)
cusips_vector <- cusips_vector[["COMPLETE_CUSIP"]]
#FISD_rating_our_cusips$needed <- 0

#for (cusip in cusips_vector){
#  bond_df <- FISD_rating_our_cusips[cusip == FISD_rating_our_cusips$COMPLETE_CUSIP,] 
#  bond_df <- bond_df %>% arrange(RATING_DATE)
#  current_rating <- 0
#  current_date <- 0
#  for (i in 1:nrow(bond_df)){
#    if (bond_df$RATING[i] != current_rating){
#      if (i == 1 | (bond_df$RATING_DATE[i] == current_date && bond_df$RATING_TYPE[i] != bond_df$RATING_TYPE[i-1])){
#        bond_df$needed[i] <- 1
#      }
#      else if (bond_df$RATING_DATE[i] != current_date){
#        bond_df$needed[i] <- 1
#      }
#      current_rating <- bond_df$RATING[i]
#    }
#    else if (bond_df$RATING_DATE[i] == current_date && bond_df$RATING_TYPE[i] != bond_df$RATING_TYPE[i-1]){
#      bond_df$needed[i] <- 1
#    }
#    }
#    current_date <- bond_df$RATING_DATE[i]
  
#  FISD_rating_our_cusips[cusip == FISD_rating_our_cusips$COMPLETE_CUSIP,] <- bond_df
#}

#FISD_rating_changes_only <- FISD_rating_our_cusips %>% filter(needed == 1)


#NOW THE SAME WITH BOND ISSUE
FISD_issue_our_cusips <- FISD_bond_issues %>% filter(COMPLETE_CUSIP %in% our_cusips)


#write csv for the extracted rating data
write.csv(FISD_rating_our_cusips, "~/Desktop/Portfolio_Trades_my_computer/data_minimizing/FISD/FISD_rating_changes_only.csv")

#write csv for the extracted issue data
write.csv(FISD_issue_our_cusips, "~/Desktop/Portfolio_Trades_my_computer/data_minimizing/FISD/FISD_issue_our_cusips.csv")

