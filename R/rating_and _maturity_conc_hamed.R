library(tidyverse)
library(ggplot2)
library(lubridate)
library(haven)
library(patchwork)


# Reading Data ------------------------------------------------------------


df.inquiry     <- read_dta("../Data/RFQ/inquiry_proccessed.dta")
df.bondFacts   <- read_dta("../Data/MergentFisd/bondFacts.dta")
df.bondRatings <- read_dta("../Data/MergentFisd/bondRating.dta")

df.inquiry  <- read.csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/inquiries_58_columns.csv")
df.bondFacts <- read.csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/FISD/FISD_issue_our_cusips.csv")
df.bondRatings <- read_dta("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/FISD/bondRating_Hamed_filtered.dta")
# Filtering and Merging Data --------------------------------------------


# Instead of using day function, as.Date can be used
df.inquiry <- df.inquiry %>%
  select(submittime , req_id , request_type , cusip , number_assets, p_type)%>%
  filter(number_assets >= 10)%>%
  mutate(date = as.Date(submittime))%>%
  select(-submittime)

df.inquiry_broker_dealer <- list(filter(df.inquiry, p_type == "Broker-Dealer"), filter(df.inquiry, p_type != "Broker-Dealer"))
  
df.bondFacts <- df.bondFacts %>% rename(cusip = COMPLETE_CUSIP)
df.bondFacts <- df.bondFacts %>%
  select(cusip , MATURITY)

df.bondRatings <- df.bondRatings %>%
  select(cusip , rating_date , ratingMR , ratingSPR)

gc()

# closest is an option in join_by which let you choose the closest date which is smaller than something
for (i in 1:2){
  df.inquiry_broker_dealer[[i]] <- left_join(df.inquiry_broker_dealer[[i]]  , df.bondFacts   , by = join_by(cusip))
  df.inquiry_broker_dealer[[i]] <- left_join(df.inquiry_broker_dealer[[i]] , df.bondRatings , by = join_by(cusip , closest(date < rating_date)))
}

# Data Manipulation -------------------------------------------------------

# ChatGPT did this for me
FUNC_convert_to_numeric <- function(credit_rating, agency) {
  # Define rating scales for S&P and Moody's
  sp_ratings <- c("AAA", "AA+", "AA", "AA-", "A+", "A", "A-", 
                  "BBB+", "BBB", "BBB-", "BB+", "BB", "BB-", 
                  "B+", "B", "B-", "CCC+", "CCC", "CCC-", "CC", "C", "D")
  
  moody_ratings <- c("Aaa", "Aa1", "Aa2", "Aa3", "A1", "A2", "A3", 
                     "Baa1", "Baa2", "Baa3", "Ba1", "Ba2", "Ba3", 
                     "B1", "B2", "B3", "Caa1", "Caa2", "Caa3", "Ca", "C")
  
  # Determine the agency and get the corresponding rating scale
  rating_scale <- switch(agency,
                         "S&P" = sp_ratings,
                         "Moody's" = moody_ratings)
  
  # Convert the credit rating to numeric value
  numeric_value <- match(credit_rating, rating_scale)
  
  return(numeric_value)
}

# interval and months can be used here.
for (i in 1:2){
  df.inquiry_broker_dealer[[i]] <- df.inquiry_broker_dealer[[i]] %>%
    mutate(y_to_matur  = interval(date , MATURITY)/months(12),
          ratingMR    = FUNC_convert_to_numeric(ratingMR  , agency = "Moody's"),
          ratingSPR   = FUNC_convert_to_numeric(ratingSPR , agency = "S&P"))%>%
    select(req_id , request_type , ratingMR , ratingSPR , y_to_matur)
}

# Doing group_by once and then using the grouped_df is better actually
df.inquiry_broker_dealer_grouped <- list()
for (i in 1:2){
  df.inquiry_broker_dealer_grouped[[i]] <- df.inquiry_broker_dealer[[i]] %>% group_by(req_id)
}

# For saving plots and dataframes of different variables (Maturity, Moody , S&P)
PLOTS_broker_dealer    <- list()
df.stats_broker_dealer <- list()


for (j in 1:2){
  PLOTS    <- list()
  df.stats <- list()
  df.inquiry_grouped <- df.inquiry_broker_dealer_grouped[[j]] %>% filter(request_type == "List")
  
  for (i in 1:3){
  
    df <- df.inquiry_grouped[,c(1:2 , i+2)]
  
    colnames(df)[3] <- "target"
  
    # Instead of making na.rm = T for each function, we can do it here and once
    df <- df %>%
      filter(!is.na(target))
  
  
    df <- df %>%
      # create variables once instead of using them for each statistic
      summarise(request_type  = unique(request_type),
                med           = median(target          ),
                MAX           = max(target             ),
                MIN           = min(target             ),
                Q3            = quantile(target , .75  ),
                Q1            = quantile(target , .25  ))%>%
      # making data ungroup can make computations faster I guess
      ungroup()%>%
      mutate(MM            = MAX/MIN,
           Q31           = Q3 / Q1,
           MM_norm       = (MAX - MIN) /med,
           Q31_norm      = (Q3 - Q1)/med)%>%
      select(-c(med , MAX , MIN , Q3 , Q1))%>%
      pivot_longer(!c(req_id , request_type), names_to = "statistic", values_to = "value")
  
    df.stats[[i]] <- df
  
    p <- ggplot((df %>% 
                   group_by(statistic) %>% 
                   arrange(value) %>% 
                   mutate(rn = row_number(), n=n()))) + 
      geom_step(aes(x=value, y=rn/n, color=statistic))+
      scale_y_continuous(labels = scales::percent) +
      #geom_histogram(aes(y = after_stat(after_stat(count / (sum(count)/4))))
                    
      #              , alpha = 0.5
      #              # For this case, bin = 50 do better I beleive
      #              , bins =  50)+
      # Put the limit to 97.5 percentile make the figure more clear
      #facet_wrap(~statistic)
      xlim(0 , quantile(df$value , 0.99)) 
     
    
    if (j == 1){
      p <- p + ggtitle(paste(colnames(df.inquiry_grouped)[i+2], "Broker-Dealer - List", sep = " "))
    } else {
      p <- p + ggtitle(paste(colnames(df.inquiry_grouped)[i+2], "Non-Broker-Dealer - List", sep = " "))
    }

    PLOTS[[i]] <- p
  }
  PLOTS_broker_dealer[[j]] <- PLOTS
  df.stats_broker_dealer[[j]] <- df.stats
  
}

#Here I make a seperate histogram for Non-Broker-Dealer PT observations
df.inquiry_grouped <- df.inquiry_broker_dealer_grouped[[2]] %>% filter(request_type == "PT")
for (i in 1:3){
  
  df <- df.inquiry_grouped[,c(1:2 , i+2)]
  
  colnames(df)[3] <- "target"
  
  # Instead of making na.rm = T for each function, we can do it here and once
  df <- df %>%
    filter(!is.na(target))
  
  
  df <- df %>%
    # create variables once instead of using them for each statistic
    summarise(request_type  = unique(request_type),
              med           = median(target          ),
              MAX           = max(target             ),
              MIN           = min(target             ),
              Q3            = quantile(target , .75  ),
              Q1            = quantile(target , .25  ))%>%
    # making data ungroup can make computations faster I guess
    ungroup()%>%
    mutate(MM            = MAX/MIN,
           Q31           = Q3 / Q1,
           MM_norm       = (MAX - MIN) /med,
           Q31_norm      = (Q3 - Q1)/med)%>%
    select(-c(med , MAX , MIN , Q3 , Q1))%>%
    pivot_longer(!c(req_id , request_type), names_to = "statistic", values_to = "value")
  
  df.stats[[i]] <- df
  
  # Note that I used request_type for fill which make PT and List overlap
  p <- ggplot((df %>% 
                 group_by(statistic) %>% 
                 arrange(value) %>% 
                 mutate(rn = row_number(), n=n()))) + 
    geom_step(aes(x=value, y=rn/n, color=statistic))+
    scale_y_continuous(labels = scales::percent) +
    #geom_histogram(aes(y = after_stat(after_stat(count / (sum(count)/4))))
    
    #              , alpha = 0.5
    #              # For this case, bin = 50 do better I beleive
    #              , bins =  50)+
    # Put the limit to 97.5 percentile make the figure more clear
    #facet_wrap(~statistic) +
    xlim(0 , quantile(df$value , 0.975)) + 
    ggtitle(paste(colnames(df.inquiry_grouped)[i+2], "Non-Broker-Dealer - PT", sep = " "))

  
  PLOTS[[i]] <- p
}
PLOTS_broker_dealer[[3]] <- PLOTS
df.stats_broker_dealer[[3]] <- df.stats


# I guess you already know how this works
pdf(file   = "~/Desktop/github/Portfolio_Trades/Outputs_David/Figures/Maturity_Rating_centrality_histograms_5.pdf",   # The directory you want to save the file in
    width  = 8, # The width of the plot in inches
    height = 4) # The height of the plot in inches

for (i in 1:3) {
  for (j in 1:3){
    plot(PLOTS_broker_dealer[[i]][[j]])
  }
}
# Step 3: Run dev.off() to create the file!
dev.off()

table(df.stats_broker_dealer[[2]][[2]]$statistic)
