library(tidyverse)
library(ggplot2)
library(lubridate)
library(haven)
library(patchwork)


#Reading data-------------------------------------------------------------------
df.inquiry  <- read_csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/inquiries_58_columns.csv")
df.bondFacts <- read_csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/FISD/FISD_issue_our_cusips.csv")
df.bondRatings <- read_dta("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/FISD/bondRating_Hamed_filtered.dta")

df.inquiry <- df.inquiry %>%
  select(submittime , req_id , request_type , cusip , number_assets, p_type)%>%
  filter(number_assets >= 10)%>%
  mutate(date = as.Date(submittime))%>%
  select(-submittime)

#Creating the subsets of data that we will be working with----------------------
df.inquiry_broker_dealer <- list(df.inquiry, filter(df.inquiry, p_type == "Broker-Dealer"), filter(df.inquiry, p_type != "Broker-Dealer"))

df.bondFacts <- df.bondFacts %>% rename(cusip = COMPLETE_CUSIP)
df.bondFacts <- df.bondFacts %>% select(cusip , MATURITY, ISSUER_ID)

df.bondRatings <- df.bondRatings %>%
  select(cusip , rating_date , ratingMR , ratingSPR)

gc()

# closest is an option in join_by which let you choose the closest date which is smaller than something
for (i in 1:3){
  df.inquiry_broker_dealer[[i]] <- left_join(df.inquiry_broker_dealer[[i]]  , df.bondFacts   , by = join_by(cusip))
  df.inquiry_broker_dealer[[i]] <- left_join(df.inquiry_broker_dealer[[i]] , df.bondRatings , by = join_by(cusip , closest(date < rating_date)))
}

#TESTING the rank creation -----------------------------------------------------
df <- data.frame(
  req_id = c(1, 1, 1, 2, 2, 3, 3, 3, 3),
  ISSUER_ID = c("A", "A", "B", "A", "C", "B", "B", "C", "C")
)

for (i in 1:3){
  rdf <- df %>%
  group_by(req_id, ISSUER_ID) %>%
  summarise(count = n(), .groups = "drop") %>% group_by(req_id) %>% 
  mutate(rank = rank(-count, ties.method = "random")) %>% ungroup() %>% 
  select(req_id, ISSUER_ID, rank) %>% right_join(df, by = c("req_id"= "req_id", "ISSUER_ID"="ISSUER_ID"))
}

#creating rankings for the inquiry df-------------------------------------------
ranked_df.inquiry <- list()
for (i in 1:3){
  ranked_df.inquiry[[i]] <- df.inquiry_broker_dealer[[i]] %>%
    group_by(req_id, ISSUER_ID) %>%
    summarise(count = n(), .groups = "drop") %>% group_by(req_id) %>% 
    mutate(rank = rank(-count, ties.method = "random")) %>% ungroup() %>% 
    select(req_id, ISSUER_ID, rank) %>% right_join(df.inquiry_broker_dealer[[i]], by = c("req_id"= "req_id", "ISSUER_ID"="ISSUER_ID"))
}
#grouping the dataframes before the calculations
ranked_df.inquiry_grouped <- list()
for (i in 1:3){
  ranked_df.inquiry_grouped[[i]] <- ranked_df.inquiry[[i]] %>% select(req_id, request_type, ISSUER_ID, p_type, rank) %>% group_by(req_id)
}

# For saving plots and dataframes of different variables (Maturity, Moody , S&P)--------------
PLOTS_broker_dealer    <- list()
df.stats_broker_dealer <- list()


for (j in 1:3){
  PLOTS    <- list()
  df.stats <- list()
  df.inquiry_grouped <- ranked_df.inquiry_grouped[[j]]
  
  #for (i in 1:3){
    
    
    #df <- df.inquiry_grouped %>% select(req_id, request_type, ISSUER_ID, p_type, rank)
    
    #colnames(df)[3] <- "target"
    
    # Instead of making na.rm = T for each function, we can do it here and once
    df <- df.inquiry_grouped
    df <- df %>% filter(!is.na(rank))
    
    
    df <- df %>%
      # create variables once instead of using them for each statistic
      summarise(request_type  = unique(request_type),
                max_ranking = max(rank),
                q_75 = quantile(rank, 0.75),
                median_ranking = median(rank) 
                )%>%
      pivot_longer(!c(req_id , request_type), names_to = "statistic", values_to = "value")
    
    df.stats[[j]] <- df
    

    p <-
      #STEP CODE----------------------------------------------------------------
      #ggplot((df %>% 
        #           group_by(statistic) %>% 
        #           arrange(value) %>% 
        #           mutate(rn = row_number(), n=n()))) + 
      #geom_step(aes(x=value, y=rn/n, color=statistic))+
      #facet_wrap(~request_type)+
      
      #Histogram code-----------------------------------------------------------
      ggplot(df, aes(x = value, fill = request_type)) +
      geom_histogram(aes(y = after_stat(density)), binwidth = 1, alpha = 0.5) +
      labs(y = "Probability", x = "Value") +
      facet_wrap(~statistic)+
      
      scale_y_continuous(labels = scales::percent)
      
    if (j==1 | j == 3){
      p <- p + xlim(0, 200)
    }
    
    
    if (j == 1){
      p <- p + ggtitle(paste(colnames(df.inquiry_grouped)[i+2], "Aggregate", sep = " "))
    }
    else if (j == 2){
      p <- p + ggtitle(paste(colnames(df.inquiry_grouped)[i+2], "Broker-Dealer", sep = " "))
    }  else if(j == 3){
      p <- p + ggtitle(paste(colnames(df.inquiry_grouped)[i+2], "Non-Broker-Dealer", sep = " "))
    }
    
    
  #}
  PLOTS_broker_dealer[[j]] <- p
  df.stats_broker_dealer[[j]] <- df.stats
}




pdf(file   = "~/Desktop/github/Portfolio_Trades/Outputs_David/Figures/Issuer_id_concetration_histogram.pdf",   # The directory you want to save the file in
    width  = 8, # The width of the plot in inches
    height = 4) # The height of the plot in inches

for (i in 1:3) {
    plot(PLOTS_broker_dealer[[i]])
}
dev.off()






