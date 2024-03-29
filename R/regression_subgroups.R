library(tidyverse)
library(purrr)
library(lfe)
library(dplyr)
library(DescTools)

#Making the report Presentable
library(gridExtra)
library(grid)


# Reading Data ------------------------------------------------------------


df.inquiry  <- read_csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/inquiries_58_columns.csv")
# write.csv(df.inquiry, "~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/inquiries_58_columns.csv", row.names = FALSE)


# FUNCTIONS ---------------------------------------------------------------

#mincost_outsidesublist, mediancost_outsidesublist
FUNC_subset_reg <- function(df){
  
  cost_outsidesublist <- df %>%
    group_by(req_id, req_quantity, sublist_length) %>%
    summarise(min_cost = min(trans_cost, na.rm = TRUE),
              median_cost = median(trans_cost, na.rm = TRUE)) %>% 
    group_by(req_id) %>% 
    mutate(
      mincost_outsidesublist = map_dbl(row_number(), function(x) {
        
        if (length(min_cost[-x]) > 0){
          indices <- which(sublist_length[-x] == max(sublist_length[-x]))
          
          # which.max doesn't randomly choose a max. It chooses the first one.
          # We use this because we want it to be random
          if (length(indices) > 1){
            ret <- min_cost[sample(indices , 1)]
          } else {
            ret <- min_cost[indices]
          }
          
        } else {
          ret <- NA_real_
        }
        
        return(ret)
        
      }),
      mediancost_outsidesublist = map_dbl(row_number(),function(x) {
        
        if (length(median_cost[-x]) > 0){
          
          indices <- which(sublist_length[-x] == max(sublist_length[-x]))
          
          # which.max doesn't randomly choose a max. It chooses the first one.
          # We use this because we want it to be random
          if (length(indices) > 1){
            ret <- median_cost[sample(indices , 1)]
          } else {
            ret <- median_cost[indices]
          }
          
        } else {
            ret <- NA_real_
        }
        
        return(ret)
        
      })) %>%
    select(-min_cost, -median_cost, -sublist_length) %>% 
    mutate(mincost_outsidesublist = if_else(is.infinite(mincost_outsidesublist) , NA , mincost_outsidesublist),
           mediancost_outsidesublist = if_else(is.infinite(mediancost_outsidesublist) , NA , mediancost_outsidesublist))
  
  df <- df %>%
    left_join(cost_outsidesublist, by=c("req_id", "req_quantity"))
  
  return(df)
}

# min/mediancost_outsidesublist redefined ---------------------------------

FUNC_subset_reg2 <- function(df){
  
  cost_outsidesublist <- df %>%
    group_by(req_id) %>%
    mutate(
      mincost_outsidesublist = map_dbl(row_number(), function(x) {
        
        cur_req_quantity <- req_quantity[x]
        
        indices <- which(cur_req_quantity != req_quantity)
        
        ret <- min(trans_cost[indices], na.rm = TRUE)
        
        return(ret)
        
      }),
      mediancost_outsidesublist = map_dbl(row_number(),function(x) {
        
        cur_req_quantity <- req_quantity[x]
        
        indices <- which(cur_req_quantity != req_quantity)
        
        ret <- median(trans_cost[indices], na.rm = TRUE)
        
        return(ret)
        
      })) %>% ungroup() %>% 
              mutate(mincost_outsidesublist = if_else(is.infinite(mincost_outsidesublist) , NA , mincost_outsidesublist),
                    mediancost_outsidesublist = if_else(is.infinite(mediancost_outsidesublist) , NA , mediancost_outsidesublist))

  
  return(cost_outsidesublist)
}

#creating a regression-running functions
#NOTE: Fixed effect at req_id level cluster standard errors at the level of (req_id)

Regression1 <-  function(df) {
  
  regr1 <- felm(filled ~ trans_cost + mediancost_insublist + mediancost_outsidesublist | req_id | 0 | req_id, data = df)
  summary_regr1 <- summary(regr1, cluster = c("req_id"))
  df.coefficients <- data.frame(summary_regr1$coefficients)
  
  for (col_name in colnames(df.coefficients)) {
    if (col_name == "Pr...t..") {
      df.coefficients[[col_name]] <- formatC(df.coefficients[[col_name]], format = "g", digits = 3)
    }else {
      df.coefficients[[col_name]] <- formatC(df.coefficients[[col_name]], format = "f", digits = 4)
    } 
  }
  
  colnames(df.coefficients) <-  c("Estimate","Cluster.s.e.","t.value","pval")
  
  return(df.coefficients)
}
#regression2: filled ~ trans_cost mincost_insublist mincost_outsidesublist
Regression2 <- function(df){
  
  regr2 <- felm(filled ~ trans_cost + mincost_insublist + mincost_outsidesublist | req_id | 0 | req_id, data = df)
  summary_regr2 <- summary(regr2, cluster = c("req_id"))
  df.coefficients <- data.frame(summary_regr2$coefficients)
  
  for (col_name in colnames(df.coefficients)) {
    if (col_name == "Pr...t..") {
      df.coefficients[[col_name]] <- formatC(df.coefficients[[col_name]], format = "g", digits = 3)
    }else {
      df.coefficients[[col_name]] <- formatC(df.coefficients[[col_name]], format = "f", digits = 4)
    }
  }
  
  colnames(df.coefficients) <-  c("Estimate","Cluster.s.e.","t.value","pval")
  
  return(df.coefficients)
}


#creating winsorize function to be used with lapply
winsorize_trans_cost <- function(df){
    df$trans_cost <- Winsorize(df$trans_cost, minval = quantile(df$trans_cost, 0.005, na.rm = TRUE), maxval = quantile(df$trans_cost, 0.995, na.rm=TRUE), na.rm = TRUE)
    return(df)
    }




# DATA Clearning ----------------------------------------------------------



# 1 - Create variables START


# Excluding product_cd == "USHG" observations before moving on
# only List observations
# Filtering out SRFQ inquiries and inquiries that do not belong to any sublist 
# (sublist is a non singleton group of inquiries) 
# Not including them is logical, because e.g. their min_cost_sublist is not defined
# list_length calculation before removing the sublists with only 1 inquiry


df.inquiry <- df.inquiry %>%
  select(product_cd , p_type , request_type , req_id , req_quantity , trade_quantity , spread)%>%
  
  filter(request_type == "List" & product_cd == "USHY" & p_type != "Broker-Dealer") %>% 
  
  # filled
  mutate(filled     = if_else(trade_quantity < (0.5*req_quantity) , 0 , 1 , 0),
         trans_cost = spread)%>%
  
  # list_length
  group_by(req_id) %>%
  mutate(list_length = n())%>%
  
  # sublist_length
  group_by(req_id, req_quantity) %>% 
  mutate(sublist_length=n()) %>% 
  filter(sublist_length > 1) %>%
  
  # mincost and mediancost insublist
  group_by(req_id, req_quantity) %>%
  mutate(mincost_insublist    = map_dbl(row_number(), ~min(trans_cost[-.x]   , na.rm = TRUE)),
         mediancost_insublist = map_dbl(row_number(), ~median(trans_cost[-.x], na.rm = TRUE)))%>%
  ungroup()%>%
  mutate(mincost_insublist = if_else(is.infinite(mincost_insublist) , NA , mincost_insublist))%>%
  
  #numsublists
  group_by(req_id) %>% 
  mutate(numsublists = n_distinct(req_quantity))%>%
  ungroup() %>%
  
  filter(list_length >= 20,
         numsublists < list_length/2,
         5 < sublist_length)




subset_largestsublist <- FUNC_subset_reg(df.inquiry)
subset_allothersublist <- FUNC_subset_reg2(df.inquiry)

#writing csvs for RegressionSummary.Rmd
write.csv(subset_largestsublist, "~/Desktop/Portfolio_Trades_my_computer/data_minimizing/regression20.03/subset_largestsublist.csv", row.names = FALSE)
write.csv(subset_allothersublist, "~/Desktop/Portfolio_Trades_my_computer/data_minimizing/regression20.03/subset_allothersublist.csv", row.names = FALSE)
write.csv(df.inquiry, "~/Desktop/Portfolio_Trades_my_computer/data_minimizing/regression20.03/trans_cost_calc.csv", row.names = FALSE)

#winsorizing trans_cost
subset_largestsublist <- winsorize_trans_cost(subset_largestsublist)
subset_allothersublist <- winsorize_trans_cost(subset_allothersublist)


# REGRESSIONS -------------------------------------------------------------


summary1_largestsublist <- Regression1(subset_largestsublist)
summary2_largestsublist <-  Regression2(subset_largestsublist)

summary1_largestsublist
summary2_largestsublist

summary1_allothersublist <-  Regression1(subset_allothersublist)
summary2_allothersublist <-  Regression2(subset_allothersublist)

summary1_allothersublist
summary2_allothersublist

# REPORTING ---------------------------------------------------------------


grob.summary1 <- lapply(list.summary1, tableGrob)
grob.summary2 <- lapply(list.summary2, tableGrob)

pdf("~/Desktop/github/Portfolio_Trades/Outputs_David/Figures/regression_results_summary.pdf", 
    width = 8,
    height = 10)

title1 <- textGrob("Regression 1", gp = gpar(fontsize = 20, fontface = "bold"), vjust = 1)
grid.arrange(
  grobs= grob.summary1[1:5], 
  ncol = 1,
  top = title1
)

title2 <- textGrob("Regression 2", gp = gpar(fontsize = 20, fontface = "bold"), vjust = 1)
grid.arrange(
  grobs = grob.summary2[1:5],
  ncol = 1,
  top = title2
)

dev.off()


pdf("~/Desktop/github/Portfolio_Trades/Outputs_David/Figures/regression_results_summary.pdf", 
    width = 8,
    height = 6)

title1 <- textGrob("Regression 1 and 2", gp = gpar(fontsize = 20, fontface = "bold"), vjust = 1)
grid.arrange(
  grob.summary1[[1]], grob.summary2[[1]],  
  ncol = 1,
  top = title1
)
dev.off()


hist <- ggplot(list.subset[[1]], aes(x=trans_cost)) + geom_histogram(binwidth = 0.0025) +  xlim(-0.03, 0.1)
hist


# INITIAL CHECK -----------------------------------------------------------

# 0 - Initial check START



initial_check <- df.inquiry %>%
  filter(request_type != "SRFQ") %>% 
  select(req_id, product_cd, request_type) %>% 
  group_by(req_id, request_type) %>%
  summarise(num_product_cd = n_distinct(product_cd))

sum(initial_check$num_product_cd==1)
# The result is 306084
sum(initial_check$num_product_cd>1)
# The result is 13649
length(initial_check$num_product_cd)
# What percentage of List requests have this property?
initial_check_list <- initial_check %>% filter(request_type=="List")

sum(initial_check_list$num_product_cd>1)
# 13088 List requests 
sum(initial_check_list$num_product_cd>1)/length(initial_check_list$num_product_cd)*100
# 4.116 % of all List requests

initial_check_list_inquiries <- df.inquiry %>% filter(request_type == "List") %>% 
  select(req_id, product_cd, request_type) %>% 
  group_by(req_id, request_type) %>% 
  mutate(num_product_cd = n_distinct(product_cd)) %>% ungroup()

sum(initial_check_list_inquiries$num_product_cd>1)
# 217503 individual List inquiries have this property
sum(initial_check_list_inquiries$num_product_cd>1)/length(initial_check_list_inquiries$num_product_cd)*100
# 5.078369 % of all List inquiries have this property


df.inquiry <- df.inquiry %>% filter(product_cd != "USHG")

# 0 - Initial check END


# TESTS -------------------------------------------------------------------



cost_outsidesublist$mincost_outsidesublist[is.infinite(cost_outsidesublist$mincost_outsidesublist)] <- NA
cost_outsidesublist$mediancost_outsidesublist[is.infinite(cost_outsidesublist$mediancost_outsidesublist)] <- NA


#There were some non existent spread variables, therefore there were 37,653 warnings for infinite values
#df.inquiry <- df.inquiry[is.finite(df.inquiry$mincost_insublist), ]
df.inquiry$mincost_insublist[is.infinite(df.inquiry$mincost_insublist)] <- NA

#df.inquiry <- df.inquiry[is.finite(df.inquiry$mediancost_insublist), ]
df.inquiry$mediancost_insublist[is.infinite(df.inquiry$mediancost_insublist)] <- NA


#test How many subgroups have only 2 inquiries                       
test  <- df.inquiry %>% group_by(req_id, req_quantity) %>%
  summarise(min_cost = min(mincost_insublist, na.rm = TRUE),
            median_cost = median(mediancost_insublist, na.rm = TRUE)) %>% 
  group_by(req_id)  %>% 
  mutate(
    mincost_outsidesublist = map_dbl(row_number(), function(x) {
      
      vec_to_sample <- min_cost[-x]
      if (length(vec_to_sample) == 1){
        1
      }
      else {
        0
      }
    }),
    mediancost_outsidesublist = map_dbl(row_number(),function(x) {
      
      vec_to_sample <- median_cost[-x]
      if (length(vec_to_sample) == 1){
        1
      } else {
        0 
      }}))
test$mincost_outsidesublist[is.infinite(test$mincost_outsidesublist)] <- NA
test$mediancost_outsidesublist[is.infinite(test$mediancost_outsidesublist)] <- NA
sum(test$mincost_outsidesublist)
sum(test$mediancost_outsidesublist)

length(test$mediancost_outsidesublist)


#Testing ----- map_dbl(row_number(), function(x){...})
names <- c("A", "B")
prices <- c(10.99, 7.99)

test <- data.frame(name = names, price = prices)

test <- test %>% mutate(min_cost_ex =  map_dbl(row_number(),function(x) {
  vec_to_sample <- price[-x]
  if (length(vec_to_sample) == 1){
    vec_to_sample[1]
  }
  else if (length(vec_to_sample) > 0) {
    sample(vec_to_sample, size = 1)
  } else {
    NA_real_ 
  }
}))
rm(names, prices, test)
