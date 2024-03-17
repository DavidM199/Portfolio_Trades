library(tidyverse)
library(purrr)
library(lfe)
library(dplyr)
df.inquiry  <- read_csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/inquiries_58_columns.csv")
write.csv(df.inquiry, "~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/inquiries_58_columns.csv", row.names = FALSE)
# 0 - Initial check START



initial_check <- df.inquiry %>% filter(request_type != "SRFQ") %>% 
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

#Excluding product_cd == "USHG" observations before moving on
df.inquiry <- df.inquiry %>% filter(product_cd != "USHG")

# 0 - Initial check END

#-------------------------------------------------------------------------------

# 1 - Create variables START


# Filtering out SRFQ inquiries and inquiries that do not belong to any sublist 
#(sublist is a non singleton group of inquiries) 
# Not including them is logical, because e.g. their min_cost_sublist is not defined

#Also included only List observations

df.inquiry <- df.inquiry %>% filter(request_type == "List") %>%  
  group_by(req_id, req_quantity) %>% 
  mutate(sublist_length=n()) %>% 
  filter(sublist_length > 1) %>% ungroup()

#date, pt, filled, trans_cost, min_cost_sublist, mediancost_insublist
df.inquiry <- df.inquiry %>% mutate(date = as.Date(submittime),
                                    pt = ifelse(request_type == "PT",1,0),
                                    filled = if_else(is.na(trade_quantity) | (trade_quantity < req_quantity*0.5), 0 , 1 , NA),
                                    trans_cost = spread)

df.inquiry <- df.inquiry %>% group_by(req_id, req_quantity) %>%
  mutate(mincost_insublist = map_dbl(row_number(), ~min(trans_cost[-.x], na.rm = TRUE)),
         mediancost_insublist = map_dbl(row_number(), ~median(trans_cost[-.x], na.rm = TRUE))) %>% ungroup()

#There were some non existent spread variables, therefore there were 16,247 warnings for infinite values
#df.inquiry <- df.inquiry[is.finite(df.inquiry$mincost_insublist), ]
df.inquiry$mincost_insublist[is.infinite(df.inquiry$mincost_insublist)] <- NA

#df.inquiry <- df.inquiry[is.finite(df.inquiry$mediancost_insublist), ]
df.inquiry$mediancost_insublist[is.infinite(df.inquiry$mediancost_insublist)] <- NA

#list_length, numsublists
df.inquiry <- df.inquiry %>% group_by(req_id) %>% mutate(list_length = n()) %>% ungroup()

numsublists <- df.inquiry %>% select(req_id, req_quantity) %>% group_by(req_id, req_quantity) %>% 
                            summarise(num=n(), .groups = 'keep') %>% 
                            group_by(req_id) %>% 
                            summarise(numsublists = n()) 

df.inquiry <- df.inquiry %>% left_join(numsublists, by="req_id")
                            
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

#mincost_outsidesublist, mediancost_outsidesublist
FUNC_outsidelist_reg <- function(df.inquiry){
  
  cost_outsidesublist <- df.inquiry %>% group_by(req_id, req_quantity) %>%
    summarise(min_cost = min(trans_cost, na.rm = TRUE),
              median_cost = median(trans_cost, na.rm = TRUE)) %>% 
    group_by(req_id) %>% 
    mutate(
      mincost_outsidesublist = map_dbl(row_number(), function(x) {
        
        vec_to_sample <- min_cost[-x]
        if (length(vec_to_sample) == 1){
          vec_to_sample[1]
        }
        else if (length(vec_to_sample) > 0) {
          sample(vec_to_sample, size = 1)
        } else {
          NA_real_ 
        }
        
      }),
      mediancost_outsidesublist = map_dbl(row_number(),function(x) {
        
        vec_to_sample <- median_cost[-x]
        if (length(vec_to_sample) == 1){
          vec_to_sample[1]
        } else if (length(vec_to_sample) > 0) {
          sample(vec_to_sample, size = 1)
        } else {
          NA_real_ 
        }
      })) %>% select(-min_cost, -median_cost)
  
  cost_outsidesublist$mincost_outsidesublist[is.infinite(cost_outsidesublist$mincost_outsidesublist)] <- NA
  cost_outsidesublist$mediancost_outsidesublist[is.infinite(cost_outsidesublist$mediancost_outsidesublist)] <- NA
  
  df.inquiry <- df.inquiry %>% left_join(cost_outsidesublist, by=c("req_id", "req_quantity"))
  
  return(df.inquiry)
  
}

list.df.inquiry<- lapply(1:5, function(x) FUNC_outsidelist_reg(df.inquiry))

#checking if all the infinite values have been eliminated
for (df in list.df.inquiry){
print(sum(is.infinite(df$mincost_outsidesublist)))

print(sum(is.infinite(df$mediancost_outsidesublist)))
}


# 1 - Create variables END


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

# 2 - Regressions START

#extracting relevant subset of the data
SubsetFunction <- function(df) {
            subset <- df %>% filter(pt == 0, 
                                  product_cd == "USHY", 
                                  p_type != "Broker-Dealer", 
                                  list_length >= 20, 
                                  numsublists < list_length/2, 
                                  5 < sublist_length)
            return(subset)
}
list.subset <- lapply(list.df.inquiry, SubsetFunction)


#creating a regression-running functions
#NOTE: Fixed effect at req_id level cluster standard errors at the level of (req_id and date)

#regression1: filled ~ trans_cost mediancost_insublist mediancost_outsidesublist
Regression1 <-  function(df) {
      
      regr1 <- felm(filled ~ trans_cost + mediancost_insublist + mediancost_outsidesublist | req_id | 0 | req_id + date, data = df)
      summary_regr1 <- summary(regr1, cluster = c("req_id", "date"))
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

      regr2 <- felm(filled ~ trans_cost + mincost_insublist + mincost_outsidesublist | req_id | 0 | req_id + date, data = df)
      summary_regr2 <- summary(regr2, cluster = c("req_id", "date"))
      df.coefficients <- data.frame(summary_regr2$coefficients)
      
      for (col_name in colnames(df.coefficients)) {
        
        df.coefficients[[col_name]] <- formatC(df.coefficients[[col_name]], format = "f", digits = 4)
        
      }
      
      colnames(df.coefficients) <-  c("Estimate","Cluster.s.e.","t.value","pval")
      
      return(df.coefficients)
}


# 2 - Regressions END


#Making the report Presentable
library(gridExtra)
library(grid)

summary1.list <- lapply(list.subset, Regression1)
summary2.list <-  lapply(list.subset, Regression2)


summary1.list <- lapply(summary1.list, tableGrob) 
summary2.list <- lapply(summary2.list, tableGrob)

pdf("~/Desktop/github/Portfolio_Trades/Outputs_David/Figures/regression_results_summary.pdf", 
    width = 8,
    height = 10)

title1 <- textGrob("Regression 1", gp = gpar(fontsize = 20, fontface = "bold"), vjust = 1)
grid.arrange(
  grobs = summary1.list[1:5], 
  ncol = 1,
  top = title1
)

title2 <- textGrob("Regression 2", gp = gpar(fontsize = 20, fontface = "bold"), vjust = 1)
grid.arrange(
  grobs = summary2.list[1:5], 
  ncol = 1,
  top = title2
)
dev.off()

