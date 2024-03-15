library(tidyverse)
library(purrr)
library(lfe)
library(dplyr)
df.inquiry  <- read_csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/inquiries_58_columns.csv")

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

#There were some non existent spread variables, therefore there were 37,653 warnings for infinite values
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
cost_outsidesublist <- df.inquiry %>% group_by(req_id, req_quantity) %>%
                                      summarise(min_cost = min(mincost_insublist, na.rm = TRUE),
                                                median_cost = median(mediancost_insublist, na.rm = TRUE)) %>% 
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
sum(is.infinite(cost_outsidesublist$mincost_outsidesublist))
cost_outsidesublist$mediancost_outsidesublist[is.infinite(cost_outsidesublist$mediancost_outsidesublist)] <- NA
sum(is.infinite(cost_outsidesublist$mediancost_outsidesublist))

df.inquiry <- df.inquiry %>% left_join(cost_outsidesublist, by=c("req_id", "req_quantity"))


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
subset <- df.inquiry %>% filter(pt == 0, 
                                product_cd == "USHY", 
                                p_type != "Broker-Dealer", 
                                list_length >= 20, 
                                numsublists < list_length/2, 
                                5 < sublist_length)


#NOTE: Fixed effect at req_id level cluster standard errors at the level of (req_id and date)

#regression1: filled ~ trans_cost mediancost_insublist mediancost_outsidesublist
regr1 <- felm(filled ~ trans_cost + mediancost_insublist + mediancost_outsidesublist | req_id | 0 | req_id + date, data = subset)


#regression2: filled ~ trans_cost mincost_insublist mincost_outsidesublist
regr2 <- felm(filled ~ trans_cost + mincost_insublist + mincost_outsidesublist | req_id | 0 | req_id + date, data = subset)


summary_regr1 <- summary(regr1, cluster = c("req_id", "date"))
summary_regr2 <- summary(regr2, cluster = c("req_id", "date"))

# Extract coefficients, standard errors, and p-values
coefs_model1 <- summary_regr1$coefficients[, "Estimate"]
coefs_model2 <- summary_regr2$coefficients[, "Estimate"]

errors_model1 <- summary_regr1$coefficients[, "Cluster s.e."]
errors_model2 <- summary_regr2$coefficients[, "Cluster s.e."]

pval_model1 <- summary_regr1$coefficients[, "Pr(>|t|)"]
pval_model2 <- summary_regr2$coefficients[, "Pr(>|t|)"]

# Combine into data frames
coefs <- data.frame(Coefficients_Model1 = coefs_model1, Coefficients_Model2 = coefs_model2)
errors <- data.frame(SE_Model1 = errors_model1, SE_Model2 = errors_model2)
pvals <- data.frame(Pval_Model1 = pval_model1, Pval_Model2 = pval_model2)


# 2 - Regressions END

library(gridExtra)
library(grid)

coefs_model1 <- tableGrob(data.frame(coef_regr1=coefs_model1)) 
coefs_model2  <- tableGrob(data.frame(coef_regr2=coefs_model2)) 
errors_model1 <- tableGrob(data.frame(errors_regr1=errors_model1)) 
errors_model2 <- tableGrob(data.frame(errors_regr2=errors_model2)) 
pval_model1  <- tableGrob(data.frame(pval_regr1=pval_model1)) 
pval_model2  <- tableGrob(data.frame(pval_regr2=pval_model2)) 

pdf("~/Desktop/github/Portfolio_Trades/Outputs_David/Figures/regression_results.pdf", 
    width = 8.5,
    height = 11)
grid.arrange(coefs_model1, coefs_model2, 
             errors_model1, errors_model2,
             pval_model1, pval_model2, 
             ncol = 2)

dev.off()

#with summary only in a more organized manner

summary1 <- data.frame(summary_regr1$coefficients )
summary2 <-  data.frame(summary_regr2$coefficients)


print(colnames(summary1))
for (col_name in colnames(summary1)) {
   if (col_name == "Pr...t..") {
     summary1[[col_name]] <- formatC(summary1[[col_name]], format = "g", digits = 3)
  }else {
    summary1[[col_name]] <- formatC(summary1[[col_name]], format = "f", digits = 4)
  } 
}
for (col_name in colnames(summary2)) {

    summary2[[col_name]] <- formatC(summary2[[col_name]], format = "f", digits = 4)
  
}

colnames(summary1) <-  c("Estimate","Cluster.s.e.","t.value","pval")
colnames(summary2) <-  c("Estimate","Cluster.s.e.","t.value","pval")

summary1 <- tableGrob(summary1)
summary2 <- tableGrob(summary2)

pdf("~/Desktop/github/Portfolio_Trades/Outputs_David/Figures/regression_results_summary.pdf", 
    width = 11,
    height = 8)
grid.arrange(summary1, summary2,
             ncol = 1)

dev.off()

