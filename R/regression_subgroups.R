library(tidyverse)
library(purrr)
library(lfe)

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

# What percentage of List requests have this property?
initial_check_list <- initial_check %>% filter(request_type=="List")
sum(initial_check_list$num_product_cd>1)/length(initial_check_list$num_product_cd) * 100
# 4.116 % 

# 0 - Initial check END

#-------------------------------------------------------------------------------

# 1 - Create variables START


# Filtering out SRFQ inquiries and inquiries that do not belong to any sublist 
#(sublist is a non singleton group of inquiries) 
# Not including them is logical, because e.g. their min_cost_sublist is not defined

#Also included only PT observations

df.inquiry <- df.inquiry %>% filter(request_type == "List") %>%  
  group_by(req_id, req_quantity) %>% 
  mutate(sublist_length=n()) %>% 
  filter(sublist_length > 1) %>% ungroup()

#date, pt, filled, trans_cost, min_cost_sublist, mediancost_insublist
df.inquiry <- df.inquiry %>% mutate(date = as.Date(submittime),
                                    pt = ifelse(request_type == "PT",1,0),
                                    filled = ifelse(is.na(trade_quantity < req_quantity*0.5) | trade_quantity < req_quantity*0.5, 0,1),
                                    trans_cost = spread)

df.inquiry <- df.inquiry %>% group_by(req_id, req_quantity) %>%
  mutate(mincost_insublist = map_dbl(row_number(), ~min(trans_cost[-.x], na.rm = TRUE)),
         mediancost_insublist = map_dbl(row_number(), ~median(trans_cost[-.x], na.rm = TRUE))) %>% ungroup()

#There were some non existent spread variables, therefore there were 37654 warnings for infinite values
df.inquiry <- df.inquiry[is.finite(df.inquiry$min_cost_sublist), ]
df.inquiry <- df.inquiry[is.finite(df.inquiry$mediancost_insublist), ]

#list_length, numsublists
df.inquiry <- df.inquiry %>% group_by(req_id) %>% mutate(list_length = n()) %>% ungroup()

numsublists <- df.inquiry %>% select(req_id, req_quantity) %>% group_by(req_id, req_quantity) %>% 
                            summarise(num=n(), .groups = 'keep') %>% 
                            group_by(req_id) %>% 
                            summarise(numsublists = n()) 

df.inquiry <- df.inquiry %>% left_join(numsublists, by="req_id")
                            
                         


#mincost_outsidesublist, mediancost_outsidesublist
cost_outsidesublist <- df.inquiry %>% group_by(req_id, req_quantity) %>%
                                      summarise(min_cost = sample(min_cost_sublist, size=1),
                                                median_cost = sample(mediancost_insublist, size=1)) %>% 
                                      group_by(req_id) %>% 
                                      mutate(
                                        mincost_outsidesublist = map_dbl(row_number(), function(x) {
                                        
                                        vec_to_sample <- min_cost[-x]
                                        if (length(vec_to_sample) > 0) {
                                          sample(vec_to_sample, size = 1)
                                        } else {
                                          NA_real_ 
                                        }
                                        
                                      }),
                                      mediancost_outsidesublist = map_dbl(row_number(),function(x) {
                                        
                                        vec_to_sample <- median_cost[-x]
                                        if (length(vec_to_sample) > 0) {
                                          sample(vec_to_sample, size = 1)
                                        } else {
                                          NA_real_ 
                                        }
                                      })) %>% select(-min_cost, -median_cost)


df.inquiry <- df.inquiry %>% left_join(cost_outsidesublist, by=c("req_id", "req_quantity"))


# 1 - Create variables END


#Testing ----- map_dbl(row_number(), function(x){...})
names <- c("Product A")
prices <- c(10.99)

test <- data.frame(name = names, price = prices)

test <- test %>% mutate(min_cost_ex =  map_dbl(row_number(),function(x) {
  vec_to_sample <- price[-x]
  if (length(vec_to_sample) > 0) {
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

coefs_grob <- tableGrob(coefs)
errors_grob <- tableGrob(errors)
pvals_grob <- tableGrob(pvals)

pdf("~/Desktop/github/Portfolio_Trades/Outputs_David/Figures/regression_results.pdf", 
    width = 8.5,
    height = 11)
grid.arrange(coefs_grob, errors_grob, pvals_grob, ncol = 1)

dev.off()







