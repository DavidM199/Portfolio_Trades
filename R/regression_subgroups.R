library(tidyverse)
library(purrr)
library(lfe)
library(dplyr)

#Making the report Presentable
library(gridExtra)
library(grid)


# Reading Data ------------------------------------------------------------


df.inquiry  <- read_csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/inquiries_58_columns.csv")
write.csv(df.inquiry, "~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/inquiries_58_columns.csv", row.names = FALSE)

df.inquiry  <- haven::read_dta("../Data/RFQ/inquiry_proccessed.dta")

# FUNCTIONS ---------------------------------------------------------------

#mincost_outsidesublist, mediancost_outsidesublist
FUNC_subset_reg <- function(df.inquiry){
  
  cost_outsidesublist <- df.inquiry %>% group_by(req_id, req_quantity, sublist_length) %>%
    summarise(min_cost = min(trans_cost, na.rm = TRUE),
              median_cost = median(trans_cost, na.rm = TRUE)) %>% 
    group_by(req_id) %>% 
    mutate(
      mincost_outsidesublist = map_dbl(row_number(), function(x) {
        
        if (length(min_cost[-x]) > 0){
          min_cost[which.max(sublist_length[-x])]
        } else {
          NA_real_
        }
        
      }),
      mediancost_outsidesublist = map_dbl(row_number(),function(x) {
        
        if (length(median_cost[-x]) > 0){
          median_cost[which.max(sublist_length[-x])]
        } else {
          NA_real_
        }
        
      })) %>% select(-min_cost, -median_cost, -sublist_length)
  
  cost_outsidesublist$mincost_outsidesublist[is.infinite(cost_outsidesublist$mincost_outsidesublist)] <- NA
  cost_outsidesublist$mediancost_outsidesublist[is.infinite(cost_outsidesublist$mediancost_outsidesublist)] <- NA
  
  df.inquiry <- df.inquiry %>% left_join(cost_outsidesublist, by=c("req_id", "req_quantity"))
  
  subset <- df.inquiry %>%
    filter(pt == 0,
           product_cd == "USHY",
           p_type != "Broker-Dealer",
           list_length >= 20,
           numsublists < list_length/2,
           5 < sublist_length)
  
  return(subset)
  
}
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
    
    df.coefficients[[col_name]] <- formatC(df.coefficients[[col_name]], format = "f", digits = 4)
    
  }
  
  colnames(df.coefficients) <-  c("Estimate","Cluster.s.e.","t.value","pval")
  
  return(df.coefficients)
}




# DATA Clearning ----------------------------------------------------------



# 1 - Create variables START


# Excluding product_cd == "USHG" observations before moving on
# only List observations
# Filtering out SRFQ inquiries and inquiries that do not belong to any sublist 
# (sublist is a non singleton group of inquiries) 
# Not including them is logical, because e.g. their min_cost_sublist is not defined
# list_lenght calculation before removing the sublists with only 1 inquiry


df.inquiry <- df.inquiry %>%
  select(product_cd , request_type , req_id , req_quantity , trade_quantity , spread)%>%
  
  filter(request_type == "List" & filter(product_cd == "USHY")) %>% 
  
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
  
  #numsublists
  group_by(req_id) %>% 
  mutate(numsublists = n_distinct(req_quantity))%>%
  ungroup()



list.subset   <- lapply(1:5         , function(df)     FUNC_subset_reg(df))
list.summary1 <- lapply(list.subset , function(subset) Regression1(subset))
list.summary2 <- lapply(list.subset , function(subset) Regression2(subset))







merged.df.inquiry<- FUNC_outsidelist_reg(df.inquiry)

#checking if all the infinite values have been eliminated

print(sum(is.infinite(merged.df.inquiry$mincost_outsidesublist)))

print(sum(is.infinite(merged.df.inquiry$mediancost_outsidesublist)))



# 1 - Create variables END






#creating a regression-running functions
#NOTE: Fixed effect at req_id level cluster standard errors at the level of (req_id and date)

#regression1: filled ~ trans_cost mediancost_insublist mediancost_outsidesublist
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
        
        df.coefficients[[col_name]] <- formatC(df.coefficients[[col_name]], format = "f", digits = 4)
        
      }
      
      colnames(df.coefficients) <-  c("Estimate","Cluster.s.e.","t.value","pval")
      
      return(df.coefficients)
}


# 2 - Regressions END




summary1 <- Regression1(subset)
summary2 <-  Regression2(subset)


summary1 <- tableGrob(summary1)
summary2 <- tableGrob(summary2)

pdf("~/Desktop/github/Portfolio_Trades/Outputs_David/Figures/regression_results_summary.pdf", 
    width = 8,
    height = 6)

title1 <- textGrob("Regression 1 and 2", gp = gpar(fontsize = 20, fontface = "bold"), vjust = 1)
grid.arrange(
  summary1, summary2, 
  ncol = 1,
  top = title1
)

dev.off()




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
