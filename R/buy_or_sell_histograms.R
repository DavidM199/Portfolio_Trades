library(tidyverse)
library(ggplot2)


inquiries <- read.csv("~/Desktop/R/data_minimizing/working_files/newest_best_inquiries.csv")

#creating the variables other_side and mixed_request                                 
mixed_request_other_side <- inquiries %>% select(req_id, legside, request_type) %>%   filter(request_type!="SRFQ")%>% mutate(buy = ifelse(legside=="OWIC", 1, 0), sell = ifelse(legside=="BWIC",1,0)) %>% 
                group_by(req_id) %>% mutate(mixed_request = ifelse(sum(buy) > 0 & sum(sell) > 0, 1, 0)) %>% 
                mutate(other_side = ifelse(mixed_request == 1, min(sum(buy), sum(sell))/(sum(buy)+sum(sell)), 0))

#separate dataframe for other_side as I don't need other_side 
other_side <- mixed_request_other_side %>% filter(other_side!=0) %>% group_by(req_id) %>% summarise(other_side = max(other_side), request_type=min(request_type))

#separating PT and List to graph and calculate individually
other_side_PT <- other_side %>% filter(request_type=="PT")

hist_other_side_PT <- ggplot(other_side_PT , aes(x = other_side , fill=request_type))+
  geom_histogram(aes(y = ..count../sum(..count..)*100)
                 , position = "identity"
                 , alpha = 0.75
                 , binwidth =  0.01) + ylab("Percentage within group")

other_side_LIST <-other_side %>% filter(request_type == "List")

hist_other_side_LIST <- ggplot(other_side_LIST , aes(x = other_side , fill=request_type))+
  geom_histogram(aes(y = ..count../sum(..count..)*100)
                 , position = "identity"
                 , alpha = 0.75
                 , binwidth =  0.01) + ylab("Percentage within group")
#Histograms 1,2
hist_other_side_PT
hist_other_side_LIST

#-----------

#mixed_request graphs

mixed_request <- mixed_request_other_side %>% group_by(req_id) %>% summarise(mixed_request = max(mixed_request), request_type = max(request_type))

mixed_request_PT <- mixed_request %>%  filter(request_type == "PT")
mixed_request_LIST <- mixed_request %>%  filter(request_type == "List")

hist_mixed_request_PT <- ggplot(mixed_request_PT, aes(x = mixed_request, fill = request_type)) + 
  geom_histogram(aes(y = ..count../sum(..count..) * 100), position = "identity",  bins = 10) + 
  ylab("Percentage")  
hist_mixed_request_LIST <- ggplot(mixed_request_LIST, aes(x = mixed_request, fill = request_type)) + 
  geom_histogram(aes(y = ..count../sum(..count..) * 100), position = "identity",  bins = 10) + 
  ylab("Percentage")  


#Histograms 3, 4
hist_mixed_request_PT
hist_mixed_request_LIST


#summary_other_side
library(dplyr)
summary_other_side <- other_side %>% group_by(request_type) %>%
  summarise(
    Mean = mean(    other_side,       na.rm = TRUE) %>% round(3),
    SD   = sd(      other_side,       na.rm = TRUE) %>% round(3),
    p1   = quantile(other_side, 0.01, na.rm = TRUE) %>% round(3),
    p5   = quantile(other_side, 0.05, na.rm = TRUE) %>% round(3),
    p10  = quantile(other_side, 0.10, na.rm = TRUE) %>% round(3),
    p50  = quantile(other_side, 0.50, na.rm = TRUE) %>% round(3),
    p90  = quantile(other_side, 0.90, na.rm = TRUE) %>% round(3),
    p95  = quantile(other_side, 0.95, na.rm = TRUE) %>% round(3),
    p99  = quantile(other_side, 0.99, na.rm = TRUE) %>% round(3)
  )

#summary_omixed_request
summary_mixed_request <- mixed_request %>% group_by(request_type) %>%
  summarise(
    Mean = mean(    mixed_request,       na.rm = TRUE) %>% round(3),
    SD   = sd(      mixed_request,       na.rm = TRUE) %>% round(3),
    p1   = quantile(mixed_request, 0.01, na.rm = TRUE) %>% round(3),
    p5   = quantile(mixed_request, 0.05, na.rm = TRUE) %>% round(3),
    p10  = quantile(mixed_request, 0.10, na.rm = TRUE) %>% round(3),
    p50  = quantile(mixed_request, 0.50, na.rm = TRUE) %>% round(3),
    p90  = quantile(mixed_request, 0.90, na.rm = TRUE) %>% round(3),
    p95  = quantile(mixed_request, 0.95, na.rm = TRUE) %>% round(3),
    p99  = quantile(mixed_request, 0.99, na.rm = TRUE) %>% round(3)
  )

library("gridExtra")

pdf("distributions_and_histograms.pdf", height=nrow(summary_other_side), width = 13)
grid.table(summary_other_side)
dev.off()

pdf("distributions_and_histograms2.pdf", height=nrow(summary_other_side), width = 13)
grid.table(summary_mixed_request)
dev.off()


