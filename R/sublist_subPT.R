

library(tidyverse)
library(gridExtra)

#  TASK #1

# We wish to see the distribution of sublists and subPTs as follows: 
# a sublist or subPT consists of all groups of inquiries in a request 
# that shares the exactly the same requested quantity. E.g., if a PT has 3 inquiries, 
# each w different quantities, then there are 3 subPTs in the PT, and if all three 
# shares the same quantity, then there's 1 subPT. Please provide the histogram(s)
# and sum stats tables of sublists and subPTs by the number of inquiries in each,
# perhaps overlapping for easy comparison.


df.inquiry  <- read_csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/inquiries_58_columns.csv")

df.inquiry_sub <- df.inquiry %>% select(req_id, request_type, req_quantity) %>% filter(request_type != "SRFQ")
df.inquiry_sub_T1 <- df.inquiry_sub %>% group_by(req_id, request_type) %>% summarise(sub = n_distinct(req_quantity)) 

plot_task1 <- ggplot(df.inquiry_sub_T1, aes(x = sub)) +
  geom_histogram(aes(y = after_stat(density), fill = request_type), 
                 binwidth = 1, alpha = 0.5, position = "identity") +
  labs(y = "Probability", x = "sublist/subPT") +
  coord_cartesian(xlim = c(0, 150)) + 
  scale_y_continuous(labels = scales::percent)

plot_task1

sum_stats_sub <- df.inquiry_sub_T1 %>% group_by(request_type) %>%
  summarise(
    Mean = mean(    sub,       na.rm = TRUE) %>% round(3),
    SD   = sd(      sub,       na.rm = TRUE) %>% round(3),
    p1   = quantile(sub, 0.01, na.rm = TRUE) %>% round(3),
    p5   = quantile(sub, 0.05, na.rm = TRUE) %>% round(3),
    p10  = quantile(sub, 0.10, na.rm = TRUE) %>% round(3),
    p50  = quantile(sub, 0.50, na.rm = TRUE) %>% round(3),
    p90  = quantile(sub, 0.90, na.rm = TRUE) %>% round(3),
    p95  = quantile(sub, 0.95, na.rm = TRUE) %>% round(3),
    p99  = quantile(sub, 0.99, na.rm = TRUE) %>% round(3))
  
pdf(file   = "~/Desktop/github/Portfolio_Trades/Outputs_David/Figures/sublist_subPT.pdf",
    width  = 8, # The width of the plot in inches
    height = 4) # The height of the plot in inches

plot(plot_task1)
plot.new()

grid.table(sum_stats_sub)

dev.off()



# TASK #2

# We want to know the proportion of each list or PT that are in nonsingleton 
# sublists or subPTs. To do this, compute for each request the percent of inquiries 
# that has at least one other inquiry in the same request that has the same request
# quantity. Then create histograms and sum stats tables as in 1.

df.inquiry_sub_T2 <- df.inquiry_sub %>% group_by(req_id, request_type, req_quantity) %>% 
                                        summarise(n_per_sub = n()) %>% group_by(req_id, request_type) %>% 
                                        summarise(n_per_req = sum(n_per_sub),
                                                  n_nonsingleton = n_per_req - sum(n_per_sub == 1),
                                                  percentage = n_nonsingleton/n_per_req) 
                                              
plot_task2 <- ggplot(df.inquiry_sub_T2, aes(x = percentage)) +
  geom_histogram(aes(y = after_stat(density)*0.03, fill = request_type), 
                 binwidth = 0.03, alpha = 0.5, position = "identity") +
  labs(y = "Probability", x = "sublist/subPT") +
  #coord_cartesian(xlim = c(0)) + 
  scale_y_continuous(labels = scales::percent)

plot_task2

sum_stats_nonsingleton <- df.inquiry_sub_T2 %>% group_by(request_type) %>%
  summarise(
    Mean = mean(    percentage,       na.rm = TRUE) %>% round(3),
    SD   = sd(      percentage,       na.rm = TRUE) %>% round(3),
    p1   = quantile(percentage, 0.01, na.rm = TRUE) %>% round(3),
    p5   = quantile(percentage, 0.05, na.rm = TRUE) %>% round(3),
    p10  = quantile(percentage, 0.10, na.rm = TRUE) %>% round(3),
    p50  = quantile(percentage, 0.50, na.rm = TRUE) %>% round(3),
    p90  = quantile(percentage, 0.90, na.rm = TRUE) %>% round(3),
    p95  = quantile(percentage, 0.95, na.rm = TRUE) %>% round(3),
    p99  = quantile(percentage, 0.99, na.rm = TRUE) %>% round(3))

pdf(file   = "~/Desktop/github/Portfolio_Trades/Outputs_David/Figures/sublist_subPT_nonsingleton.pdf",
    width  = 8, # The width of the plot in inches
    height = 4) # The height of the plot in inches

plot(plot_task2)
plot.new()

grid.table(sum_stats_nonsingleton)

dev.off()
