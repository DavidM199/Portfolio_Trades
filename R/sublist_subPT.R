

library(tidyverse)
library(gridExtra)
library(grid)

#  TASK #1

# We wish to see the distribution of sublists and subPTs as follows: 
# a sublist or subPT consists of all groups of inquiries in a request 
# that shares the exactly the same requested quantity. E.g., if a PT has 3 inquiries, 
# each w different quantities, then there are 3 subPTs in the PT, and if all three 
# shares the same quantity, then there's 1 subPT. Please provide the histogram(s)
# and sum stats tables of sublists and subPTs by the number of inquiries in each,
# perhaps overlapping for easy comparison.

# TASK #2

# We want to know the proportion of each list or PT that are in nonsingleton 
# sublists or subPTs. To do this, compute for each request the percent of inquiries 
# that has at least one other inquiry in the same request that has the same request
# quantity. Then create histograms and sum stats tables as in 1.


df.inquiry  <- read_csv("~/Desktop/Portfolio_Trades_my_computer/data_minimizing/working_files/inquiries_58_columns.csv")


df.inquiry_sub <- df.inquiry %>% select(req_id, request_type, req_quantity, number_assets, p_type) %>% filter(request_type != "SRFQ", p_type != "Broker-Dealer") 
sum(df.inquiry_sub$p_type == "Broker-Dealer")
#Putting the later computations in a function for faster computing and clear pdf creation in the end
sublist_subPT <- function(df.inquiry_sub, filter = FALSE) {
  
 if (filter){
   df.inquiry_sub <- df.inquiry_sub  %>% filter(number_assets >= 10)
 }
  
 df.inquiry_sub_T1 <- df.inquiry_sub %>% group_by(req_id, request_type, req_quantity) %>% summarise(sub_n = n()) 
 
 plot_task1 <- ggplot(df.inquiry_sub_T1, aes(x = sub_n)) +
   geom_histogram(aes(y = after_stat(density), fill = request_type), 
                  binwidth = 1, alpha = 0.5, position = "identity") +
   labs(y = "Probability", x = "sublist/subPT") +
   coord_cartesian(xlim = c(0, 15)) + 
   scale_y_continuous(labels = scales::percent) 
   
 
 cumulative_plot_task1 <-  
   ggplot((df.inquiry_sub_T1 %>%
             group_by(request_type) %>%
             arrange(sub_n) %>%
             mutate(rn = row_number(), n=n()))) +
   geom_step(aes(x=sub_n, y=rn/n, color=request_type))+
   coord_cartesian(xlim = c(0, 15)) + 
   labs(y = "Cumulative Probability")

 
 sum_stats_sub <- df.inquiry_sub_T1 %>% group_by(request_type) %>%
   summarise(
     Mean = mean(    sub_n,       na.rm = TRUE) %>% round(3),
     SD   = sd(      sub_n,       na.rm = TRUE) %>% round(3),
     p1   = quantile(sub_n, 0.01, na.rm = TRUE) %>% round(3),
     p5   = quantile(sub_n, 0.05, na.rm = TRUE) %>% round(3),
     p10  = quantile(sub_n, 0.10, na.rm = TRUE) %>% round(3),
     p50  = quantile(sub_n, 0.50, na.rm = TRUE) %>% round(3),
     p90  = quantile(sub_n, 0.90, na.rm = TRUE) %>% round(3),
     p95  = quantile(sub_n, 0.95, na.rm = TRUE) %>% round(3),
     p99  = quantile(sub_n, 0.99, na.rm = TRUE) %>% round(3))
 
 df.inquiry_sub_T2 <- df.inquiry_sub %>% group_by(req_id, request_type, req_quantity) %>% 
                                         summarise(n_per_sub = n(), .groups = 'keep') %>% group_by(req_id, request_type) %>% 
                                         summarise(n_per_req = sum(n_per_sub),
                                                   n_nonsingleton = n_per_req - sum(n_per_sub == 1),
                                                   percentage = n_nonsingleton/n_per_req) 
                                               
 plot_task2 <- ggplot(df.inquiry_sub_T2, aes(x = percentage)) +
   geom_histogram(aes(y = after_stat(density)*0.03, fill = request_type), 
                  binwidth = 0.03, alpha = 0.5, position = "identity") +
   labs(y = "Probability", x = "sublist/subPT") +
   scale_y_continuous(labels = scales::percent)
   
 
 cumulative_plot_task2 <- ggplot((df.inquiry_sub_T2 %>%
             group_by(request_type) %>%
             arrange(percentage) %>%
             mutate(rn = row_number(), n=n()))) +
   geom_step(aes(x=percentage, y=rn/n, color=request_type)) +
   scale_y_continuous(labels = scales::percent) +
   labs(y = "Cumulative Probability", x = "percentage")

 
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
 
 if (filter) {
   plot_task1 = plot_task1 + ggtitle("Task 1 - number of inquiries in sublist/subPT - number_assets >= 10")
   cumulative_plot_task1 = cumulative_plot_task1 + ggtitle("Cumulative plot for Task one - number_assets >= 10")
   plot_task2 = plot_task2 + ggtitle("Task 2 - percent of nonsingletons for each request - number_assets >= 10")
   cumulative_plot_task2 = cumulative_plot_task2 + ggtitle("Task 2 - Cumulative Probability - number_assets >= 10") 
 } else {
   plot_task1 = plot_task1 + ggtitle("Task 1 - number of inquiries in sublist/subPT")
   cumulative_plot_task1 = cumulative_plot_task1 + ggtitle("Cumulative plot for Task one - number of inquiries per subgroup")
   plot_task2 = plot_task2 + ggtitle("Task 2 - percent of nonsingletons for each request")
   cumulative_plot_task2 = cumulative_plot_task2 + ggtitle("Task 2 - Cumulative Probability") 
 }
 
 return(list(plot_task1, cumulative_plot_task1, sum_stats_sub, plot_task2, cumulative_plot_task2, sum_stats_nonsingleton))
}

 
#-----------------------------------------------------------
#Creating the pdf file with all the plots and tables in it - report is added afterwards 

non_filtered <- sublist_subPT(df.inquiry_sub, filter = FALSE)
filtered <- sublist_subPT(df.inquiry_sub, filter = TRUE)
variations <- list(non_filtered, filtered)

pdf(file = "~/Desktop/github/Portfolio_Trades/Outputs_David/Figures/sublist-subPT-T1-T2.pdf",
    width = 8, 
    height = 6)

for (plots in variations){
 plot(plots[[1]])
 
 plot(plots[[2]])
 grid.newpage()
 
 grid.text("Sum stats Task 1 - number of inquiries per subgroup", x = 0.5, y = 0.95, just = "center", gp = gpar(fontsize = 12))
 grid.table(plots[[3]], vp = viewport(x = 0.5, y = 0.5, width = 0.9, height = 0.4))
 
 plot(plots[[4]])
 
 plot(plots[[5]])
 grid.newpage()
 
 grid.text("Sum stats Task 2 - percent of nonsingletons per request", x = 0.5, y = 0.95, just = "center", gp = gpar(fontsize = 12))
 grid.table(plots[[6]], vp = viewport(x = 0.5, y = 0.5, width = 0.9, height = 0.4))
}
dev.off()
 
 
 
 
 