library(dplyr)
library(ggplot2)

df <- haven::read_dta("../Data/RFQ/inquiry_proccessed.dta")


df1 <- df %>%
  filter(product_cd == "USHY" & p_type != "Broker-Dealer")%>%
  select(isin , req_id)%>%
  group_by(isin)%>%
  summarise(n = n_distinct(req_id))%>%
  arrange(-n)%>%
  mutate(rank = dense_rank(n))%>%
  group_by(rank)%>%
  summarise(n_obs = n())%>%
  ungroup()%>%
  mutate(cumsum = cumsum(n_obs),
         percent = cumsum/sum(n_obs))

df2 <- df %>%
  filter(product_cd == "USHY" & p_type != "Broker-Dealer" & request_type != "SRFQ")%>%
  select(isin , req_id)%>%
  group_by(isin)%>%
  summarise(n = n_distinct(req_id))%>%
  arrange(-n)%>%
  mutate(rank = dense_rank(n))%>%
  group_by(rank)%>%
  summarise(n_obs = n())%>%
  ungroup()%>%
  mutate(cumsum = cumsum(n_obs),
         percent = cumsum/sum(n_obs))



p1 <- ggplot(df1 , aes(x = rank , y = percent))+
  geom_line()+
  coord_cartesian(xlim =c(0 , 1000) , ylim = c(0,1)) +
  labs(y = "Probability", x = "Number of requests per isin")+
  ggtitle("Cumulative distribution rank number of request per isin_ Excluding: (USHG , Broker-Dealer)")

p2 <- ggplot(df2 , aes(x = rank , y = percent))+
  geom_line()+
  coord_cartesian(xlim =c(0 , 1000) , ylim = c(0,1)) +
  labs(y = "Probability", x = "Number of requests per isin")+
  ggtitle("Cumulative distribution rank number of request per isin_ Excluding: (SRFQ , USHG , Broker-Dealer)")


pdf(file   = "../Outputs_Hamed/Figures/CumDist_ISINreq.pdf",
    width  = 8, # The width of the plot in inches
    height = 4) # The height of the plot in inches

plot(p1)
plot(p2)

dev.off()



