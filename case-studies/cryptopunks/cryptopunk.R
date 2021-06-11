library(knitr)
library(tidyverse)
library(ggfortify)
library(formattable)
library(ggnewscale)
library(scales)



punks_transactions <- read_csv("transactions_cryptopunks.csv")

punks_transactions %>% 
  filter(Type == "Claimed") %>% 
  summarise((n_distinct(To)))

punks_transactions %>%
  summarise((n_distinct(To)))

firstday <- punks_transactions %>% 
  filter(Txn == '2017-06-23' & Type != 'Claimed')

firstday %>% group_by(Type) %>%
  summarise(Count = n())

punks_transactions %>%
  group_by(ID) %>%
  summarise(num_transaction = n())

punks_transactions %>%
  filter(Type == "Bid") %>%
  group_by(ID) %>%
  summarise(num_bids = n(), unique_bidders = n_distinct(From))

punks_transactions %>%
  filter(Type == "Offered") %>%
  group_by(ID) %>%
  summarise(num_offered = n())

punks_transactions %>%
  filter(Type == "Bid Withdrawn") %>%
  group_by(ID) %>%
  summarise(num_withdrawn = n())
  
num_sold <- punks_transactions %>%
  filter(Type == "Sold") %>%
  group_by(ID) %>%
  summarise(num_sold = n())

punks_transactions %>%
  filter(Txn == "2017-06-23" & Type == "Claimed") %>%
  summarise(num_claimed = n())

last_sold_by_ID <- punks_transactions %>%
  filter(Type == "Sold") %>%
  group_by(ID) %>%
  filter(row_number()==1) %>%
  rename(last_price_ETH = Crypto, last_price_USD = USD)

first_sold_by_ID <- punks_transactions %>%
  filter(Type == "Sold") %>%
  group_by(ID) %>%
  filter(row_number()==n())%>%
  rename(first_price_ETH = Crypto, first_price_USD = USD)

sales <- last_sold_by_ID %>% 
  select(ID, last_price_ETH, last_price_USD) %>% 
  inner_join(first_sold_by_ID, by = c("ID"="ID")) %>%
  inner_join(num_sold, by = c("ID"="ID")) %>%
  mutate(price_change_ETH = last_price_ETH-first_price_ETH,
         price_change_USD = last_price_USD-first_price_USD) %>%
  mutate(percentage_change_ETH = price_change_ETH/first_price_ETH*100,
         percentage_change_USD = price_change_ETH/first_price_USD*100) %>%
  mutate(average_change_ETH = price_change_ETH/num_sold,
         average_change_USD = price_change_USD/num_sold) %>%
  mutate(resell_frequency = ifelse(num_sold<=3, "Low", ifelse(num_sold<=5, "Medium", "High")))

scaleFUN <- function(x) sprintf("%.0f", x)
x_breaks = c(1,2,3,5,10,20,30,50,100,200,300,500,1000,2000,3000,5000,10000,20000,30000,50000,100000,
             200000,300000,500000)

transaction %>% 
  filter(num_sold>1) %>%
  mutate(resell_frequency = factor(resell_frequency, levels=c("Low", "Medium", "High"))) %>%
  ggplot(aes(x=percentage_change_ETH))+
  geom_histogram(aes(fill = resell_frequency), colour="black")+
  scale_x_log10(breaks = x_breaks,
                labels = comma_format())+
  scale_y_continuous(breaks = scales::pretty_breaks(n=20))+
  theme(axis.text.x = element_text(angle = 45, vjust=0.9))+
  labs(title = "Ditribution of Percentage Changed in Sold Price (ETH)",
       subtitle = "for all Crypto Punks that have been resold", x = "Percentage Change (%)")+
  scale_fill_discrete(name = "Resell Frequency", labels = c("Low (2,3)", "Medium (4,5)", "High (>5)"))

transaction %>% 
  filter(num_sold>1 & first_price_ETH!=0) %>%
  group_by(resell_frequency) %>%
  summarise(count = n(),
            mean = mean(percentage_change_ETH),
            min = min(percentage_change_ETH),
            first_quartile = quantile(percentage_change_ETH, 0.25),
            median = median(percentage_change_ETH),
            third_quartile = quantile(percentage_change_ETH, 0.75),
            max = max(percentage_change_ETH))

punks_transactions %>%
  filter(Type == "Sold" | Type == "Transfer" | Type == "Claimed") %>%
  group_by(ID) %>%
  summarise(Unique_Owners = n_distinct(To)) 


punks_transactions %>%
  group_by(ID) %>%
  summarise(num_interactions = n()) 


punks_raw <- read_csv("cryptopunks.csv")
punks_rank <- punks_raw %>%
  mutate(Rank = parse_number(Rank)) %>%
  mutate(RankNum = rank(desc(Rank))) %>%
  select(Punk, RankNum)


LastPrice_Rank <- last_sold_by_ID %>% 
  select(ID, Crypto, USD) %>%
  inner_join(punks_rank, by = c("ID"="Punk")) 


scaleFUN <- function(x) sprintf("%.2f", x)
x_breaks = c(0.01,0.02,0.03,0.05,0.07, 0.1,0.2,0.3,0.5,0.7,1,2,3,5,7,10,20,30,50, 70,
             100,200, 300, 500, 700, 1000, 2000, 3000, 5000, 7000, 10000, 20000, 30000, 50000, 70000, 100000)
LastPrice_Rank %>% 
  ggplot(aes(x=RankNum, y=USD))+
  geom_point()+
  labs(title = "Last Sold Price vs Rank", y = "Last Sold Price (USD)", x = "Rank")+
  scale_x_continuous(breaks = scales::pretty_breaks(n=20))+
  scale_y_log10(breaks = x_breaks,
                labels = scaleFUN)+
  geom_smooth(aes(colour = ..y..), )+
  scale_colour_gradient(low = "red", high = "#006400", limits= c(0,5000))+
  theme_bw()
  


scaleFUN <- function(x) sprintf("%.2f", x)
x_breaks = c(0.01,0.02,0.03,0.05,0.07, 0.1,0.2,0.3,0.5,0.7,1,2,3,5,7,10,20,30,50, 70,
             100,200, 300, 500, 700, 1000, 2000, 3000, 5000, 7000, 10000, 20000, 30000, 50000, 70000, 100000)
LastPrice_Rank %>% 
  ggplot(aes(x=RankNum, y=Crypto))+
  geom_point()+
  geom_smooth(aes(colour = ..y..), se=FALSE, size=2)+
  scale_colour_gradient(low = "red", high = "green")+
  labs(title = "Crypto Punks Last Sold Price vs Uniqueness Score", 
       y = "Last Sold Price (ETH)", x = "Uniqueness Score")+
  scale_x_continuous(breaks = scales::pretty_breaks(n=20))+
  scale_y_log10(breaks = x_breaks,
                labels = scaleFUN)


LastPrice <- last_sold_by_ID %>% 
  select(ID, Crypto, USD) %>%
  inner_join(punks_raw, by = c("ID"="Punk"))

scaleFUN <- function(x) sprintf("%.2f", x)
x_breaks = c(0.01,0.03, 0.05,0.1,0.3, 0.5, 1, 3,5, 10, 30,50, 100)
LastPrice %>% ggplot(aes(x=Crypto))+
  geom_histogram(aes(fill=Type),color ="black", boundary=0)+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  scale_x_log10(breaks = x_breaks,
                labels = scaleFUN)+
  facet_wrap(~Type, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 45, vjust=0.9))+
  labs(x="Last Sale Price (ETH)")

scaleFUN <- function(x) sprintf("%.2f", x)
x_breaks = c(0.01,0.03, 0.05,0.1,0.3, 0.5, 1, 3,5, 10, 30,50, 100)
LastPrice %>% 
  ggplot(aes(x=Crypto))+
  geom_histogram(aes(fill=factor(Slots)),color ="black", boundary=0)+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  scale_x_log10(breaks = x_breaks,
                labels = scaleFUN)+
  facet_wrap(~Slots, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 45, vjust=0.9))+
  labs(x="Last Sale Price (ETH)")

scaleFUN <- function(x) sprintf("%.2f", x)
x_breaks = c(0.01,0.03, 0.05,0.1,0.3, 0.5, 1, 3,5, 10, 30,50, 100)
LastPrice %>% 
  ggplot(aes(x=Crypto))+
  geom_histogram(aes(fill=Skin),color ="black", boundary=0)+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  scale_x_log10(breaks = x_breaks,
                labels = scaleFUN)+
  facet_wrap(~Skin, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 45, vjust=0.9))+
  labs(x="Last Sale Price (ETH)")


transaction_rank <- transaction %>% 
  filter(num_sold>1) %>%
  inner_join(punks_rank, by = c("ID"="Punk")) 

transaction_rank %>% 
  ggplot(aes(x=RankNum, y= percentage_change_ETH))+
  geom_point()+
  scale_y_log10()

transaction$Month_Yr <- format(as.Date(transaction$Txn), "%Y-%m")

transaction %>%
  group_by(Month_Yr) %>%
  summarise(num_sales = n()) %>%
  ggplot(aes(x=Month_Yr,y=num_sales, group=1))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  geom_line(color="blue")+
  theme(axis.text.x = element_text(angle = 90, vjust=0.9))+
  labs(title = "Number of Sales over time", y = "Number of Sales", x= "Time")

transaction %>%
  group_by(Month_Yr) %>%
  summarise(total_sales = sum(last_price_ETH)) %>%
  ggplot(aes(x=Month_Yr,y=total_sales, group=1))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  geom_line(color="blue")+
  theme(axis.text.x = element_text(angle = 90, vjust=0.9))+
  labs(title = "Total Sales over time", y = "Total Sales (ETH)", x= "Time")

