library(knitr)
library(tidyverse)
library(ggfortify)
library(formattable)
library(ggnewscale)
library(scales)

torn <- read_csv("airdrop.csv")
torn <- torn %>% mutate(TORN = Raw/10^(18)) %>% arrange(desc(TORN))
torn %>% summarise( sum = sum(TORN))

scaleFUN <- function(x) sprintf("%.2f", x)
x_breaks = c(0.01,0.03,0.1,0.3, 1, 3, 10,30,100,300,1000)
torn %>% ggplot(aes(x=TORN))+
  geom_histogram(color="darkblue", fill="lightblue", boundary=0)+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  scale_x_log10(breaks = x_breaks,
                labels = scaleFUN)+
  labs(title = "Distribution of TORN Tokens", x = "TORN Tokens")+
  theme(axis.text.x = element_text(angle = 45, vjust=0.9))

Tornado.cash is a fully autonomous, decentralized transaction mixer that 
provides private value transfers on the Ethereum blockchain. On December 17th, 
a Decentralized Autonomous Organization was formed around the Tornado.Cash protocol, 
and it's native token $vTORN was distributed to early adopters and users of the service.  
As it stands, the $vTRON tokens have a 45 day lock up where transfers have been disabled until 
governance has decided on the best way forward.  Having said that, it is quite common for 
token's "airdropped" on to users to have some fair market trade value.  
This fun case study will be speculative in nature and involve the procurement of the data, 
transforming variables using a few common themes we have learned from other cryptocurrency analysis, 
and end with a set of estimates of the average expected dollar value of the tokens a recipient will gain.


cap <- torn %>% 
  select(TORN) %>%
  mutate(Val1MCap = TORN*2,
         '$Badger2MCap' = TORN*4,
         Val5MCap = TORN*10,
         Val10MCap = TORN*20,
         'MIR17MCap' = TORN*34,
         Val25MCap = TORN*50,
         Val50MCap = TORN*100,
         Val100MCap = TORN*200,
         '$Inch164MCap' = TORN*328,
         Val250MCap = TORN*500,
         Val500MCap = TORN*1000,
         '$UNI734MCap' = TORN*1468,
         Val1BCap = TORN*2000) 

summary <- cap %>% 
  gather() %>%
  group_by(key) %>%
  summarise(Max = max(value),
            '90%-tile' = quantile(value, 0.9),
            '70%-tile' = quantile(value, 0.75),
            Median = mean(value),
            '25%-tile' = quantile(value, 0.25),
            '10%-tile' = quantile(value,0.1),
            Min = min(value)) %>%
  column_to_rownames(var="key")

summary_t <- t(summary)

summary_t <- as.data.frame(summary_t) %>%
  rownames_to_column(., var = "rowname") %>%
  mutate_if(is.numeric, round, 2) %>%
  column_to_rownames(var="rowname") %>%
  select(TORN,
         Val1MCap,
         '$Badger2MCap',
         Val5MCap,
         Val10MCap,
         'MIR17MCap',
         Val25MCap,
         Val50MCap,
         Val100MCap,
         '$Inch164MCap',
         Val250MCap,
         Val500MCap,
         '$UNI734MCap',
         Val1BCap)
  


colortile <- function(min.color = "lightpink", max.color = "lightgreen", fun = "currency") {
  fun <- match.fun(fun)
  formatter("span", x ~ fun(x),
            style = function(y) style(
              display = "block",
              direction = "rtl",
              "border-radius" = "4px",
              "padding-right" = "2px",
              "background-color" = csscolor(gradient(as.numeric(y),min.color='lightpink', max.color = 'lightgreen'))
            )
  )
}


formattable(summary_t,
  lapply(1:nrow(summary_t), function(row) {
  area(row, col = -1) ~ colortile()
    }
  )
)
  
  

  
  
  
  
  
