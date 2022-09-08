library(uniswappeR)

drops <- list(
				UNI = "0x1f9840a85d5aF5bf1D1762F925BDADdC4201F984",
				BADGER = "0x3472A5A71965499acd81997a54BBA8D852C6E53d",
				GTC = "0xde30da39c46104798bb5aa3fe8b9e0e1f348163f",
				TORN = "0x77777FeDdddFfC19Ff86DB637967013e6C6A116C",
				PSP = "0xcAfE001067cDEF266AfB7Eb5A286dCFD277f3dE5",
				PAINT = "0x4C6eC08CF3fc987c6C4BEB03184D335A2dFc4042",
				MIR = "0x09a3EcAFa817268f77BE1283176B946C4ff2E608",
				PUSH = "0xf418588522d5dd018b425E472991E52EBBeEEEEE",
				HOP = "0xc5102fe9359fd9a28f877a67e36b0f050d81a3cc"
			)

## Define function to get liquidity across all pairs deployed with this token
get_liq_data <- function(x,token)
{
	v2_pairs <- token_pair_map_v2(x)
	v3_pairs <- token_pair_map_v3(x)

	## V2
	v2_liq <- list()
	for(idx in 1:nrow(v2_pairs)) 
	{
		m_data <- pair_mint_txs_v2(pair_address = v2_pairs$id[idx])
		if(nrow(m_data)==0) next
		token2 <- ifelse(m_data$pair$token0$symbol[1]==token,m_data$pair$token1$symbol[1],m_data$pair$token0$symbol[1])
		token2add <- ifelse(m_data$pair$token0$symbol[1]==token,m_data$pair$token1$id[1],m_data$pair$token0$id[1])
		m_data <- m_data[,c("timestamp","amountUSD")]
		m_data$Version <- 2
		m_data$Type <- "Mint"
		m_data$Token <- token
		m_data$Token2 <- token2
		m_data$Token2Add <- token2add
		b_data <- pair_burn_txs_v2(pair_address = v2_pairs$id[idx])
		if(nrow(b_data)==0) next
		b_data <- b_data[,c("timestamp","amountUSD")]
		b_data$Version <- 2
		b_data$Type <- "Burn"
		b_data$Token <- token
		b_data$Token2 <- token2
		b_data$Token2Add <- token2add
		v2_liq[[idx]] <- rbind(m_data,b_data)
		message(paste0(token,":V2:Pair",idx,"_",nrow(v2_pairs)))
	}
	v2_liq <- do.call(rbind,v2_liq)

	## V3
	v3_liq <- list()
	for(idx in 1:nrow(v3_pairs)) 
	{
		m_data <- pair_mint_txs_v3(pair_address = v3_pairs$id[idx])
		if(nrow(m_data)==0) next
		token2 <- ifelse(m_data$pool$token0$symbol[1]==token,m_data$pool$token1$symbol[1],m_data$pool$token0$symbol[1])
		token2add <- ifelse(m_data$pool$token0$symbol[1]==token,m_data$pool$token1$id[1],m_data$pool$token0$id[1])
		m_data <- m_data[,c("timestamp","amountUSD")]
		m_data$Version <- 3
		m_data$Type <- "Mint"
		m_data$Token <- token
		m_data$Token2 <- token2
		m_data$Token2Add <- token2add
		b_data <- pair_burn_txs_v3(pair_address = v3_pairs$id[idx])
		if(nrow(b_data)==0) next
		b_data <- b_data[,c("timestamp","amountUSD")]
		b_data$Version <- 3
		b_data$Type <- "Burn"
		b_data$Token <- token
		b_data$Token2 <- token2
		b_data$Token2Add <- token2add
		v3_liq[[idx]] <- rbind(m_data,b_data)		
		message(paste0(token,":V3:Pair:",idx,"_",nrow(v3_pairs)))
	}
	v3_liq <- do.call(rbind,v3_liq)

	## Return All
	return(rbind(v2_liq,v3_liq))
}

## Loop for all tokens
liq_burns <- list()
for(idx in 1:length(drops))
{
	liq_burns[[idx]] <- get_liq_data(drops[[idx]],names(drops)[idx])
}
saveRDS(liq_burns,"~/Desktop/Uniswap_Plots/liq_burns.RDS")


## Analysis and plot
liq_burns <- readRDS("~/Desktop/Uniswap_Plots/liq_burns.RDS")
library(lubridate)
library(ggplot2)
library(cowplot)
library(dplyr)
library(scales)
plots <- list()
for(idx in 1:length(liq_burns))
{
	t_data <- liq_burns[[idx]]
	t_data$timestamp <- as_datetime(as.numeric(t_data$timestamp))
	t_data$start <- min(t_data$timestamp)
	t_data$duration_d <- as.numeric(difftime(t_data$timestamp,t_data$start,units="days"))
	t_data <- t_data[t_data$duration_d<=14,]
	t_data <- t_data[order(t_data$duration_d),]
	t_data$duration_d <- ifelse(t_data$duration_d==0,.1,ceiling(t_data$duration_d*10)/10)
	t_data$amountUSD <- ifelse(t_data$Type=="Mint",as.numeric(t_data$amountUSD),-1*as.numeric(t_data$amountUSD))
	t_data2 = t_data %>% group_by(Type,duration_d) %>% summarise(amountUSDT = sum(amountUSD,na.rm=TRUE))
	t_data2 <- t_data2[order(t_data2$duration_d,t_data2$Type,decreasing=c(FALSE,TRUE),method="radix"),]
	t_data2$amountUSDC <- cumsum(t_data2$amountUSDT)
	t_data2 <- rbind(t_data2,
					data.frame(
								Type = "Balance",
								duration_d = unique(t_data2$duration_d),
								amountUSDT = t_data2$amountUSDC[sapply(unique(t_data2$duration_d),function(x,y) max(which(y$duration_d==x)),y=t_data2)]
							)
				)
	t_data2$Type <- ifelse(t_data2$Type=="Mint","Liquidity Added",ifelse(t_data2$Type=="Burn","Liquidity Removed","Liquidity Balance"))
	# t_data2 <- t_data2[t_data2$Type!="Liquidity Balance",-4]
	plots[[idx]] <- ggplot(t_data2, aes(fill=Type, y=amountUSDT, x=duration_d)) +
					geom_area(alpha=0.4 , size=.2, colour="black",position = "identity")+
					theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
					scale_x_continuous(breaks = seq(0, 14, by = 1),limits=c(0,14),labels=format(as.Date(t_data$start[1])+days(0:14),"%b-%d-%y"))+
					scale_y_continuous(labels = dollar_format(),breaks = pretty(t_data2$amountUSDT,10),lim = range(pretty(t_data2$amountUSDT,10)) )+
					scale_fill_manual(values = c("#619CFF","#00BFC4","#F8766D"),breaks=c("Liquidity Balance","Liquidity Added","Liquidity Removed"))+
					labs(title = toupper(t_data$Token[1]))+
					xlab("")+
					ylab("Amount in USD")
	message(idx)
}

## With dates as x axis for first 14 days.
plot <- plot_grid(
					plotlist=lapply(plots,function(x) x+theme(legend.position="none")),
					nrow=3,ncol=3,
					align="hv",axis="lr",
					greedy=TRUE
				)
legend <- get_legend(plots[[1]] + theme(legend.box.margin = margin(0, 0, 0, 12)))
title <- ggdraw() + draw_label("First 14 Days : Liquidity Added/Removed across Uniswap V2 and V3 In all Pairs With Token Present", fontface='bold',size = 20)
plot_final <- plot_grid(title, plot, ncol=1, rel_heights=c(0.05, 1))
plot_final <- plot_grid(plot_final, legend, rel_widths = c(3, .3))
ggsave("~/AirdropLiq.jpeg",plot=plot_final,width = 18,height=12)




