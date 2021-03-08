library(tidyverse)
library(ggimage)
library(psych)

stab <- read_csv("stablecoins.csv")

mypc <- prcomp(stab %>% select(-Coin), scale = TRUE)$x %>%
    as_tibble() %>%
    mutate(Coin = stab$Coin,
           Path = paste0("images/", Coin, ".png")) %>%
    mutate(PC1Back = PC1,
           PC1 = -PC2,
           PC2 = -PC1Back)

ggplot(data = mypc, aes(x = PC1, y = PC2)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    geom_image(aes(image = Path), size = 0.08) +
    geom_label(aes(label = Coin), alpha = 0.8) +
    scale_y_continuous(limits = c(-3, 3), breaks = -3:3, labels = c("", "Lower Scores", expression(symbol("\254")), "", expression(symbol("\256")), "Higher Scores", "")) +
    scale_x_continuous(limits = c(-3, 3), breaks = -3:3, labels = c("", "Investment Focused", expression(symbol("\254")), "", expression(symbol("\256")), "Security Focused", "")) +
    coord_flip() +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
    #watermark_img("images/image.png", 
    #              location = "bl", alpha = .25, width = 70) + 
    labs(
        y = "Overall Score",
        x = "Focus",
        title = "A Visualization of the Stablecoin Landscape",
        subtitle = "        Analysis and Rankings by @denomeme"
    )



    ylab(expression(paste("Lower Scores ", symbol("\254"), "           ", symbol("\256"), " Higher Scores"))) +
    xlab(expression(paste("Security Focused ", symbol("\254"), "           ", symbol("\256"), " Investment Focused")))
