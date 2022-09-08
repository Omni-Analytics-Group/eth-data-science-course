library(tidyverse)

prop <- read_csv("~/Downloads/prop_df (1).csv") %>%
    mutate(
        choices = strsplit(choices, ";"),
        scores = strsplit(scores, ";")
    ) %>%
    select(id, title, votes, choices, scores) %>%
    unnest(c(choices, scores)) %>%
    group_by(id) %>%
    mutate(choices = factor(choices, labels = paste0("Choice ", 1:length(unique(choices)), " Score"))) %>%
    spread(key = choices, value = scores) %>%
    mutate(across(starts_with("Choice "), readr::parse_number)) %>%
    rowwise() %>%
    mutate(
        ScoreSum = sum(c(`Choice 1 Score`, `Choice 2 Score`, `Choice 3 Score`, `Choice 4 Score`,
                             `Choice 5 Score`, `Choice 6 Score`, `Choice 7 Score`, `Choice 8 Score`,
                             `Choice 9 Score`, `Choice 10 Score`, `Choice 11 Score`, `Choice 12 Score`), na.rm = TRUE)
    ) %>%
    mutate(
        Dissent = 100 * (1 - (max(c(`Choice 1 Score`, `Choice 2 Score`, `Choice 3 Score`, `Choice 4 Score`,
                         `Choice 5 Score`, `Choice 6 Score`, `Choice 7 Score`, `Choice 8 Score`,
                         `Choice 9 Score`, `Choice 10 Score`, `Choice 11 Score`, `Choice 12 Score`), na.rm = TRUE) / ScoreSum))
    ) %>%
    select(-ScoreSum) %>%
    mutate(Consensus = 100 - Dissent)

p1 <- ggplot(data = prop, aes(x = `Dissent`)) +
    geom_histogram(colour = "black", fill = "red4") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                       limits = c(NA, 1),
                       labels = scales::percent) +
    scale_x_continuous(trans=scales::pseudo_log_trans(base = 10),
                       breaks = c(0, .01, .02, .05, .1, .2, .5, 1) * 100,
                       limits = c(NA, 100),
                       labels = function(.) scales::percent(. / 100, accuracy = 1)) +
    labs(
        title = "Dissent Score Distribution",
        subtitle = "For 56 Uniswap Proposals",
        x = "Dissent Score",
        y = "Count"
    )

ggsave(p1, filename = "dissent.png", dpi = 300, width = 10, height = 6)

p2 <- ggplot(data = prop, aes(x = `Consensus`)) +
    geom_histogram(colour = "black", fill = "green4") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_log10(breaks = c(.01, .02, .05, .1, .2, .5, 1) * 100,
                  limits = c(1, NA),
                  labels = function(.) scales::percent(. / 100, accuracy = 1)) +
    labs(
        title = "Consensus Score Distribution",
        subtitle = "For 56 Uniswap Proposals",
        x = "Consensus Score",
        y = "Count"
    )

ggsave(p2, filename = "consensus.png", dpi = 300, width = 10, height = 6)

write_csv(prop, "dissent_scores.csv")

dat %>%
    mutate(Rank = paste0("#", rank(`Dissent Score`, ties.method = "first")))
