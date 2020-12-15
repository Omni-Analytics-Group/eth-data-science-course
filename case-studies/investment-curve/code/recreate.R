library(tidyverse)
library(ggrepel)

ln <- read_csv("Investment_Bubble_Cycle_Line.csv") %>%
    mutate(Y = Y + 50 + 28)
cv <- read_csv("Investment_Bubble_Cycle.csv") %>%
    mutate(Y = Y + 50)

mapping = tibble(
    X = cv$X[c(20, 50, 70, 75, 105, 125, 138, 150, 175, 165, 185, 208, 225, 255, 280)],
    Label = c("Take off", "First Sell off", "Bear trap", "Media attention", "Enthusiasm",
              "Greed", "Delusion", "\"New Paradigm\"!!!", "Bull trap", "Denial",
              "Return to \"normal\"", "Fear", "Capitulation", "Despair", "Return to\nthe mean")
) %>%
    left_join(cv) %>%
    mutate(Y = ifelse(Label %in% c("Bear trap"), Y - 40, Y)) %>%
    mutate(Y = ifelse(Label %in% c("Media attention"), Y + 30, Y),
           X = ifelse(Label %in% c("Enthusiasm"), X - 50, X),
           X = ifelse(Label %in% c("Greed"), X - 30, X),
           X = ifelse(Label %in% c("Delusion"), X - 40, X),
           X = ifelse(Label %in% c("Denial"), X + 30, X),
           Y = ifelse(Label %in% c("Denial"), Y + 10, Y),
           X = ifelse(Label %in% c("Return to \"normal\""), X + 90, X),
           Y = ifelse(Label %in% c("\"New Paradigm\"!!!"), Y + 15, Y),
           Y = ifelse(Label %in% c("Return to\nthe mean"), Y - 30, Y),
           X = ifelse(Label %in% c("Fear"), X + 25, X),
           X = ifelse(Label %in% c("Capitulation"), X + 55, X),
           Y = ifelse(Label %in% c("Despair"), Y + 45, Y),
           X = ifelse(Label %in% c("Despair"), X + 18, X),
           Y = ifelse(Label %in% c("Bull trap"), Y - 30, Y))

verticals <- tibble(
    X = cv$X[c(20, 70, 150)],
)

sub_labels <- tibble(
    X = cv$X[c(8, 45, 92, 250)],
    lbl = c("Stealth Phase", "Awareness Phase", "Mania Phase", "Blow off Phase"),
    lbl2 = c("Smart Money", "Institutional Investors", "Public", NA)
)

p1 <- ggplot(data = ln, aes(x = X, y = Y)) +
    geom_line(linetype = "dashed") +
    geom_line(data = cv, colour = "red", size = 1.5)

p2 <- p1 + 
    geom_text(data = mapping, aes(label = Label), vjust = -1.5, fontface = "bold", size = 3.5)

p3 <- p2 + geom_vline(data = verticals, aes(xintercept = X), linetype = "dotted", colour = "grey80") +
    geom_text(data = sub_labels, aes(label = lbl, y = 10), fontface = "bold") +
    geom_text(data = sub_labels, aes(label = lbl2, y = 375), fontface = "bold", colour = "grey80", size = 6) +
    annotate("text", x = 425, y = 108, label = "Mean", angle = 4, fontface = "bold")

p4 <- p3 + theme_bw()

p5 <- p4 + theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank())

p6 <- p5 + theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank(), 
    axis.line = element_line(size = 1.5, colour = "grey55", arrow = grid::arrow(length = unit(0.3, "cm"))),
    axis.title = element_text(size = 16, face = "bold", hjust = 1),
    axis.title.y = element_text(angle = 0, margin = margin(l = 20, r = -80)),
    axis.title.x = element_text(hjust = 1.08, vjust = 4.5),
    plot.margin=unit(c(1.5,2.5,1.5,1.2),"cm"),
    panel.grid = element_blank()
) +
    labs(
        x = "Time",
        y = "Valuation"
    )

ggsave(p1, filename = "p1.png", dpi = 300, width = 10, height = 8)
ggsave(p2, filename = "p2.png", dpi = 300, width = 10, height = 6)
ggsave(p3, filename = "p3.png", dpi = 300, width = 10, height = 6)
ggsave(p4, filename = "p4.png", dpi = 300, width = 10, height = 6)
ggsave(p5, filename = "p5.png", dpi = 300, width = 10, height = 6)
ggsave(p6, filename = "p6.png", dpi = 300, width = 10, height = 8)

