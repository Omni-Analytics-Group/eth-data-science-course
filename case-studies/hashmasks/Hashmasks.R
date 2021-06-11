library(tidyverse)
library(RColorBrewer)

hashmasks_raw <- read_csv('All Hashmasks.csv')

hashmasks <- hashmasks_raw %>%
  select(-X2, -URL, -Preview)

skincolor <-  c(Freak = "#516818", Dark = "#613F04", Light = "#FACD7F", 
                Gold = "gold", Wood = "#DEA034", Gray = "#3F4756", 
                Steel = "#8B7D64", Blue = "#20407B", Mystical = "#E5CAE5", Transparent = "#F8F4F8")

hashmasks %>%
  ggplot(aes(x = Skin, fill = Skin))+
  geom_bar(color = "black")+
  scale_y_continuous(breaks = scales::pretty_breaks(n=20), expand = c(0,0), limits = c(0,4800))+
  geom_text(aes(label= scales::percent(..count../sum(..count..))), stat= "count", vjust = -0.25)+
  theme(legend.position = "OFF")+
  labs(title = "Distribution of Hashmasks' Skin Types")+
  scale_fill_manual(values = skincolor)

hashmasks %>%
  ggplot(aes(x = Character, fill = Character))+
  geom_bar(color = "black")+
  scale_y_continuous(breaks = scales::pretty_breaks(n=20), expand = c(0, 0), limits = c(0,9000))+
  geom_text(aes(label= scales::percent(..count../sum(..count..))), stat= "count", vjust = -0.25)+
  theme(legend.position = "OFF")+
  labs(title = "Distribution of Hashmasks' Character Types")+
  scale_fill_brewer(palette = "Set1")

eyescolor <-  c(Freak = "#D34818", Dark = "#47585E", Green = "#9AC828", 
               Blue = "#289FC8", Glass = "gold", Painted = "black", 
               Mystical = "#F2C623", Heterochromatic = "red")
hashmasks %>%
  ggplot(aes(x = Eyes, fill = Eyes))+
  geom_bar(color = "black")+
  scale_y_continuous(breaks = scales::pretty_breaks(n=20), expand = c(0, 0), limits = c(0, 8000))+
  geom_text(aes(label= scales::percent(..count../sum(..count..))), stat= "count", vjust = -0.25)+
  theme(legend.position = "OFF")+
  labs(title = "Distribution of Hashmasks' Eyes Types")+
  scale_fill_manual(values = eyescolor)

cols <- colorRampPalette(brewer.pal(8, "Set1"))
mymask <- cols(length(unique(hashmasks$Mask)))

hashmasks %>%
  ggplot(aes(x = Mask, fill = Mask))+
  geom_bar(color = "black")+
  scale_y_continuous(breaks = scales::pretty_breaks(n=20), expand = c(0, 0), limits = c(0,2600))+
  geom_text(aes(label= scales::percent(..count../sum(..count..))), stat= "count", vjust = -0.25)+
  theme(legend.position = "OFF")+
  labs(title = "Distribution of Hashmasks' Mask Types")+
  scale_fill_manual(values = mymask)
  
hashmasks %>%
  ggplot(aes(x = Item, fill = Item))+
  geom_bar(color = "black")+
  scale_y_continuous(breaks = scales::pretty_breaks(n=20), expand = c(0, 0), limits = c(0,15000))+
  geom_text(aes(label= scales::percent(..count../sum(..count..))), stat= "count", vjust = -0.25)+
  theme(legend.position = "OFF")+
  labs(title = "Distribution of Hashmasks' Item Types")+
  scale_fill_brewer(palette = "Set1")

hashmasks %>%
  ggplot(aes(x = Rarity))+
  geom_histogram(fill = "#EA5600", colour = "grey60")+
  scale_y_continuous(breaks = scales::pretty_breaks(n=20))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))+
  labs(title = "Distribution of Rarity")

hashmasks %>%
  ggplot(aes(x = Rarity,fill = Skin))+
  geom_histogram()+
  scale_y_continuous(breaks = scales::pretty_breaks(n=20))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))+
  labs(title = "Distribution of Rarity")+
  facet_wrap(~Skin)

hashmasks %>%
  ggplot(aes(x = Rarity,fill = Character))+
  geom_histogram()+
  scale_y_continuous(breaks = scales::pretty_breaks(n=20))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))+
  labs(title = "Distribution of Rarity")+
  facet_wrap(~Character)

hashmasks %>%
  ggplot(aes(x = Rarity,fill = Eyes))+
  geom_histogram()+
  scale_y_continuous(breaks = scales::pretty_breaks(n=20))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))+
  labs(title = "Distribution of Rarity")+
  facet_wrap(~Eyes)

hashmasks %>%
  ggplot(aes(x = Rarity,fill = Mask))+
  geom_histogram()+
  scale_y_continuous(breaks = scales::pretty_breaks(n=20))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))+
  labs(title = "Distribution of Rarity")+
  facet_wrap(~Mask)



hashmasks %>%
  arrange(Rarity) %>%
  mutate(Index = 1:nrow(.)) %>%
  ggplot(aes(x=Index, y= Rarity, color = Rarity))+
  geom_line(size = 2)+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10), expand = c(0,0))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=20), expand = c(0,0),limits = c(0, 16500))+
  labs(title = "Hashmasks Rarity Curve",
       y= "Rarity Score")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(legend.position = "OFF")

m1 <- lm(Rarity ~ relevel(as.factor(Skin), ref = "Gray")+
           relevel(as.factor(Character), ref = "Male")+
           relevel(as.factor(Eyes), ref = "Dark")+
           relevel(as.factor(Mask), ref = "Mexican")+
           relevel(as.factor(Item), ref = "No Item"), data = hashmasks)
summary(m1)

GoldenRobot <- hashmasks %>% 
  filter(Character == 'Golden Robot')

Mystical <- hashmasks %>%
  filter(Character == "Mystical")

Puppet <- hashmasks %>%
  filter(Character == "Puppet")

Robot <- hashmasks %>%
  filter(Character == "Robot")


ggplot(hashmasks, aes(x = Skin, y = Rarity, fill = Skin))+
  geom_boxplot()+
  labs(title = "Boxplot of Rarity vs Skin")+
  scale_fill_manual(values = skincolor)+
  theme(legend.position = "OFF")+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))

ggplot(hashmasks, aes(x = Character, y = Rarity, fill = Character))+
  geom_boxplot()+
  labs(title = "Boxplot of Rarity vs Character")+
  scale_fill_brewer(palette = "Set1")+
  theme(legend.position = "OFF")+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))

ggplot(hashmasks, aes(x = Eyes, y = Rarity, fill = Eyes))+
  geom_boxplot()+
  labs(title = "Boxplot of Rarity vs Eyes")+
  scale_fill_manual(values = eyescolor)+
  theme(legend.position = "OFF")+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))

ggplot(hashmasks, aes(x = Mask, y = Rarity, fill = Mask))+
  geom_boxplot()+
  labs(title = "Boxplot of Rarity vs Mask")+
  scale_fill_manual(values = mymask)+
  theme(legend.position = "OFF")+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))

ggplot(hashmasks, aes(x = Item, y = Rarity, fill = Item))+
  geom_boxplot()+
  labs(title = "Boxplot of Rarity vs Item")+
  scale_fill_brewer(palette = "Set1")+
  theme(legend.position = "OFF")+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))

hashmasks %>%
  filter(!is.na(Rarity)) %>%
  group_by(Skin) %>%
  summarise(Mean = mean(Rarity), Median = median(Rarity), Min = min(Rarity), Max = max(Rarity))

hashmasks %>%
  filter(!is.na(Rarity)) %>%
  group_by(Character) %>%
  summarise(Mean = mean(Rarity), Median = median(Rarity), Min = min(Rarity), Max = max(Rarity))

hashmasks %>%
  filter(!is.na(Rarity)) %>%
  group_by(Eyes) %>%
  summarise(Mean = mean(Rarity), Median = median(Rarity), Min = min(Rarity), Max = max(Rarity))

hashmasks %>%
  filter(!is.na(Rarity)) %>%
  group_by(Mask) %>%
  summarise(Mean = mean(Rarity), Median = median(Rarity), Min = min(Rarity), Max = max(Rarity))

hashmasks %>%
  filter(!is.na(Rarity)) %>%
  group_by(Item) %>%
  summarise(Mean = mean(Rarity), Median = median(Rarity), Min = min(Rarity), Max = max(Rarity))

A <- hashmasks %>%
  filter(!is.na(Rarity)) %>%
  group_by(Skin, Character, Eyes, Mask, Item) %>%
  summarise(Mean = mean(Rarity), Median = median(Rarity), Min = min(Rarity), Max = max(Rarity), Count=n())

m3 <- lm(Rarity ~ 
           relevel(as.factor(Skin_Character_Eyes), ref = "Gray_Male_Dark")+
           relevel(as.factor(Mask), ref = "Mexican")+
           relevel(as.factor(Item), ref = "No Item"), data = A)

model <- lm(Rarity ~ relevel(as.factor(Skin), ref = "Gray")+
              relevel(as.factor(Character), ref = "Male")+
              relevel(as.factor(Eyes), ref = "Dark")+
              relevel(as.factor(Mask), ref = "Mexican")+
              relevel(as.factor(Item), ref = "No Item"), 
            data = hashmasks)
