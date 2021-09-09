#+ setup, warning=FALSE, message=FALSE, fig.width=10, fig.height=8, echo=FALSE

## Libraries
library(tidyverse)
library(lubridate)
library(tidytext)
library(ggmap)
library(ggrepel)
library(maps)
library(rworldmap)
library(cowplot)
library(RColorBrewer)
library(omnitheme)
library(emojifont)

## Utility Function
coords2country = function(points) {  
    countriesSP <- getMap(resolution='low')
    #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
    
    # convert our list of points to a SpatialPoints object
    
    # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
    
    #setting CRS directly to that from rworldmap
    pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
    
    
    # use 'over' to get indices of the Polygons object containing each point 
    indices = over(pointsSP, countriesSP)
    
    # return the ADMIN names of each country
    indices$ADMIN  
    #indices$ISO3 # returns the ISO3 code 
    #indices$continent   # returns the continent (6 continent model)
    #indices$REGION   # returns the continent (7 continent model)
}

## Data
scrape_date <- ymd("2018-10-25")
jobs <- read_csv("data/Crypto_Jobs_data.csv")
load("data/geocode_adr.RData")

## World Map Data
world <- map_data("world")
world <- world[world$region != "Antarctica",]

## Map of Locations
register_google(key = "AIzaSyAINXlwbP7h16oZ8iflQBedCocVR4h6YNM")
geocode_adr <- geocode(unique(jobs$Location)[!is.na(unique(jobs$Location))])
# save(geocode_adr, file = "data/geocode_adr.RData")


## Jobs over Time
jobs_weekly <- jobs %>%
    group_by(Week = floor_date(Time, "week")) %>%
    summarise(Count = n()) %>%
    arrange(Week) %>%
    mutate(Outlier = (Count > 38))

## Weekly Jobs over Time
p1 <- ggplot(data = jobs_weekly, aes(x = Week, y = Count)) +
    geom_point() +
    geom_line() +
    geom_text(aes(label = Count), vjust = -0.5, hjust = 0, data = jobs_weekly %>% filter(Outlier)) +
    geom_smooth() +
    xlab("Date") +
    ylab("Job Postings") +
    theme_minimal(14) +
    labs(
        title = "Weekly Count of Crypto Job Postings",
        subtitle = paste0("For jobs available on the site as of ", scrape_date)
    )
p1

## World Map
p2 <- ggplot(data = world, aes(x = long, y = lat)) +
    geom_map(map = world, aes(map_id = region), fill = "grey90", colour = "grey60") +
    geom_point(data = geocode_adr, aes(x = lon, y = lat), colour = "#e6550d", alpha = 0.7, size = 1.5) +
    theme_map() +
    coord_equal() +
    labs(
        title = "Map of Job Postings by Location",
        subtitle = paste0("For jobs available on Crypto.jobs as of ", scrape_date)
    ) +
    theme(
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)
    ) +
    watermark_img(system.file("images", "OAG_CLR_web_big.png", package = "omnitheme"), location = "bl", alpha = .35, width = 120)
p2

## Geocode the Addresses
geocode_adr_complete <- geocode_adr %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    mutate(Country = coords2country(.)) %>%
    mutate(Country2 = ifelse(Country == "United States of America", "USA", as.character(Country))) %>%
    mutate(Country2 = ifelse(Country2 == "United Kingdom", "UK", as.character(Country2)))

all_counts <- geocode_adr_complete %>%
    group_by(Country2) %>%
    summarise(Count = n()) %>%
    mutate(Interval = as.character(cut(Count, breaks = c(-Inf, 0, 1, 3, 8, 16, Inf),
                          labels = c("0", "1", "2-3", "4-8", "9-16", ">16"))))

world_merge <- world %>%
    left_join(all_counts, by = c("region" = "Country2")) %>%
    mutate(Interval = ifelse(is.na(Interval), "0", Interval)) %>%
    mutate(Interval = factor(Interval, levels = c("0", "1", "2-3", "4-8", "9-16", ">16")))

ors <- c("white", brewer.pal(n = 6, name = "Oranges")[1:5])

## World Map
p21 <- ggplot(data = world_merge, aes(x = long, y = lat)) +
    geom_map(map = world, aes(map_id = region, fill = Interval), colour = "grey60") +
    #geom_point(data = geocode_adr, aes(x = lon, y = lat), colour = "red", alpha = 0.7, size = 1.5) +
    scale_fill_manual("Job Count", values = ors) +
    theme_map() +
    coord_equal() +
    labs(
        title = "Map of Job Postings by Location",
        subtitle = paste0("For jobs available on Crypto.jobs as of ", scrape_date)
    ) +
    theme(
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        legend.position = "bottom"
    ) +
    watermark_img(system.file("images", "OAG_CLR_web_big.png", package = "omnitheme"), location = "bl", alpha = .35, width = 120)
p21

## Bind the locations to the original data
jobs_location <- jobs %>%
    left_join(
        geocode_adr %>%
            cbind(Location = unique(jobs$Location)[!is.na(unique(jobs$Location))]) %>%
            left_join(geocode_adr_complete)
    , by = c("Location" = "Location"))

## Unnest the words
jobs_twograms <- jobs_location %>%
    mutate(Name = gsub(" of | and | a |^a ", "", tolower(Name))) %>%
    unnest_tokens(word, Name, token = "ngrams", n = 2, n_min = 2)
jobs_words <- jobs_location %>%
    mutate(Name = gsub(" of | and | end | a |^a ", "", tolower(Name))) %>%
    unnest_tokens(word, Name) 

country_count <- jobs_location %>%
    group_by(Country) %>%
    summarise(Count = n())

counts <- as.data.frame(table(jobs_words$word)) %>% mutate(Type = "one")
twocounts <- as.data.frame(table(jobs_twograms$word)) %>% mutate(Type = "two")

mydat <- rbind(counts, twocounts) %>%
    rename(Word = Var1, Count = Freq) %>%
    arrange(desc(Count))

## Plot word counts by country
p31 <- ggplot(data = filter(mydat, Type == "one") %>% arrange(desc(Count)) %>% slice(1:20) %>% mutate(Word = factor(Word, levels = Word)), aes(x = Word, y = Count, fill = Count)) +
    geom_bar(stat = "identity", colour = "black") +
    scale_fill_gradient(low = "#bcbddc", high = "#756bb1") +
    scale_y_continuous(breaks = seq(0, 600, by = 50)) +
    theme_minimal(14) +
    theme(
        legend.position = "off",
        axis.text.x = element_text(angle = 20, hjust = 1)
    ) +
    labs(
        title = "Top 20 Words in Job Titles",
        subtitle = paste0("From Crypto.jobs Job Postings as of ", scrape_date)
    ) +
    watermark_img(system.file("images", "OAG_CLR_web_big.png", package = "omnitheme"), location = "tr", alpha = .35, width = 120)
p31

## Plot word counts by country
p32 <- ggplot(data = filter(mydat, Type == "two") %>% arrange(desc(Count)) %>% slice(1:20) %>% mutate(Word = factor(Word, levels = Word)), aes(x = Word, y = Count, fill = Count)) +
    geom_bar(stat = "identity", colour = "black") +
    scale_fill_gradient(low = "#9ecae1", high = "#3182bd") +
    scale_y_continuous(breaks = seq(0, 160, by = 20)) +
    theme_minimal(14) +
    theme(
        legend.position = "off",
        axis.text.x = element_text(angle = 20, hjust = 1),
        plot.margin = margin(10, 10, 10, 50)
    ) +
    labs(
        title = "Top 20 Two-Grams in Job Titles",
        subtitle = paste0("From Crypto.jobs Job Postings as of ", scrape_date)
    ) +
    xlab("Two-Gram") +
    watermark_img(system.file("images", "OAG_CLR_web_big.png", package = "omnitheme"), location = "tr", alpha = .35, width = 120)
p32

## Create word count data
word_count <- jobs_words %>%
    filter(Country %in% as.character(unlist(country_count[country_count$Count >= 20, "Country"]))) %>%
    group_by(Country, word) %>%
    summarise(Count = n()) %>%
    filter(!is.na(Country)) %>%
    group_by(Country) %>%
    arrange(desc(Count)) %>%
    slice(1:5) %>%
    mutate(word = factor(word, labels = paste0(Country, "__", word))) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(word = factor(word, levels = word),
           word2 = factor(word, labels = gsub(".*__(.*)", "\\1", word)))
twogram_count <- jobs_twograms %>%
    filter(Country %in% as.character(unlist(country_count[country_count$Count >= 20, "Country"]))) %>%
    group_by(Country, word) %>%
    summarise(Count = n()) %>%
    filter(!is.na(Country)) %>%
    group_by(Country) %>%
    arrange(desc(Count)) %>%
    slice(1:5) %>%
    mutate(word = factor(word, labels = paste0(Country, "__", word))) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(word = factor(word, levels = word),
           word2 = factor(word, labels = gsub(".*__(.*)", "\\1", word)))

## Plot word counts by country
p4 <- expr(ggplot(data = word_count, aes(x = word, y = Count, fill = Country)) +
    geom_bar(stat = "identity", colour = "black") +
    scale_x_discrete(labels = function(.) gsub(".*__(.*)", "\\1", .)) +
    scale_fill_brewer(palette = "Set3") +
    facet_wrap(~Country, scales = "free") +
    theme_minimal(14) +
    theme(
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "off",
        plot.margin = margin(10, 10, 10, 60)
    ) +
    labs(
        title = "Top Five Words by Country for Job Titles",
        subtitle = paste0("For Countries with at least 20 Postings on Crypto.jobs as of ", scrape_date)
    ))
p4s <- eval(p4)
word_count <- word_count %>%
    arrange(Count) %>%
    mutate(word = forcats::fct_rev(word))

p4f <- eval(p4) +
    theme(
        axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "off"
    ) +
    coord_flip()
p4f
p41 <- expr(ggplot(data = twogram_count, aes(x = word, y = Count, fill = Country)) +
    geom_bar(stat = "identity", colour = "black") +
    scale_x_discrete(labels = function(.) gsub(".*__(.*)", "\\1", .)) +
    scale_fill_brewer(palette = "Set3") +
    facet_wrap(~Country, scales = "free") +
    theme_minimal(14) +
    theme(
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "off",
        plot.margin = margin(10, 10, 10, 60)
    ) +
    labs(
        title = "Top Five 2-Grams by Country for Job Titles",
        subtitle = paste0("For Countries with at least 20 Postings on Crypto.jobs as of ", scrape_date)
    ) )
p41s <- eval(p41)

twogram_count <- twogram_count %>%
    arrange(Count) %>%
    mutate(word = forcats::fct_rev(word))
p41f <- eval(p41) +
    theme(
        axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "off"
    ) +
    coord_flip()
p41f

###
### Compensation
###
jobs_comp <- jobs %>%
    mutate(Compensation = tolower(Compensation),
           Compensation = gsub(" ", "", Compensation),
           Compensation = gsub("to", "-", Compensation)) %>%
    slice(grep("USD|\\$", Compensation)) %>%
    mutate(CleanComp = gsub("[^0-9–-]", "", Compensation),
           Equity = grepl("Equity|equity", Compensation),
           Thousands = grepl("[0-9]+k", Compensation),
           Hourly = grepl("/hour|/h|/ hour|/ h", Compensation),
           Monthly = grepl("/month|/ month|amonth|/m|/ m", Compensation),
           Range = grepl("-", CleanComp),
           Low = as.numeric(sapply(strsplit(CleanComp, "-"), `[`, 1)),
           High = as.numeric(sapply(strsplit(CleanComp, "-"), `[`, 2)),
           TempComp = as.numeric(ifelse(is.na(High), CleanComp, (Low + High) / 2)),
           TempComp2 = ifelse(Thousands, TempComp * 1000, TempComp),
           FinalComp = ifelse(Monthly, 12 * TempComp2, ifelse(Hourly, 40 * 52 * TempComp2, TempComp2))) %>%
    select(Name, Company, Type, Field, Compensation, FinalComp, Equity) %>%
    arrange(desc(FinalComp)) %>%
    mutate(FinalComp = c(290000, 290000, 75000, tail(FinalComp, -3))) %>%
    filter(!is.na(FinalComp), FinalComp > 100)

jobs_aggcomp <- jobs_comp %>%
    group_by(Field) %>%
    summarise(Compensation = median(FinalComp),
              Count = n(), 
              Equity = mean(Equity)) %>%
    arrange(desc(Compensation)) %>%
    filter(Count > 1) %>%
    ungroup() %>%
    mutate(Field = factor(Field, levels = unique(Field)))

jobs_considered <- jobs_comp %>%
    filter(Field %in% jobs_aggcomp$Field) %>%
    mutate(Field = factor(Field, levels = unique(jobs_aggcomp$Field))) %>%
    group_by(Field) %>%
    mutate(Label = ifelse(FinalComp == max(FinalComp), paste0(Company, ": ", Name), NA)) %>%
    ungroup() %>%
    mutate(Label = ifelse(Field %in% c("Design", "Business Development", "Tech", "Marketing"), Label, NA))

p5 <- ggplot(data = jobs_considered, aes(x = Field, y = FinalComp, fill = Field)) +
    #geom_bar(stat = "identity", position = "dodge", colour = "black") +
    geom_boxplot() +
    geom_label_repel(aes(label = Label), colour = "white") +
    scale_fill_brewer(palette = "Dark2", drop = FALSE) +
    scale_y_continuous(labels = scales::dollar, breaks = seq(0, 350000, by = 50000), limits = c(0, 350000)) +
    theme_minimal(14) +
    theme(legend.position = "off") +
    labs(title = "Distribution of Job Compensation by Field",
         subtitle = paste0("For Crypto.jobs postings as of ", scrape_date, " with compensation reported in USD"),
         y = "Compensation (USD)") +
    watermark_img(system.file("images", "OAG_CLR_web_big.png", package = "omnitheme"), location = "tr", alpha = .35, width = 120)
p5  

###
### Skills
###
#install.packages("ggwordcloud")
library(ggwordcloud)

jobs_skills <- jobs %>%
    mutate(Field = factor(Field, levels = unique(jobs$Field))) %>%
    group_by(Field) %>%
    mutate(Skill_list = gsub("(good)|(great)|(skills)", "", tolower(Skills))) %>% #Remove 'stop words'
    mutate(Skill_list = gsub("(communications)", "(communication)", Skill_list)) %>% #Remove typos
    mutate(Skill_list = strsplit(Skill_list, ",|;|\\. | & | and | or ")) %>%
    unnest() %>%
    mutate(Skill_list = str_trim(Skill_list))

jobs_skills_agg <- jobs_skills %>%
    filter(!is.na(Skill_list), Skill_list != "") %>%
    group_by(Field, Skill_list) %>%
    summarise(Count = length(Skill_list)) %>%
    arrange(desc(Count)) %>%
    group_by(Field) %>%
    mutate(Count = (Count - min(Count)) / (max(Count) - min(Count))) %>%
    slice(1:25) %>%
    ungroup() %>%
    mutate(Field = paste(Field, rep(c("\U0001f5fa", "\U0001f469\U0001f4bb", "\U0001f574", "\U0001f4bc",
                                  "\U0001f4de", "\U0001f5e3", "\U0001f4d6", "\U0001f4ca", "\U0001f468\U0001f3a8"), each = 25)))

col_palette <- colorRampPalette(brewer.pal(8, "Set1"))(9)

load.emojifont("EmojiOne.ttf")

p61 <- ggplot(data = jobs_skills_agg, aes(label = Skill_list, size = Count, color = Field)) +
   # geom_text_wordcloud() +
    scale_color_manual(values = col_palette) +
    scale_size_continuous(range = c(2, 22)) +
  #  scale_size_area(max_size = 10) +
    geom_text_wordcloud_area(area_corr_power = 1, shape = "square", xlim = c(0, 1), ylim = c(0, 1), rm_outside = TRUE) +
    geom_rect(data = jobs_skills_agg %>% group_by(Field) %>% slice(1), colour = NA, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, aes(fill = Field), alpha = 0.15) +
    scale_fill_manual(values = col_palette) +
    theme_minimal(14) +
    facet_wrap(~Field, scales = "free") +
    theme(
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", colour = "#656373"),
        strip.text = element_text(face = "bold"),
        legend.position = "off"
    ) +
    labs(title = "Skills Required by Field",
         subtitle = paste0("For Crypto.jobs postings as of ", scrape_date, " "))

p6 <- ggplot(data = jobs_skills_agg, aes(x = Skill_list, y = Count, fill = Field)) +
    geom_bar(stat = "identity", colour = "black") +
 #   scale_fill_brewer(palette = "Dark2", drop = FALSE) +
    theme_minimal(14) +
    theme(legend.position = "off") +
    labs(title = "Skills Required by Field",
         subtitle = paste0("For Crypto.jobs postings as of ", scrape_date, " "),
         y = "Count of Occurrence", 
         x = "Skill") +
    facet_wrap(~Field, scales = "free") # +
  #  watermark_img(system.file("images", "OAG_CLR_web_big.png", package = "omnitheme"), location = "tr", alpha = .35, width = 120)
p6
ggsave(p2, filename = "output_images/world_map_locations.png", dpi = 300, width = 14, height = 8)
ggsave(p21, filename = "output_images/world_map_fill.png", dpi = 300, width = 14, height = 8)

ggsave(p3, filename = "output_images/two_grams_country.png", dpi = 300, width = 14, height = 8)
ggsave(p31, filename = "output_images/one_grams.png", dpi = 300, width = 14, height = 8)
ggsave(p32, filename = "output_images/two_grams.png", dpi = 300, width = 14, height = 8)

ggsave(p4s, filename = "output_images/one_grams_country.png", dpi = 300, width = 14, height = 8)
ggsave(p41s, filename = "output_images/two_grams_country.png", dpi = 300, width = 14, height = 8)

ggsave(p4f, filename = "output_images/one_grams_country_flipped.png", dpi = 300, width = 14, height = 8)
ggsave(p41f, filename = "output_images/two_grams_country_flipped.png", dpi = 300, width = 17, height = 8)

ggsave(p5, filename = "output_images/compensation_by_field.png", dpi = 300, width = 14, height = 8)

ggsave(p61, filename = "output_images/skills_by_field.png", dpi = 300, width = 14, height = 8)
