library(googlesheets4)
library(tidyverse)
library(magick)
library(uwot)
library(ggimage)
library(ggthemes)


dist_mat <- hashmasks_raw %>%
  select(ID, Skin, Character, Eyes, Mask, Item, Rarity) %>%
  mutate(File = gsub(".png", ".jpg", basename(hashmasks_raw$Preview))) %>%
  filter(File %in% dir("hashmasks1")) %>%
  arrange(Skin, Character, Eyes, Mask, Item, Rarity)

mylst <- list()
for (i in 1:nrow(dist_mat)) {
  print(i)
  x <-image_read(file.path("hashmasks1", paste0(dist_mat$File[i])))
  mydat <- image_data(x)
  mylst[[length(mylst) + 1]] <- as.numeric(c(mydat))
}

image_df <- do.call(rbind, mylst)

image_umap <- umap(image_df, n_components = 3) %>%
  as_tibble() %>%
  mutate(File = file.path("hashmasks", dist_mat$File[1:length(mylst)]))

p <- ggplot(image_umap, aes(x = `V1`, y = `V2`)) +
  geom_image(aes(image = File), size=.01) +
  theme_map() +
  theme(plot.title = element_text(size = 36, hjust = 0.5, face = "bold")) +
  labs(
    title = "A UMAP Clustering of the Hashmask Images"
  )
ggsave(p, filename = "hashmask_umap.png", dpi = 300, width = 25, height = 25)