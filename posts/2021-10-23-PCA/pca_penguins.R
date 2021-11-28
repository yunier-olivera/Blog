
library(palmerpenguins)
library(vegan)
library(ggvegan)
library(tidyverse)
library(broom)
#library(xkcd)


data("penguins")
penguins

penguins <- penguins |> 
  filter(!is.na(bill_length_mm))
  

pca <- penguins |>
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) |> 
  rda(scale = T)


autoplot(pca)

x <- pca$CA$v |> as_tibble() |> mutate(D = c("bill_length_mm", "bill_depth_mm",
                                             "flipper_length_mm", "body_mass_g"))
x <- x |> mutate(PC1 = PC1 / 10,
                 PC2 = PC2 / 10)

p <- left_join(
  penguins |> rowid_to_column(),
  pca$CA$u |>as_tibble() |>  rowid_to_column()
) |> 
  ggplot(aes(PC1, PC2, color = species)) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(show.legend = FALSE) +
  geom_segment(data = x,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               size = 0.5, color = "gray30",
               arrow = arrow(length = unit(2, "mm")),
               show.legend = FALSE) +
  geom_text(data = x, aes(x = PC1, y = PC2, label = D),
            size = 3, color = "gray30",
            nudge_x = 0.03,
            show.legend = FALSE) +
  theme_minimal()


ggsave("pca_penguins.png", p, height = 4, width = 4, dpi = 320, units = "in")

