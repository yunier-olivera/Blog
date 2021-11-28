library(vegan)
library(tidyverse)

#  Principal coordinates analysis of Bray Curtis distances of dune data
data(dune)
dune
d <- vegdist(t(dune))
ord <- wcmdscale(d, eig = TRUE)

plot(ord)

pcoa_p <- ord$points |> 
  as_tibble(rownames = "spp") |> 
  ggplot(aes(Dim1, Dim2)) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_hline(yintercept = 0, color = "grey") +
  geom_text(aes(label = spp), size = 3.5) +
   labs(tag = "A") +
  theme_minimal()


#  Shepard plot
p <- stressplot(ord)   

shepard_p <- left_join(enframe(d), enframe(p), by = "name") |> 
  ggplot(aes(value.x, value.y)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = 'dodgerblue') +
  labs(x = "Distancias reales", y = "Distancias en el PCoA", tag = "B") +
   xlim(0, 1) +
   ylim(0, 1) +
  theme_linedraw(base_size = 12) +
  theme(panel.grid = element_blank())

#  % explicado por los ejes del PCoA
100 * ord$eig / sum(ord$eig)
25.45376023 + 19.76131217


ggsave("posts/2021-10-31_PCoA/pcoa_plot.png",
       plot = pcoa_p, height = 4, width = 5,
       dpi = 320, units = "in")
ggsave("posts/2021-10-31_PCoA/shepard_plot.png",
       plot = shepard_p, height = 4, width = 6,
       dpi = 320, units = "in")
