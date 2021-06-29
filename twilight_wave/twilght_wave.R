library(tidyverse)
library(ggforce)


cosine.curves <- 
lapply(1:11, function(i){
  
  sq <- seq(5, 0, by = -0.5)
  a <- sq[i] + 0
  b <- sq[i] + 7
  
  data.frame(
    y = c(-1, seq(-1, 1, length.out=100), 1),
    x = cos(c(a*pi, seq(a*pi, b*pi, length=100), b*pi))*1/14 + (i/6.5),
    gr = as.character(i)
  )
  
}) 

rescale_x <- mean(range(sapply(cosine.curves, function(z) range(z$x))))

cosine.curves <- lapply(cosine.curves, function(z){
  z$x <- z$x - rescale_x
  
  z
  })
  


pol.list <- 
lapply(1:11, function(i){
  
  xlim <- 1
  
  if(i < 6) {
    df <- cosine.curves[[i]]
    df[c(1, nrow(df)), "x"] <- -xlim
    df <- df %>% mutate(col = "white")
  }
  
  if(i > 6) {
    df <- cosine.curves[[i]]
    df[c(1, nrow(df)), "x"] <- xlim
    df <- df %>% mutate(col = "white")
  }
  
  if(i == 6) {
    df.l <- cosine.curves[[i]]
    df.l[c(1, nrow(df.l)), "x"] <- -xlim
    df.l <- df.l %>% 
      mutate(
        gr = paste0("l_", gr),
        col = "#a396a7")
    
    df.r <- cosine.curves[[i]]
    df.r[c(1, nrow(df.r)), "x"] <- xlim
    df.r <- df.r %>% 
      mutate(
        gr = paste0("r_", gr),
        col = "#81b4ab") 
    
    df <- bind_rows(df.l, df.r)
  }
  
  df
}) 


side.df <- 
  pol.list[-c(6)] %>% 
  bind_rows()

center.df <- 
  pol.list[c(6)] %>% 
  bind_rows()

suns_over_waves <- 
ggplot() +
  geom_polygon(
    data = center.df, 
    aes(x, y, group = gr),
    fill = center.df$col) +
  geom_polygon(
    data = side.df, 
    aes(x, y, group = gr),
    alpha = .2, fill = side.df$col) +
  geom_circle(
    aes(
      x0 = rep(0, 7), 
      y0 = seq(-.85, .85, length.out = 7),
      r = 0.07), 
    fill = "#eca63c", col = NA
  ) +
  coord_fixed(
    ylim = c(-.85, 0.85)
  ) +
  theme_void()

ggsave(
  filename = "twilight_wave/suns_over_waves.png",
  suns_over_waves,
  height = 4,
  width = 4
)


# Comparion with the original  --------------------------------------------

library(magick)


img_new <- image_trim(image_read("twilight_wave/suns_over_waves.png"))

image_write(img_new, "twilight_wave/suns_over_waves_2.png")

img_ori <- image_read("twilight_wave/twilight_wave_original.png")
