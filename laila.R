library(ggplot2)
library(dplyr)
library(magick)
library(paletteer) 
library(ggridges)

# https://github.com/paezha/genuary2022/tree/master/06-trade-styles

# Photograph of the Finnish vocalist Laila Kinnunen (1939–2000).
# Author: Kuvasiskot
# Collection: Finnish Heritage Agency
# Historian kuvakokoelma, Studio Kuvasiskojen kokoelma

img <- image_read("https://upload.wikimedia.org/wikipedia/commons/7/79/Laila-Kinnunen-1965.jpg") %>%
  image_convert(colorspace = "gray") %>%
  image_trim()

img_w <- image_info(img)$width
img_h <- image_info(img)$height
img_ratio <- img_w / img_h

if (img_w >= img_h) {
  img <- image_resize(img, "160")
} else {
  img <- image_resize(img, ("x160"))
}

img_array <- drop(as.integer(img[[1]]))
rownames(img_array) <- 1:nrow(img_array)
colnames(img_array) <- 1:ncol(img_array)

img_df <- as.data.frame.table(img_array) %>% 
  `colnames<-`(c("y", "x", "b")) %>% 
  mutate(
    across(everything(), as.numeric),
    n = row_number()
  ) %>%
  filter(n %% 2 == 0)

col_palette <- paletteer_d("colRoz::m_horridus")
col_fill <- col_palette[5]

ggplot(img_df) +
  geom_rect(aes(xmin = 0 + 2, xmax = max(x),
                ymin = 0 - 2, ymax = max(y)),
            fill = col_fill) +
  geom_ridgeline_gradient(aes(x, 
                              y, 
                              height = b/50,
                              group = y, 
                              fill = b), 
                          color = col_palette[1],
                          size = 0.35) +
  scale_y_reverse() +
  scale_fill_gradientn(colours = rev(col_palette)) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")  


ggsave("laila.png", dpi = 320, width = 7, height = 7 / img_ratio)





