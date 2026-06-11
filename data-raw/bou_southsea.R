## code to prepare `bou_southsea` dataset goes here
## Source: ChinaBound2024, the "十段线" (ten-segment line) national boundary
## in the South China Sea region.
library(sf)

bou_southsea <- read_sf("Z:/ShapeFiles/ChinaBound2024/shp/bou1_4l_十段线.shp")
bou_southsea <- bou_southsea["geometry"] # keep only geometry

usethis::use_data(bou_southsea, overwrite = TRUE)
