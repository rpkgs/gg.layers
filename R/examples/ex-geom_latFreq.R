library(ggplot2)
# the lattice version: https://github.com/CUG-hydro/Liu2021-JGRA-ET_trends_attribution

# f <- "data-raw/AridityIndex_MSWEP-prcp_div_GLEAM-Ep_1980-2020.tif"
# r <- terra::rast(f)
# d <- as.data.table(r, xy = TRUE) |> set_names(c("x", "y", "z"))
d = AridityIndex

# make_latFreq(d$y, d$z, debug = TRUE, zlim = c(-2, 2), is_spatial = TRUE)
# brks <- c(-Inf, 0.05, 0.2, 0.5, 0.65, Inf)
brks <- c(-Inf, 0.05, 0.2, 0.5, 0.65, 1:5, 20, Inf)

nbrk <- length(brks) - 1
cols <- rcolors::get_color("amwg256", nbrk) |> rev()

p = ggplot(d, aes(x, y, z = z)) + 
  geom_raster_filled(breaks = brks) + 
  geom_latFreq(options = list(is_spatial = TRUE, zlim = c(-1, 1)*10), 
    bbox = c(190, 240, -60, 90)) + 
  coord_cartesian(xlim = c(-180, 240), ylim = c(-60, 90), expand = FALSE, clip = "on") +
  scale_x_continuous(limits = c(-180, 240), breaks = seq(-180, 180, 60)) + 
  scale_fill_manual(values =cols) + 
  labs(x = NULL, y = NULL)
p
