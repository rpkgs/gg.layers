library(grid)

brks  <- c(-Inf, seq(110, 150, 5), Inf)
key <- list(
    at = brks, 
    space = 'right', height = 1,
    tri.upper = 0.05,  tri.lower = 0.05, 
    labels=list(cex=1.2, fontface='bold'), 
    rect = list(col = "black", lwd = 0.2)
)

g <- draw.colorkey2(key)
g

## sp
\dontrun{
library(sp)
demo(meuse, ask = FALSE, echo = FALSE)
spplot(meuse, c("ffreq"), col.regions= "black", 
       pch=c(1,2,3), key.space=list(x=0.1,y=.95,corner=c(0,1)))
spplot(meuse.grid)
}
