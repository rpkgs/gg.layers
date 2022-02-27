x <- seq(pi/4, 5 * pi, length.out = 100)
y <- seq(pi/4, 5 * pi, length.out = 100)
r <- as.vector(sqrt(outer(x^2, y^2, "+")))
grid <- expand.grid(x=x, y=y)
grid$z <- cos(r^2) * exp(-r/(pi^3))
at = c(0.1, 0.2, 0.5, 1, Inf) %>% c(-rev(.), 0, .)
levelplot(z ~ x * y, grid, cuts = 50, scales=list(log="e"), xlab="",
        ylab="", main="Weird Function", sub="with log scales",
        at = at,
        colorkey = TRUE, region = TRUE) +
    theme_lattice(plot.margin = c(1, 4, 1, 0))
