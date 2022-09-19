library(ggplot2)
 
# Data
n = 200
set.seed(1)
a <- data.frame( x=rnorm(n, 10, 1.9), y=rnorm(n, 10, 1.2) )
b <- data.frame( x=rnorm(n, 14.5, 1.9), y=rnorm(n, 14.5, 1.9) )
c <- data.frame( x=rnorm(n, 9.5, 1.9), y=rnorm(n, 15.5, 1.9) )
data <- rbind(a,b,c)

# Show the area only
ggplot(data, aes(x=x, y=y) ) +
  stat_prob_2d(aes(fill = ..level.., color =..level.. ), geom = "polygon")

ggplot(data, aes(x=x, y=y) ) +
  stat_prob_2d(aes(fill = ..level..), geom = "polygon", color = "white")

ggplot(data, aes(x=x, y=y) ) +
  stat_prob_2d(aes(color =..level.. ), geom = "path")
