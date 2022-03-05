library(ggplot2)

p <- ggplot(mpg, aes(class, hwy))
p + geom_boxplot2()
# Orientation follows the discrete axis
# ggplot(mpg, aes(hwy, class)) + geom_boxplot2()

p + geom_boxplot2(notch = TRUE)
p + geom_boxplot2(varwidth = TRUE)
p + geom_boxplot2(fill = "white", colour = "#3366FF")
# By default, outlier points match the colour of the box. Use
# outlier.colour to override
p + geom_boxplot2(outlier.colour = "red", outlier.shape = 1)
# Remove outliers when overlaying boxplot with original data points
p + geom_boxplot2(outlier.shape = NA) + geom_jitter(width = 0.2)

# Boxplots are automatically dodged when any aesthetic is a factor
p + geom_boxplot2(aes(colour = drv))

# You can also use boxplots with continuous x, as long as you supply
# a grouping variable. cut_width is particularly useful
# ggplot(diamonds, aes(carat, price)) +
#   geom_boxplot2()
# ggplot(diamonds, aes(carat, price)) +
#   geom_boxplot2(aes(group = cut_width(carat, 0.25)))
# Adjust the transparency of outliers using outlier.alpha
# ggplot(diamonds, aes(carat, price)) +
#   geom_boxplot2(aes(group = cut_width(carat, 0.25)), outlier.alpha = 0.1)

# It's possible to draw a boxplot with your own computations if you
# use stat = "identity":
y <- rnorm(100)
df <- data.frame(
  x = 1,
  y0 = min(y),
  y25 = quantile(y, 0.25),
  y50 = median(y),
  y75 = quantile(y, 0.75),
  y100 = max(y)
)
ggplot(df, aes(x)) +
  geom_boxplot2(
   aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
   stat = "identity"
 )
