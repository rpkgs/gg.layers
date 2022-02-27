
test_that("stat_reg works", {
  library(ggplot2)
  library(data.table)

  dat <- data.table(mtcars)
  dat$cyl <- as.factor(dat$cyl)
  # table(dat$cyl)
  dat2 = subset(dat, cyl ==4)

  expect_true({
    
    p1 = ggplot(dat, aes(wt, mpg, color = cyl)) +
      geom_point() +
      stat_reg(data = dat2, y = 1, mar = 0, position = "none") +
      facet_wrap(~cyl)

    p2 = ggplot(dat, aes(wt, mpg, color = cyl)) +
      geom_point() +
      stat_reg(
        data = dat2, y = 1,
        position = "dodge",
        height.factor = 1.2,
        unit = "gC m^-2 d^-1"
      )

    p3 = ggplot(dat, aes(wt, mpg, color = cyl)) +
      geom_point() +
      stat_reg(
        data = dat2, y = 1,
        position = "none",
        height.factor = 1.2,
        unit = "gC m^-2 d^-1"
      ) +
      facet_wrap(~cyl)

    p4 = ggplot(dat, aes(wt, mpg, color = cyl)) +
      geom_point() +
      stat_gof(x = 0, y = 1) +
      # stat_reg(data = subset(dat, cyl == 4), y = 1, color = "red") +
      # stat_reg(data = subset(dat, cyl == 6), y = 0.8) +
      facet_wrap(~cyl)
    print(p1)
    print(p2)
    print(p3)
    print(p4)
    TRUE
  })
})
