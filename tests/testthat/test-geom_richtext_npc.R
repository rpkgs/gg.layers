test_that("geom_richtext_npc works", {
  library(ggplot2)

  labels <- c(
    "gC m^{-2} d^{-1}",
    "gC m^-2 d^-1",
    "gC m_{-2} d_{-1}",
    "gC m_-2 d_-1"
    # "gC \n mm/d"
  )

  d = data.frame(x = 0.2, y = seq_along(labels) / 10, label = labels)
  p1 = ggplot(d, aes(npcx = x, npcy = y)) +
    geom_richtext_npc(aes(npcx = x, npcy = y, label = label))

  # remove fill and label.color
  p2 = ggplot(d, aes(npcx = x, npcy = y)) +
    geom_richtext_npc(aes(npcx = x, npcy = y, label = label),
      fill = NA, label.color = NA
    )

  ## second example
  d$label <- c(
    "Some text **in bold.**",
    "Linebreaks<br>Linebreaks<br>Linebreaks",
    "*x*<sup>2</sup> + 5*x* + *C*<sub>*i*</sub>",
    "Some <span style='color:blue'>blue text **in bold.**</span><br>And *italics text.*<br>
    And some <span style='font-size:18pt; color:black'>large</span> text."
  )
  p3 = ggplot(d, aes(npcx = x, npcy = y)) +
    geom_richtext_npc(aes(npcx = x, npcy = y, label = label),
      fill = NA, label.color = NA
    )
  expect_silent(print(p1)) 
  expect_silent(print(p2))
  expect_silent(print(p3))
})
