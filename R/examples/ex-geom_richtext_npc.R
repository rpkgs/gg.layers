library(ggplot2)

labels <- c(
  "gC m^{-2} d^{-1}",
  "gC m^-2 d^-1",
  "gC m_{-2} d_{-1}",
  "gC m_-2 d_-1"
  # "gC \n mm/d"
)

d = data.frame(x = 0.2, y = seq_along(labels)/10, label = labels)
ggplot(d, aes(x, y)) +
  geom_richtext_npc(aes(npcx = x, npcy = y, label = label))

# remove fill and label.color
ggplot(d, aes(x, y)) +
  geom_richtext_npc(aes(npcx = x, npcy = y, label = label),
                    fill = NA, label.color = NA)

## second example
d$label <- c(
  "Some text **in bold.**",
  "Linebreaks<br>Linebreaks<br>Linebreaks",
  "*x*<sup>2</sup> + 5*x* + *C*<sub>*i*</sub>",
  "Some <span style='color:blue'>blue text **in bold.**</span><br>And *italics text.*<br>
    And some <span style='font-size:18pt; color:black'>large</span> text."
)
ggplot(d, aes(x, y)) +
  geom_richtext_npc(aes(npcx = x, npcy = y, label = label),
                    fill = NA, label.color = NA)
