library(ggplot2)

## first example
labels <- c(
  "gC m^{-2} d^{-1}",
  "gC m^-2 d^-1",
  "gC m_{-2} d_{-1}",
  "gC m_-2 d_-1"
  # "gC \n mm/d"
)
x = 0.2
y = seq_along(labels)/10

ggplot() + annotate_richtext_npc(x, y, labels, size = 5)
ggplot() + annotate_richlabel_npc(x, y, labels, size = 5, label.color = "red")

# Another option
d = data.frame(x = 0.2, y = seq_along(labels)/10, label = labels)
ggplot(d, aes(npcx = x, npcy = y)) +
  geom_richtext_npc(aes(npcx = x, npcy = y, label = label))

# remove fill and label.color
ggplot(d, aes(npcx = x, npcy = y)) +
  geom_richtext_npc(aes(npcx = x, npcy = y, label = label),
                    fill = "white", label.color = "red")

## second example
d$label <- c(
  "Some text **in bold.**",
  "Linebreaks<br>Linebreaks<br>Linebreaks",
  "*x*<sup>2</sup> + 5*x* + *C*<sub>*i*</sub>",
  "Some <span style='color:blue'>blue text **in bold.**</span><br>And *italics text.*<br>
    And some <span style='font-size:18pt; color:black'>large</span> text."
)
ggplot(d, aes(npcx = x, npcy = y)) +
  geom_richtext_npc(aes(npcx = x, npcy = y, label = label))

## test for `str_mk`
library(magrittr)
indexes_lev = c("DOY_first", "DOY_last", "HWD", "HWI", "HWS_mean", "HWS_sum", "HWA_avg", "HWA_max", "HWA_sum")
labels = indexes_lev %>% str_mk() #%>% label_tag(expression = F)
d = data.frame(x = 0.5, y = 0.5, label = labels)

ggplot(d) +
  facet_wrap(~label) +
  theme(strip.text.x = element_textbox(face = "bold")) +
  geom_richtext_npc(aes(npcx = x, npcy = y, label = label))
