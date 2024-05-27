library(data.table)
data("GPP_US_MMS")
dat = melt(GPP_US_MMS[, .(date, SM, GPP)], "date")

ggplot(dat, aes(date, value)) + 
  geom_line() + 
  geom_movmean(halfwin = 5, linewidth = 1, alpha = 0.7, color = "pink") + 
  # geom_movmean(halfwin = 5, show.diff=TRUE, linewidth = 0.2, alpha = 0.7, color = "red") + 
  geom_spike(halfwin = 2, sd.times = 6, color = "red", verbose = TRUE) +
  # geom_spike(halfwin = 5, trs = 3, color = "blue") +
  facet_wrap(~variable, scales = "free_y") 
