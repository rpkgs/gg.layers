library(dplyr)

obs = 1:10
mod1 = obs + rnorm(10)
mod2 = obs + rnorm(10)*2
mod3 = obs + rnorm(10)*3

l_mod = listk(mod1, mod2, mod3)
d = map(l_mod, ~data.table(obs, mod = .x)) %>% melt_list("model")
dummy_model = list(var1 = d, var2 = d) %>% melt_list("variable")

use_data(dummy_model, overwrite = TRUE)
