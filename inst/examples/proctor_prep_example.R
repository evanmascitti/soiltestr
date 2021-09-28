# prepare tibble from existing water contents and estimated w_opt values
library(tibble)

y <- tibble(
  sample_name = paste0("mix_0", 1:3),
  w_extant = c(0.048, 0.027, 0.052),
  PL= c(0.095, 0.10, 0.14))

# generate the reference table
proctor_prep(sample_name = y$sample_name,
             w_extant = y$w_extant,
             pl = y$PL)

# widen the interval for a clayey soil
mix_03 <- w_info %>%
  filter(sample_name == 'mix_03')
proctor_prep(df= mix_03, date= Sys.Date(), w_int = 0.025)

# the function works fine with only one compaction effort but be sure this is
# still one of the columns
mix_03 %>%
  filter(effort == 'modified') %>%
  proctor_prep(date= Sys.Date())


