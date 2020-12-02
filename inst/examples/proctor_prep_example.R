# prepare tibble from existing water contents and estimated w_opt values (these
# should be estimated visually and confirmed w, but are simply typed here for demonstration)
library(tibble)
library(dplyr)
library(tidyr)

w_extant_values <- tibble(
  sample_ID = paste0("mix_0", 1:3),
  w_extant = c(0.048, 0.057, 0.062))

PL_values <- tibble(
  sample_ID = paste0("mix_0", 1:3),
  PL= c(0.095, 0.10, 0.14),
  standard = 0.9*PL,
  modified= standard - 0.03)

w_info <- w_extant_values %>%
  left_join(PL_values) %>%
  select(-PL) %>%
  pivot_longer(cols= c(standard:modified),
               names_to =  'effort',
               values_to = 'est_w_opt')%>%
  select(sample_ID, effort, everything()) %>%
  arrange(desc(effort))

# generate the reference table
proctor_prep(df= w_info, date= Sys.Date())

# widen the interval for a clayey soil
mix_03 <- w_info %>%
  filter(sample_ID == 'mix_03')
proctor_prep(df= mix_03, date= Sys.Date(), w_int = 0.025)

# the function works fine with only one compaction effort but be sure this is
# still one of the columns
mix_03 %>%
  filter(effort == 'modified') %>%
  proctor_prep(date= Sys.Date())


