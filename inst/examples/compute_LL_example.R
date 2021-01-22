# These operations work best with the pipe and other tidyverse conventions
library(dplyr)
library(magrittr)
library(purrr)
library(tidyr)

# compute LL for one sample
sample_2_LL <- example_LL_data[5:8, ] %>%
  left_join(example_tin_tares$`2020-05-24`) %>%
  add_w() %>%
  compute_LL()
sample_2_LL

# use dplyr and purrr to iterate over multiple samples using a list-column
# inside a nested tibble
example_LL_data %>%
  left_join(example_tin_tares$`2020-05-24`) %>%
  add_w() %>%
  group_by(expt_mix_num) %>%
  nest() %>%
  mutate(LL = map_dbl(data, compute_LL) )
