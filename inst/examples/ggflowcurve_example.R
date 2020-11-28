# These operations work best with the pipe and other tidyverse conventions
library(dplyr)
library(magrittr)
library(purrr)
library(tidyr)
library(ggplot2)

# one sample
sample_3 <- example_LL_data[9:12, ] %>%
  left_join(asi468_tin_tares$`2020-05-24`) %>%
  add_w()
ggflowcurve(sample_3)

# three samples, faceted

example_LL_data %>%
  left_join(asi468_tin_tares$`2020-05-24`) %>%
  add_w() %>%
  ggflowcurve()+
  facet_wrap(~expt_mix_num)

# add colors and vertical arrows mapped to the LL in order to highlight the blow count of
# 25

lines_data <- example_LL_data %>%
  left_join(asi468_tin_tares$`2020-05-24`) %>%
  add_w() %>%
  group_by(expt_mix_num) %>%
  nest() %>%
  mutate(LL = map_dbl(data, compute_LL),
         blow_count = 25) %>%
  ungroup()

fulldata <- example_LL_data %>%
  left_join(asi468_tin_tares$`2020-05-24`) %>%
  add_w()

ggflowcurve(fulldata, color = factor(expt_mix_num))+
  geom_segment(data= lines_data,
               aes(x= blow_count, xend= blow_count, y= -Inf, yend = LL*0.99),
               arrow = arrow(length = unit(0.1, "in"))) +
  labs(color= "Mix number")+
  facet_wrap(~expt_mix_num)

