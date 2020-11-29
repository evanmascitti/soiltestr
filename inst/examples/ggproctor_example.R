# compute raw density values
library(ggplot2)
library(dplyr)

compaction_data <- diRtscience::example_proctor_data %>%
  diRtscience::add_physical_properties()

# a single sample
standard_data <- filter(compaction_data, compaction_effort == "standard")
ggproctor2(standard_data)

# two samples on same plot; annotations turned off
ggproctor2(compaction_data, annotate = FALSE)

# facet and leave data annotations on but turn off 90% saturation line
ggproctor2(compaction_data, identifier = compaction_effort, sat_90 = FALSE)+
  facet_wrap(~compaction_effort)

# change color of both curves to the same value
ggproctor2(compaction_data, sat_90 = FALSE,
           identifier = compaction_effort, color = 'darkgreen')+
  facet_wrap(~compaction_effort)
