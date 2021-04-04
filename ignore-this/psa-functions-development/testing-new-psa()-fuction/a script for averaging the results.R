# It would be awesome to tack on a second list item to each list item which has numeric values; i.e. a summary. Then for every analysis I will be able to choose if I want all the data or just the average of the replications .

# It should be relatively straightforward to write a helper function
# something like `compute_avg_per_replication()` and apply it conditionally to any data frames having the required characteristics, for example their names must contain either "microns", "sand", etc. Then do a group_by and a summarize (using across() if need be to perform the operation on multiple columns) to compute the average value for an arbitrary number of replicates.

# As it stands now, this would apply for list elements 1, 2, 3, and 6.....for the plots, it could to an identical plot and possibly with a confidence band around the line and alternative text; this would also be simple....probably best to write a function for this action rather than doing it inline with the pipe.

# It would be wise to wrap the averaging function inside possibly() because some of the pretreatment data have NULL values (should I change these to NA??) and functionals will break when they encounter these.


library(tidyverse)

pass <- psa("ignore-this/psa-functions-development/testing-new-psa()-fuction/psa-data_2021-03-04/") %>%
  purrr::pluck('cumulative_percent_passing')

summary <- pass %>%
  group_by(across(-c(replication, batch_sample_number, percent_passing))) %>%
# this approach is great when there are multiple variables;
  # it will be very useful for the size class bins.
  # However across is not so good for a single variable
    # summarise(percent_passing= across(
  #   .cols = percent_passing,
  #   .fns = mean,
  #   #.fns = list(mean, ~1.96*sd(.)),
  #   #.names = "{.fn}_pct_passing")) %>%
  # )) %>%
  summarise(percent_passing = mean(percent_passing)) %>%
  dplyr::arrange(desc(microns)) %>%
  ungroup()

