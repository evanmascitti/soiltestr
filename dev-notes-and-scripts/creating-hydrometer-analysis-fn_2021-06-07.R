# library(soiltestr)
#
# setwd("E:/OneDrive - The Pennsylvania State University/PSU2019-present/A_inf_soils_PhD/data-lab/loose_data_files/10reps hydrometer")
#
# blank_correction_data <- readr::read_csv(
#   file = './psa-data_2021-03-30/psa-hydrometer-blank-correction-w-companion-data_2021-03-30.csv',
#   col_types = 'Dccciidccccccdddc',
#   na = '-',
#   trim_ws = TRUE,
#   skip_empty_rows = TRUE,
#   lazy = FALSE
# )%>%
#   dplyr::transmute(approx_ESD = approx_ESD, blank_hydrometer_reading = hydrometer_reading)
# blank_correction_data
#
# hydrometer_data <- readr::read_csv(
#   './psa-data_2021-03-30/psa-hydrometer-data_2021-03-30.csv',
#   col_types = 'Dccciiiiddccccccdddc',
#   na = '-',
#   trim_ws = TRUE,
#   skip_empty_rows = TRUE,
#   lazy = FALSE
# ) %>%
#   dplyr::left_join(blank_correction_data) %>%
#   dplyr::left_join(asi468::bouyoucos_cylinders) %>%
#   dplyr::left_join(h2o_properties_w_temp_c) %>%
#   dplyr::mutate(hydrometer_reading = hydrometer_reading)
#
# # read the specimen mass data
#
# specimen_masses <- readr::read_csv(
#   './psa-data_2021-03-30/psa-specimen-masses-data_2021-03-30.csv',
#   col_types = 'Dcciidc',
#   na = '-',
#   skip_empty_rows = TRUE,
#   trim_ws = TRUE,
#   lazy = FALSE
# ) %>%
#   soiltestr:::clean_g()
#
# tin_tares <- dplyr::bind_rows(asi468::tin_tares)
#
# hygroscopic_corrections <- readr::read_csv(
#   './psa-data_2021-03-30/psa-hygroscopic-corrections-data_2021-03-30.csv',
#   col_types = 'Dcciiccccc',
#   na = '-',
#   skip_empty_rows = TRUE,
#   trim_ws = TRUE,
#   lazy = FALSE
# ) %>%
#   soiltestr:::clean_g() %>%
#   dplyr::left_join(tin_tares) %>%
#   add_w() %>%
#   dplyr::transmute(batch_sample_number = batch_sample_number, hygroscopic_water_content = water_content)
#
# hygroscopic_corrections
#
# OD_specimen_masses <- specimen_masses %>%
#   dplyr::left_join(hygroscopic_corrections) %>%
#   dplyr::mutate(OD_soil_only = air_dry_specimen_mass_for_test / (1 + hygroscopic_water_content)) %>%
#   dplyr::select(batch_sample_number, OD_soil_only)
#
# OD_specimen_masses
#
# # put the datetimes together for stirring and sampling
# all_samples <- hydrometer_data %>%
#   tidyr::unite(col = 'sampling_datetime',
#                sampling_date, sampling_time, sampling_AM_PM,
#                sep = ' ') %>%
#   tidyr::unite(col = 'stir_datetime',
#                stir_date, stir_time, stir_AM_PM,
#                sep = ' ') %>%
#   dplyr::mutate(sampling_datetime = lubridate::ymd_hm(sampling_datetime, tz = Sys.timezone()),
#                 stir_datetime = lubridate::ymd_hm(stir_datetime, tz = Sys.timezone())) %>%
#   dplyr::mutate(elapsed_time = lubridate::as.duration(sampling_datetime - stir_datetime)) %>%
#   dplyr::left_join(OD_specimen_masses)
# # %>%
#   # dont think I need this for the companion measurements....will need it
#   # if using calibration relationship dplyr::left_join(h2o_properties_w_temp_c) %>%
#   # remove the filter statement later
#
#
# # one_sample <- all_samples %>%   dplyr::filter(replication == 1L)
#
# # set some parameters for the hydrometer and sedimentation cylinder in use
#
# hydrometer_ID <- unique(hydrometer_data$hydrometer_ID)
# hydrometer_dims <- asi468::astm_152H_hydrometers[asi468::astm_152H_hydrometers$hydrometer_ID == hydrometer_ID]
#
# rho_water_152H <- h2o_properties_w_temp_c[h2o_properties_w_temp_c$water_temp_c == 20L, "water_density_Mg_m3"][[1]]
#
# rho_water_152H
#
# # this is for the 151H hydrometer, not the one we have # mass_percent_finer <- one_sample %>%
# #   dplyr::mutate(mass_pct_finer = (Gs/(Gs-1)) * (1000 / OD_soil_only) * rho_water_151H * (hydrometer_reading - blank_hydrometer_reading))
#
# # calculate the mass percent finer, effective depth, and % remaining in suspension --------
#
#
# mass_percent_finer <- all_samples %>%
#   dplyr::mutate(
#     mass_pct_finer = 0.6226 * (Gs/(Gs-1)) * (1000/OD_soil_only) * (hydrometer_reading - blank_hydrometer_reading) * (100/1000)
#   )
#
#
# mass_percent_finer$mass_pct_finer
#
#
# effective_depth <- mass_percent_finer %>%
#   dplyr::mutate(Hm_cm = hydrometer_dims$Hr2_cm + ( (hydrometer_dims$Hr1_cm - hydrometer_dims$Hr2_cm)/((60) - (-5)) * (60- hydrometer_reading + meniscus_correction) ) - (hydrometer_dims$Vhb_mL / (2 * area_cm2)))
#
# effective_depth$Hm_cm
#
# max_diam_in_suspension <- effective_depth %>%
#   dplyr::mutate(
#     root_term1 = (18*water_absolute_viscosity_poises) / (water_density_Mg_m3 * 980.7 * (Gs-1)),
#     root_term_2 = (Hm_cm / as.double(elapsed_time)),
#     root_term_total = sqrt(root_term1 * root_term_2),
#     D_m_mm = root_term_total * 10,
#     D_m_microns = D_m_mm * 1000)
#
#
# max_diam_in_suspension %>%
#   dplyr::mutate(percent_passing = mass_pct_finer * 0.01,
#                 microns = D_m_microns) %>%
#   dplyr::select(date:batch_sample_number, microns, percent_passing)
#
# # now need to do the log-linear interpolation to calculate % clay....
# # best to write a function for this and then nest the data and apply it....
# # function should take as inputs:
# # - a data frame containing columns called microns and percent_passing
# # - a numeric argument for the diameter to interpolate
# # then the function should find two particle diameters which bracket that number
# # and then fit a model as percent_passing ~ log10(microns), and finally predict
# # the value for the specified diameter using that model object .
#
#
#
#
#
#
# # temporarily set df to the above data frame and diameter to 2
# # df <- max_diam_in_suspension <- max_diam_in_suspension %>%
# # 	dplyr::mutate(percent_passing = mass_pct_finer * 0.01,
# # 								microns = D_m_microns) %>%
# # 	dplyr::select(date:batch_sample_number, microns, percent_passing)
# #
# # diameter <- 2
#
#
# generalized_finer_D_x <- function(df, d_microns){
#
#   browser()
#
# hydrometer_percent_finer_D_x <- function(df, d_microns) {
#
#    browser()
#
#
#   mget(c("df", "d_microns"), envir = rlang::caller_env()) %>%
#     list2env()
#
#
#   # calculate differences for all sampled diameters from the specified diameter
#   distances_df <- df %>%
#     dplyr::mutate(diff_from_desired_diameter = microns - d_microns)
#
#   # this finds the difference in microns between the closest size actually measured
#   # and the requested particle diameter. The min_pos_dist value is the distance of the diameter slightly larger
#   #than the one requested, and the min_neg_dist is the distance which is slightly smaller than the one
#   # requested.
#
#   min_pos_dist <- min(purrr::pluck(distances_df[distances_df$diff_from_desired_diameter > 0, ,], "diff_from_desired_diameter"))
#
#   min_neg_dist <- min(abs(purrr::pluck(distances_df[distances_df$diff_from_desired_diameter <= 0, ,], "diff_from_desired_diameter")))
#
#   # those values can now be used to filter the main data frame to include only rows having those
#   # values for the diff_from_desired_diameter variable
#
#   filtered_distances_df <- distances_df %>%
#     dplyr::filter(
#       {dplyr::near(diff_from_desired_diameter, min_pos_dist) ||
#           dplyr::near(diff_from_desired_diameter, -min_neg_dist)
#       })
#
#   # next, fit a log-linear model to those two data points
#
#   log_lin_mod <- lm(
#     data = filtered_distances_df,
#     formula = percent_passing ~ log10(microns)
#   )
#
#   # finally, predict the percent passing for the requested particle diameter
#   predicted_percent_passing <- predict(object = log_lin_mod,
#                                        newdata = data.frame(microns = d_microns)
#   ) %>%
#     purrr::set_names(paste0('percent < ', d_microns, '\u03bcm'))
#
#
#   # add the predicted value onto the original data frame,
#   # select only the desired columns,
#   # and keep oonly distinct rows (there should be only two rows, but
#   # because the predicted percent passing is simply recycled for both of them,
#   # all the relevant metadata values should be identicacl)
#   return_df <- df %>%
#     dplyr::mutate("percent_passing_{d_microns}_microns" := predicted_percent_passing) %>%
#     dplyr::select(.data$date,
#                   .data$experiment_name,
#                   .data$protocol_ID,
#                   .data$sample_name,
#                   .data$replication,
#                   .data$batch_sample_number,
#                   dplyr::matches(
#                     "^percent_passing_\\d.*_microns$"
#                   )) %>%
#     dplyr::distinct()
#
#   #browser()
#
#   return(return_df)
#
#   # end of function to predict a single diameter
#
#   }
#
#   ####
#
# # now use a loop to do the same operation for all diameters listed in
# # the initial function call
#
# # browser()
#
# dfs_list <- vector(mode = "list",
#                    length = length(d_microns))
#
# #names(dfs_list) <- as.character(d_microns)
#
# for (i in seq_along(d_microns) ) {
#
#   dfs_list[[i]] <- hydrometer_percent_finer_D_x(df, d_microns[[i]])
#
#   }
#
# # pick out only the relevant columns and put back into long format
#
# all_sizes_predicted <- purrr::reduce(dfs_list, dplyr::left_join,
#                                      by = c("date",
#                                             "experiment_name",
#                                             "protocol_ID",
#                                             "sample_name",
#                                             "replication",
#                                             "batch_sample_number"))  %>%
#   dplyr::select(.data$date,
#                 .data$experiment_name,
#                 .data$protocol_ID,
#                 .data$sample_name,
#                 .data$replication,
#                 .data$batch_sample_number,
#                 dplyr::matches(
#                   "^percent_passing_\\d.*_microns$"
#                 )) %>%
#   tidyr::pivot_longer(
#     cols =  dplyr::matches("^percent_passing_\\d.*_microns$"),
#     names_to = 'microns',
#     values_to = 'percent_passing'
#   ) %>%
#   dplyr::mutate(microns = readr::parse_number(.data$microns)
#   ) %>%
#   dplyr::arrange(dplyr::desc(microns))
#
#
# return(all_sizes_predicted)
#
# ####
#
# }
#
#
# # test it out
#
# check <- max_diam_in_suspension %>%
#   dplyr::mutate(percent_passing = mass_pct_finer * 0.01,
#                 microns = D_m_microns) %>%
#   dplyr::filter(batch_sample_number == 1L)
#
# generalized_check <- generalized_finer_D_x(df = check, d_microns = c(1.7, 2))
#
# # cols_to_keep <- c("date", "experiment_name", "protocol_ID",
# #                   "sample_name", "replication", "batch_sample_number")
#
#
# generalized_check %>%
#   dplyr::select(.data$date,
#                 .data$experiment_name,
#                 .data$protocol_ID,
#                 .data$sample_name,
#                 .data$replication,
#                 .data$batch_sample_number,
#                 dplyr::matches(
#                   "^percent_passing_\\d.*_microns$"
#                 ))
#
#
# # I could write a wrapper to check if there is more than one specimen and #only use group_by, nest, map, unnest if there is > 1....but this
# # approach will also work just fine even if there is only one sample, so I might as well just do that.
# # Yes for a single sample it performs an un-necessary step, but it's actually less code, and most of the time I am using multiple samples
# # anyway; that's a design philosophy of the whole package.
#
#
