1. make tibble of all needed arguments - number of rows must match length of vector arguments 

2. write a helper function to generate the new water contents from the tibble above and the other arguments supplied to main function

3. group the tibble in (1) by sample name and effort, then nest it and use `purrr::map_df()` inside `dplyr::mutate()` to generate a list-column of the new water contents 

4. unnest the list-column

5. add the cyinder numbers by repeating a numeric vector of 1:n_cylinders as many times as there are sample_name x effort combinations 

