select_sample_str <- function(sampling_frame, allocation,
                              sample_size = sample_size, stratum = stratum, is_frac = FALSE, seed = NA,
                              replace = FALSE){
  if (!is.na(seed)) set.seed(seed)
  sample_size <- dplyr::enquo(sample_size)
  sample_size_name <- dplyr::quo_name(sample_size)
  
  stratum_var_string <- deparse(substitute(stratum))
  stratum <- dplyr::enquo(stratum)
  
  if (is_frac) {
    sample <- sampling_frame %>%
      dplyr::left_join(allocation, by = stratum_var_string) %>%
      split(.[stratum_var_string]) %>%
      purrr::map_df(~dplyr::sample_frac(.,
                                        size = dplyr::pull(., sample_size_name)[1],
                                        replace = replace)) %>%
      dplyr::select(dplyr::one_of(colnames(sampling_frame)))
  } else {
    # if sample size not integer we round it
    allocation <- allocation %>%
      dplyr::mutate(!!sample_size_name := round(!!sample_size))
    
    sample <- sampling_frame %>%
      dplyr::left_join(allocation, by = stratum_var_string) %>%
      split(.[stratum_var_string]) %>%
      purrr::map_df(~dplyr::sample_n(.,
                                     size = dplyr::pull(., sample_size_name)[1],
                                     replace = replace)) %>%
      dplyr::select(dplyr::one_of(colnames(sampling_frame)))
  }
  return(sample)
}


select_sample_prop <- function(sampling_frame, stratum = stratum, frac,
                               seed = NA, replace = FALSE){
  if (!is.na(seed)) set.seed(seed)
  if (missing(stratum)){
    sample <- dplyr::sample_frac(sampling_frame, size = frac,
                                 replace = replace)
  } else {
    stratum <- dplyr::enquo(stratum)
    sample <- sampling_frame %>%
      dplyr::group_by(!!stratum) %>%
      dplyr::sample_frac(size = frac, replace = replace) %>% 
      dplyr::ungroup()
  }
  return(sample)
}
