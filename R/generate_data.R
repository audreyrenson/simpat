#' Generate simulated data that meets the parallel trends assumption
#'
#' @param n_obs int. Number of independent observations
#' @param n_covs_binary int. Number of binary covariates observed at each period
#' @param n_covs_continuous int. Number of continuous covariates observed at each period
#' @param a_spec `spec` object (as returned by `create_spec()`) specifying how the treatments are generated as a function of measured history
#' @param y_spec `spec` object specifying how the outcomes are generated as a function of measured history
#' @param tau int. Number of periods, minus 1. I.e. there are Tt + 1 periods.
#' @param potential_outcomes logical. Should outcomes and covariates be generated with exposure set to 0 at all times?
#'
#' @return Data frame with `n_obs` \times `(tau+1)` rows and `n_covs` + 4 columns: `uid` is a unique identifier, `u` is an 'unmeasured' baseline covariate, `a` is a treatment, `y` outcomes, and `wj` (j=1,...,`n_covs`) are covariates.
#' @export
#'
#' @examples
generate_data <- function(n_obs=100,
                          n_covs_binary=2,
                          n_covs_continuous=2,
                          a_spec,
                          y_spec,
                          tau=2,
                          potential_outcomes=FALSE){

  df = data.frame(uid = seq_len(n_obs),
                  u   = runif(n_obs))

  n_covs = n_covs_binary + n_covs_continuous
  w_formula = paste0('~', paste(sapply(1:n_covs, function(j) paste0('w', j, '{t-1}')), collapse='+'))
  w_bin_spec = create_spec(binomial(), w_formula)
  w_con_spec = create_spec(gaussian(), w_formula)
  w_bin_indices = 1:n_covs_binary
  w_con_indices = n_covs_binary + (1:n_covs_continuous)

  for(t in 0:tau) {

    #Obey the temporal ordering {w_t, a_t, y_t}
    for(j in w_bin_indices) {
      df[[glue::glue('w{j}{t}')]] = simulate.spec(object = w_bin_spec, data = df, glue_vars = list(t=t))
    }
    for(j in w_con_indices) {
      df[[glue::glue('w{j}{t}')]]  = simulate.spec(w_con_spec, data = df, glue_vars = list(t=t))
    }

    # df[[glue::glue('a{t}')]] = simulate(a_specification, df)
    # df[[glue::glue('y{t}')]] = sim_spec(y_specification, n_obs)

  }

  return(
    pivot_longer_gendata(df)
  )

}

pivot_longer_gendata = function(df_wide) {
  # df_wide %>%
  #   tidyr::pivot_longer(c(dplyr::starts_with('A'), dplyr::starts_with("L"), dplyr::starts_with('W'), dplyr::starts_with('Y'))) %>%
  #   tidyr::separate(name, into=c('var','t'), sep=1) %>%
  #   tidyr::pivot_wider(names_from = var, values_from = value) %>%
  #   dplyr::mutate(t=as.numeric(t))
}

