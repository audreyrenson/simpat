#' Generate simulated data that meets the parallel trends assumption
#'
#' @param n_obs int. Number of independent observations
#' @param n_covs_binary int. Number of binary covariates observed at each period
#' @param n_covs_continuous int. Number of continuous covariates observed at each period
#' @param a_formula glue-style formula specifying how the treatments depend on history of covariates and treatments
#' @param y_formula glue-style formula specifying how the outcomes depend on history of covariates and treatments
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
                          a_specification,
                          y_specification,
                          tau=2,
                          potential_outcomes=FALSE){

  df = data.frame(uid = seq_len(n_obs),
                  u   = runif(n_obs))


  for(t in 0:tau) {

    #Obey the temporal ordering {w_t, a_t, y_t}
    for(j in 1:n_covs_binary) {
      df[[glue::glue('w{jt}')]] = rbinom_logit() #what does wjt depend on and how to generate that model? possibly let it depend on everything except u and control the mean and variance?
    }
    for(j in 1:n_covs_continuous) {
      df[[glue::glue('w{jt}')]] = rnorm_identity() #ditto above?
    }

    df[[glue::glue('a{t}')]] = sim_spec(a_specification, n_obs)
    df[[glue::glue('y{t}')]] = sim_spec(y_specification, n_obs)

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

# convert these to generic spec objects (i.e. main terms linear model)
rbinom_logit <- function(X, Beta, N, n=1) rbinom(n=N, p=plogis(as.matrix(X) %*% Beta), size=n)
norm_identity <- function(X, Beta, N, sd=1) rnorm(n=N, mean =as.matrix(X) %*% Beta, sd=sd)


#specifications
a_specification = create_spec(binomial('logit'), '~w{1t}+w{2t}+log(w{1t})+I(w{2t}^2)')

create_spec = function(family, formula) {
  #return spec object that tells how to simulate data
}

sim_spec = function(spec, data, nobs) { #consider renaming 'spec' b/c exists in readr package
  #code to generate data from a spec object
}

