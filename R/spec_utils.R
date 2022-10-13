
#' Simulate data from a generalized linear model based on a simulation specification (i.e. a `spec` object)
#'
#' @param family a `stats::family` object specifying the distribution and link function to simulate from
#' @param formula a glue-style formula possibly including time indices. See documentation for glue-style formulas (to be added)
#' @param object `spec` object, as returned by `create_spec()`
#' @param data data frame containing the variables referenced in the `formula` slot in `object`
#' @param params optional, numeric vector generalized linear model parameters
#' @param nsim integer. number of datasets to return
#' @param seed integer. random seed for reproducible results
#' @param ... values passed to `glue_formula()`
#'
#' @return `create_spec` returns a `spec` object, which is a list containing `family` and `formula`. `simulate.spec` returns a matrix of dimension `nrow(data)` \times `nsims` containing simulated values.
#' @export
#'
#' @examples
#' a_spec = create_spec(binomial('logit'), '~w1{t}+w2{t}+log(w1{t})+I(w2{t}^2)')
#' data = data.frame(w11=rnorm(20), w12=rnorm(20))
#' simulate(a_spec, data=data, t=1)
#'
#' @rdname spec
create_spec = function(family, formula) {
  #return spec object that tells how to simulate data
  result = list(family = family, formula = formula)
  class(result) = c('spec', 'list')
  result
}
#' @rdname spec
simulate.spec <- function(object, data, glue_vars=list()) {

  rhs_formula = glue_formula(object$formula, glue_vars)
  X = model.matrix(rhs_formula, data)
  X_pars = rnorm(ncol(X), mean=1) #goals: package this as a separate function, normalize parameters to control variance

  data$yhat = object$family$linkinv(c(X %*% X_pars))
  full_formula = glue_formula(paste0('yhat', object$formula), glue_vars)
  sim_glm = withCallingHandlers({
    glm(full_formula, family=object$family, data=data)
  }, warning = function(w) {
    # non-integer warning will always happen for discrete distribution because yhat is continuous.
    # This is okay because the glm object will still have the right coefficients.
    if (startsWith(conditionMessage(w), "non-integer"))
      invokeRestart("muffleWarning")
  })
  simulate(sim_glm, nsim=1)[,1]
}

