test_that("can simulate from a poisson, logistic, or linear spec", {

  n = 10

  pois = create_spec(poisson(), '~w{s}')
  logi = create_spec(binomial(), '~w{s}')
  line = create_spec(gaussian(), '~w{s}')

  data = data.frame(w1 = rnorm(n))

  expect_equal(n, length(simulate.spec(pois, data=data, glue_vars=list(s=1))))
  expect_equal(n, length(simulate.spec(logi, data=data, glue_vars=list(s=1))))
  expect_equal(n, length(simulate.spec(line, data=data, glue_vars=list(s=1))))
})
