generate_data = function(n, p)
{
  covariates = rnorm(n*p, 0, 1)
  covariates = matrix(draws, nrow=n, ncol=p)
  responses = rnorm(n, 0, 1)
  return (list(covariates, responses))
}
