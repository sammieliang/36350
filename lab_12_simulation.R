generate_data = function(n, p)
{
  responses = rnorm(n, 0, 1)
  covariates = matrix(responses, nrow=n, ncol=p)
  
  res = list(covariates, responses)
  names(res) = c("covariates", "responses")
  return (res)
}


