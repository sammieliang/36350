generate_data = function(n, p)
{
  responses = rnorm(n, 0, 1)
  covariates = matrix(responses, nrow=n, ncol=p)
  
  res = list(covariates, responses)
  names(res) = c("covariates", "responses")
  return (res)
}

model_select = function(covariates, responses, cutoff)
{
  linear.reg = lm(responses ~ covariates)
  p-vals = summary(linear.reg)$coefficients[, 4]
  linear.reg2 = lm(responses ~ covariates[p-vals <= cutoff])
  p-vals.retained = summary(linear.reg2)$coefficients[, 4]
  if (length(p-vals.retained == 0))
  {
    return (vector(length=0))
  }
  else
  {
    return (p-vals.retained)
  }
}

