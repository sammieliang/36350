generate_data = function(n, p)
{
  responses = rnorm(n, 0, 1)
  covariates = matrix(rnorm((n*p), 0, 1), nrow=n, ncol=p)
  
  res = list(covariates, responses)
  names(res) = c("covariates", "responses")
  return (res)
}

model_select = function(covariates, responses, cutoff)
{
  linear.reg = lm(responses ~ covariates)
  x = summary(linear.reg)$coefficients[-1, 4]
  bool.vec = x <= cutoff
  if (sum(bool.vec) == 0)
  {
    return (c())
  }
  linear.reg2 = lm(responses ~ covariates[,bool.vec])
  retained = summary(linear.reg2)$coefficients[-1, 4]
  return (retained)
}