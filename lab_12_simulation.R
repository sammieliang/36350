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
    return (vector(length=0))
  }
  linear.reg2 = lm(responses ~ covariates[,bool.vec])
  print("Here!")
  retained = summary(linear.reg2)$coefficients[-1, 4]
  return (retained)
}

run_simulation = function(n_trials, n, p, cutoff)
{
  
  for (curr_n in n)
  {
    for (curr_p in p)
    {
      data.list = generate_data(curr_n, curr_p)
      vals = model_select(data.list[["covariates"]], data.list[["responses"]], cutoff)
      if (length(vals) != 0)
      {
        hist(vals)
      }
    }
  }
}

run_simulation(5, c(100, 1000, 10000), p=c(10, 20, 50), cutoff=0.05)

