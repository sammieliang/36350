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

run_simulation = function(n_trials, n, p, cutoff)
{
  all.vals = c()
  for (i in 1:n_trials)
  {
    for (curr_n in n)
    {
      for (curr_p in p)
      {
        data.list = generate_data(curr_n, curr_p)
        vals = model_select(data.list[["covariates"]], data.list[["responses"]], cutoff)
        all.vals = c(vals, all.vals)
      }
    }
  }
  save(all.vals, file="p-vals.RData")
}

run_simulation(5, c(100, 1000, 10000), p=c(10, 20, 50), cutoff=0.05)

make_plot = function(datapath)
{
  load(datapath)
  hist(all.vals)
}

make_plot("p-vals.RData")
