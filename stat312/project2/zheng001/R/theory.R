#'@title theoretical_rate
#'@param params Simulation parameters
#'@param de A sample of densities for points drawn from the prior
#'@export
theoretical_rate <- function(
  params, 
  de = density_at(params@prior,
                  sample_points(params@prior, 1e5))
)
{
  d <- params@prior@dimension
  temp <- (params@sigma ^2)*(de * (params@k - 1) * vball(d))^(2/d)
  mean(pchisq(1/temp, d))
}