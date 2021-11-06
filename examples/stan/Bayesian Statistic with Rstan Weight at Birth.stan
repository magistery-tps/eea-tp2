data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  alpha ~ normal(0, 1000);
  beta ~ normal(0, 100);
  sigma ~ exponential(0.1);
#  y ~ normal(alpha + beta * x, sigma);
  y ~ student_t(2,alpha + beta * x, sigma);

}