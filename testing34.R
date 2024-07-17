
sim1 <- function(i){
 x <- rpois(25, lambda = 2.5)
 return(c(mean = mean(x), median = median(x)))
}

sim1 <- function(i){
  x <- rpois(25, lambda = 50)
  return(c(mean = mean(x), median = median(x)))
}

sim1 <- function(i){
  x <- rpois(5, lambda = 3.4)
  return(c(mean = mean(x), median = median(x)))
}


res <- replicate(10000, sim1(1)) 
rowMeans(res)
apply(res, 1, sd)

sim2 <- function(i){
x1 <- rnorm(1000, mean = 8)
x2 <- rnorm(1000, mean = -4)
x3 <- x2 * 2 + rnorm(1000, sd = 0.05)
y <- 4 - 2.3 * x1 + 3.4 * x2 + rnorm(1000, sd = 2.3)
df <- tibble(x1, x2, x3, y)
res <- lm(y ~ x1, data = df)
return(c(coef(res), sd = sigma(res)))
}
sres <- replicate(10000, sim2(2))
rowMeans(sres)


## Bernoulli Distribution
sims3 <- function(i){
x1 <- rpois(1000, lambda = 3)
x2 <- rbinom(1000, 1, prob = 0.8)
eta <- boot::inv.logit(0.25 + .83 * x1 + .25 * x2)
y <- rbinom(1000, size = 1, prob = eta)
df <- tibble(x1, x2, y)
res <- glm(y ~ x1 + x2, df, family=binomial())
return(coef(res))
}
sres <- replicate(1000, sims3(4))
rowMeans(sres)

x1 <- rpois(1000, lambda = 3)
x2 <- rbinom(1000, 1, prob = 0.8)
eta <- boot::inv.logit(0.25 + .83 * x1 + .25 * x2)
y <- rbinom(1000, size = 1, prob = eta)
df <- tibble(x1, x2, y)
library(brms)
brm(y ~ x1 + x2, df, family = bernoulli())
glm(y ~ x1 + x2, df, family=binomial())


x <- rnorm(1000)
eta <- exp(-0.85 + 1.3 * x)
y <- rnbinom(1000, mu = eta, size = 0.5)
MASS::glm.nb(y ~ x)
brm(y ~ x, tibble(x, y), family = negbinomial())
