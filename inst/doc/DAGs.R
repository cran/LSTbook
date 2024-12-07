## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE------------------------------------------------------------
library(LSTbook)
library(tibble)

## -----------------------------------------------------------------------------
Example1 <- datasim_make(
  x <- rnorm(n, mean = 0, sd = 2), 
  y <- -3 + 10 * x + rnorm(n, mean = 0, sd = 5)
)
Example1 |> take_sample(n=6,  seed = 101)

## ----error = TRUE-------------------------------------------------------------
Wrong_way <- datasim_make(x = rnorm(n, mean=0, sd=2)) # don't use =

## -----------------------------------------------------------------------------
Example1 |> take_sample(n = 3)
Example1 |> take_sample(n = 2)
# and so on

## -----------------------------------------------------------------------------
print(sim_04) # One of the simulations in {LST}

## ----label='DAGsMFIYTT'-------------------------------------------------------
dag_draw(sim_04)

## ----error = TRUE-------------------------------------------------------------
cycle <- datasim_make(x <- y, y <- -x)

## ----label='DAGswTgvGw'-------------------------------------------------------
print(sim_vaccine)
dag_draw(sim_vaccine)

## -----------------------------------------------------------------------------
sim_data <- sim_vaccine |> take_sample(n = 10000)

## -----------------------------------------------------------------------------
sim_data |> model_train(flu ~ vaccinated) |> conf_interval()

## -----------------------------------------------------------------------------
sim_data |> model_train(zero_one(died, one="yes") ~ flu) |> conf_interval()

## ----label='DAGsJsATe2'-------------------------------------------------------
print(sim_vaccine, report_hidden = TRUE)
dag_draw(sim_vaccine, report_hidden = TRUE)

## -----------------------------------------------------------------------------
Randomized_trial_sim <- datasim_intervene(sim_vaccine, .v <- c(-5, 5))
dag_draw(Randomized_trial_sim, report_hidden = TRUE)

## -----------------------------------------------------------------------------
RTdata <- Randomized_trial_sim |> take_sample(n=10000, report_hidden = TRUE)
RTdata |> model_train(.f ~ .v + .h) |> conf_interval()

## -----------------------------------------------------------------------------
n <- 100
x <- rnorm(n, mean = 0, sd = 2) 
y <- -3 + 10 * x + rnorm(n, mean = 0, sd = 5)

## -----------------------------------------------------------------------------
n <- 10
color <- categorical(n, "red", "blue", "green")
color

## -----------------------------------------------------------------------------
color <- categorical(n, red = .1, blue = .2, green = .7)
color

## -----------------------------------------------------------------------------
flip <- bernoulli(n)
flip

## -----------------------------------------------------------------------------
flip <- bernoulli(n, labels = c("bad", "good"))
flip

## -----------------------------------------------------------------------------
vals <- cat2value(flip, good = 2, bad = -1 )
vals
result <- bernoulli(logodds = vals, labels = c("fail", "succeed"))
result

## -----------------------------------------------------------------------------
result <- bernoulli(logodds = cat2value(flip, good=2, bad = -1))
result

## -----------------------------------------------------------------------------
treatment <- block_by(color, levels = c("glue", "nails", "screws"))
treatment

## -----------------------------------------------------------------------------
all_together <- datasim_make(
  color <- categorical(n, red = .1, blue = .2, green = .7),
  flip <- bernoulli(n, labels = c("bad", "good")),
  result <- bernoulli(logodds = cat2value(flip, good=2, bad = -1)),
  treatment <- block_by(color, levels = c("glue", "nails", "screws"))
)
all_together |> take_sample(n = 20) |> arrange(color)

## -----------------------------------------------------------------------------
Mix_them <- datasim_make(
  x <- rnorm(n),
  y <- mix_with(x, R2 = 0.71, var = 4.35)
)
Dat <- Mix_them |> take_sample(n = 100)
Dat |> model_train(y ~ x) |> R2()
Dat |> summarize(var(y))

## -----------------------------------------------------------------------------
Sick_sim <- datasim_make(
  health <- rnorm(n, sd=2), 
  sick   <- bernoulli(logodds = health, labels = c("ill", "OK"))
)
Sim_dat <- Sick_sim |> take_sample(n = 1000)
Sim_dat |> point_plot(sick ~ health)

## -----------------------------------------------------------------------------
# This won't work!
sum_sim <- datasim_make(
  k <- take_sample(c(1, 4, 16, 32, 64, 128), n = n, replace = TRUE),
  y <- sum(rnorm(k))
)
sum_sim |> take_sample(n = 5)

## -----------------------------------------------------------------------------
# This will do what we want!
sum_sim <- datasim_make(
  k <- take_sample(c(1, 4, 16, 32, 64, 128), n = n, replace = TRUE),
  y <- each(sum(rnorm(k)))
)
sum_sim |> take_sample(n = 5)

## -----------------------------------------------------------------------------
sum_sim |> take_sample(n = 10000) |>
  point_plot(y ~ k, point_ink = 0.1)

