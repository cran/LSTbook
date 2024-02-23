## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(LSTbook)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
mtcars |> point_plot(mpg ~ hp, annot = "model")

## -----------------------------------------------------------------------------
mtcars |> 
  dplyr::mutate(consumption = 235.2 / mpg) |>
  model_train(consumption ~ hp + wt) |> 
  conf_interval()

## -----------------------------------------------------------------------------
mtcars |> 
  point_plot(mpg ~ hp * wt) |> 
  add_plot_labels(x = "Engine power (hp)", y = "Fuel economy (mpg)")

## -----------------------------------------------------------------------------
sim_06

## -----------------------------------------------------------------------------
mtcars |> 
  sample(replace = TRUE) |> # resampling here!
  model_train(mpg ~ hp) |> 
  conf_interval() |> 
  filter(term == "hp") |>
  trials(5) 
  

