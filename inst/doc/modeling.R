## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(LSTbook)

## -----------------------------------------------------------------------------
height_model <- mosaicData::Galton |> model_train(height ~ sex + mother + father)

## -----------------------------------------------------------------------------
vote_model <- 
  Go_vote |> 
  model_train(zero_one(primary2006, one = "voted") ~ yearofbirth * primary2004 * hhsize * yearofbirth )

## -----------------------------------------------------------------------------
height_model |> model_plot()
height_model |> conf_interval()
vote_model |> model_plot()
vote_model |> R2()

## -----------------------------------------------------------------------------
vote_model |> model_eval(yearofbirth=c(1960, 1980), primary2004="voted", hhsize=4)

