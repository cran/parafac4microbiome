## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 12,
  fig.height = 6
)
options(tibble.print_min = 6L, tibble.print.max = 6L, digits = 3)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(parafac4microbiome)
library(dplyr)
library(ggplot2)

## ----datasets-----------------------------------------------------------------
dim(Fujita2023$data)
dim(Shao2019$data)
dim(vanderPloeg2024$data)

# We focus on Fujita2023
head(Fujita2023$data[,,1])
head(Fujita2023$mode1)
head(Fujita2023$mode2)
head(Fujita2023$mode3)

## ----data processing----------------------------------------------------------
processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, CLR=TRUE, centerMode=1, scaleMode=2)
head(processedFujita$data[,,1])

## ----fujita2023 modelling-----------------------------------------------------
set.seed(0) # for reproducibility
model = parafac(processedFujita$data, nfac=3, verbose=FALSE)

head(model$Fac[[1]])
head(model$Fac[[2]])
head(model$Fac[[3]])
model$varExp

## ----plotting-----------------------------------------------------------------
plotPARAFACmodel(model$Fac, processedFujita,
  numComponents = 3,
  colourCols = c("", "Genus", ""),
  legendTitles = c("", "Genus", ""),
  xLabels = c("Replicate", "Feature index", "Time point"),
  legendColNums = c(0,5,0),
  arrangeModes = c(FALSE, TRUE, FALSE),
  continuousModes = c(FALSE,FALSE,TRUE),
  overallTitle = "Fujita PARAFAC model")

