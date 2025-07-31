## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 12,
  fig.height = 6
)
options(tibble.print_min = 6L, tibble.print.max = 6L, digits = 3)

## ----setup--------------------------------------------------------------------
library(parafac4microbiome)
library(dplyr)
library(ggplot2)
library(ggpubr)

## ----data processing----------------------------------------------------------
processedPloeg = processDataCube(vanderPloeg2024$upper_jaw_lingual, sparsityThreshold=0.50, considerGroups=TRUE, groupVariable="RFgroup", CLR=TRUE, centerMode=1, scaleMode=2)

## ----vanderPloeg2024 num comp selection---------------------------------------
# Setup
# For computational purposes we deviate from the default settings
minNumComponents = 1
maxNumComponents = 3
numRepetitions = 3 # number of randomly initialized models
numFolds = 5 # number of jack-knifed models
ctol = 1e-5
maxit = 200
numCores = 1

colourCols = c("RFgroup", "Phylum", "")
legendTitles = c("RF group", "Phylum", "")
xLabels = c("Subject index", "Feature index", "Time index")
legendColNums = c(3,5,0)
arrangeModes = c(TRUE, TRUE, FALSE)
continuousModes = c(FALSE,FALSE,TRUE)

# Assess the metrics to determine the correct number of components
qualityAssessment = assessModelQuality(processedPloeg$data, minNumComponents, maxNumComponents, numRepetitions, ctol=ctol, maxit=maxit, numCores=numCores)

## ----overview plot------------------------------------------------------------
qualityAssessment$plots$overview

## ----model selection----------------------------------------------------------
numComponents = 2
modelChoice = which(qualityAssessment$metrics$varExp[,numComponents] == max(qualityAssessment$metrics$varExp[,numComponents]))
finalModel = qualityAssessment$models[[numComponents]][[modelChoice]]

## ----model plot---------------------------------------------------------------
plotPARAFACmodel(finalModel$Fac, processedPloeg, 2, colourCols, legendTitles, xLabels, legendColNums, arrangeModes, continuousModes,
  overallTitle = "vanderPloeg2024 PARAFAC model")

## ----flip loadings------------------------------------------------------------
finalModel = flipLoadings(finalModel, processedPloeg$data)

plotPARAFACmodel(finalModel$Fac, processedPloeg, 2, colourCols, legendTitles, xLabels, legendColNums, arrangeModes,
  continuousModes = c(FALSE,FALSE,TRUE),
  overallTitle = "Ploeg PARAFAC model")

