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
processedPloeg = processDataCube(vanderPloeg2024, sparsityThreshold=0.50, considerGroups=TRUE, groupVariable="RFgroup", CLR=TRUE, centerMode=1, scaleMode=2)

## ----vanderPloeg2024 num comp selection---------------------------------------
# Setup
# For computational purposes we deviate from the default settings
minNumComponents = 1
maxNumComponents = 3
numRepetitions = 5 # number of randomly initialized models
numFolds = 5 # number of jack-knifed models
ctol = 1e-6
maxit = 250
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

## ----model stability----------------------------------------------------------
stabilityAssessment = assessModelStability(processedPloeg, minNumComponents=1, maxNumComponents=3, numFolds=numFolds, considerGroups=TRUE,
                                           groupVariable="Delivery_mode", colourCols, legendTitles, xLabels, legendColNums, arrangeModes,
                                           ctol=ctol, maxit=maxit, numCores=numCores)

stabilityAssessment$modelPlots[[1]]
stabilityAssessment$modelPlots[[2]]
stabilityAssessment$modelPlots[[3]]

## ----model selection----------------------------------------------------------
numComponents = 2
modelChoice = which(qualityAssessment$metrics$varExp[,numComponents] == max(qualityAssessment$metrics$varExp[,numComponents]))
finalModel = qualityAssessment$models[[numComponents]][[modelChoice]]

## ----model plot---------------------------------------------------------------
plotPARAFACmodel(finalModel$Fac, processedPloeg, 2, colourCols, legendTitles, xLabels, legendColNums, arrangeModes, continuousModes,
  overallTitle = "vanderPloeg2024 PARAFAC model")

## ----flip loadings------------------------------------------------------------
finalModel$Fac[[1]] = -1 * finalModel$Fac[[1]]         # all of mode 1
finalModel$Fac[[2]] = -1 * finalModel$Fac[[2]]         # all of mode 2

plotPARAFACmodel(finalModel$Fac, processedPloeg, 2, colourCols, legendTitles, xLabels, legendColNums, arrangeModes,
  continuousModes = c(FALSE,FALSE,TRUE),
  overallTitle = "Ploeg PARAFAC model")

