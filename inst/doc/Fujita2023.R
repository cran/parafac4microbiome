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
processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, CLR=TRUE, centerMode=1, scaleMode=2)

## ----Fujita num comp selection------------------------------------------------
# Setup
minNumComponents = 1
maxNumComponents = 3
numRepetitions = 3 # number of randomly initialized models
numFolds = 4 # number of jack-knifed models
ctol = 1e-5
maxit = 200
numCores= 1

# Plot settings
colourCols = c("", "Genus", "")
legendTitles = c("", "Genus", "")
xLabels = c("Replicate", "Feature index", "Time point")
legendColNums = c(0,5,0)
arrangeModes = c(FALSE, TRUE, FALSE)
continuousModes = c(FALSE,FALSE,TRUE)

# Assess the metrics to determine the correct number of components
qualityAssessment = assessModelQuality(processedFujita$data, minNumComponents, maxNumComponents, numRepetitions, ctol=ctol, maxit=maxit, numCores=numCores)

## ----overview plot------------------------------------------------------------
qualityAssessment$plots$overview

## ----model stability----------------------------------------------------------
stabilityAssessment = assessModelStability(processedFujita, minNumComponents=1, maxNumComponents=3, numFolds=numFolds, considerGroups=FALSE,
                                           groupVariable="", colourCols, legendTitles, xLabels, legendColNums, arrangeModes,
                                           ctol=ctol, maxit=maxit, numCores=numCores)
stabilityAssessment$modelPlots[[1]]
stabilityAssessment$modelPlots[[2]]
stabilityAssessment$modelPlots[[3]]

## ----model selection----------------------------------------------------------
numComponents = 3
modelChoice = which(qualityAssessment$metrics$varExp[,numComponents] == max(qualityAssessment$metrics$varExp[,numComponents]))
finalModel = qualityAssessment$models[[numComponents]][[modelChoice]]

## ----model plot---------------------------------------------------------------
plotPARAFACmodel(finalModel$Fac, processedFujita, 3, colourCols, legendTitles, xLabels, legendColNums, arrangeModes,
  continuousModes = c(FALSE,FALSE,TRUE),
  overallTitle = "Fujita PARAFAC model")

## ----flip loadings------------------------------------------------------------
finalModel = flipLoadings(finalModel, processedFujita$data)

plotPARAFACmodel(finalModel$Fac, processedFujita, 3, colourCols, legendTitles, xLabels, legendColNums, arrangeModes,
  continuousModes = c(FALSE,FALSE,TRUE),
  overallTitle = "Fujita PARAFAC model")

