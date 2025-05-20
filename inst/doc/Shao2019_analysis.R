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
processedShao = processDataCube(Shao2019, sparsityThreshold=0.9, considerGroups=TRUE, groupVariable="Delivery_mode", CLR=TRUE, centerMode=1, scaleMode=2)

## ----Shao2019 num comp selection----------------------------------------------
# Setup
# For computational purposes we deviate from the default settings
minNumComponents = 1
maxNumComponents = 4
numRepetitions = 3 # number of randomly initialized models
numFolds = 5 # number of jack-knifed models
maxit = 200
ctol= 1e-5 # this is a really bad setting but is needed to save computational time
numCores = 1

colourCols = c("Delivery_mode", "phylum", "")
legendTitles = c("Delivery mode", "Phylum", "")
xLabels = c("Subject index", "Feature index", "Time index")
legendColNums = c(3,5,0)
arrangeModes = c(TRUE, TRUE, FALSE)
continuousModes = c(FALSE,FALSE,TRUE)

# Assess the metrics to determine the correct number of components
qualityAssessment = assessModelQuality(processedShao$data, minNumComponents, maxNumComponents, numRepetitions, ctol=ctol, maxit=maxit, numCores=numCores)

## ----overview plot------------------------------------------------------------
qualityAssessment$plots$overview

## ----model stability----------------------------------------------------------
stabilityAssessment = assessModelStability(processedShao, minNumComponents=1, maxNumComponents=4, numFolds=numFolds, considerGroups=TRUE,
                                           groupVariable="Delivery_mode", colourCols, legendTitles, xLabels, legendColNums, arrangeModes,
                                           ctol=ctol, maxit=maxit, numCores=numCores)

stabilityAssessment$modelPlots[[1]]
stabilityAssessment$modelPlots[[2]]
stabilityAssessment$modelPlots[[3]]
stabilityAssessment$modelPlots[[4]]

## ----model selection----------------------------------------------------------
numComponents = 3
modelChoice = which(qualityAssessment$metrics$varExp[,numComponents] == max(qualityAssessment$metrics$varExp[,numComponents]))
finalModel = qualityAssessment$models[[numComponents]][[modelChoice]]

## ----model plot---------------------------------------------------------------
plotPARAFACmodel(finalModel$Fac, processedShao, 3, colourCols, legendTitles, xLabels, legendColNums, arrangeModes,
  continuousModes = c(FALSE,FALSE,TRUE),
  overallTitle = "Shao PARAFAC model")

## ----flip loadings------------------------------------------------------------
finalModel = flipLoadings(finalModel, processedShao$data)

plotPARAFACmodel(finalModel$Fac, processedShao, 3, colourCols, legendTitles, xLabels, legendColNums, arrangeModes,
  continuousModes = c(FALSE,FALSE,TRUE),
  overallTitle = "Shao PARAFAC model")

