suppressPackageStartupMessages({
  if (!require(quanteda)) {install.packages("quanteda")}; library(quanteda)
  if (!require(magrittr)) {install.packages("magrittr")}; library(magrittr)
  if (!require(caret)){install.packages("caret")}; library(caret)
  if (!require(quanteda.textmodels)) {install.packages("quanteda.textmodels")}; library(quanteda.textmodels)
  if (!require(shinyWidgets)) {install.packages("shinyWidgets")}; library(shinyWidgets)
  if (!require(DT)) {install.packages("DT")}; library(DT)
  if (!require(shiny)) {install.packages("shiny")}; library(shiny)
  if (!require(wordcloud)) {install.packages("wordcloud")}; library(wordcloud)
  
})
