suppressPackageStartupMessages({
  if (!require(quanteda)) {install.packages("quanteda")}; library(quanteda)
  if (!require(magrittr)) {install.packages("magrittr")}; library(magrittr)
  if (!require(caret)){install.packages("caret")}; library(caret)
  if (!require(quanteda.textmodels)) {install.packages("quanteda.textmodels")}; library(quanteda.textmodels)
  if (!require(shinyWidgets)) {install.packages("shinyWidgets")}; library(shinyWidgets)
  if (!require(DT)) {install.packages("DT")}; library(DT)
  if (!require(shiny)) {install.packages("shiny")}; library(shiny)
  if (!require(wordcloud)) {install.packages("wordcloud")}; library(wordcloud)
  if (!require(e1071)) {install.packages("e1071")}; library(e1071)
  if (!require(RColorBrewer)) {install.packages("RColorBrewer")}; library(RColorBrewer)
  if (!require(tidyr)) {install.packages("tidyr")}; library(tidyr)
  
  #if (!require(lexion)) {install.packages("lexion")}; library(lexion)
  
  
})
