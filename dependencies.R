suppressPackageStartupMessages({
  if (!require(quanteda)) {install.packages("quanteda")}; library(quanteda)
  if (!require(magrittr)) {install.packages("magrittr")}; library(magrittr)
  if (!require(caret)){install.packages("caret")}; library(caret)
  if (!require(quanteda.textmodels)) {install.packages("quanteda.textmodels")}; library(quanteda.textmodels)
})
