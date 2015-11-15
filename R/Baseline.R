#### Build right baseline model

# Load libraries
library(dplyr)
library(caret)
library(magrittr)
library(tidyr)
source("./R/Utils.R")

# Read data
train <- readr::read_csv("./Data/train.csv")

# Build median impute model based on target variables
impute_model <-
    preProcess(train %>% select(148:209),
               method = "medianImpute")

# Build empty data frame
target_vars <- c(paste0("Ret_", 121:180), "Ret_PlusOne", "Ret_PlusTwo")
preds <- matrix(data = NA, nrow = 60000, ncol = length(target_vars)) %>%
    data.frame
colnames(preds) <- target_vars
preds %<>% predict(impute_model, .)

# Process for export
preds %<>% PrepData
write.csv(preds, file = "./Data/Baseline.csv", row.names = FALSE)