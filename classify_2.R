library(data.table)
library(magrittr)
library(randomForest)
if (!"subtitle" %in% names(formals(ggplot2::ggtitle))) {
  devtools::install_github("hadley/ggplot2")
}
library(ggplot2)
library(cowplot)

dataset_to_use <- "web_excl-known-automata.rds"
source("config.R")
queries <- readr::read_rds(file.path(data_root, dataset_to_use))
source("refine.R") # yields: features_matrix

standardize <- function(x) { return((x - mean(x))/sd(x)) }

# Do a test run with all available data:
queries_subset <- as.matrix(as.data.frame(queries)[, c("zero_result", "n_terms", "n_chars", "n_feats")])
queries_subset[, "n_terms"] <- standardize(log10(queries_subset[, "n_terms"] + 1))
queries_subset[, "n_chars"] <- standardize(log10(queries_subset[, "n_chars"] + 1))
queries_subset[, "n_feats"] <- standardize(sqrt(queries_subset[, "n_feats"]))
temp <- cbind(queries_subset, as.matrix(features_matrix))
rm(queries_subset, queries, features_matrix)
training_idx <- sample.int(nrow(temp), floor(0.8 * nrow(temp)), replace = FALSE)
test_idx <- setdiff(1:nrow(temp), training_idx)
x_train <- temp[training_idx, -1]; x_test <- temp[test_idx, -1]
y_train <- factor(temp[training_idx, 1], 0:1, c("some results", "zero results"))
y_test <- factor(temp[test_idx, 1], 0:1, c("some results", "zero results"))
rm(temp)

system.time({
  rf <- randomForest(x = x_train, xtest = x_test, y = y_train, ytest = y_test,
                     samplesize = ceiling(0.6 * nrow(x_train)),
                     nodesize = 99, mtry = 19, ntree = 400, keep.forest = TRUE,
                     do.trace = 10, importance = TRUE, proximity = FALSE)
})['elapsed']
save(rf, file = "random_forest.RData")

# Parallelize:
library(foreach)
library(doMC)
registerDoMC(cores = 4)

rf <- foreach(ntree = rep(100, 4), .combine = combine, .multicombine = TRUE, .maxcombine = 4) %dopar%
  randomForest(x = x_train, xtest = x_test, y = y_train, ytest = y_test,
               samplesize = 2800000, nodesize = 99, mtry = 19, ntree = ntree,
               importance = TRUE, keep.forest = TRUE, proximity = FALSE)
save(rf, file = "random_forest.RData")
