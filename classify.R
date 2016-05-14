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

set.seed(20160512)

n_train <- 10000
n_validation <- 1000
n_sets <- 100

subset_idx <- sample.int(nrow(queries), size = n_train + n_validation * n_sets, replace = FALSE)
queries_subset <- as.matrix(as.data.frame(queries)[subset_idx, c("zero_result", "n_terms", "n_chars", "n_feats")])
queries_subset[, "n_terms"] <- standardize(log10(queries_subset[, "n_terms"] + 1))
queries_subset[, "n_chars"] <- standardize(log10(queries_subset[, "n_chars"] + 1))
queries_subset[, "n_feats"] <- standardize(sqrt(queries_subset[, "n_feats"]))
# countries_subset <- dummy::dummy(as.data.frame(queries)[subset_idx, "country", drop = FALSE])
features_matrix_subset <- as.matrix(features_matrix[subset_idx, ])
temp <- cbind(queries_subset, features_matrix_subset)
# temp <- cbind(queries_subset[, "zero_result"], countries_subset)
# temp <- cbind(queries_subset, features_matrix_subset, countries_subset)
rm(queries_subset, features_matrix_subset, countries_subset); rm(subset_idx)
validation_idx <- sample.int(nrow(temp), n_validation * n_sets, replace = FALSE)
# ^ will be used for out-of-bag validation
remainder_idx <- setdiff(1:nrow(temp), validation_idx)
training_idx <- sample(remainder_idx, floor(0.8 * length(remainder_idx)), replace = FALSE)
test_idx <- setdiff(remainder_idx, training_idx)

# We don't need no cross validation with the amount of data we have!
library(progress)
pb <- progress_bar$new(total = (ncol(temp)-1) * n_sets)
validation_sets <- matrix(validation_idx, ncol = n_sets)
misclassification_errors <- lapply(1:(ncol(temp)-1), function(m) {
  rf <- randomForest(x = temp[training_idx, -1], xtest = temp[test_idx, -1],
                     y = factor(temp[training_idx, 1], 0:1, c("some results", "zero results")),
                     ytest = factor(temp[test_idx, 1], 0:1, c("some results", "zero results")),
                     samplesize = ceiling(0.6 * nrow(temp)), nodesize = 3, mtry = m,
                     importance = FALSE, keep.forest = TRUE, proximity = FALSE)
  misclassification_rates <- apply(validation_sets, 2, function(validation_set) {
    # predictions <- predict(rf, temp[validation_set, -1], type = "vote", norm.votes = TRUE)
    predictions <- predict(rf, temp[validation_set, -1], type = "response")
    levels(predictions) <- union(levels(predictions), c("some results", "zero results"))
    true_labels <- factor(temp[validation_set, 1], 0:1, c("some results", "zero results"))
    # predicted_labels <- colnames(predictions)[apply(predictions, 1, which.max)]
    confusion_matrix <- table(true_labels, predictions)
    pb$tick()
    return((confusion_matrix["some results", "zero results"] + confusion_matrix["zero results", "some results"])/length(validation_set))
  })
  return(data.frame(point.est = mean(misclassification_rates),
                    std.dev = sd(misclassification_rates),
                    lower = unname(quantile(misclassification_rates, 0.025)),
                    upper = unname(quantile(misclassification_rates, 0.975))))
}) %>% dplyr::bind_rows(.id = "m")
misclassification_errors$m <- as.numeric(misclassification_errors$m)

print(misclassification_errors$m[which.min(misclassification_errors$point.est)])
# 17.17% at mtry = 19 (without country data)

library(txtplot) # install.packages("txtplot")
txtplot(misclassification_errors$m, misclassification_errors$point.est,
        ylim = range(misclassification_errors[, c("lower", "upper")]),
        xlab = "mtry", ylab = "misclassification error")

p <- ggplot(data = misclassification_errors,
            aes(x = m, y = point.est)) +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.1) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymax = point.est + std.dev, ymin = point.est - std.dev)) +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  scale_y_continuous("Misclassification Error Rate", labels = scales::percent_format()) +
  scale_x_continuous(breaks = misclassification_errors$m) +
  labs(x = "Number of Variables Considered At Each Split",
       subtitle = paste("Validating a random forest (trained on", n_train, "units) using", n_sets, "sets of", n_validation, "units each."),
       title = "Misclassification using validation dataset.") +
  geom_vline(xintercept = misclassification_errors$m[which.min(misclassification_errors$point.est)],
             linetype = "dashed")
print(p)
ggsave("mis_rate.png", p, path = fig_path, units = "in", dpi = 150, height = 8, width = 12)

# misclassification_errors$m[which.min(misclassification_errors$point.est)] # 16?
rf <- randomForest(x = temp[training_idx, -1], xtest = temp[test_idx, -1],
                   y = factor(temp[training_idx, 1], 0:1, c("some results", "zero results")),
                   ytest = factor(temp[test_idx, 1], 0:1, c("some results", "zero results")),
                   ntree = 200, samplesize = ceiling(0.6 * nrow(temp)),
                   nodesize = 5, mtry = 19, # mtry = floor(sqrt(ncol(temp)-1)), # tune with CV
                   importance = TRUE, keep.forest = TRUE, proximity = TRUE, do.trace = TRUE)

# plot(rf); varImpPlot(rf); par(mfrow = c(1, 1))
# MDSplot(rf, fac = factor(temp[test_idx, 1], 0:1, c("some results", "zero results")))

variable_importance <- data.frame(Variable = rownames(importance(rf)), importance(rf)) %>%
  set_rownames(NULL)
p1 <- ggplot(data = variable_importance,
             aes(x = reorder(Variable, -zero.results),
                 y = zero.results)) +
  geom_bar(stat = "identity", aes(fill = ifelse(zero.results >= 0, "Higher", "Lower"))) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Set1")[2:1],
                    guide = guide_legend(title = "Chances of zero results")) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  labs(x = "Variable", y = "Mean Importance", subtitle = fig_subtitle,
       title = "Importance of features in deciding whether query will have zero results.") +
  theme(legend.position = "bottom")
p2 <- ggplot(data = variable_importance,
             aes(x = reorder(Variable, -MeanDecreaseGini),
                 y = MeanDecreaseGini)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  labs(x = "Variable", y = "Mean Decrease Gini", subtitle = fig_subtitle,
       title = "Importance of features in partitioning.")
p3 <- ggplot(data = variable_importance,
             aes(x = reorder(Variable, -MeanDecreaseAccuracy),
                 y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0) +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  labs(x = "Variable", y = "Mean Decrease in Accuracy", subtitle = fig_subtitle,
       title = "Importance of features in quality of classification.") +
  theme(legend.position = "bottom")
p <- plot_grid(p1, p2, p3, nrow = 1)
print(p)
ggsave("var_imp.png", p, path = fig_path, units = "in", dpi = 150, height = 8, width = 24)

## Note: look into parallel random forests -- https://bitbucket.org/mkuhn/parallelrandomforest
## See also: http://blog.mckuhn.de/2013/09/introducing-parallelrandomforest-faster.html
# devtools::install_bitbucket("mkuhn/parallelRandomForest", ref = "parallelRandomForest")

# Parallelize:
library(foreach)
library(doMC)
registerDoMC(cores = 4)
## Use all available data:
queries_subset <- as.matrix(as.data.frame(queries)[, c("zero_result", "n_terms", "n_chars", "n_feats")])
queries_subset[, "n_terms"] <- standardize(log10(queries_subset[, "n_terms"] + 1))
queries_subset[, "n_chars"] <- standardize(log10(queries_subset[, "n_chars"] + 1))
queries_subset[, "n_feats"] <- standardize(sqrt(queries_subset[, "n_feats"]))
temp <- cbind(queries_subset, as.matrix(features_matrix))
rm(queries_subset, queries, features_matrix); rm(subset_idx)
training_idx <- sample.int(nrow(temp), floor(0.8 * nrow(temp)), replace = FALSE)
test_idx <- setdiff(1:nrow(temp), training_idx)
x_train <- temp[training_idx, -1]; x_test <- temp[test_idx, -1]
y_train <- factor(temp[training_idx, 1], 0:1, c("some results", "zero results"))
y_test <- factor(temp[test_idx, 1], 0:1, c("some results", "zero results"))
rm(temp, n_validation, n_sets)
rf <- foreach(ntree = rep(50, 4), .combine = combine, .multicombine = TRUE, .maxcombine = 3) %dopar%
  randomForest(x = x_train, xtest = x_test, y = y_train, ytest = y_test,
               samplesize = ceiling(0.6 * nrow(temp)),
               nodesize = 99, mtry = 19, ntree = ntree,
               importance = TRUE, keep.forest = TRUE, proximity = FALSE)
save(rf, file = "random_forest.RData")
