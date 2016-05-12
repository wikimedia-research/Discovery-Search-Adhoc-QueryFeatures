library(data.table)
library(magrittr)
library(randomForest)
if (!"subtitle" %in% names(formals(ggplot2::ggtitle))) {
  devtools::install_github("hadley/ggplot2")
}
library(ggplot2)
library(cowplot)

source("config.R")
queries <- readr::read_rds(file.path(data_root, dataset_to_use))
source("refine.R") # yields: features_matrix

standardize <- function(x) { return((x - mean(x))/sd(x)) }

set.seed(20160512)
subset_idx <- sample.int(nrow(queries), size = 11000, replace = FALSE)
queries_subset <- as.matrix(as.data.frame(queries)[subset_idx, c("zero_result", "n_terms", "n_chars", "n_feats")])
queries_subset[, "n_terms"] <- standardize(log10(queries_subset[, "n_terms"]))
queries_subset[, "n_chars"] <- standardize(log10(queries_subset[, "n_chars"]))
queries_subset[, "n_chars"] <- standardize(sqrt(queries_subset[, "n_feats"]))
# countries_subset <- dummy::dummy(as.data.frame(queries)[subset_idx, "country", drop = FALSE])
features_matrix_subset <- as.matrix(features_matrix[subset_idx, ])
temp <- cbind(queries_subset, features_matrix_subset) #, countries_subset)
rm(queries_subset, features_matrix_subset, countries_subset); rm(subset_idx)
validation_idx <- sample.int(nrow(temp), 1000, replace = FALSE)
# ^ will be used for out-of-bag validation
remainder_idx <- setdiff(1:nrow(temp), validation_idx)
training_idx <- sample(remainder_idx, floor(0.8 * length(remainder_idx)), replace = FALSE)
test_idx <- setdiff(remainder_idx, training_idx)

# distance_matrix <- dist(temp)
# hc <- hclust(distance_matrix) # plot(hc)

set.seed(0)
rf <- randomForest(x = temp[training_idx, -1], xtest = temp[test_idx, -1],
                   y = factor(temp[training_idx, 1], 0:1, c("some results", "zero results")),
                   ytest = factor(temp[test_idx, 1], 0:1, c("some results", "zero results")),
                   ntree = 200, samplesize = ceiling(0.6 * nrow(temp)),
                   mtry = floor(sqrt(ncol(temp)-1)), # tune this with CV???
                   importance = TRUE, keep.forest = TRUE, proximity = TRUE)
# plot(rf)
varImpPlot(rf); par(mfrow = c(1, 1))
MDSplot(rf, fac = factor(temp[training_idx, 1], 0:1, c("some results", "zero results")))

variable_importance <- data.frame(Variable = rownames(importance(rf)), importance(rf)) %>%
  set_rownames(NULL)
p1 <- ggplot(data = variable_importance,
             aes(x = reorder(Variable, -zero.results),
                 y = zero.results)) +
  geom_bar(stat = "identity", aes(fill = ifelse(zero.results >= 0, "Higher", "Lower"))) +
  scale_fill_brewer(type = "qual", palette = "Set1", direction = -1,
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
  geom_bar(stat = "identity", aes(fill = ifelse(MeanDecreaseAccuracy >= 0, "Better", "Worse"))) +
  scale_fill_brewer(type = "qual", palette = "Set1", direction = -1,
                    guide = guide_legend(title = "Accuracy")) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  labs(x = "Variable", y = "Mean Decrease in Accuracy", subtitle = fig_subtitle,
       title = "Importance of features in quality of classification.") +
  theme(legend.position = "bottom")
p <- plot_grid(p1, p2, p3, nrow = 1)
print(p)
ggsave("var_imp.png", p, path = fig_path, units = "in", dpi = 150, height = 8, width = 24)

predictions <- predict(rf, temp[validation_idx, -1], type = "vote", norm.votes = TRUE)
true_labels <- as.character(factor(temp[validation_idx, 1], 0:1, c("some results", "zero results")))
predicted_labels <- colnames(predictions)[apply(predictions, 1, which.max)]
(confusion_matrix <- table(true_labels, predicted_labels))

## Note: look into parallel random forests -- https://bitbucket.org/mkuhn/parallelrandomforest
## See also: http://blog.mckuhn.de/2013/09/introducing-parallelrandomforest-faster.html
# library(devtools)
# install_bitbucket("mkuhn/parallelRandomForest", ref = "parallelRandomForest")
