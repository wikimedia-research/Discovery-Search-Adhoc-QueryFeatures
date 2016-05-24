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

n_train <- 0.1 * nrow(queries)
n_validation <- n_train/10

subset_idx <- sample.int(nrow(queries), size = n_train + n_validation, replace = FALSE)
queries_subset <- as.matrix(as.data.frame(queries)[subset_idx, c("zero_result", "n_terms", "n_chars", "n_feats")])
queries_subset[, "n_terms"] <- standardize(log10(queries_subset[, "n_terms"] + 1))
queries_subset[, "n_chars"] <- standardize(log10(queries_subset[, "n_chars"] + 1))
queries_subset[, "n_feats"] <- standardize(sqrt(queries_subset[, "n_feats"]))
# countries_subset <- dummy::dummy(as.data.frame(queries)[subset_idx, "country", drop = FALSE])
features_matrix_subset <- as.matrix(features_matrix[subset_idx, ])
temp <- cbind(queries_subset, features_matrix_subset) #, countries_subset)
rm(queries_subset, features_matrix_subset, countries_subset); rm(subset_idx)
validation_idx <- sample.int(nrow(temp), n_validation, replace = FALSE)
# ^ will be used for out-of-bag validation
remainder_idx <- setdiff(1:nrow(temp), validation_idx)

logistic_regression <- glm(zero_result ~ ., data = as.data.frame(temp[, 1:23]), family = "binomial")
summary(logistic_regression) # confint(logistic_regression)

predictions <- predict(logistic_regression, newdata = as.data.frame(temp[validation_idx , 2:23]),
                       type = "response")

confidence_intervals <- confint.default(logistic_regression) %>%
  as.data.frame %>% { .$term <- gsub("`", "", rownames(.)); . } %>%
  set_rownames(NULL) %>%
  dplyr::mutate(estimate = coef(logistic_regression),
                `increases chances of` = ifelse(estimate > 0, "zero results", "some results"))
p <- ggplot(confidence_intervals, aes(y = estimate, x = term, color = `increases chances of`)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = `2.5 %`, ymax = `97.5 %`)) +
  scale_y_continuous(limits = c(-10, 10)) +
  scale_color_brewer(type = "qual", palette = "Set1", direction = -1) +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  labs(x = "Variable", y = "Estimate", subtitle = fig_subtitle,
       title = "Logistic regression coefficients") +
  theme(legend.position = "bottom", panel.grid = element_line(color = "gray80"),
        panel.grid.major.y = element_line(color = "black", size = 0.1))
print(p)
ggsave("logit_coef.png", p, path = fig_path, units = "in", dpi = 150, height = 6, width = 8)

library(caret)
x <- confusionMatrix(data = factor(predictions > 0.5, c(FALSE, TRUE), c("some results", "zero results")),
                     reference = factor(temp[validation_idx, 1], 0:1, c("some results", "zero results")),
                     positive = "zero results")

table(actual = factor(temp[validation_idx, 1], 0:1, c("some results", "zero results")),
      predicted = factor(predictions > 0.5, c(FALSE, TRUE), c("some results", "zero results")))[2:1, 2:1]

logistic_regression %>%
  broom::tidy() %>%
  dplyr::select(c(term, estimate, std.error)) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(`odds ratio` = exp(estimate),
                `increases chances of` = ifelse(estimate > 0, "zero results", "some results")) %>%
  knitr::kable(digits = 3)
