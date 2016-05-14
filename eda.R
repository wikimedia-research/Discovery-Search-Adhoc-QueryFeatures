library(data.table)
library(magrittr)
library(tidyr)
import::from(dplyr, group_by, summarize, ungroup, mutate, rename, keep_where = filter, tbl_df, arrange, select)
# Make sure we're only using the version of ggplot2 that supports subtitles:
if (!"subtitle" %in% names(formals(ggplot2::ggtitle))) {
  devtools::install_github("hadley/ggplot2")
}
library(ggplot2)
library(cowplot)

dataset_to_use <- "web_excl-known-automata.rds"
source("config.R")
queries <- readr::read_rds(file.path(data_root, dataset_to_use))
source("refine.R") # yields: features_matrix

# Histogram of searches per sessionb
temp <- queries[, j = list(requests = .N), by = "identity"][, j = list(freq = .N), by = "requests"]
temp$requests[temp$requests >= 10] <- "10+"
p <- temp[, j = list(freq = sum(freq)), by = "requests"] %>%
  mutate(prop = freq/sum(freq)) %>%
  ggplot(aes(x = requests, y = prop)) +
  geom_bar(stat = "identity") +
  scale_y_continuous("Proportion of users", labels = scales::percent_format()) +
  scale_x_discrete("Searches", limits = c(1:9, "10+")) +
  geom_text(aes(label = sprintf("%.1f%%", 100*prop)), nudge_y = 0.025) +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  ggtitle("Proportion of users (identified via 'identity' field) making N searches", subtitle = fig_subtitle)
ggsave("searches_proportions.png", p, path = fig_path, units = "in", dpi = 150, height = 5, width = 8)
rm(temp, p)

# Zero results rate by number of features
queries[, j = list(queries = .N), by = c("query_type", "zero_result", "n_feats")] %>%
  rename(`Number of Features per Query` = n_feats, `Query Type` = query_type) %>%
  mutate(zero_result = ifelse(zero_result, "Queries with zero results", "Queries with some results")) %>%
  spread(zero_result, queries) %>%
  mutate(Queries = `Queries with zero results` + `Queries with some results`,
         `Queries with zero results` = sprintf("%.1f%%", 100*`Queries with zero results`/Queries),
         `Queries with some results` = sprintf("%.1f%%", 100*`Queries with some results`/Queries),
         Queries = polloi::compress(Queries)) %>%
  knitr::kable(align = c("l", "r", "r", "r", "r", "r", "r"), format = "markdown")

# Zero results rate by feature
temp <-  lapply(features_matrix, function(feature) {
  return(queries[i = feature, j = list(queries = .N), by = "zero_result"])
})
p <- temp %>%
  dplyr::bind_rows(.id = "feature") %>%
  mutate(zero_result = ifelse(zero_result, "Zero", "Some")) %>%
  tidyr::spread(zero_result, queries, fill = 0) %>%
  group_by(feature) %>%
  mutate(Total = Zero + Some, ZRR = Zero/Total) %>%
  ggplot(aes(x = feature, y = ZRR)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, 0.1), limits = c(0, 1.2)) +
  geom_text(aes(label = sprintf("%.1f%% of %s", 100*ZRR, polloi::compress(Total, 1))), nudge_y = 0.11) +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  ggtitle("Proportion of searches with zero results by query feature", subtitle = fig_subtitle)
ggsave("zrr_by_feature.png", p, path = fig_path, units = "in", dpi = 150, height = 10, width = 10)
rm(temp, p)

# Most common feature combinations and ZR%
temp <- queries[, j = list(queries = .N), by = c("zero_result", "features")] %>%
  mutate(zero_result = ifelse(zero_result, "Zero Results", "Some Results")) %>%
  tidyr::spread(zero_result, queries, fill = 0) %>%
  mutate(`Total Queries` = `Zero Results` + `Some Results`, `Zero Results Rate` = `Zero Results`/`Total Queries`)
p <- temp %>%
  keep_where(`Total Queries` > 100) %>%
  ggplot(aes(x = reorder(features, -`Total Queries`), y = `Zero Results Rate`)) +
  # ggplot(aes(x = reorder(features, -`Zero Results Rate`), y = `Zero Results Rate`)) +
  geom_bar(stat = "identity", aes(fill = log10(`Total Queries`))) +
  scale_fill_gradient("Number of Queries", low = "#ffeda0", high = "#f03b20",
                      breaks = 1:7, labels = polloi::compress(10^(1:7)),
                      guide = guide_legend(keyheight = 1, heywidth = 3)) +
  scale_x_discrete("Features") +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, 0.1), limits = c(0, 1.2)) +
  geom_text(aes(label = sprintf("%.1f%% of %s", 100*`Zero Results Rate`, polloi::compress(`Total Queries`, 1))),
            nudge_y = 0.1) +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  theme(legend.position = "bottom") +
  ggtitle("Proportion of searches with zero results by query features (sorted by # of queries)", subtitle = fig_subtitle)
  # ggtitle("Proportion of searches with zero results by query features (sorted by ZRR)", subtitle = fig_subtitle)
ggsave("zrr_by_feature_combo_sort-count.png", p, path = fig_path, units = "in", dpi = 150, height = 17, width = 13)
# ggsave("zrr_by_feature_combo_sort-zrr.png", p, path = fig_path, units = "in", dpi = 150, height = 17, width = 13)
rm(temp, p)

# Let's take a look at high-profile users:
most_active_identities <- queries[, j = list(requests = .N), by = "identity"] %>%
  dplyr::top_n(6, requests)
temp <- queries[queries$identity %in% most_active_identities$identity,
        j = list(queries = .N),
        by = c("identity", "n_terms", "zero_result") ] %>%
  group_by(identity, zero_result) %>%
  summarize(n_terms = n_terms, prop = queries/sum(queries)) %>%
  ungroup %>%
  dplyr::left_join(most_active_identities) %>%
  mutate(zero_result = ifelse(zero_result == "TRUE",
                              "Query Yielded Zero Results",
                              "Query Yielded Some Results"),
         identity = polloi::compress(requests, 2))
temp %>%
  ggplot(aes(x = n_terms, y = prop)) +
  geom_bar(stat = "identity") +
  facet_grid(identity~zero_result) +
  scale_y_continuous("Proportion of Queries", labels = scales::percent_format(), limits = 0:1) +
  scale_x_continuous("Number of Terms in Simple Query (All queries were simple queries.)",
                     breaks = 1:20, labels = 1:20) +
  geom_text(aes(label = sprintf("%.2f%%", 100*prop)), nudge_y = 0.3, angle = 270, family = "Gill Sans") +
  ggthemes::theme_gdocs(base_family = "Gill Sans", base_size = 14) +
  theme(panel.grid.major = element_line(color = "gray90")) +
  ggtitle(paste("Most active users on", as.character(head(queries$date, 1), format = "%a (%d %b %Y)")),
          subtitle = paste0("Showing top 6 users with ", min(temp$identity), "-", max(temp$identity), " ", tolower(fig_subtitle))) +
  theme(panel.border = element_blank())
rm(temp, most_active_identities)

# Zero results rate by country
p <- queries[, j = list(queries = .N), by = c("country", "zero_result")] %>%
  mutate(zero_result = ifelse(zero_result, "Zero Results", "Some Results")) %>%
  tidyr::spread(zero_result, queries, fill = 0) %>%
  mutate(`Total Queries` = `Zero Results` + `Some Results`,
         `Zero Results Rate` = `Zero Results`/`Total Queries`,
         `Proportion of Searches` = `Total Queries`/sum(`Total Queries`)) %>%
  dplyr::top_n(20, `Total Queries`) %>%
  ggplot(aes(x = reorder(country, -`Proportion of Searches`), y = `Zero Results Rate`)) +
  geom_bar(stat = "identity", aes(fill = `Proportion of Searches`)) +
  scale_fill_gradient("Proportion of Searches Accounted For", low = "#ffeda0", high = "#f03b20",
                      labels = scales::percent_format()) +
  scale_x_discrete("Countries") +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 0.4, 0.05), limits = c(0, 0.5)) +
  geom_text(aes(label = sprintf("%.1f%% of %s", 100*`Zero Results Rate`, polloi::compress(`Total Queries`, 1))),
            nudge_y = 0.05) +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  theme(legend.position = "bottom") +
  ggtitle("Zero results rate in top 20 countries by volume of searches",
          subtitle = paste(fig_subtitle, "on", as.character(head(queries$date, 1), format = "%a (%d %b %Y)")))
ggsave("zrr_by_country.png", p, path = fig_path, units = "in", dpi = 150, height = 10, width = 10)
rm(p)
