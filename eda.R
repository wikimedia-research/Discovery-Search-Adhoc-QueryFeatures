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
switch(dataset_to_use,
       "api_known-automata.rds" = {
         fig_path <- "figures/api/known_automata"
         fig_subtitle <- "Search queries made via the API, known automata only"
         data_root <- "cirrus_query_features"
       },
       "api_excl-known-automata.rds" = {
         fig_path <- "figures/api/excl_known_automata"
         fig_subtitle <- "Search queries made via the API, excluding known automata"
         data_root <- "cirrus_query_features"
       },
       "web_known-automata.rds" = {
         fig_path <- "figures/web/known_automata"
         fig_subtitle <- "Search queries made on the web, known automata only"
         data_root <- "cirrus_query_features"
       },
       "web_excl-known-automata.rds" = {
         fig_path <- "figures/web/excl_known_automata"
         fig_subtitle <- "Search queries made on the web, excluding known automata"
         data_root <- "data"
       })
if (!dir.exists(fig_path)) dir.create(fig_path, recursive = TRUE)
queries <- readr::read_rds(file.path(data_root, dataset_to_use))


strsplit(head(queries$features, 200), ", ") %>%
  lapply(...)
  lapply(dummy::dummy)
