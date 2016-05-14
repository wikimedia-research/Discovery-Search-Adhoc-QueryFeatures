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
