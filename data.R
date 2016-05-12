end_date <- as.Date("2016-05-01") # Sys.Date()-1
start_date <- end_date - 0
queries <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching query features from", as.character(date), "\n")
  query <- paste0("ADD JAR /home/bearloga/Code/analytics-refinery-jars/refinery-hive.jar;
                   CREATE TEMPORARY FUNCTION array_sum AS 'org.wikimedia.analytics.refinery.hive.ArraySumUDF';
                   CREATE TEMPORARY FUNCTION is_spider as 'org.wikimedia.analytics.refinery.hive.IsSpiderUDF';
                   CREATE TEMPORARY FUNCTION ua_parser as 'org.wikimedia.analytics.refinery.hive.UAParserUDF';
                   CREATE TEMPORARY FUNCTION deconstruct AS 'org.wikimedia.analytics.refinery.hive.DeconstructSearchQueryUDF';
                   CREATE TEMPORARY FUNCTION geocode_country as 'org.wikimedia.analytics.refinery.hive.GeocodedCountryUDF';
                   CREATE TEMPORARY FUNCTION country_name as 'org.wikimedia.analytics.refinery.hive.CountryNameUDF';
                   USE wmf_raw;
                   SELECT
                     source, identity, country_code, country_name(country_code) AS country, is_automata,
                     query_type, zero_result, n_terms, n_chars,
                     SIZE(SPLIT(features, ', ')) AS n_feats,
                     SUBSTR(features, 2, LENGTH(features)-2) AS features
                   FROM (
                     SELECT
                       source, identity, geocode_country(ip) AS country_code,
                       CASE
                         WHEN ((ua_parser(useragent)['device_family'] = 'Spider') OR
                                is_spider(useragent) OR ip = '127.0.0.1')
                         THEN 'TRUE' ELSE 'FALSE'
                       END AS is_automata,
                       array_sum(requests.hitstotal, -1) = 0 AS zero_result,
                       requests[size(requests)-1].querytype AS query_type,
                       deconstruct(requests.query[SIZE(requests.query)-1]) AS features,
                       SIZE(SPLIT(requests.query[SIZE(requests.query)-1], ' +')) AS n_terms,
                       LENGTH(requests.query[SIZE(requests.query)-1]) AS n_chars
                    FROM cirrussearchrequestset ", wmf::date_clause(date)$date_clause, "
                      AND requests[size(requests)-1].querytype IN('full_text', 'comp_suggest')
                   ) AS deconstructed_queries;")
  elapsed_seconds <- system.time({
    results <- wmf::query_hive(query)
  })['elapsed']
  results <- results[results$query_type != "", ]
  cat("It took", tolower(lubridate::seconds_to_period(round(as.numeric(elapsed_seconds)))),
      "seconds to fetch and process", nrow(results), "queries.\n")
  results <- cbind(date = date, results, stringsAsFactors = FALSE)
  return(results)
}))

queries$zero_result <- queries$zero_result == "true"

library(data.table)
queries <- as.data.table(queries, key = c("date", "source", "identity", "country_code", "country", "is_automata", "query_type"))
# queries[,j = list(queries = .N), by = c("date", "source", "is_automata")]

dir.create("cirrus_query_features")
readr::write_rds(queries[queries$source == "web" & !queries$is_automata,], "~/cirrus_query_features/web_excl-known-automata.rds", "gz")
readr::write_rds(queries[queries$source == "api" & queries$is_automata,], "~/cirrus_query_features/api_known-automata.rds", "gz")
readr::write_rds(queries[queries$source == "api" & !queries$is_automata,], "~/cirrus_query_features/api_excl-known-automata.rds", "gz")
readr::write_rds(queries[queries$source == "web" & queries$is_automata,], "~/cirrus_query_features/web_known-automata.rds", "gz")
readr::write_rds(queries, "~/cirrus_query_features/all.rds", "gz")

## Locally:
dir.create("data")
# system("scp stat2:/home/bearloga/cirrus_query_features/web_excl-known-automata.rds data/")
