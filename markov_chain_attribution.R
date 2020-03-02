### Load Libraries ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, dplyr, ChannelAttribution, ggplot2, readr)


### Load Datasets ----

campaign_data = fread("sample_datasets/campaign_data.csv") %>% arrange(cookie, time)
campaign_budget_daily = fread("sample_datasets/budget_sample_daily.csv")

campaign_data <- campaign_data %>% head(10000)

summary(campaign_data)
unique(campaign_data$interaction)
summary(campaign_budget_daily)


campaign_data %>%
    group_by(cookie) %>%
    arrange(time) %>%
    mutate(cumsum_conv=cumsum(conversion),
           lag_cumsum_conv=lag(cumsum(conversion)),
           path_no = ifelse(is.na(lag_cumsum_conv), 0, lag_cumsum_conv) + 1) %>%
    ungroup() %>%
    select(-lag_cumsum_conv) %>%
    as.data.frame() %>%
    arrange(cookie, time) %>%
    rt_peak()

### Prepare the files - Split Paths ----
df_split = campaign_data %>%
    group_by(cookie) %>%
    arrange(time) %>%
    mutate(cumsum_conv=cumsum(conversion),
         path_no = ifelse(is.na(lag(cumsum(conversion))), 0, lag(cumsum(conversion))) + 1) %>%
    ungroup() %>%
    mutate(path_id = paste0(cookie, path_no)) %>%
    arrange(cookie, time)

df_split %>% rt_peak()

### Prepare the file - Create the paths ----
df_paths = df_split %>%
  group_by(path_id) %>%
  arrange(time) %>%
  summarise(path = paste(channel, collapse = ">"),
            total_conversions = sum(conversion)) %>%
  ungroup() %>% 
  mutate(null_conversion = ifelse(total_conversions == 1, 0, 1)) # adding information about path that have not led to conversion

head(df_paths, 100) %>% View()

### Markov Chain and Heuristic Models ----
?markov_model
markov_attribution <- markov_model(df_paths,
                             var_path = "path",
                             var_conv = "total_conversions",
                             var_value = NULL,
                             order = 2, # higher order markov chain
                             var_null = "null_conversion",
                             out_more = TRUE,
                             sep=">")


markov_attribution <- markov_model(df_paths,
                                   var_path = "path",
                                   var_conv = "total_conversions",
                                   var_value = NULL,
                                   order = 1, # higher order markov chain
                                   var_null = "null_conversion",
                                   out_more = TRUE,
                                   sep=">")

# markov_attribution <- markov_model(df_paths,
#                                    var_path = "path",
#                                    var_conv = "total_conversions",
#                                    var_value = NULL,
#                                    order = 3, # higher order markov chain
#                                    var_null = "null_conversion",
#                                    out_more = TRUE,
#                                    sep=">")


heuristic_attribution <- heuristic_models(df_paths,
                                     var_path = "path",
                                     var_conv = "total_conversions")



### Prepare final joint dataset ----

# Join attribution results
all_model_results = merge(markov_attribution$result, heuristic_attribution)

colSums(all_model_results %>% select_if(is.numeric))

# Aggregate budget
campaign_budget_total = as.data.table(
  campaign_budget_daily %>%
    group_by(channel) %>%
    summarise(total_cost = round(sum(cost), 1))
)

# Join into final results
campaign_attribution = merge(all_model_results, campaign_budget_total, 
                             by.x = "channel_name", by.y = "channel")

#### Calculate ROAS and CPA
campaign_attribution = 
  campaign_attribution %>%
  mutate(channel_weight = (total_conversions / sum(total_conversions)),
         cost_weight = (total_cost / sum(total_cost)),
         roas = channel_weight / cost_weight,
         optimal_budget = total_cost * roas,
         CPA = total_cost / total_conversions)

# Change the name of markov results column
names(campaign_attribution)[names(campaign_attribution) == "total_conversions"] = "markov_result"

# Save the outputs
write_csv(campaign_attribution, "sample_datasets/campaign_attribution.csv")
write_csv(campaign_attribution, "results_visualization/campaign_attribution.csv")

