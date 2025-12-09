library(dplyr)
library(readr)
df <- read_rds("~/index_based_drought_monitoring-stineb/data/machine_learning_training_data.rds") # only on beni's machine ;-)

set.seed(123)  # for reproducibility

# remove vegtypes where we have only one site:
#      This concerns CSH (closed shrublands) site (IT-Noe)
df <- df |>
  group_by(vegtype) |> filter(length(unique(site)) > 1) |> ungroup()

# withold sites
df_sites_test <- df |> 
  select(site,  vegtype) |> 
  distinct() |> 
  group_by(vegtype) |> 
  sample_n(size = 1)

# construct training set
df_test <- df |> 
  filter(site %in% df_sites_test$site)

df_train <- df |> 
  filter(!(site %in% df_sites_test$site)) |> 
  
  # create missingness in RS data
  mutate(across(
    .cols = c(starts_with("NR"), "LST"),
    .fns = ~ {
      n <- length(.x)
      
      # number of values to replace with NA
      n_miss <- ceiling(0.25 * n)
      
      # randomly choose positions to set NA (independent per column)
      miss_idx <- sample(seq_len(n), n_miss)
      
      .x[miss_idx] <- NA
      .x
    }
  ))

# visdat::vis_miss(
#   df_train, 
#   warn_large_data = FALSE
# )

write_csv(
  df_train,
  here::here("../data/competition2025_training_data.csv")
)

write_csv(
  df_test |> 
    select(-flue),
  here::here("../data/competition2025_testing_data.csv")
)

write_csv(
  df_test,
  here::here("../data/competition2025_testing_data_full.csv")
)
