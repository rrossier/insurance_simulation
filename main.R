library(tidyverse)
library(plotly)

get_insurance_propensity_matrix <- function() {
  aversion_scores <- 1:100
  affordability_scores <- 1:100
  scale_min <- 1
  scale_max <- 100
  factor_aversion <- 0.75
  power_aversion <- 2
  factor_affordability <- 1
  power_affordability <- 1.5
  propensity_data <- lapply(affordability_scores, function(affordability_score){
    factor_aversion * pmax(scale_min, pmax(scale_min, scale_max - aversion_scores)) ^ power_aversion +
      factor_affordability * pmax(scale_min, pmax(scale_min, scale_max - affordability_score)) ^ power_affordability %>%
      as.double()
  }) %>%
  do.call("cbind", .) %>%
  t()
  z_propensity_data <- 1-(propensity_data - min(propensity_data)*0.95) / (max(propensity_data)*1.05 - min(propensity_data)*0.95)
  
  return(z_propensity_data)
}

insurance_propensity <- function(aversion_score, affordability_score, .matrix = get_insurance_propensity_matrix()) {
  return(.matrix[aversion_score, affordability_score])
}

# plot_ly(x = 1:100, y = 1:100, z = get_insurance_propensity_matrix(), type = "surface") %>%
#   layout(scene = list(
#     xaxis = list(title = "Affordability"),
#     yaxis = list(title = "Aversion")
#     )
#   )

insurance_propensity(1, 15)
insurance_propensity(15, 1)
insurance_propensity(1, 1)

test_user_insurance <- function(user){
  random_value <- runif(1, 0, 1)
  user_aversion_score <- user[["aversion_score"]]
  user_affordability_score <- user[["affordability_score"]]
  user_propensity <- insurance_propensity(user_aversion_score, user_affordability_score)
  return(random_value <= user_propensity)
}

check_take_insurance <- function(user_aversion_score, user_affordability_score){
  random_value <- runif(1, 0, 1)
  user_propensity <- insurance_propensity(user_aversion_score, user_affordability_score)
  return(random_value <= user_propensity)
}

test_user_switch_insurer <- function(user){
  return(FALSE)
}

nb_users <- 1000
nb_areas <- 50
perils_names <- c("Storm", "Flood", "Fire", "Hail", "Quake")
users <- tibble(id = 1:nb_users,
                area = round(runif(nb_users, min = 1, max = nb_areas)),
                current_policy = NA,
                aversion_score = round(runif(nb_users, 1, 100)),
                affordability_score = round(runif(nb_users, 1, 100))
                )
areas <- tibble(id = 1:nb_areas) %>%
  bind_cols(
    replicate(nb_areas, runif(length(perils_names), 95, 100)) %>%
    t() %>%
    as_tibble(.name_repair = ~perils_names)
  )

policies <- tibble(id = NA,
                   user = NA,
                   start_date = NA,
                   premium_amount = NA)

assets <- tibble(id = 1:nb_users,
                 user_id = 1:nb_users,
                 value = round(runif(nb_users, 50e3, 10e6)),
                 vulnerability = runif(nb_users, 95, 100),
                 as_at_date = 1)

events <- tibble(id = NA,
                 area = NA,
                 peril = NA,
                 date = NA)

losses <- tibble(id = NA,
                 event_id = NA,
                 asset_id = NA,
                 amount = NA)

weeks_in_a_year <- 1:52

for(current_week in weeks_in_a_year){
  message("Day ", current_week)
  show_message <- !!!(current_week%%10)
  # users take, switch, drop insurance
  if(show_message) message("Check users")
  
  users_ids_new_policies <- users %>%
    rowwise() %>%
    mutate(take_insurance = ifelse(is.na(current_policy), check_take_insurance(aversion_score, affordability_score), FALSE)) %>%
    filter(take_insurance == TRUE) %>%
    pull(id)
  
  n_new_policies <- length(users_ids_new_policies)
  if(n_new_policies > 0){
    n_policies <- max(0, nrow(policies))
    new_policies <- tibble(id = (n_policies+1):(n_policies + n_new_policies),
                           user = users_ids_new_policies,
                           start_date = current_week,
                           premium_amount = runif(n_new_policies, 50, 200))
    
    policies <- policies %>%
      bind_rows(new_policies)
    
    # TODO: tidy
    users$current_policy <- new_policies[match(users$id, new_policies$user), c("id")]
  }
  
  # generate events
  if(show_message) message("Generate events")
  daily_trigger <- runif(1)
  areas_events <- areas %>%
    mutate(across(perils_names, function(x){
      return(daily_trigger < ((100 - x) / length(weeks_in_a_year)))
      })
    ) %>%
    filter(if_any(perils_names) == TRUE) %>%
    pivot_longer(!id, names_to = "peril", values_to = "is_happening") %>%
    filter(is_happening == TRUE)
  
  n_new_events <- nrow(areas_events)
  if(n_new_events > 0){
    if(show_message) message(n_new_events, " new events")
    n_events <- max(0, nrow(events))
    new_events <- areas_events %>%
      rename(area = id) %>%
      mutate(id = (n_events+1):(n_events + n_new_events),
             date = current_week) %>%
      select(id, area, peril, date)
    
    events <- events %>%
      bind_rows(new_events)
    
    # calculate loss amounts
    users_concerned_by_new_events <- users %>%
      filter(area %in% (new_events %>% pull(area)))
    assets_at_risk <- assets %>%
      left_join(users %>% select(id, area), by = c("user_id" = "id")) %>%
      left_join(areas %>% select(id), by = c("area" = "id")) %>%
      filter(area %in% (new_events %>% pull(area))) %>%
      left_join(new_events %>% select(id, area) %>% rename(event_id = id), by = c("area" = "area"), relationship = "many-to-many") %>%
      mutate(has_loss = runif(n(), 1, 100) > vulnerability,
             amount = runif(n()) * vulnerability / 1000 * value) %>%
      filter(has_loss == TRUE) %>%
      select(id, event_id, amount) %>%
      rename(asset_id = id)
    
    n_new_losses <- nrow(assets_at_risk)
    if(n_new_losses > 0){
      n_losses <- nrow(losses)
      new_losses <- assets_at_risk %>%
        mutate(id = (n_losses+1):(n_losses + n_new_losses))
      
      losses <- losses %>%
        bind_rows(new_losses)
    }
  
  
  }
  
  # update users scores
  
  
}
