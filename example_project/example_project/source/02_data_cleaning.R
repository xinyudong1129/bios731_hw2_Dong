state = "ga"

# load raw data
load(here::here("data", "raw.Rdata"))

# grab only observations from the specified state
covid = covid %>%
  filter(grepl(state, key_plot_id))


# only include columns from counties dataset we are interested in
counties = counties %>%
  filter(grepl(state, key_plot_id)) %>%
  select(key_plot_id, wwtp_id, county = county_names,
         county_fips, population_served) %>%
  distinct()

# merge covid data with the county label information
# convert  variables from character to numeric
# concentration variable more intuitive name
covid = left_join(covid, counties, by = "key_plot_id") %>%
  select(-key_plot_id) %>%
  mutate(pcr_conc_lin = as.numeric(pcr_conc_lin),
         population_served = as.numeric(population_served)) %>%
  rename(concentration = pcr_conc_lin)


rm(counties)

## save tidied data object and date data was accessed
save(covid, file = here::here("data", "clean.Rdata"))
