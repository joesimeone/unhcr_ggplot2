

syrian_flows <- refugees::flows %>% 
  filter(coo_name == "Syrian Arab Rep." & year %in% c(2011:2024))


top5_coa <- 
  syrian_flows %>% 
  summarise(sum_asy = sum(asylum_seekers),
            sum_ref = sum(refugees),
            .by = c(year, coa_name)) %>% 
  mutate(totals = sum_asy + sum_ref) %>% 
  arrange(desc(totals)) %>% 
  distinct(coa_name) %>% 
  slice_head(n = 5) %>% 
  pull()




syr_viz_dat <- 
  syrian_flows %>% 
  mutate(
    coa_tweaked = 
      if_else(!coa_name %in% top5_coa,
              "Other", coa_name)) %>% 
  summarise(sum_asy = sum(asylum_seekers),
            sum_ref = sum(refugees), 
            .by = c(year, coa_tweaked)) %>% 
  mutate(
    across(c(sum_asy, sum_ref), ~ if_else(is.na(.x), 0, .x))
  ) %>% 
  mutate(totals = 
           sum_asy + sum_ref) %>% 
  ungroup()

## Bad news ------- but why??? 
# ggplot(syr_viz_dat, aes(year, totals, color = coa_tweaked)) +
#   geom_line()
# 
# ## Thing 1: The x axis is broken 
# syr_viz_dat <-
#   syr_viz_dat %>% 
#   mutate(year_tweaked = make_date(year, 01, 01))
# 
# ggplot(syr_viz_dat, aes(year_tweaked, totals, color = coa_tweaked, fill = coa_tweaked)) +
#  # geom_area(alpha = 0.9) +
#   geom_line() +
#   scale_x_date(labels = scales::date_format("%Y"),
# breaks = seq(min(syr_viz_dat$year_tweaked), max(syr_viz_dat$year_tweaked), by = "2 year")
# )
# glimpse(syr_viz_dat
#         )
# test
# glimpse(hmm)
# 
# 
# turkey_helper <- tribble(~year, ~coa_tweaked, ~sum_asy, ~sum_ref, ~totals, ~year_tweaked,
#                          2019, "Türkiye", 0, 0, 0, "2019-01-01",
#                          2020, "Türkiye", 0, 0, 0, "2020-01-01",
#                          2021, "Türkiye", 0, 0, 0, "2021-01-01",
#                          2022, "Türkiye", 0, 0, 0, "2022-01-01",
#                          2023, "Türkiye", 0, 0, 0, "2023-01-01",
#                          2024, "Türkiye", 0, 0, 0, "2024-01-01")
# 
# syr_viz_dat2 <- 
#   syr_viz_dat %>% 
#   rbind(turkey_helper) %>% 
#   arrange(year, coa_tweaked) 
#   
# 
# ggplot(syr_viz_dat2, aes(year_tweaked, totals, color = coa_tweaked, fill = coa_tweaked)) +
#   # geom_area(alpha = 0.9) +
#   geom_line() +
#   geom_area() +
#   scale_x_date(labels = scales::date_format("%Y"),
#                breaks = seq(min(syr_viz_dat$year_tweaked), max(syr_viz_dat$year_tweaked), by = "2 year")
#   )
# glimpse(syr_viz_dat
# )
# 
# arrange(syrian_flows, desc(refugees), coa) %>% view()
# 
# devtools::install_github("hrbrmstr/streamgraph")
# 
# library(streamgraph)
# 
# streamgraph(syr_viz_dat2, key="coa_tweaked", value="totals", date="year_tweaked", height="300px", width="1000px") %>%
#   sg_legend(show=TRUE, label="names: ")
