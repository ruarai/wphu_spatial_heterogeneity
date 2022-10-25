


periods <- tribble(
 ~name, ~label, ~date_start, ~date_end,
 "first_wave", "First wave", ymd("2020-01-01"), ymd("2020-06-01"),
 "second_wave", "Second wave", ymd("2020-06-01"), ymd("2020-11-01"),
 "low_cases", "Low case period", ymd("2020-11-01"), ymd("2021-07-01"),
 "delta", "Delta period", ymd("2021-07-01"), ymd("2021-12-15"),
 "omicron", "Omicron period", ymd("2021-12-15"), ymd("2022-07-01")
) %>%
  arrange(date_start) %>%
  mutate(alternate_coding = factor(row_number() %% 2))
