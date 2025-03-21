
library(tidyverse)
library(readxl)

file <- "ERO Admin Arrests_2025-ICLI-00019_2024-ICFO-39357_LESA-STU_FINAL.xlsx"

arrests <-
  file |>
  excel_sheets() |>
  set_names() |>
  map_dfr(~read_excel(file, sheet = .x, skip = 6), .id = "sheet") |>
  janitor::clean_names() |>
  mutate(apprehension_date = as.Date(apprehension_date)) |>
  mutate(apprehension_method = fct(apprehension_method)) |>
  mutate(criminality = apprehension_criminality == "1 Convicted Criminal") |>
  mutate(n_dupe = row_number(), .by = c("apprehension_date", "apprehension_method", "apprehension_criminality", "unique_identifier")) |>
  filter(is.na(unique_identifier) | n_dupe == 1) |>
  select(apprehension_date, apprehension_method, criminality, unique_identifier) |>
  mutate(week = floor_date(apprehension_date, "week", week_start = "Monday"))

figure_1 <-
  arrests |>
  filter(week != "2025-02-17") |>
  count(week) |>
  ggplot(aes(week, n)) +
  geom_vline(data = presidential_inaugurations, aes(xintercept = inauguration_date), color = gray(0.4), linetype = "dashed") +
  geom_text(data = presidential_inaugurations |> filter(president == "Trump II"),
            aes(x = inauguration_date - days(7), y = 4800, label = "Trump II\ninauguration"),
            hjust = 1,
            size = 3.5, color = gray(0.4)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(clip = "off") +
  labs(title = "Weekly Arrests by Immigrations and Customs Enforcement (ICE)", x = "", y = "Number of Arrests per Week", caption = "Source: ICE via FOIA request") +
  theme_minimal() +
  theme(plot.margin = margin(5, 0, 5, 5),
        plot.title = element_text(margin = margin(0,0,10,0)),
        axis.title.x = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.position = "bottom",
        axis.title.y = element_text(margin = margin(0, 15, 0, 0)),
        strip.text = element_text(hjust = 0, size = 12))

ggsave(figure_1, filename = "news/img/2025-03-21-ice-arrests.svg", width = 6.1, height = 3.5)

figure_2 <-
  arrests |>
  mutate(week = floor_date(apprehension_date, "week", week_start = "Monday")) |>
  filter(week != "2025-02-17") |>
  mutate(criminality = factor(criminality, levels = c(TRUE, FALSE), labels = c("Criminal conviction", "No criminal conviction"))) |>
  count(criminality, week) |>
  ggplot(aes(week, n, color = criminality, group = criminality)) +
  geom_vline(data = NULL, xintercept = as.Date("2025-01-20"), color = gray(0.2), linetype = "dashed") +
  annotate("text", x = as.Date("2025-01-20") - days(7), y = 2850, label = "Trump II\ninauguration", hjust = 1, size = 3.5, color = gray(0.2)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 3000)) +
  scale_color_manual(values = c("#0072B2", "#D55E00")) +
  coord_cartesian(clip = "off") +
  labs(color = "", title = "Weekly Arrests by Immigrations and Customs Enforcement (ICE)", subtitle = "by Criminal Conviction Status", x = "", y = "Number of Arrests per Week", caption = "Source: ICE via FOIA request") +
  theme_minimal() +
  theme(plot.margin = margin(5, 0, 5, 5),
        plot.title = element_text(margin = margin(0,0,5,0)),
        plot.subtitle = element_text(margin = margin(0, 0, 10, 0)),
        axis.title.x = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.position = "bottom",
        axis.title.y = element_text(margin = margin(0, 15, 0, 0)),
        strip.text = element_text(hjust = 0, size = 12))

ggsave(figure_2, filename = "news/img/2025-03-21-ice-arrests-by-criminal-convictions.svg", width = 6.1, height = 3.8)

