library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(ggrepel)

# =====================================
# 1. LPI data
# =====================================
lpi <- read_csv("LPIGlobal.csv", show_col_types = FALSE) %>%
  rename(
    year  = Year,
    mean  = LPI_final,
    lower = CI_low,
    upper = CI_high
  ) %>%
  mutate(
    year  = as.numeric(year),
    mean  = as.numeric(mean),
    lower = as.numeric(lower),
    upper = as.numeric(upper)
  ) %>%
  filter(year >= 1970, year <= 2020) %>%
  arrange(year)

# =====================================
# 2. BII data
# =====================================
bii_raw <- readRDS("long_data.RDS")

region_lookup <- c(
  "001-002" = "Africa",
  "001-019" = "Americas",
  "001-142" = "Asia",
  "001-150" = "Europe",
  "global"  = "Globe"
)

bii_plot <- bii_raw %>%
  filter(
    area_code %in% names(region_lookup),
    scenario == "historical",
    variable == "bii"
  ) %>%
  mutate(
    year  = as.numeric(year),
    bii   = as.numeric(value),
    lower = as.numeric(lower_uncertainty),
    upper = as.numeric(upper_uncertainty),
    region = recode(area_code, !!!region_lookup)
  ) %>%
  filter(year >= 1970, year <= 2020) %>%
  transmute(year, region, bii, lower, upper) %>%
  mutate(
    region = factor(region, levels = c("Africa", "Americas", "Asia", "Europe", "Globe"))
  ) %>%
  arrange(region, year)

# =====================================
# 3. GDP data
# =====================================
gdp_summary <- df %>%
  mutate(
    year = as.numeric(year),
    mean_GDPPP = as.numeric(mean_GDPPP)
  ) %>%
  filter(!is.na(mean_GDPPP)) %>%
  group_by(year) %>%
  summarise(
    mean_gdp = mean(mean_GDPPP, na.rm = TRUE),
    sd_gdp   = sd(mean_GDPPP, na.rm = TRUE),
    lower    = mean_gdp - sd_gdp,
    upper    = mean_gdp + sd_gdp,
    .groups = "drop"
  ) %>%
  filter(year >= 1970, year <= 2020)

# =====================================
# 4. Shared theme
# =====================================
theme_nature <- function() {
  theme_classic(base_size = 11) +
    theme(
      axis.title = element_text(face = "plain", color = "black"),
      axis.text = element_text(color = "black"),
      axis.line = element_line(linewidth = 0.4, color = "black"),
      axis.ticks = element_line(linewidth = 0.35, color = "black"),
      plot.title = element_text(face = "plain", size = 11, hjust = 0),
      plot.subtitle = element_text(size = 9.5, hjust = 0),
      plot.tag = element_text(face = "bold", size = 13),
      legend.position = "none",
      plot.margin = margin(6, 18, 6, 6)
    )
}

# =====================================
# 5. Panel A: LPI
# =====================================
p_a <- ggplot(lpi, aes(x = year, y = mean)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill = "#d9d9d9",
    alpha = 0.8
  ) +
  geom_line(
    colour = "#b22222",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 1997,
    y = 0.23,
    label = "73% decline\n1970–2020",
    hjust = 0,
    size = 3.3
  ) +
  scale_x_continuous(
    limits = c(1970, 2020),
    breaks = seq(1970, 2020, 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, max(lpi$upper, na.rm = TRUE) * 1.03),
    expand = c(0, 0)
  ) +
  labs(
    title = "Living Planet Index",
    x = NULL,
    y = "Index value\n(1970 = 1)",
    tag = "A"
  ) +
  theme_nature()

# =====================================
# 6. Panel B: BII
# =====================================
bii_cols <- c(
  Africa   = "#c43c39",
  Americas = "#f08a24",
  Asia     = "#c9a227",
  Europe   = "#4f8fc7",
  Globe    = "black"
)

bii_labels <- bii_plot %>%
  group_by(region) %>%
  filter(year == max(year)) %>%
  ungroup()

p_b <- ggplot(bii_plot, aes(x = year, y = bii, colour = region)) +
  geom_line(linewidth = 0.95) +
  geom_text_repel(
    data = bii_labels,
    aes(label = region),
    direction = "y",
    hjust = 0,
    nudge_x = 2.2,
    segment.color = NA,
    size = 3.1,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = bii_cols) +
  scale_x_continuous(
    limits = c(1970, 2024),
    breaks = seq(1970, 2020, 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.01),
    expand = c(0.02, 0)
  ) +
  labs(
    title = "Biodiversity Intactness Index",
    x = NULL,
    y = "BII",
    tag = "B"
  ) +
  theme_nature()

# =====================================
# 7. Panel C: GDP
# =====================================
p_c <- ggplot(gdp_summary, aes(x = year, y = mean_gdp)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill = "grey75",
    alpha = 0.7
  ) +
  geom_line(
    colour = "black",
    linewidth = 1
  ) +
  scale_x_continuous(
    limits = c(1970, 2020),
    breaks = seq(1970, 2020, 10),
    expand = c(0, 0)
  ) +
  labs(
    title = "GDP per capita",
    x = "Year",
    y = "Mean across\ncountries ± SD",
    tag = "C"
  ) +
  theme_nature()

# =====================================
# 8. Combine
# =====================================
fig_abc <- p_a / p_b / p_c +
  plot_layout(heights = c(1, 1, 1))

fig_abc




###### alt


library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(ggrepel)
library(WDI)

# =====================================
# 1. LPI data
# =====================================
lpi <- read_csv("LPIGlobal.csv", show_col_types = FALSE) %>%
  rename(
    year  = Year,
    mean  = LPI_final,
    lower = CI_low,
    upper = CI_high
  ) %>%
  mutate(
    year  = as.numeric(year),
    mean  = as.numeric(mean),
    lower = as.numeric(lower),
    upper = as.numeric(upper)
  ) %>%
  filter(year >= 1970, year <= 2020) %>%
  arrange(year)

# =====================================
# 2. BII data
# =====================================
bii_raw <- readRDS("long_data.RDS")

region_lookup <- c(
  "001-002" = "Africa",
  "001-019" = "Americas",
  "001-142" = "Asia",
  "001-150" = "Europe",
  "global"  = "Globe"
)

bii_plot <- bii_raw %>%
  filter(
    area_code %in% names(region_lookup),
    scenario == "historical",
    variable == "bii"
  ) %>%
  mutate(
    year  = as.numeric(year),
    bii   = as.numeric(value),
    lower = as.numeric(lower_uncertainty),
    upper = as.numeric(upper_uncertainty),
    region = recode(area_code, !!!region_lookup)
  ) %>%
  filter(year >= 1970, year <= 2020) %>%
  transmute(year, region, bii, lower, upper) %>%
  mutate(
    region = factor(region, levels = c("Africa", "Americas", "Asia", "Europe", "Globe"))
  ) %>%
  arrange(region, year)

# =====================================
# 3. GDP data from World Bank
# Indicator: GDP per capita, PPP (constant international $)
# Code: NY.GDP.PCAP.PP.KD
# =====================================

library(WDI)
library(dplyr)
library(ggplot2)

gdp_raw <- WDI(
  country = "all",
  indicator = "NY.GDP.PCAP.KD",
  start = 1970,
  end = 2020
) %>%
  rename(gdppc = NY.GDP.PCAP.KD)

gdp_change <- gdp_raw %>%
  mutate(
    year = as.numeric(year),
    gdppc = as.numeric(gdppc)
  ) %>%
  filter(!is.na(gdppc), !is.na(country)) %>%
  group_by(country) %>%
  mutate(
    gdp_1970 = gdppc[year == 1970][1],
    pct_change = 100 * (gdppc - gdp_1970) / gdp_1970
  ) %>%
  ungroup() %>%
  filter(!is.na(gdp_1970), !is.na(pct_change))

gdp_summary <- gdp_change %>%
  group_by(year) %>%
  summarise(
    median_change = median(pct_change, na.rm = TRUE),
    q25 = quantile(pct_change, 0.25, na.rm = TRUE),
    q75 = quantile(pct_change, 0.75, na.rm = TRUE),
    n_countries = n_distinct(country),
    .groups = "drop"
  )

# =====================================
# 4. Shared theme
# =====================================
theme_nature <- function() {
  theme_classic(base_size = 11) +
    theme(
      axis.title = element_text(face = "plain", color = "black"),
      axis.text = element_text(color = "black"),
      axis.line = element_line(linewidth = 0.4, color = "black"),
      axis.ticks = element_line(linewidth = 0.35, color = "black"),
      plot.title = element_text(face = "plain", size = 11, hjust = 0),
      plot.tag = element_text(face = "bold", size = 13),
      legend.position = "none",
      plot.margin = margin(6, 18, 6, 6)
    )
}

# =====================================
# 5. Panel A: LPI
# =====================================
p_a <- ggplot(lpi, aes(x = year, y = mean)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill = "#d9d9d9",
    alpha = 0.8
  ) +
  geom_line(
    colour = "#b22222",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 1997,
    y = 0.23,
    label = "73% decline\n1970–2020",
    hjust = 0,
    size = 3.3
  ) +
  scale_x_continuous(
    limits = c(1970, 2020),
    breaks = seq(1970, 2020, 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, max(lpi$upper, na.rm = TRUE) * 1.03),
    expand = c(0, 0)
  ) +
  labs(
    title = "Living Planet Index",
    x = NULL,
    y = "Index value\n(1970 = 1)",
    tag = "A"
  ) +
  theme_nature()

# =====================================
# 6. Panel B: BII
# =====================================
bii_cols <- c(
  Africa   = "#c43c39",
  Americas = "#f08a24",
  Asia     = "#c9a227",
  Europe   = "#4f8fc7",
  Globe    = "black"
)

bii_labels <- bii_plot %>%
  group_by(region) %>%
  filter(year == max(year)) %>%
  ungroup()

p_b <- ggplot(bii_plot, aes(x = year, y = bii, colour = region)) +
  geom_line(linewidth = 0.95) +
  geom_text_repel(
    data = bii_labels,
    aes(label = region),
    direction = "y",
    hjust = 0,
    nudge_x = 2.2,
    segment.color = NA,
    size = 3.1,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = bii_cols) +
  scale_x_continuous(
    limits = c(1970, 2024),
    breaks = seq(1970, 2020, 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.01),
    expand = c(0.02, 0)
  ) +
  labs(
    title = "Biodiversity Intactness Index",
    x = NULL,
    y = "BII",
    tag = "B"
  ) +
  theme_nature()

# =====================================
# 7. Panel C: GDP
# =====================================
p_c <- ggplot(gdp_summary, aes(x = year, y = mean_gdp)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill = "grey75",
    alpha = 0.7
  ) +
  geom_line(
    colour = "black",
    linewidth = 1
  ) +
  scale_x_continuous(
    limits = c(1970, 2020),
    breaks = seq(1970, 2020, 10),
    expand = c(0, 0)
  ) +
  labs(
    title = "GDP per capita",
    x = "Year",
    y = "Mean across\ncountries ± SD",
    tag = "C"
  ) +
  theme_nature()

# =====================================
# 8. Combine
# =====================================
fig_abc <- p_a / p_b / p_c +
  plot_layout(heights = c(1, 1, 1))

fig_abc

# Save
ggsave("Figure_ABC_LPI_BII_GDP.png", fig_abc, width = 7.2, height = 9.2, dpi = 500)
ggsave("Figure_ABC_LPI_BII_GDP.pdf", fig_abc, width = 7.2, height = 9.2)


####

library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(ggrepel)
library(WDI)

# =====================================
# 1. LPI
# =====================================
lpi <- read_csv("LPIGlobal.csv", show_col_types = FALSE) %>%
  rename(
    year  = Year,
    mean  = LPI_final,
    lower = CI_low,
    upper = CI_high
  ) %>%
  mutate(
    year  = as.numeric(year),
    mean  = as.numeric(mean),
    lower = as.numeric(lower),
    upper = as.numeric(upper)
  ) %>%
  filter(year >= 1970, year <= 2020) %>%
  arrange(year)

# =====================================
# 2. BII
# =====================================
bii_raw <- readRDS("long_data.RDS")

region_lookup <- c(
  "001-002" = "Africa",
  "001-019" = "Americas",
  "001-142" = "Asia",
  "001-150" = "Europe",
  "global"  = "Globe"
)

bii_plot <- bii_raw %>%
  filter(
    area_code %in% names(region_lookup),
    scenario == "historical",
    variable == "bii"
  ) %>%
  mutate(
    year  = as.numeric(year),
    bii   = as.numeric(value),
    lower = as.numeric(lower_uncertainty),
    upper = as.numeric(upper_uncertainty),
    region = recode(area_code, !!!region_lookup)
  ) %>%
  filter(year >= 1970, year <= 2020) %>%
  transmute(year, region, bii, lower, upper) %>%
  mutate(
    region = factor(region, levels = c("Africa", "Americas", "Asia", "Europe", "Globe"))
  ) %>%
  arrange(region, year)

# =====================================
# 3. GDP from World Bank
# =====================================
gdp_raw <- WDI(
  country = "all",
  indicator = "NY.GDP.PCAP.KD",
  start = 1970,
  end = 2020,
  extra = TRUE
) %>%
  rename(gdppc = NY.GDP.PCAP.KD)

gdp_change <- gdp_raw %>%
  mutate(
    year = as.numeric(year),
    gdppc = as.numeric(gdppc)
  ) %>%
  filter(
    !is.na(gdppc),
    !is.na(region),
    region != "Aggregates"
  ) %>%
  mutate(
    continent = case_when(
      region == "Africa" ~ "Africa",
      region %in% c("Latin America & Caribbean", "North America") ~ "Americas",
      region %in% c("East Asia & Pacific", "South Asia", "Middle East & North Africa") ~ "Asia",
      region == "Europe & Central Asia" ~ "Europe",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(continent)) %>%
  group_by(country) %>%
  mutate(
    gdp_1970 = gdppc[year == 1970][1],
    pct_change = 100 * (gdppc - gdp_1970) / gdp_1970
  ) %>%
  ungroup() %>%
  filter(!is.na(gdp_1970), !is.na(pct_change))

# continent summaries
gdp_cont <- gdp_change %>%
  group_by(year, continent) %>%
  summarise(
    median_change = median(pct_change, na.rm = TRUE),
    q25 = quantile(pct_change, 0.25, na.rm = TRUE),
    q75 = quantile(pct_change, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    continent = factor(continent, levels = c("Africa", "Americas", "Asia", "Europe"))
  )

# global summary
gdp_global <- gdp_change %>%
  group_by(year) %>%
  summarise(
    median_change = median(pct_change, na.rm = TRUE),
    q25 = quantile(pct_change, 0.25, na.rm = TRUE),
    q75 = quantile(pct_change, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(continent = "Globe")

# combine for labels/lines
gdp_plot <- bind_rows(
  gdp_cont %>% rename(region = continent),
  gdp_global %>% rename(region = continent)
) %>%
  mutate(region = factor(region, levels = c("Africa", "Americas", "Asia", "Europe", "Globe")))

# =====================================
# 4. Shared theme
# =====================================
theme_nature <- function() {
  theme_classic(base_size = 11) +
    theme(
      axis.title = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      axis.line = element_line(linewidth = 0.4, color = "black"),
      axis.ticks = element_line(linewidth = 0.35, color = "black"),
      plot.title = element_text(size = 11, hjust = 0),
      plot.tag = element_text(face = "bold", size = 13),
      legend.position = "none",
      plot.margin = margin(5, 22, 5, 5)
    )
}

cols_regions <- c(
  Africa   = "#c43c39",
  Americas = "#f08a24",
  Asia     = "#c9a227",
  Europe   = "#4f8fc7",
  Globe    = "black"
)

# =====================================
# 5. Panel A
# =====================================
p_a <- ggplot(lpi, aes(year, mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "#d9d9d9", alpha = 0.85) +
  geom_line(colour = "#b22222", linewidth = 1) +
  annotate(
    "text",
    x = 1997,
    y = 0.23,
    label = "73% decline\n1970-2020",
    hjust = 0,
    size = 3.2
  ) +
  scale_x_continuous(
    limits = c(1970, 2020),
    breaks = seq(1970, 2020, 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, max(lpi$upper, na.rm = TRUE) * 1.03),
    expand = c(0, 0)
  ) +
  labs(
    title = "Living Planet Index",
    x = NULL,
    y = "Index value\n(1970 = 1)",
    tag = "A"
  ) +
  theme_nature()

# =====================================
# 6. Panel B
# =====================================
bii_labels <- bii_plot %>%
  group_by(region) %>%
  filter(year == max(year)) %>%
  ungroup()

p_b <- ggplot(bii_plot, aes(year, bii, colour = region)) +
  geom_line(linewidth = 0.95) +
  geom_text_repel(
    data = bii_labels,
    aes(label = region),
    direction = "y",
    hjust = 0,
    nudge_x = 2.2,
    segment.color = NA,
    size = 3.1,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = cols_regions) +
  scale_x_continuous(
    limits = c(1970, 2024),
    breaks = seq(1970, 2020, 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.01),
    expand = c(0.02, 0)
  ) +
  labs(
    title = "Biodiversity Intactness Index",
    x = NULL,
    y = "BII",
    tag = "B"
  ) +
  theme_nature()

# =====================================
# 7. Panel C
# =====================================
gdp_labels <- gdp_plot %>%
  group_by(region) %>%
  filter(year == max(year)) %>%
  ungroup()

p_c <- ggplot() +
  geom_ribbon(
    data = gdp_cont,
    aes(x = year, ymin = q25, ymax = q75, fill = continent, group = continent),
    alpha = 0.14,
    colour = NA
  ) +
  geom_line(
    data = gdp_cont,
    aes(x = year, y = median_change, colour = continent),
    linewidth = 0.95
  ) +
  geom_line(
    data = gdp_global,
    aes(x = year, y = median_change),
    colour = "black",
    linewidth = 1.1
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey45",
    linewidth = 0.35
  ) +
  geom_text_repel(
    data = gdp_labels,
    aes(x = year, y = median_change, label = region, colour = region),
    direction = "y",
    hjust = 0,
    nudge_x = 2.2,
    segment.color = NA,
    size = 3.1,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = cols_regions) +
  scale_fill_manual(values = cols_regions) +
  scale_x_continuous(
    limits = c(1970, 2024),
    breaks = seq(1970, 2020, 10),
    expand = c(0, 0)
  ) +
  labs(
    title = "GDP per capita",
    x = "Year",
    y = "% change relative\nto 1970",
    tag = "C"
  ) +
  theme_nature()

# =====================================
# 8. Combine
# =====================================
fig_abc <- p_a / p_b / p_c +
  plot_layout(heights = c(1, 1, 1))

fig_abc

ggsave("Figure_ABC_LPI_BII_GDP_clean.png", fig_abc, width = 7.2, height = 9.2, dpi = 500)
ggsave("Figure_ABC_LPI_BII_GDP_clean.pdf", fig_abc, width = 7.2, height = 9.2)






### 

pwt<-read.csv("cwtfpPENN.csv")
# pwt is  Penn World Table dataframe
# Check names first:
names(pwt)

cwtfp_index <- pwt %>%
  mutate(
    year = as.numeric(year),
    cwtfp = as.numeric(cwtfp)
  ) %>%
  filter(year >= 1970, year <= 2020, !is.na(cwtfp)) %>%
  group_by("Country") %>%
  mutate(
    cwtfp_1970 = cwtfp[year == 1970][1],
    cwtfp_index = 100 * cwtfp / cwtfp_1970
  ) %>%
  ungroup() %>%
  filter(!is.na(cwtfp_1970), !is.na(cwtfp_index)) %>%
  group_by(year) %>%
  summarise(
    median_cwtfp = median(cwtfp_index, na.rm = TRUE),
    q25 = quantile(cwtfp_index, 0.25, na.rm = TRUE),
    q75 = quantile(cwtfp_index, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

p_d <- ggplot(cwtfp_index, aes(x = year, y = median_cwtfp)) +
  geom_ribbon(aes(ymin = q25, ymax = q75),
              fill = "#74c476", alpha = 0.35) +
  geom_line(colour = "#238b45", linewidth = 1) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
  scale_x_continuous(
    limits = c(1970, 2020),
    breaks = seq(1970, 2020, 10),
    expand = c(0, 0)
  ) +
  labs(
    title = "Welfare-relevant TFP",
    x = "Year",
    y = "Index (1970 = 100)",
    tag = "D"
  ) +
  theme_nature()

p_d

fig_abcd <- p_a / p_b / p_c / p_d +
  plot_layout(heights = c(1, 1, 1,1))


ggsave("Figure_ABCD_LPI_BII_GDP_clean.pdf", fig_abcd, width = 7.2, height = 9.2)

