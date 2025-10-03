# hello

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(strucchange)
library(ggrepel)

file <- "WID_Metadata_03102025-201237.csv"

# Read full file without skipping, but don't auto-parse headers
raw <- read_lines(file)

# Row 1 = metadata, Row 2 = column names, Rest = data
col_names <- read_csv2(file, skip = 1, n_max = 0) %>% colnames()

# Alternatively, grab them directly from line 2:
# col_names <- strsplit(raw[2], ";")[[1]]

# Now read the actual data, starting from line 3
df <- read_csv2(file, skip = 2, col_names = col_names)

# Rename columns (based on what WID puts in this extract)
colnames(df) <- c(
  "year", 
  "top10_share", 
  "bottom50_share", 
  "top1_share", 
  "gdp_pc_ppp_const2024"
)

# ---- Plotting section ----

# Keep only the shares
df_shares <- df %>%
  select(year, top10_share, bottom50_share, top1_share)

# Ensure numeric values (sometimes read_csv2 reads them as characters)
df_shares <- df_shares %>%
  mutate(across(-year, as.numeric))

# Reshape into long format for plotting
df_long <- df_shares %>%
  pivot_longer(
    cols = -year,
    names_to = "variable",
    values_to = "value"
  )

# Plot all three shares together
ggplot(df_long, aes(x = year, y = value, color = variable)) +
  geom_line(size = 1) +
  geom_point(size = 2) +  # add points to see each year
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Evolution of income shares in Australia (WID data)",
       x = "Year", y = "Share of pre-tax national income",
       color = "Income Share") +
  theme_minimal()


# Plot all three shares together with trendlines (LOESS smoothed)
ggplot(df_long, aes(x = year, y = value, color = variable)) +
  geom_line(size = 1) +
  geom_point(size = 2) +  # add points to see each year
  geom_smooth(aes(group = variable), method = "loess", se = FALSE, color = "black", linetype = "dashed") +  # black trendlines
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Evolution of income shares in Australia (WID data)",
       x = "Year", y = "Share of pre-tax national income",
       color = "Income Share") +
  theme_minimal()

# ---- Structural Break Tests ----

# Ensure numeric values
df_test <- df %>%
  mutate(across(c(top10_share, bottom50_share, top1_share), as.numeric))

# Function to run structural break test for a single series
run_break_test <- function(y, years) {
  # Fit linear model via formula directly in breakpoints()
  bp <- breakpoints(y ~ years)
  
  # Summary of breakpoints
  summary(bp)
}

# Run tests for each income share
cat("Top 10% share:\n")
top10_break <- run_break_test(df_test$top10_share, df_test$year)

cat("\nBottom 50% share:\n")
bottom50_break <- run_break_test(df_test$bottom50_share, df_test$year)

cat("\nTop 1% share:\n")
top1_break <- run_break_test(df_test$top1_share, df_test$year)

# ---- Structural Break Tests and Plot Overlay ----

# Ensure numeric values
df_test <- df %>%
  mutate(across(c(top10_share, bottom50_share, top1_share), as.numeric))

# Detect breakpoints for each income share
bp_top10 <- breakpoints(top10_share ~ year, data = df_test)
bp_bottom50 <- breakpoints(bottom50_share ~ year, data = df_test)
bp_top1 <- breakpoints(top1_share ~ year, data = df_test)

# Extract the break years
break_years_top10 <- df_test$year[bp_top10$breakpoints]
break_years_bottom50 <- df_test$year[bp_bottom50$breakpoints]
break_years_top1 <- df_test$year[bp_top1$breakpoints]

# ---- Plotting with breakpoints ----

# Keep only the shares
df_shares <- df %>%
  select(year, top10_share, bottom50_share, top1_share)

# Ensure numeric values
df_shares <- df_shares %>%
  mutate(across(-year, as.numeric))

# Reshape into long format for plotting
df_long <- df_shares %>%
  pivot_longer(
    cols = -year,
    names_to = "variable",
    values_to = "value"
  )

# Plot all three shares with trendlines and breakpoints
ggplot(df_long, aes(x = year, y = value, color = variable)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_smooth(aes(group = variable), method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  # Add vertical lines for breakpoints
  geom_vline(xintercept = break_years_top10, color = "blue", linetype = "dotted", size = 0.7) +
  geom_vline(xintercept = break_years_bottom50, color = "green", linetype = "dotted", size = 0.7) +
  geom_vline(xintercept = break_years_top1, color = "red", linetype = "dotted", size = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Evolution of income shares in Australia with structural breaks (WID data)",
       x = "Year", y = "Share of pre-tax national income",
       color = "Income Share") +
  theme_minimal()

# ---- Plotting with breakpoints and labels ----

# Keep only the shares
df_shares <- df %>%
  select(year, top10_share, bottom50_share, top1_share)

# Ensure numeric values
df_shares <- df_shares %>%
  mutate(across(-year, as.numeric))

# Reshape into long format for plotting
df_long <- df_shares %>%
  pivot_longer(
    cols = -year,
    names_to = "variable",
    values_to = "value"
  )

# Plot all three shares with trendlines and breakpoints
ggplot(df_long, aes(x = year, y = value, color = variable)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_smooth(aes(group = variable), method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  # Add vertical lines for breakpoints
  geom_vline(xintercept = break_years_top10, color = "blue", linetype = "dotted", size = 0.7) +
  geom_vline(xintercept = break_years_bottom50, color = "green", linetype = "dotted", size = 0.7) +
  geom_vline(xintercept = break_years_top1, color = "red", linetype = "dotted", size = 0.7) +
  # Add labels for breakpoints
  geom_text_repel(
    data = data.frame(x = break_years_top10, y = rep(max(df_long$value), length(break_years_top10)), label = break_years_top10),
    aes(x = x, y = y, label = label),
    color = "blue", angle = 90, vjust = -0.5, size = 3
  ) +
  geom_text_repel(
    data = data.frame(x = break_years_bottom50, y = rep(max(df_long$value), length(break_years_bottom50)), label = break_years_bottom50),
    aes(x = x, y = y, label = label),
    color = "green", angle = 90, vjust = -0.5, size = 3
  ) +
  geom_text_repel(
    data = data.frame(x = break_years_top1, y = rep(max(df_long$value), length(break_years_top1)), label = break_years_top1),
    aes(x = x, y = y, label = label),
    color = "red", angle = 90, vjust = -0.5, size = 3
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Evolution of income shares in Australia with structural breaks (WID data)",
       x = "Year", y = "Share of pre-tax national income",
       color = "Income Share") +
  theme_minimal()

# ---- GDP with income shares shaded ----

# Ensure numeric values
df_gdp <- df %>%
  mutate(
    gdp_pc_ppp_const2024 = as.numeric(gdp_pc_ppp_const2024),
    top10_share = as.numeric(top10_share),
    bottom50_share = as.numeric(bottom50_share),
    top1_share = as.numeric(top1_share)
  )

# Clean GDP data based on previous 2 years

df_gdp <- df_gdp %>%
  arrange(year) %>%
  filter(year != 1910) %>%  # remove first datapoint
  mutate(
    prev1 = lag(gdp_pc_ppp_const2024, 1),
    prev2 = lag(gdp_pc_ppp_const2024, 2)
  ) %>%
  filter(
    is.na(prev1) | gdp_pc_ppp_const2024 >= 0.5 * prev1,
    is.na(prev2) | gdp_pc_ppp_const2024 >= 0.5 * prev2
  ) %>%
  select(-prev1, -prev2)  # remove helper columns


# Compute the absolute GDP portion of each share
df_gdp <- df_gdp %>%
  mutate(
    gdp_top10 = gdp_pc_ppp_const2024 * top10_share,
    gdp_bottom50 = gdp_pc_ppp_const2024 * bottom50_share,
    gdp_top1 = gdp_pc_ppp_const2024 * top1_share
  )

# Reshape for plotting stacked areas
df_long_gdp <- df_gdp %>%
  select(year, gdp_top10, gdp_bottom50, gdp_top1) %>%
  pivot_longer(
    cols = -year,
    names_to = "variable",
    values_to = "value"
  )

# Optional: assign nicer labels
df_long_gdp$variable <- recode(df_long_gdp$variable,
                               "gdp_top10" = "Top 10%",
                               "gdp_bottom50" = "Bottom 50%",
                               "gdp_top1" = "Top 1%")

# Plot
ggplot(df_long_gdp, aes(x = year, y = value, fill = variable)) +
  geom_area(alpha = 0.6, position = "stack") +  # stacked shading
  geom_line(data = df_gdp, aes(x = year, y = gdp_pc_ppp_const2024), 
            color = "black", size = 1, inherit.aes = FALSE) +
  labs(title = "GDP per capita with income share portions (WID data)",
       x = "Year", y = "GDP per capita (constant 2024 PPP USD)",
       fill = "Income Share") +
  theme_minimal()


# ---- Clean GDP data ----

df_gdp <- df_gdp %>%
  arrange(year) %>%
  filter(year != 1910) %>%  # remove first datapoint
  mutate(
    prev1 = lag(gdp_pc_ppp_const2024, 1),
    prev2 = lag(gdp_pc_ppp_const2024, 2)
  ) %>%
  filter(
    row_number() <= 2 |  # keep first 2 remaining years
      (gdp_pc_ppp_const2024 >= 0.5 * prev1 & gdp_pc_ppp_const2024 >= 0.5 * prev2)
  ) %>%
  select(-prev1, -prev2)

# ---- Prepare income portions ----

df_gdp <- df_gdp %>%
  mutate(
    bottom50_share = as.numeric(bottom50_share),
    top10_share = as.numeric(top10_share),
    top1_share = as.numeric(top1_share),
    gdp_pc_ppp_const2024 = as.numeric(gdp_pc_ppp_const2024),
    gdp_bottom50 = gdp_pc_ppp_const2024 * bottom50_share,
    gdp_top1 = gdp_pc_ppp_const2024 * top1_share,
    gdp_top10_excl1 = gdp_pc_ppp_const2024 * (top10_share - top1_share),
    gdp_middle40 = gdp_pc_ppp_const2024 - gdp_bottom50 - gdp_top10_excl1 - gdp_top1
  )

# ---- Reshape into long format for stacked plot ----
df_long_gdp <- df_gdp %>%
  select(year, gdp_bottom50, gdp_middle40, gdp_top10_excl1, gdp_top1) %>%
  pivot_longer(
    cols = -year,
    names_to = "variable",
    values_to = "value"
  )

# ---- Compute cumulative layers for correct stacking ----

df_gdp <- df_gdp %>%
  mutate(
    bottom50_share = as.numeric(bottom50_share),
    top10_share = as.numeric(top10_share),
    top1_share = as.numeric(top1_share),
    gdp_pc_ppp_const2024 = as.numeric(gdp_pc_ppp_const2024),
    gdp_bottom50 = gdp_pc_ppp_const2024 * bottom50_share,
    gdp_top1 = gdp_pc_ppp_const2024 * top1_share,
    gdp_top10_excl1 = gdp_pc_ppp_const2024 * (top10_share - top1_share),
    gdp_middle40 = gdp_pc_ppp_const2024 - gdp_bottom50 - gdp_top10_excl1 - gdp_top1,
    # cumulative layers
    bottom50_cummin = 0,
    bottom50_cummax = gdp_bottom50,
    middle40_cummin = gdp_bottom50,
    middle40_cummax = gdp_bottom50 + gdp_middle40,
    top10_cummin = middle40_cummax,
    top10_cummax = middle40_cummax + gdp_top10_excl1,
    top1_cummin = top10_cummax,
    top1_cummax = top10_cummax + gdp_top1
  )

# ---- Reshape for plotting ----
df_long_gdp <- df_gdp %>%
  select(year,
         bottom50_cummin, bottom50_cummax,
         middle40_cummin, middle40_cummax,
         top10_cummin, top10_cummax,
         top1_cummin, top1_cummax) %>%
  pivot_longer(
    cols = -year,
    names_to = c("layer", ".value"),
    names_pattern = "(.*)_cum(min|max)"
  )

# Rename layers nicely
df_long_gdp$layer <- factor(df_long_gdp$layer,
                            levels = c("bottom50", "middle40", "top10", "top1"),
                            labels = c("Bottom 50%", "Next 40%", "Top 10% (excluding 1%)", "Top 1%"))

# ---- Plot ----
ggplot(df_long_gdp, aes(x = year, ymin = min, ymax = max, fill = layer)) +
  geom_ribbon(alpha = 0.6) +
  geom_line(data = df_gdp, aes(x = year, y = gdp_pc_ppp_const2024),
            color = "black", size = 1, inherit.aes = FALSE) +
  labs(title = "GDP per capita with income shares in Australia",
       x = "Year", y = "GDP per capita (constant 2024 PPP USD)",
       fill = "Income Share") +
  theme_minimal()

# ---- Compute growth rates (reuse df_gdp) ----
df_growth <- df_gdp %>%
  arrange(year) %>%
  mutate(
    growth_bottom50 = (gdp_bottom50 / lag(gdp_bottom50) - 1) * 100,
    growth_middle40 = (gdp_middle40 / lag(gdp_middle40) - 1) * 100,
    growth_top10_excl1 = (gdp_top10_excl1 / lag(gdp_top10_excl1) - 1) * 100,
    growth_top1 = (gdp_top1 / lag(gdp_top1) - 1) * 100,
    growth_total_gdp = (gdp_pc_ppp_const2024 / lag(gdp_pc_ppp_const2024) - 1) * 100
  ) %>%
  filter(!is.na(growth_bottom50))  # remove first year with NA

# ---- Reshape for plotting ----
df_growth_long <- df_growth %>%
  select(year, growth_bottom50, growth_middle40, growth_top10_excl1, growth_top1, growth_total_gdp) %>%
  pivot_longer(
    cols = -year,
    names_to = "group",
    values_to = "growth_rate"
  ) %>%
  mutate(group = recode(group,
                        "growth_bottom50" = "Bottom 50%",
                        "growth_middle40" = "Next 40%",
                        "growth_top10_excl1" = "Top 10% (excluding 1%)",
                        "growth_top1" = "Top 1%",
                        "growth_total_gdp" = "Total GDP"))

# ---- Plot ----
ggplot(df_growth_long, aes(x = year, y = growth_rate, color = group)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Annual GDP Growth Rates by Income Subsection",
       x = "Year",
       y = "Growth Rate (%)",
       color = "Income Group") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")