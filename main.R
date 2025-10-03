# hello

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(strucchange)

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
