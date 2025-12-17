# MORTALITY ANALYSIS AND LIFE TABLE CONSTRUCTION
# Complete analysis from raw CDC data to actuarial calculations
# Author: Lavanya Khurana
# Date: December 2025

# ==============================================================================
# SECTION 1: SETUP AND LIBRARIES
# ==============================================================================
#loading libraries 
library(tidyverse)
library(ggplot2)
library(scales)
library(viridis)
library(gridExtra)
library(knitr)
library(kableExtra)

#Setting working directory
setwd("C:/Lavanya Code/projects/Mortality Analysis")

# Set plot theme
theme_set(theme_minimal() +
            theme(plot.title = element_text(size = 14, face = "bold"),
                  axis.title = element_text(size = 11),
                  legend.position = "bottom"))

# ==============================================================================
# SECTION 2: DATA LOADING AND CLEANING
# ==============================================================================

# Read the data (adjust filename as needed)
# If you saved it as CSV, use read.csv()
# If pasted in Excel and saved as txt, use read.delim()

#Loading mortality data
mortality_raw <- read.csv("cdc_mortality_data.csv", stringsAsFactors = FALSE)


# Clean column names
names(mortality_raw) <- c("Notes", "Year", "Year_Code", "Age_Label", 
                          "Age", "Sex", "Sex_Code", "Deaths", 
                          "Population", "Crude_Rate")

# Data cleaning and processing
mortality_clean <- mortality_raw %>%
  # Remove notes column
  select(-Notes, -Year_Code, -Sex_Code, -Age_Label) %>%
  # Convert to numeric (handle "< 1 year" notation)
  mutate(
    Age = case_when(
      grepl("<", Age) ~ 0,
      grepl("year", Age) ~ as.numeric(gsub("[^0-9]", "", Age)),
      TRUE ~ as.numeric(Age)
    ),
    Deaths = as.numeric(gsub(",", "", Deaths)),
    Population = as.numeric(gsub(",", "", Population)),
    Year = as.numeric(Year)
  ) %>%
  # Remove any rows with missing data
  filter(!is.na(Age) & !is.na(Deaths) & !is.na(Population)) %>%
  # Calculate mortality rate
  mutate(
    mortality_rate = Deaths / Population,
    mortality_rate = pmin(mortality_rate, 1.0)
  ) %>%
  # Filter to reasonable age range
  filter(Age >= 0 & Age <= 100)

# Average across years for more stable estimates
mortality_avg <- mortality_clean %>%
  group_by(Age, Sex) %>%
  summarize(
    Deaths = mean(Deaths, na.rm = TRUE),
    Population = mean(Population, na.rm = TRUE),
    mortality_rate = mean(mortality_rate, na.rm = TRUE),
    .groups = "drop"
  )

cat("Data loaded successfully!\n")
cat(paste("Total observations:", nrow(mortality_clean), "\n"))
cat(paste("Years covered:", paste(unique(mortality_clean$Year), collapse = ", "), "\n"))
cat(paste("Age range:", min(mortality_clean$Age), "to", max(mortality_clean$Age), "\n\n"))

# Quick visualization to verify data quality
p_check <- ggplot(mortality_avg, aes(x = Age, y = mortality_rate, color = Sex)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(se = FALSE, linewidth = 1) +
  scale_y_log10(labels = percent_format()) +
  labs(title = "Data Quality Check: Mortality Rates by Age",
       subtitle = "Should show exponential increase with age",
       x = "Age", y = "Mortality Rate (log scale)") +
  scale_color_manual(values = c("Female" = "#e74c3c", "Male" = "#3498db"))

print(p_check)
ggsave("01_data_quality_check.png", width = 10, height = 6, dpi = 300)

# ==============================================================================
# SECTION 3: LIFE TABLE CONSTRUCTION
# ==============================================================================

build_life_table <- function(mortality_rates, initial_cohort = 100000) {
  n_ages <- length(mortality_rates)
  
  life_table <- data.frame(
    age = 0:(n_ages - 1),
    qx = mortality_rates,
    lx = numeric(n_ages),
    dx = numeric(n_ages),
    px = 1 - mortality_rates,
    Lx = numeric(n_ages),
    Tx = numeric(n_ages),
    ex = numeric(n_ages)
  )
  
  # lx: survivors at each age
  life_table$lx[1] <- initial_cohort
  for (i in 1:(n_ages - 1)) {
    life_table$dx[i] <- life_table$lx[i] * life_table$qx[i]
    life_table$lx[i + 1] <- life_table$lx[i] - life_table$dx[i]
  }
  life_table$dx[n_ages] <- life_table$lx[n_ages]
  
  # Lx: person-years lived
  for (i in 1:(n_ages - 1)) {
    life_table$Lx[i] <- (life_table$lx[i] + life_table$lx[i + 1]) / 2
  }
  life_table$Lx[n_ages] <- life_table$lx[n_ages] * 0.5
  
  # Tx: total person-years remaining
  life_table$Tx[n_ages] <- life_table$Lx[n_ages]
  for (i in (n_ages - 1):1) {
    life_table$Tx[i] <- life_table$Tx[i + 1] + life_table$Lx[i]
  }
  
  # ex: life expectancy
  life_table$ex <- life_table$Tx / life_table$lx
  
  return(life_table)
}

# Build separate life tables
life_table_male <- mortality_avg %>%
  filter(Sex == "Male") %>%
  arrange(Age) %>%
  pull(mortality_rate) %>%
  build_life_table()
life_table_male$gender <- "Male"

life_table_female <- mortality_avg %>%
  filter(Sex == "Female") %>%
  arrange(Age) %>%
  pull(mortality_rate) %>%
  build_life_table()
life_table_female$gender <- "Female"

life_table_combined <- bind_rows(life_table_male, life_table_female)

# Display key results
cat("\n=== LIFE TABLE SUMMARY ===\n")
summary_table <- life_table_combined %>%
  filter(age %in% c(0, 20, 40, 60, 65, 80)) %>%
  select(age, gender, qx, lx, ex) %>%
  pivot_wider(names_from = gender, values_from = c(qx, lx, ex))

print(kable(summary_table, digits = 2, format.args = list(big.mark = ",")))

# Save life tables
write.csv(life_table_male, "life_table_male.csv", row.names = FALSE)
write.csv(life_table_female, "life_table_female.csv", row.names = FALSE)
cat("\nLife tables saved to CSV files.\n")

# ==============================================================================
# SECTION 4: LIFE EXPECTANCY ANALYSIS
# ==============================================================================

# Visualization: Life expectancy by age
p1 <- ggplot(life_table_combined, aes(x = age, y = ex, color = gender)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = life_table_combined %>% filter(age %% 10 == 0), size = 3) +
  labs(title = "Life Expectancy by Current Age and Gender",
       subtitle = "Expected remaining years of life",
       x = "Current Age", y = "Life Expectancy (years)", color = "Gender") +
  scale_color_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c"))

print(p1)
ggsave("02_life_expectancy_by_age.png", width = 10, height = 6, dpi = 300)

# Key statistics
cat("\n=== KEY LIFE EXPECTANCY FINDINGS ===\n")
cat(sprintf("Life expectancy at birth (e0):\n"))
cat(sprintf("  Males:   %.1f years\n", life_table_male$ex[1]))
cat(sprintf("  Females: %.1f years\n", life_table_female$ex[1]))
cat(sprintf("  Gap:     %.1f years (%.1f%% difference)\n", 
            life_table_female$ex[1] - life_table_male$ex[1],
            ((life_table_female$ex[1] - life_table_male$ex[1]) / life_table_male$ex[1]) * 100))

cat(sprintf("\nLife expectancy at age 65 (retirement):\n"))
cat(sprintf("  Males:   %.1f more years (live to age %.1f)\n", 
            life_table_male$ex[66], 65 + life_table_male$ex[66]))
cat(sprintf("  Females: %.1f more years (live to age %.1f)\n", 
            life_table_female$ex[66], 65 + life_table_female$ex[66]))

# ==============================================================================
# SECTION 5: SURVIVAL CURVES
# ==============================================================================

# Calculate median lifespan
cat("\n=== DEBUGGING SURVIVAL RATES ===\n")
cat("Last few ages in male table:\n")
print(tail(life_table_male %>% mutate(survival_pct = lx/100000) %>% select(age, lx, survival_pct), 10))
cat("\nLast few ages in female table:\n")
print(tail(life_table_female %>% mutate(survival_pct = lx/100000) %>% select(age, lx, survival_pct), 10))

# Robust median calculation
calc_median <- function(life_table) {
  candidate <- life_table %>%
    filter(lx / 100000 <= 0.5) %>%
    slice(1) %>%
    pull(age)
  
  if(length(candidate) == 0) {
    # If no age reaches 50% mortality, find closest
    min_survival <- min(life_table$lx / 100000)
    cat(sprintf("  Warning: Survival never drops below 50%%. Minimum survival: %.1f%%\n", 
                min_survival * 100))
    return(NA)
  }
  return(candidate)
}

median_male <- calc_median(life_table_male)
median_female <- calc_median(life_table_female)

# Survival curve visualization (with conditional median lines)
p2 <- ggplot(life_table_combined, aes(x = age, y = lx / 100000, color = gender)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5)

# Only add median lines if they exist
if(!is.na(median_male)) {
  p2 <- p2 + geom_vline(xintercept = median_male, color = "#3498db", 
                        linetype = "dashed", alpha = 0.5)
}
if(!is.na(median_female)) {
  p2 <- p2 + geom_vline(xintercept = median_female, color = "#e74c3c", 
                        linetype = "dashed", alpha = 0.5)
}

p2 <- p2 +
  labs(title = "Survival Curves by Gender",
       subtitle = "Proportion of original cohort surviving to each age",
       x = "Age", y = "Proportion Surviving", color = "Gender") +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
  annotate("text", x = 75, y = 0.55, label = "50% survival line", hjust = 0)

print(p2)
ggsave("03_survival_curves.png", width = 10, height = 6, dpi = 300)

cat(sprintf("\n=== MEDIAN LIFESPAN ===\n"))
if(!is.na(median_male) && !is.na(median_female)) {
  cat(sprintf("Age by which 50%% of cohort has died:\n"))
  cat(sprintf("  Males:   %d years\n", median_male))
  cat(sprintf("  Females: %d years\n", median_female))
} else {
  cat("Note: Data may not extend to ages where 50% mortality is reached.\n")
  if(!is.na(median_male)) cat(sprintf("  Males:   %d years\n", median_male))
  if(!is.na(median_female)) cat("  Females: Data insufficient\n")
}

# Quartile analysis with robust handling
calc_percentile <- function(lt, pct) {
  result <- lt %>% filter(lx / 100000 <= pct/100) %>% slice(1) %>% pull(age)
  if(length(result) == 0) return(NA)
  return(result)
}

quartiles <- data.frame(
  Percentile = c("25th (75% alive)", "50th (Median)", "75th (25% alive)"),
  Male = c(calc_percentile(life_table_male, 75),
           median_male,
           calc_percentile(life_table_male, 25)),
  Female = c(calc_percentile(life_table_female, 75),
             median_female,
             calc_percentile(life_table_female, 25))
)
quartiles$Gap <- quartiles$Female - quartiles$Male
quartiles$Note <- c("", 
                    ifelse(is.na(median_female), "Female exceeds data range (>84)", ""),
                    "")

cat("\nQuartile Analysis:\n")
print(kable(quartiles, digits = 0))

# ==============================================================================
# SECTION 6: ACTUARIAL PRESENT VALUES
# ==============================================================================

interest_rate <- 0.05
v <- 1 / (1 + interest_rate)

# Whole life insurance calculation
calc_life_insurance_pv <- function(life_table, v = 1/1.05) {
  lt <- life_table %>%
    mutate(
      death_prob = (lx / lx[1]) * qx,
      pv_factor = v^(age + 0.5),
      epv_contribution = death_prob * pv_factor
    )
  list(table = lt, total_epv = sum(lt$epv_contribution))
}

life_ins_male <- calc_life_insurance_pv(life_table_male, v)
life_ins_female <- calc_life_insurance_pv(life_table_female, v)

cat("\n=== ACTUARIAL PRESENT VALUES ===\n")
cat(sprintf("Present value of $1 whole life insurance (issued at birth):\n"))
cat(sprintf("  Males:   $%.4f (Fair premium: $%.2f per $1,000 coverage)\n", 
            life_ins_male$total_epv, life_ins_male$total_epv * 1000))
cat(sprintf("  Females: $%.4f (Fair premium: $%.2f per $1,000 coverage)\n", 
            life_ins_female$total_epv, life_ins_female$total_epv * 1000))
cat(sprintf("  Ratio:   %.2f%% (females pay %.1f%% less)\n", 
            (life_ins_female$total_epv / life_ins_male$total_epv) * 100,
            ((life_ins_male$total_epv - life_ins_female$total_epv) / life_ins_male$total_epv) * 100))

# Life annuity calculation
calc_annuity_pv <- function(life_table, v = 1/1.05, start_age = 65) {
  lt <- life_table %>%
    filter(age >= start_age) %>%
    mutate(
      survival_prob = lx / lx[age == start_age],
      pv_factor = v^(age - start_age),
      epv_contribution = survival_prob * pv_factor
    )
  list(table = lt, total_epv = sum(lt$epv_contribution))
}

annuity_male <- calc_annuity_pv(life_table_male, v, 65)
annuity_female <- calc_annuity_pv(life_table_female, v, 65)

cat(sprintf("\nPresent value of $1/year life annuity (starting at age 65):\n"))
cat(sprintf("  Males:   $%.2f\n", annuity_male$total_epv))
cat(sprintf("  Females: $%.2f\n", annuity_female$total_epv))
cat(sprintf("  Ratio:   %.2f%% (females cost %.1f%% more)\n", 
            (annuity_female$total_epv / annuity_male$total_epv) * 100,
            ((annuity_female$total_epv - annuity_male$total_epv) / annuity_male$total_epv) * 100))

cat(sprintf("\nTo fund $1,000/month ($12,000/year) pension starting at 65:\n"))
cat(sprintf("  Males:   $%s\n", format(round(annuity_male$total_epv * 12000), big.mark = ",")))
cat(sprintf("  Females: $%s\n", format(round(annuity_female$total_epv * 12000), big.mark = ",")))

# Visualize annuity EPV
annuity_viz <- bind_rows(
  annuity_male$table %>% mutate(gender = "Male"),
  annuity_female$table %>% mutate(gender = "Female")
)

p3 <- ggplot(annuity_viz, aes(x = age, y = epv_contribution, color = gender)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Expected Present Value of Annuity Payments by Age",
       subtitle = "Shows when most annuity value comes from (females cost more due to longevity)",
       x = "Age", y = "EPV Contribution", color = "Gender") +
  scale_color_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
  theme_minimal()

print(p3)
ggsave("04_annuity_epv_by_age.png", width = 10, height = 6, dpi = 300)

# ==============================================================================
# SECTION 7: COMPREHENSIVE DASHBOARD
# ==============================================================================

# Create 4-panel visualization
p_dash1 <- ggplot(life_table_combined, aes(x = age, y = ex, color = gender)) +
  geom_line(linewidth = 1) +
  labs(title = "A. Life Expectancy", x = "Age", y = "Years") +
  scale_color_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
  theme(legend.position = "none")

p_dash2 <- ggplot(life_table_combined, aes(x = age, y = lx/100000, color = gender)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "B. Survival Probability", x = "Age", y = "Surviving") +
  scale_color_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
  theme(legend.position = "none")

p_dash3 <- ggplot(life_table_combined, aes(x = age, y = qx, color = gender)) +
  geom_line(linewidth = 1) +
  scale_y_log10(labels = percent_format()) +
  labs(title = "C. Mortality Rate (log)", x = "Age", y = "qx") +
  scale_color_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
  theme(legend.position = "none")

p_dash4 <- ggplot(life_table_combined %>% filter(age >= 65), 
                  aes(x = age, y = dx, color = gender)) +
  geom_line(linewidth = 1) +
  labs(title = "D. Deaths by Age (65+)", x = "Age", y = "Deaths") +
  scale_color_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
  theme(legend.position = "bottom")

dashboard <- grid.arrange(p_dash1, p_dash2, p_dash3, p_dash4, ncol = 2,
                          top = "Mortality Analysis Dashboard")
ggsave("05_mortality_dashboard.png", dashboard, width = 12, height = 10, dpi = 300)

# ==============================================================================
# SECTION 8: SUMMARY STATISTICS TABLE
# ==============================================================================

summary_stats <- data.frame(
  Metric = c("Life expectancy at birth", "Life expectancy at 65",
             "Median lifespan", "% surviving to 85",
             "Whole life insurance PV ($1)", "Life annuity PV ($1/yr at 65)"),
  Male = c(life_table_male$ex[1], life_table_male$ex[66], 
           ifelse(is.na(median_male), NA, median_male),
           ifelse(86 <= nrow(life_table_male), (life_table_male$lx[86] / 100000) * 100, NA),
           life_ins_male$total_epv, annuity_male$total_epv),
  Female = c(life_table_female$ex[1], life_table_female$ex[66], 
             ifelse(is.na(median_female), NA, median_female),
             ifelse(86 <= nrow(life_table_female), (life_table_female$lx[86] / 100000) * 100, NA),
             life_ins_female$total_epv, annuity_female$total_epv)
) %>%
  mutate(
    Gap = Female - Male,
    Pct_Diff = ifelse(!is.na(Male) & Male != 0, (Gap / Male) * 100, NA)
  )

cat("\n=== COMPREHENSIVE GENDER COMPARISON ===\n")
print(kable(summary_stats, digits = 2, format.args = list(big.mark = ",")))

# Save summary
write.csv(summary_stats, "mortality_summary_statistics.csv", row.names = FALSE)


# ==============================================================================
# SECTION 9: INSURANCE PRICING EXAMPLES
# ==============================================================================

cat("\n\n=== PRACTICAL INSURANCE EXAMPLES ===\n")
cat("\n1. LIFE INSURANCE PRICING:\n")
cat(sprintf("$250,000 whole life policy issued at birth (with 30%% loading):\n"))
cat(sprintf("  Male fair premium:   $%s\n", 
            format(round(life_ins_male$total_epv * 250000), big.mark = ",")))
cat(sprintf("  Male loaded premium: $%s\n", 
            format(round(life_ins_male$total_epv * 250000 * 1.3), big.mark = ",")))
cat(sprintf("  Female fair premium:   $%s\n", 
            format(round(life_ins_female$total_epv * 250000), big.mark = ",")))
cat(sprintf("  Female loaded premium: $%s (saves $%s)\n", 
            format(round(life_ins_female$total_epv * 250000 * 1.3), big.mark = ","),
            format(round((life_ins_male$total_epv - life_ins_female$total_epv) * 250000 * 1.3), big.mark = ",")))

cat("\n3. PENSION ANNUITY PRICING:\n")
cat(sprintf("$60,000/year pension starting at age 65:\n"))
cat(sprintf("  Male lump sum needed:   $%s\n", 
            format(round(annuity_male$total_epv * 60000), big.mark = ",")))
cat(sprintf("  Female lump sum needed: $%s (%.1f%% more)\n", 
            format(round(annuity_female$total_epv * 60000), big.mark = ","),
            ((annuity_female$total_epv - annuity_male$total_epv) / annuity_male$total_epv) * 100))

cat("\n3. SOCIAL SECURITY IMPLICATIONS:\n")
cat(sprintf("Average retirement period from age 65:\n"))
cat(sprintf("  Males:   %.1f years\n", life_table_male$ex[66]))
cat(sprintf("  Females: %.1f years\n", life_table_female$ex[66]))
cat(sprintf("  Mixed:   %.1f years (50/50 split)\n", 
            (life_table_male$ex[66] + life_table_female$ex[66]) / 2))

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n\n" , paste(rep("=", 70), collapse = ""))
cat("\n=== ANALYSIS COMPLETE ===\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("FILES CREATED:\n")
cat("  - life_table_male.csv\n")
cat("  - life_table_female.csv\n")
cat("  - mortality_summary_statistics.csv\n")
cat("  - 01_data_quality_check.png\n")
cat("  - 02_life_expectancy_by_age.png\n")
cat("  - 03_survival_curves.png\n")
cat("  - 04_annuity_epv_by_age.png\n")
cat("  - 05_mortality_dashboard.png\n\n")
