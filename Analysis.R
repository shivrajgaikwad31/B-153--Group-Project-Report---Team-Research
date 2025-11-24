# Load required packages
library(readr)
library(dplyr)
library(ggplot2)

# 1. Load data  

# Ensure bangalore.csv is in your working directory
bangalore <- read_csv("bangalore.csv")

# 2. Data cleaning  

bangalore_clean <- bangalore %>%
  mutate(
    # Remove thousand separators and convert to numeric
    Price_num = as.numeric(gsub(",", "", Price)),
    Tax_num   = as.numeric(gsub(",", "", Tax))
  ) %>%
  # Ensure key variables are present
  filter(!is.na(Rating),
         !is.na(Price_num),
         !is.na(`Star Rating`))

# Optional: check sample size used
nrow(bangalore_clean)

# 3. Scatter plot: Rating vs Price 

p_scatter <- ggplot(bangalore_clean,
                    aes(x = Rating, y = Price_num)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Figure 1. Relationship between hotel rating and nightly room price",
    x = "Guest rating (0–5 scale)",
    y = "Nightly room price (INR)"
  ) +
  theme_minimal()

print(p_scatter)

# 4. Histogram of price  

p_hist <- ggplot(bangalore_clean,
                 aes(x = Price_num)) +
  geom_histogram(bins = 15) +
  labs(
    title = "Figure 2. Distribution of nightly room prices",
    x = "Nightly room price (INR)",
    y = "Number of hotels"
  ) +
  theme_minimal()

print(p_hist)

# 5. Pearson correlation test  

cor_result <- cor.test(
  bangalore_clean$Rating,
  bangalore_clean$Price_num,
  method = "pearson",
  alternative = "greater"  # one-sided test: positive correlation
)

cor_result

# Optional: also compute two-sided correlation if desired
cor.test(
  bangalore_clean$Rating,
  bangalore_clean$Price_num,
  method = "pearson",
  alternative = "two.sided"
)
