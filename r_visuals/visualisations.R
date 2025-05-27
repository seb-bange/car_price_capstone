# ========================
# Load libraries
# ========================
library(ggplot2)
library(dplyr)
library(readr)
library(ggthemes)
library(corrplot)

# ========================
# Load data
# ========================
car <- read_csv("data/car_data.csv")

# Create cleaned feature: Engine HP new (filled with median)
car$`Engine HP new` <- ifelse(
  is.na(car$`Engine HP`),
  median(car$`Engine HP`, na.rm = TRUE),
  car$`Engine HP`
)

# Create output directory for plots if it doesn't exist
dir.create("r_visuals/plots", recursive = TRUE, showWarnings = FALSE)

# ========================
# Plot 1: Histogram of City MPG
# ========================
p1 <- ggplot(car, aes(x = `city mpg`)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black") +
  labs(title = "Distribution of City MPG", x = "City MPG", y = "Frequency") +
  theme_minimal()
print(p1)
ggsave("r_visuals/plots/city_mpg_histogram.png", p1, width = 8, height = 5)

# ========================
# Plot 2: Average MSRP by Vehicle Size
# ========================
avg_msrp <- car %>%
  group_by(`Vehicle Size`) %>%
  summarise(MSRP = mean(MSRP, na.rm = TRUE))

vehicle_order <- c("Compact", "Midsize", "Large")

p2 <- ggplot(avg_msrp, aes(x = `Vehicle Size`, y = MSRP)) +
  geom_bar(stat = "identity", fill = "orange") +
  scale_x_discrete(limits = vehicle_order) +
  labs(title = "Average MSRP by Vehicle Size", x = "Vehicle Size", y = "Average MSRP") +
  theme_minimal()
print(p2)
ggsave("r_visuals/plots/msrp_by_vehicle_size.png", p2, width = 8, height = 5)

# ========================
# Plot 3: Scatterplot – Engine HP vs MSRP
# ========================
p3 <- ggplot(car, aes(x = `Engine HP`, y = MSRP)) +
  geom_point(color = "purple", alpha = 0.6) +
  scale_y_log10() +
  labs(title = "Engine HP vs MSRP", x = "Engine HP", y = "MSRP (log)") +
  theme_minimal()
print(p3)
ggsave("r_visuals/plots/hp_vs_msrp_log.png", p3, width = 8, height = 5)

# ========================
# Plot 4: Engine HP vs MSRP by Year Category
# ========================
if ("Year Category" %in% colnames(car)) {
  p4 <- ggplot(
    car %>% filter(!is.na(`Engine HP new`), MSRP > 0),
    aes(x = `Engine HP new`, y = MSRP, color = `Year Category`)
  ) +
    geom_point(alpha = 0.6) +
    scale_y_log10() +
    labs(title = "Engine HP vs MSRP by Year Category", x = "Engine HP", y = "MSRP (log)") +
    theme_minimal()
  print(p4)
  ggsave("r_visuals/plots/hp_vs_msrp_by_year.png", p4, width = 8, height = 5)
}

# ========================
# Plot 5: Correlation Matrix
# ========================
num_vars <- car %>%
  select(`Engine HP`, `Engine Cylinders`, `city mpg`, `highway MPG`, Popularity, MSRP) %>%
  na.omit()

corr_matrix <- cor(num_vars)
png("r_visuals/plots/correlation_matrix.png", width = 800, height = 600)
corrplot(corr_matrix, method = "circle", type = "upper", tl.cex = 0.8)
dev.off()