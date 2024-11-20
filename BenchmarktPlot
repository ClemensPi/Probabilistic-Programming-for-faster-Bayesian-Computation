library(ggplot2)
library(reshape2)

# Create data frames for each benchmark result
data_n_100 <- data.frame(
  N = 100,
  Version = c("BRMS", "Sufficient_Statistics"),
  Min = c(3.322817, 1.328045),
  LQ = c(3.397887, 1.338193),
  Mean = c(3.395526, 1.354527),
  Median = c(3.409733, 1.359211),
  UQ = c(3.422702, 1.370201),
  Max = c(3.424491, 1.376983)
)

data_n_1000 <- data.frame(
  N = 1000,
  Version = c("BRMS", "Sufficient_Statistics"),
  Min = c(41.645302, 1.356832),
  LQ = c(42.160084, 1.357438),
  Mean = c(43.375043, 1.368021),
  Median = c(43.118318, 1.361214),
  UQ = c(44.499932, 1.370161),
  Max = c(45.451579, 1.394457)
)

data_n_10000 <- data.frame(
  N = 10000,
  Version = c("BRMS", "Sufficient_Statistics"),
  Min = c(1304.325031, 1.539309),
  LQ = c(1304.325031, 1.539309),
  Mean = c(1304.325031, 1.539309),
  Median = c(1304.325031, 1.539309),
  UQ = c(1304.325031, 1.539309),
  Max = c(1304.325031, 1.539309)
)

# Combine all data frames into one
benchmark_data <- rbind(data_n_100, data_n_1000, data_n_10000)

# Reshape the data for plotting
benchmark_data_long <- melt(benchmark_data, id.vars = c("N", "Version"), 
                             measure.vars = c("Min", "LQ", "Mean", "Median", "UQ", "Max"),
                             variable.name = "Statistic", value.name = "Time")

# Extract median values for line plot
median_data <- benchmark_data_long[benchmark_data_long$Statistic == "Median", ]

# Create the box plot with median lines
ggplot(benchmark_data_long, aes(x = factor(N), y = Time, fill = Version)) +
  geom_boxplot(position = position_dodge(0)) +
  geom_line(data = median_data, aes(x = factor(N), y = Time, group = Version, color = Version), size = 1) +
  geom_point(data = median_data, aes(x = factor(N), y = Time, color = Version), size = 3) +
  labs(title = "Benchmark Results for Different N",
       x = "N",
       y = "Time (seconds)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")

# Create the log-transformed box plot with median lines
ggplot(benchmark_data_long, aes(x = factor(N), y = log(Time), fill = Version)) +
  geom_boxplot(position = position_dodge(0)) +
  geom_line(data = median_data, aes(x = factor(N), y = log(Time), group = Version, color = Version), size = 1) +
  geom_point(data = median_data, aes(x = factor(N), y = log(Time), color = Version), size = 3) +
  labs(title = "Benchmark Results for Different N",
       x = "N",
       y = "log(Time) (seconds)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")
