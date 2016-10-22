library(ggplot2)

data <- read.csv("proptax.csv")
data$start.year <- as.integer(substr(data$year, 1, 4))
p <- ggplot(data, aes(x=start.year, y=total.1)) + geom_point() + ylim(0, NA)
data$total.growth <- numeric(nrow(data))
data$taxable.growth <- numeric(nrow(data))

# increases <- data.frame(year <- integer(nrow(data) - 1), total.growth <- numeric(nrow(data) - 1), taxable.growth <- numeric(nrow(data) - 1))
for (i in 2:nrow(data)) {
  # increases$year[i - 1] <- data$start.year[i]
  # increases$total.growth[i - 1] <- data$total.1[i] / data$total.1[i - 1]
  # increases$taxable.growth[i - 1] <- data$taxable.value[i] / data$taxable.value[i - 1]
  data$total.growth[i] <- data$total[i] / data$total[i - 1]
  data$taxable.growth[i] <- data$taxable.value[i] / data$taxable.value[i - 1]
}

# increases$taxable.percent.growth <- (increases$taxable.growth - 1) * 100
data$total.percent.growth <- (data$total.growth - 1) * 100
x.breaks <- min(data$start.year):max(data$start.year)
y.breaks <- as.integer((min(data$total.percent.growth) - 1):(max(data$total.percent.growth) + 1))
p2 <- ggplot(data[-1,], aes(x=start.year, y=total.percent.growth)) +
  geom_point(color="red", size=2) +
  scale_x_continuous("year", breaks=x.breaks) +
  scale_y_continuous("assessed value percent growth", breaks=y.breaks) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(color="gray40"), panel.background = element_rect(fill="navy"))