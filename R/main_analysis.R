install.packages("tidyverse")
install.packages("lubridate")

library(tidyverse)
library(lubridate)

data_path <- "/r-sleep-data-analysis/data/sleep_data.csv"
sleep_data <- read.csv(data_path)

summary(sleep_data)
sleep_data_clean <- na.omit(sleep_data)
sleep_data <- sleep_data_clean

sleep_data$gender <- as.factor(sleep_data$gender)
sleep_data$naps <- as.factor(sleep_data$naps)

sleep_data$bedtime_parsed <- hms(sleep_data$bedtime)
sleep_data$wake_time_parsed <- hms(sleep_data$wake_time)

time_diff <- sleep_data$wake_time_parsed - sleep_data$bedtime_parsed

sleep_data$sleep_duration_hours <- 
ifelse(time_diff < 0, as.numeric(time_diff + hours(24), units = "hours"), as.numeric(time_diff, units = "hours"))

print(summary(sleep_data$sleep_duration_hours))
print(head(sleep_data))

plots_dir <- "/r-sleep-data-analysis/plots"
dir.create(plots_dir, showWarnings = FALSE)

plot_hist <- 
    ggplot(sleep_data, aes(x = sleep_duration_hours)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    labs(title = "Distribution of Sleep Duration", x = "Sleep Duration (Hours)", y = "Frequency") +
    theme_minimal()
    ggsave(filename = file.path(plots_dir, "sleep_duration_histogram.png"), plot = plot_hist)

plot_box <-
    ggplot(sleep_data, aes(x = gender, y = sleep_duration_hours, fill = gender)) +
    geom_boxplot() +
    labs(title = "Sleep Duration by Gender", x = "Gender", y = "Sleep Duration (Hours)") +
    theme_minimal()
    ggsave(filename = file.path(plots_dir, "sleep_duration_by_gender_boxplot.png"), plot = plot_box)

correlation_test <- cor.test(sleep_data$sleep_duration_hours, sleep_data$GPA)
print(correlation_test)

simple_model <- lm(GPA ~ sleep_duration_hours, data = sleep_data)
print(summary(simple_model))

multi_model <- lm(GPA ~ sleep_duration_hours + screen_usage + naps + gender, data = sleep_data)
print(summary(multi_model))