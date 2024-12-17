library(ggplot2)
data("economics")
?economics
library(ggplot2)
library(reshape2)

ggplot(economics, aes(x = date, y = pce)) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method = "loess", color = "darkred", linetype = "dashed", se = FALSE) +
  ggtitle("Personal Consumption Expenditures Over Time") +
  labs(
    subtitle = "Visualizing trends in PCE over time",
    x = "Year",
    y = "PCE (Billions of Dollars)",
    caption = "Source: 'economics' dataset"
  ) +
  theme_minimal()

ggplot(economics, aes(x = date)) +
  geom_line(aes(y = unemploy, color = "Number of Unemployed")) +
  geom_line(aes(y = uempmed * 1000, color = "Median Duration of Unemployment")) +
  scale_y_continuous(
    name = "Unemployed (in thousands)",
    sec.axis = sec_axis(~ . / 1000, name = "Median Duration (weeks)")
  ) +
  scale_color_manual(values = c("red", "green")) +
  ggtitle("Unemployment Numbers and Median Duration Over Time") +
  labs(
    subtitle = "Dual-axis comparison of unemployment and duration trends",
    caption = "Source: 'economics' dataset"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

ggplot(economics) +
  geom_line(aes(x = date, y = pop), color = "purple", size = 1.2) +
  geom_line(aes(x = date, y = psavert * 10000), color = "green", size = 1, linetype = "dashed") +
  ggtitle("Population and Personal Savings Rate Trends Over Time") +
  labs(
    subtitle = "Comparing US Population Growth with Personal Savings Rate",
    x = "Year",
    y = "Population (Thousands)",
    caption = "Source: 'economics' dataset"
  ) +
  scale_y_continuous(
    labels = scales::comma_format(),
    sec.axis = sec_axis(~ . / 10000, name = "Savings Rate (%)")
  ) +
  theme_minimal()

ggplot(economics, aes(x = psavert, y = unemploy)) +
  geom_point(aes(color = uempmed), alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", color = "darkgreen", linetype = "dotted", se = TRUE) +
  ggtitle("Savings Rate vs. Unemployment with Median Duration") +
  labs(
    subtitle = "Exploring the correlation between savings rate and unemployment",
    x = "Personal Savings Rate (%)",
    y = "Unemployed (Thousands)",
    caption = "Source: 'economics' dataset"
  ) +
  scale_color_gradient(low = "yellow", high = "red", name = "Median Duration (Weeks)") +
  theme_minimal()

ggplot(economics, aes(x = psavert, y = uempmed, size = pop, label = format(date, "%Y"))) +
  geom_point(alpha = 0.7, fill = "lightblue", color = "darkblue", shape = 21) +
  scale_size_area(max_size = 15) +
  ggtitle("Bubble Chart: Savings Rate, Unemployment Duration, and Population") +
  labs(
    subtitle = "A multi-dimensional visualization of key metrics",
    x = "Personal Savings Rate (%)",
    y = "Median Unemployment Duration (weeks)",
    caption = "Source: 'economics' dataset"
  ) +
  theme_minimal()

economics_long <- melt(economics, id.vars = "date", 
                       measure.vars = c("pce", "pop", "unemploy", "uempmed"))
ggplot(economics_long, aes(x = date, y = value, color = variable)) +
  geom_line(size = 1) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  labs(
    title = "Faceted Time Series Analysis of Economics Variables",
    subtitle = "Exploring trends across key variables",
    x = "Year",
    y = "Value",
    caption = "Source: 'economics' dataset"
  ) +
  theme_minimal()

ggplot(economics, aes(x = pop, y = unemploy)) +
  geom_point(aes(color = psavert, size = uempmed), alpha = 0.7) +
  geom_smooth(method = "lm", color = "darkblue", se = FALSE, linetype = "dashed") +
  scale_color_gradient(low = "green", high = "orange", name = "Savings Rate (%)") +
  scale_size_continuous(name = "Median Duration (Weeks)", range = c(2, 10)) +
  ggtitle("Population vs. Unemployed with Savings Rate and Duration") +
  labs(
    subtitle = "Highlighting savings rate and median duration",
    x = "Population (Thousands)",
    y = "Unemployed (Thousands)",
    caption = "Source:2 'economics' dataset"
  ) +
  theme_minimal()

