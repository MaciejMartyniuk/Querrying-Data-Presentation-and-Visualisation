library(dplyr)

data("LifeCycleSavings")

summary(LifeCycleSavings)

boxplot(LifeCycleSavings$sr, 
        main = "Distribution of Aggregate Personal Savings", 
        ylab = "Savings Ratio (%)", 
        col = "lightblue")

country_names <- row.names(LifeCycleSavings) 

plot(LifeCycleSavings$dpi, LifeCycleSavings$sr, 
     main = "Relationship Between Income and Savings", 
     xlab = "Per-Capita Disposable Income (DPI)", 
     ylab = "Savings Ratio (%)", 
     pch = 21, bg = "lightblue")
abline(lm(sr ~ dpi, data = LifeCycleSavings), col = "red", lwd = 2)
legend("topright", legend = "Linear Fit", col = "red", lwd = 2)

text(LifeCycleSavings$dpi, LifeCycleSavings$sr, 
     labels = country_names, 
     cex = 0.7, pos = 4)

plot(LifeCycleSavings$ddpi, LifeCycleSavings$sr, 
     main = "Relationship Between Income Growth and Savings", 
     xlab = "Growth Rate of Disposable Income (%)", 
     ylab = "Savings Ratio (%)", 
     pch = 21, bg = "lightgreen")
abline(lm(sr ~ ddpi, data = LifeCycleSavings), col = "blue", lwd = 2)
legend("topright", legend = "Linear Fit", col = "blue", lwd = 2)

text(LifeCycleSavings$ddpi, LifeCycleSavings$sr, 
     labels = country_names, 
     cex = 0.7, pos = 4)

hist(LifeCycleSavings$sr, 
     breaks = 10, 
     main = "Distribution of Savings Ratio Across Countries", 
     xlab = "Savings Ratio (%)", 
     col = "lightblue")

average_population <- c(mean(LifeCycleSavings$pop15), mean(LifeCycleSavings$pop75))
names(average_population) <- c("Population Under Age 15", "Population Over Age 75")
pie(average_population, 
    main = "Proportion of Population by Age Groups", 
    col = c("lightgreen", "lightblue"))

correlation_matrix <- cor(LifeCycleSavings)
print("Correlation Matrix of LifeCycleSavings Dataset:")
print(correlation_matrix)

boxplot(LifeCycleSavings$sr, main = "Distribution of Aggregate Personal Savings", ylab = "Savings Ratio (%)", col = "lightblue")
plot(LifeCycleSavings$dpi, LifeCycleSavings$sr, main = "Income vs Savings Relationship", xlab = "Per-Capita Disposable Income (DPI)", ylab = "Savings Ratio (%)", pch = 21, bg = "lightblue")
plot(LifeCycleSavings$ddpi, LifeCycleSavings$sr, main = "Income Growth vs Savings Relationship", xlab = "Growth Rate of Disposable Income (%)", ylab = "Savings Ratio (%)", pch = 21, bg = "lightgreen")
hist(LifeCycleSavings$sr, breaks = 10, main = "Savings Ratio Distribution", xlab = "Savings Ratio (%)", col = "lightblue")
pie(average_population, main = "Proportion of Population by Age Groups", col = c("lightgreen", "lightblue"))


