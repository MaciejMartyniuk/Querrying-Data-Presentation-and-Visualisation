library(lattice)

# Histogram of xx
xx <- c(9.20, 6.00, 6.00, 11.25, 11.00, 7.25, 9.7, 13.25, 14.00, 8.00)
histogram(xx, breaks = c(6, 8, 10, 12, 14), type = "count", 
          main = "Histogram of xx", 
          xlab = "Values", 
          ylab = "Frequency", 
          right = FALSE)

# Boxplot of Groups
set.seed(591)
xx1 <- rnorm(20, mean = 3, sd = 3.6)
xx2 <- rpois(40, lambda = 3.5)
xx3 <- rchisq(31, df = 5, ncp = 0)
data <- data.frame(
  values = c(xx1, xx2, xx3),
  groups = factor(rep(c("Group-1", "Group-2", "Group-3"), 
                      times = c(length(xx1), length(xx2), length(xx3))))
)
bwplot(values ~ groups, data = data, 
       main = "Boxplot of Groups", 
       xlab = "Groups", 
       ylab = "Values")

# Matrix Plot
mdf <- cbind(3:6, (3:6)^2, (3:6)^3)
colnames(mdf) <- c("Y1", "Y2", "Y3")
mdf_df <- data.frame(x = 1:4, Y1 = mdf[, 1], Y2 = mdf[, 2], Y3 = mdf[, 3])
xyplot(Y1 + Y2 + Y3 ~ x, data = mdf_df, type = "o", auto.key = list(columns = 3))

# Judge Ratings 
USJudge <- apply(USJudgeRatings, 2, function(x) x / max(x))
USJudge_df <- as.data.frame(USJudge)
USJudge_df$Judge <- rownames(USJudgeRatings)

dotplot(as.table(as.matrix(USJudge_df[, -ncol(USJudge_df)])), 
        groups = factor(USJudge_df$Judge), 
        main = "Judge Ratings (Normalized)",
        xlab = "Rating Criteria", 
        ylab = "Normalized Score", 
        auto.key = list(space = "right"))

# Contour Plot
aaa <- seq(0, pi, length = 10)
xxx <- rep(aaa, 10)
yyy <- rep(aaa, each = 10)
zzz <- sin(xxx) + sin(yyy)
matrix_data <- matrix(zzz, ncol = 10)
contourplot(matrix_data, main = "Contour Plot", xlab = "X", ylab = "Y", region = TRUE)

# Maunga Whau Volcano
x <- 10 * (1:nrow(volcano))
y <- 10 * (1:ncol(volcano))
volcano_df <- expand.grid(x = x, y = y)
volcano_df$z <- as.vector(volcano)
contourplot(z ~ x * y, data = volcano_df, 
            cuts = 20, col.regions = terrain.colors(100), 
            main = "Maunga Whau Volcano")

# 3D Visualization
z <- 2 * volcano
x <- 10 * (1:nrow(z))
y <- 10 * (1:ncol(z))
volcano_df <- expand.grid(x = x, y = y)
volcano_df$z <- as.vector(z)
cloud(z ~ x * y, data = volcano_df, col = "green3", main = "3D Visualization")

# 3D Sin Visualization
aaa <- seq(0, pi, length = 10)
xxx <- rep(aaa, 10)
yyy <- rep(aaa, each = 10)
zzz <- sin(xxx) + sin(yyy)
matrix_data <- matrix(zzz, ncol = 10)
cloud(as.vector(matrix_data) ~ rep(1:10, 10) * rep(1:10, each = 10), 
      col = "gray", main = "3D Sin Visualization")

# Mosaic Chart Proxy
aa <- table(factor(rep(1:3, each = 6)), 
            factor(c(rep(1:3, 3:1), rep(1:3, 2), rep(1:3, 1:3))))
barchart(aa, stack = TRUE, main = "Mosaic Chart Proxy", 
         xlab = "X", ylab = "Y")

# Boxplot of Treatments
data(OrchardSprays)
bwplot(decrease ~ treatment, data = OrchardSprays, 
       main = "Boxplot of Treatments", 
       xlab = "Treatment", ylab = "Decrease",
       scales = list(y = list(log = TRUE)))

# VA Deaths
VADeaths_subset <- VADeaths[1:2,]
barchart(VADeaths_subset, stack = TRUE, main = "VA Deaths",
         auto.key = list(columns = 2))

# Correlation Heatmap
Eggs <- read.csv("http://jolej.linuxpl.info/Eggs.csv", header = TRUE)
cor_matrix <- cor(Eggs[, sapply(Eggs, is.numeric)], use = "complete.obs")
cor_data <- as.data.frame(as.table(cor_matrix))
levelplot(Freq ~ Var1 * Var2, data = cor_data, 
          main = "Correlation Heatmap",
          xlab = "Variables", ylab = "Variables",
          scales = list(x = list(rot = 90, cex = 0.8), 
                        y = list(cex = 0.8)),
          col.regions = colorRampPalette(c("red", "white", "blue"))(100),
          at = seq(-1, 1, length.out = 100),
          panel = function(...) {
            panel.levelplot(...)
            panel.text(cor_data$Var1, cor_data$Var2, 
                       round(cor_data$Freq, 2), 
                       cex = 0.8, col = "black")
          })











