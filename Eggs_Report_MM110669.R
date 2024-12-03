Eggs <- read.csv("http://jolej.linuxpl.info/Eggs.csv", header=TRUE)

head(Eggs)
str(Eggs)

summary(Eggs)

Eggs$Time <- Eggs$Week

colSums(is.na(Eggs))

plot(Eggs$Time, Eggs$Cases, type="l", main="Egg Sales (Cases) Over Time", xlab="Time (Week)", ylab="Sales Volume (Cases)", col="blue")

plot(Eggs$Egg.Pr, Eggs$Cases, main="Effect of Egg Price on Sales", xlab="Egg Price", ylab="Sales Volume (Cases)", pch=19, col="darkgreen")
abline(lm(Eggs$Cases ~ Eggs$Egg.Pr), col="blue", lwd=2) 

plot(Eggs$Beef.Pr, Eggs$Cases, main="Effect of Beef Price on Sales", xlab="Beef Price", ylab="Sales Volume (Cases)", pch=19, col="red")
abline(lm(Eggs$Cases ~ Eggs$Beef.Pr), col="blue", lwd=2) 

plot(Eggs$Pork.Pr, Eggs$Cases, main="Effect of Pork Price on Sales", xlab="Pork Price", ylab="Sales Volume (Cases)", pch=19, col="purple")
abline(lm(Eggs$Cases ~ Eggs$Pork.Pr), col="blue", lwd=2) 

plot(Eggs$Chicken.Pr, Eggs$Cases, main="Effect of Chicken Price on Sales", xlab="Chicken Price", ylab="Sales Volume (Cases)", pch=19, col="orange")
abline(lm(Eggs$Cases ~ Eggs$Chicken.Pr), col="blue", lwd=2) 

plot(Eggs$Cereal.Pr, Eggs$Cases, main="Effect of Cereal Price on Sales", xlab="Cereal Price", ylab="Sales Volume (Cases)", pch=19, col="brown")
abline(lm(Eggs$Cases ~ Eggs$Cereal.Pr), col="blue", lwd=2)

plot(Eggs$Time, Eggs$Cases, type="l", col="blue", xlab="Time (Week)", ylab="Sales Volume (Cases)", main="Sales and Egg Price Over Time", ylim=c(min(Eggs$Cases), max(Eggs$Cases)))

par(new=TRUE)
plot(Eggs$Time, Eggs$Egg.Pr, type="l", col="red", axes=FALSE, xlab="", ylab="", ylim=c(min(Eggs$Egg.Pr), max(Eggs$Egg.Pr)))
axis(side=4, col="red", col.axis="red")
mtext("Egg Price", side=4, line=3, col="red")

legend("topright", legend=c("Sales (Cases)", "Egg Price"), col=c("blue", "red"), lty=1)

Eggs$Month <- as.factor(Eggs$Month)
boxplot(Cases ~ Month, data=Eggs, main="Sales Volume by Month", xlab="Month", ylab="Sales Volume (Cases)", col="lightblue")

Eggs$Easter <- as.factor(Eggs$Easter)
boxplot(Cases ~ Easter, data=Eggs, main="Effect of Easter on Sales Volume", xlab="Easter (Non Easter vs Easter)", ylab="Sales Volume (Cases)", col="lightgreen")

correlation_matrix <- cor(Eggs[, sapply(Eggs, is.numeric)], use="complete.obs")
print(correlation_matrix)

library(corrplot)

color_palette <- colorRampPalette(c("blue", "white", "red"))(200)

corrplot(correlation_matrix, 
         method="color", 
         type="upper", 
         col=color_palette, 
         tl.cex=0.8,       # Text size for labels
         addCoef.col="black", # Add correlation values in black
         number.cex=0.8,   # Text size for correlation values
         cl.cex=0.8)       # Text size for the color legend






