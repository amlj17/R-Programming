#Proyeccion de HR's
# Create a sample of 489 numbers which are incremented by 1.
# 489 es el n, ABs
x <- seq(0,489,by = 1)

# Create the binomial distribution.
# 489 At Bats de Yelich es n
# Promedio de HR por At Bat es de 0.09
y <- dbinom(x,489,0.09)

# Give the chart file a name.
png(file = "HomeRuns.png")

# Plot the graph for this sample.
plot(x,y,col = "green",  type = "l", xlab = "Home Runs", ylab = "P(x)", main = "Binomial Distribution for Home Runs")

# Save the file.
dev.off()
