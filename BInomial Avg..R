#Proyeccion de Promedio de Bateo
# Create a sample of 489 numbers which are incremented by 1.
# 489 es el n, ABs
x <- seq(0,15,by = 1)

# Create the binomial distribution.
# 489 At Bats de Yelich es n
# Promedio de bateo de 0.296 es p
y <- dbinom(x,15,0.534)

# Give the chart file a name.
png(file = "Avg.png")

# Plot the graph for this sample.
plot(x,y,col = "blue", pch =19,xlab = "Games with Hit", ylab = "P(x)", main = "Binomial Distribution for Games with a Hit")

# Save the file.
dev.off()

