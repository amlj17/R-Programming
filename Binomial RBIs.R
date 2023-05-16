#Proyeccion de RBI's
# Create a sample of 489 numbers which are incremented by 1.
# 489 es el n, ABs
x <- seq(0,489,by = 1)

# Create the binomial distribution.
# 489 At Bats de Yelich es n
# Promedio de RBI por At Bat es de 0.198
y <- dbinom(x,489,0.198)

# Give the chart file a name.
png(file = "RBI.png" )

# Plot the graph for this sample.
plot(x,y,col = "green",  type = "l", xlab = "Runs Batted In", ylab = "P(x)", main = "Binomial Distribution for RBIs")

# Save the file.
dev.off()

