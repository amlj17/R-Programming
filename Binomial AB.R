#Proyeccion de AB's
# Create a sample of 580 numbers which are incremented by 1.
# 580 es el n de AB's
x <- seq(0,580,by = 1)

# Create the binomial distribution.
# 580 At Plate Apps de Yelich es n
# Probabilidad qe un PA sea un AB
y <- dbinom(x,580,0.843)

# Give the chart file a name.
png(file = "AtBats.png")

# Plot the graph for this sample.
plot(x,y,col = "blue",  type = "l", xlab = "At Bats", ylab = "P(x)", main = "Binomial Distribution for At Bats")

# Save the file.
dev.off()

