library(igraph)

# Let's generate networks using the Barabási–Albert model
networks <- list(barabasi.game(100, m=2, directed=F), 
barabasi.game(100, m=2, directed=F), barabasi.game(100, m=2, directed=F))

# Define a vector of colors
colors <- c("red", "green", "blue")

# Initialize the plot with log-log scale
plot(1, type="n", xlim=c(1, max(sapply(networks, function(x) max(degree(x))))), ylim=c(0, 1), xlab="Degree (log scale)", ylab="Probability (log scale)", main="Degree Distributions of Barabási–Albert Networks (Log-Log Scale)", log="xy")

for (i in 1:length(networks)) {
  G <- networks[[i]]
  degree_values <- degree(G)
  print(table(degree_values))
  hist_values <- hist(degree_values, plot=FALSE)
  
  # Add lines to the plot
  lines(hist_values$mids, hist_values$density, col=colors[i])
}

# Initialize the plot with normal scale
plot(1, type="n", xlim=c(1, max(sapply(networks, function(x) max(degree(x))))), ylim=c(0, 1), xlab="Degree", ylab="Probability", main="Degree Distributions of Barabási–Albert Networks (Normal Scale)")

for (i in 1:length(networks)) {
  G <- networks[[i]]
  degree_values <- degree(G)
  hist_values <- hist(degree_values, plot=FALSE)
  
  # Add lines to the plot
  lines(hist_values$mids, hist_values$density, col=colors[i])
}