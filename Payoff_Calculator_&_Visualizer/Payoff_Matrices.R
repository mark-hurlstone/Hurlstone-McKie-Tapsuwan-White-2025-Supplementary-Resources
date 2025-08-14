# Load required libraries
library(ggplot2)
library(gridExtra)
library(dplyr)

################################################################
# Payoff calculator for a 2-firm version of Hennessy' (2008)   #
# model of biosecurity management and visualization            #
################################################################

# Model parameters
v     <- 1   # Crop revenue
cp    <- 0.2 # Production cost
cb    <- 0.4 # Biosecurity cost

# Define sigma values to explore (these will also be the matrices plotted)
sigma_values <- c(1.0, 0.9, 0.8, 0.70, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0)

# Define strategies
strategies <- c("Biosecure", "Not Biosecure")

# Function to compute payoff matrix with both firms' payoffs
compute_combined_payoff_matrix <- function(sigma) {
  combined_payoffs <- matrix("", nrow = 2, ncol = 2)
  
  for (i in 1:2) {
    for (j in 1:2) {
      strat1 <- strategies[i]
      strat2 <- strategies[j]
      
      # Compute Firm 1's and Firm 2's payoffs
      if (strat1 == "Biosecure" & strat2 == "Biosecure") {
        f1_payoff <- v - cb - cp 
        f2_payoff <- v - cb - cp
      } else if (strat1 == "Not Biosecure" & strat2 == "Biosecure") {
        f1_payoff <- sigma * v - cp
        f2_payoff <- sigma * v - cb - cp 
      } else if (strat1 == "Biosecure" & strat2 == "Not Biosecure") {
        f1_payoff <- sigma * v - cb - cp
        f2_payoff <- sigma * v - cp 
      } else {  # Both Not Biosecure
        f1_payoff <- sigma^2 * v - cp
        f2_payoff <- sigma^2 * v - cp
      }
      
      # Store the payoffs as a string "(F1, F2)" in the matrix
      combined_payoffs[i, j] <- paste0("(", round(f1_payoff, 2), ", ", round(f2_payoff, 2), ")")
    }
  }
  
  return(combined_payoffs)
}

# Function to convert computed payoff matrix into a dataframe for plotting
convert_to_dataframe <- function(sigma, payoff_matrix) {
  df <- data.frame(
    Firm1 = factor(rep(c("Biosecure", "Not Biosecure"), times = 2), levels = c("Not Biosecure", "Biosecure")),
    Firm2 = factor(rep(c("Biosecure", "Not Biosecure"), each = 2), levels = c("Biosecure", "Not Biosecure")),
    Payoff = as.vector(payoff_matrix)
  )
  
  df$Label <- df$Payoff  # Keep payoffs formatted
  
  # Compute protection probability based on number of biosecuring firms
  df$Protection <- c(
    sigma^(2-2),  # Both biosecure: sigma^(2-2) = sigma^0 = 1
    sigma^(2-1),  # Only Firm 2 biosecures: sigma^(2-1) = sigma^1 = sigma
    sigma^(2-1),  # Only Firm 1 biosecures: sigma^(2-1) = sigma^1 = sigma
    sigma^(2-0)   # Neither biosecures: sigma^(2-0) = sigma^2
  )
  
  # Format protection probability to 2 decimal places
  df$Protection <- sprintf("%.2f", df$Protection)
  
  # Define Nash equilibrium shading **per matrix**
  df$NE <- "No"  # Default no shading
  
  if (sigma > 0.6) {
    df$NE[df$Firm1 == "Not Biosecure" & df$Firm2 == "Not Biosecure"] <- "NE"  # Only bottom-right
  } else {
    df$NE[df$Firm1 == df$Firm2] <- "NE"  # Both top-left (Biosecure, Biosecure) & bottom-right (Not Biosecure, Not Biosecure)
  }
  
  return(df)
}

# Compute the matrices and store them as dataframes for plotting
payoff_matrices <- lapply(sigma_values, function(sigma) {
  payoff_matrix <- compute_combined_payoff_matrix(sigma)
  convert_to_dataframe(sigma, payoff_matrix)
})

names(payoff_matrices) <- paste0("σ = ", sigma_values)  # Assign names for plotting

# Function to plot a single payoff matrix with a red outline & red cross at σ = 0.6
plot_payoff_matrix <- function(df, title, sigma) {
  p <- ggplot(df, aes(x = Firm2, y = Firm1, fill = NE)) +
    geom_tile(color = "black", size = 1.5) +
    geom_text(aes(label = Label), size = 5) +
    geom_text(aes(label = Protection, x = as.numeric(Firm2) + 0.3, y = as.numeric(Firm1) - 0.35, fontface = "bold"), size = 5) +
    scale_fill_manual(values = c("NE" = "grey", "No" = "white"), guide = "none") +
    theme_minimal() +
    labs(title = title, x = "Firm 2", y = "Firm 1") +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 14, margin = margin(t = 1)),
      axis.text.y = element_text(size = 14, angle = 45, hjust = 1, margin = margin(r = 10)),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 10, 30, 10)
    ) +
    scale_x_discrete(position = "top")
  
  return(p)
}

# Generate individual plots for each sigma value
plots <- mapply(function(s, sigma) {
  plot_payoff_matrix(payoff_matrices[[s]], s, sigma)
}, names(payoff_matrices), sigma_values, SIMPLIFY = FALSE)

# Arrange the plots in a grid
grid.arrange(grobs = plots, ncol = 3)  # Adjust columns as needed

# Save the entire grid of matrices as a single image
final_plot <- grid.arrange(grobs = plots, ncol = 3)  # Combine all plots

# Use ggsave to save the full figure
ggsave("Figure1.png", plot = final_plot, width = 15, height = 18, dpi = 600)
