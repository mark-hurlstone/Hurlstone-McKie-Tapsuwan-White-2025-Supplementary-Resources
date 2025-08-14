# ------------------------------------------------------------------------------
# This script performs the main analyses for the biosecurity collective-risk
# social dilemma experiment, including:
#   1. Biosecurity investments by risk, communication, and round
#   2. Outbreak frequency by risk, communication, and round
#   3. Group payoffs by risk, communication, and round
#   4. Correlations between risk/loss aversion and biosecurity investments
#   5. Trust as a function of communication
#   6. Player strategies as a function of communication
#
# All analyses use the unified Data.csv file containing:
# - LC_x, HC_x: investments choices
# - LO_x, HO_x: outbreak occurrences
# - LP_x, HP_x: payoff per player per round
# - ST_Px: situational trust items
# - GT_Ix: general trust items
# - Sigma, Lambda, RA, LA: measures of risk/loss aversion
# - Strategy: self-reported strategy classifications
#
# ------------------------------------------------------------------------------

# Dependencies
library(tidyverse)
library(rstatix)
library(ggplot2)
library(Hmisc) 

# ------------------------------------------------------------------------------
# 1. Import data
# ------------------------------------------------------------------------------
data <- read_csv("Data.csv")

# ------------------------------------------------------------------------------
# Tidy data into long format by risk/round for a chosen variable prefix
# ------------------------------------------------------------------------------
tidy_long <- function(df, prefix_low, prefix_high, value_name) {
  cols <- c(
    grep(paste0("^", prefix_low, "_[0-9]+$"), names(df), value = TRUE),
    grep(paste0("^", prefix_high, "_[0-9]+$"), names(df), value = TRUE)
  )
  
  df %>%
    gather(Newvar, !!value_name, all_of(cols), factor_key = TRUE) %>%
    separate(Newvar, c("Risk", "Round")) %>%
    mutate(
      Risk = recode_factor(Risk,
                           !!prefix_low := "Low",
                           !!prefix_high := "High"),
      Communication = factor(Communication, levels = c("No Communication", "Communication")),
      Round = as.numeric(Round)
    )
}

# ------------------------------------------------------------------------------
# 2. Biosecurity Investments Analysis
# ------------------------------------------------------------------------------
bio_long <- tidy_long(data, "LC", "HC", "FracCoop")

bio_summary <- bio_long %>%
  group_by(Group, Risk, Communication, Round) %>%
  get_summary_stats(FracCoop, show = c("mean", "sd", "se", "ci"))

bio_summary <- bio_summary %>%
  mutate(n = 4) # group size is constant; for specifying weights below

# GLMs with weighted binomial distribution
model1 <- glm(mean ~ Risk + Communication + Round,
              family = binomial, weights = n, data = bio_summary)
summary(model1)

model2 <- glm(mean ~ Risk + Communication + Round +
                Risk:Communication + Risk:Round + Communication:Round,
              family = binomial, weights = n, data = bio_summary)
summary(model2)

model3 <- glm(mean ~ Risk + Communication + Round +
                Risk:Communication + Risk:Round + Communication:Round +
                Risk:Communication:Round,
              family = binomial, weights = n, data = bio_summary)
summary(model3)

# Model comparisons
anova(model1, model2, model3, test = "Chisq")

# Plot
bio_plot_data <- bio_summary %>%
  group_by(Risk, Communication, Round) %>%
  get_summary_stats(mean, show = c("mean"))

ggplot(bio_plot_data, aes(x = Round, y = mean, group = Communication, color = Communication)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#355C7D", "#F67280")) +
  facet_wrap(~Risk, nrow = 1) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(1, 15), breaks = seq(1, 15, by = 2)) +
  labs(x = "", y = "Biosecurity Investment\n(Fraction of Players Protecting)",
       color = "Communication") +
  theme_bw() +
  theme(strip.text = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(.15, .8),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 12))
ggsave("BiosecurityInvestments.pdf", width = 8, height = 4)

# ------------------------------------------------------------------------------
# 3. Outbreak Frequency Analysis
# ------------------------------------------------------------------------------
outbreak_long <- tidy_long(data, "LO", "HO", "FracOut") %>%
  group_by(Group, Risk, Communication, Round) %>%
  summarise(FracOut = unique(FracOut), .groups = "drop")

# GLMs with binomial distribution
model1 <- glm(FracOut ~ Risk + Communication + Round, family = binomial, data = outbreak_long)
summary(model1)

model2 <- glm(FracOut ~ Risk + Communication + Round +
                Risk:Communication + Risk:Round + Communication:Round,
              family = binomial, data = outbreak_long)
summary(model2)

model3 <- glm(FracOut ~ Risk + Communication + Round +
                Risk:Communication + Risk:Round + Communication:Round +
                Risk:Communication:Round,
              family = binomial, data = outbreak_long)
summary(model3)

# Model comparisons
anova(model1, model2, model3, test = "Chisq")

# Plot
outbreak_plot_data <- outbreak_long %>%
  group_by(Risk, Communication, Round) %>%
  summarise(mean = mean(FracOut), .groups = "drop")

ggplot(outbreak_plot_data, aes(x = Round, y = mean, group = Communication, color = Communication)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#355C7D", "#F67280")) +
  facet_wrap(~Risk, nrow = 1) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(1, 15), breaks = seq(1, 15, by = 2)) +
  labs(x = "", y = "Outbreak Frequency\n(Fraction of Pest Outbreaks)") +
  theme_bw() +
  theme(strip.text = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.position = "none")
ggsave("OutbreakFrequency.pdf", width = 8, height = 4)

# ------------------------------------------------------------------------------
# 4. Group Payoffs Analysis
# ------------------------------------------------------------------------------
payoff_long <- tidy_long(data, "LP", "HP", "Payoff")

payoff_summary <- payoff_long %>%
  group_by(Group, Risk, Communication, Round) %>%
  get_summary_stats(Payoff, show = c("mean"))

# Convert mean payoff per player to group total
payoff_summary$mean <- payoff_summary$mean * 4

# GLMs with Gaussian distribution
model1 <- glm(mean ~ Risk + Communication + Round, family = gaussian(), data = payoff_summary)
summary(model1)

model2 <- glm(mean ~ Risk + Communication + Round + Risk:Communication + Risk:Round + Communication:Round,
              family = gaussian(), data = payoff_summary)
summary(model2)

model3 <- glm(mean ~ Risk + Communication + Round + Risk:Communication + Risk:Round +
                Communication:Round + Risk:Communication:Round, family = gaussian(),
              data = payoff_summary)
summary(model3)

# Model comparisons
anova(model1, model2, model3, test = "Chisq")

# Plot
payoff_plot_data <- payoff_summary %>%
  group_by(Risk, Communication, Round) %>%
  get_summary_stats(mean, show = c("mean", "sd", "se", "ci"))

ggplot(payoff_plot_data, aes(x = Round, y = mean, group = Communication, color = Communication)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#355C7D", "#F67280")) +
  facet_wrap(~Risk, nrow = 1) +
  scale_y_continuous(labels = scales::dollar_format(),
                     limits = c(0, 62.5),
                     breaks = seq(0, 62.5, by = 10)) +
  scale_x_continuous(limits = c(1, 15), breaks = seq(1, 15, by = 2)) +
  labs(x = "Round", y = "\n Mean Group Payoff") +
  theme_bw() +
  theme(strip.text = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.position = "none")
ggsave("GroupPayoffs.pdf", width = 8, height = 4)

# ------------------------------------------------------------------------------
# 5. Correlation Analysis: Risk/Loss Aversion vs. Biosecurity Investments
# ------------------------------------------------------------------------------

# Create composite biosecurity investment measures (per player)
data <- data %>%
  rowwise() %>%
  mutate(
    Mean_Low_Investment  = mean(c_across(LC_1:LC_15), na.rm = TRUE),
    Mean_High_Investment = mean(c_across(HC_1:HC_15), na.rm = TRUE)
  ) %>%
  ungroup()

# Select variables of interest
corr_data <- data %>%
  select(Sigma, Lambda, RA, LA, Mean_Low_Investment, Mean_High_Investment)

# Compute correlation matrix with p-values
corr_results <- rcorr(as.matrix(corr_data), type = "pearson")

# Extract correlation coefficients and p-values
cor_matrix <- corr_results$r
p_matrix   <- corr_results$P

# Print correlation coefficients
cat("\nCorrelation Coefficients:\n")
print(cor_matrix)

# Print p-values
cat("\nP-values:\n")
print(p_matrix)

# ------------------------------------------------------------------------------
# 6. Trust Analysis: Situational (ST_P1–P4) and General (GT_I1–I9)
# ------------------------------------------------------------------------------

# Create composite trust measures
data <- data %>%
  mutate(
    Situational_Trust = ifelse(
      rowSums(!is.na(select(., ST_P1:ST_P4))) == 0,
      NA_real_,
      rowMeans(select(., ST_P1:ST_P4), na.rm = TRUE)
    ),
    General_Trust = ifelse(
      rowSums(!is.na(select(., GT_I1:GT_I9))) == 0,
      NA_real_,
      rowMeans(select(., GT_I1:GT_I9), na.rm = TRUE)
    )
  )

# Prepare data in long format for plotting/analysis
trust_long <- data %>%
  select(Group, Player, Communication, Situational_Trust, General_Trust) %>%
  pivot_longer(cols = c(Situational_Trust, General_Trust),
               names_to = "Scale", values_to = "Rating") %>%
  mutate(
    Scale = recode(Scale,
                   Situational_Trust = "Situational Trust",
                   General_Trust = "General Trust"),
    Communication = factor(Communication,
                           levels = c("No Communication", "Communication"))
  )

# Summary stats per player per scale
trust_summary <- trust_long %>%
  group_by(Group, Player, Communication, Scale) %>%
  get_summary_stats(Rating, show = c("mean"))

# t-tests comparing Communication vs No Communication for each trust scale
trust_summary %>%
  group_by(Scale) %>%
  t_test(mean ~ Communication, var.equal = TRUE)

# Descriptive stats for plotting
descriptives <- trust_long %>%
  group_by(Communication, Scale) %>%
  get_summary_stats(Rating, show = c("mean", "se"))

# Plot
ggplot(data = descriptives, aes(x = Communication, y = mean, fill = Communication)) +
  geom_col(width = 0.55, position = position_dodge(0.55)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.08, size = 0.5, position = position_dodge(0.55)) +
  coord_cartesian(ylim = c(1, 5)) +
  scale_fill_manual(labels = c("No Communication", "Communication"),
                    values = c("#355C7D", "#F67280")) +
  facet_wrap(~Scale, nrow = 1) +
  labs(x = "Communication", y = "Degree of Trust") +
  theme_bw() +
  theme(strip.text = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = "none")
ggsave("Trust.pdf", width = 8, height = 5)

# ------------------------------------------------------------------------------
# 7. Player Strategy Analysis
# ------------------------------------------------------------------------------

# Map numeric strategy codes to descriptive labels
strategy_labels <- c(
  "1" = "Non Contributor",
  "2" = "Free Rider",
  "3" = "Loss Averse",
  "4" = "Conditional Cooperator",
  "5" = "Moral Cooperator",
  "6" = "Worthwhile Cooperator"
)

data <- data %>%
  mutate(
    Strategy_Label = recode(as.character(Strategy), !!!strategy_labels),
    Communication = factor(Communication, levels = c("No Communication", "Communication"))
  )

# --------------------------------------------------------------------------
# Fisher's Exact Test (Overall)
# --------------------------------------------------------------------------

# Create contingency table of Strategy x Communication
strategy_table <- table(data$Strategy_Label, data$Communication)

# Run Fisher's exact test
fisher_overall <- fisher.test(strategy_table)
print(fisher_overall)

# --------------------------------------------------------------------------
# Fisher's Exact Test (Per Strategy)
# --------------------------------------------------------------------------
# This compares "adopted this strategy" vs "did not adopt this strategy"
# for each strategy individually.
strategy_tests <- lapply(unique(data$Strategy_Label), function(strat) {
  tab <- table(
    adopted = data$Strategy_Label == strat,
    Communication = data$Communication
  )
  list(strategy = strat, fisher = fisher.test(tab))
})

# Print results for each strategy
strategy_tests

# --------------------------------------------------------------------------
# Plot: Proportion of participants adopting each strategy by Communication
# --------------------------------------------------------------------------

# Compute proportions
strategy_plot_data <- data %>%
  group_by(Communication, Strategy_Label) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Communication) %>%
  mutate(Proportion = count / sum(count)) %>%
  ungroup()

# Ensure factors are in desired order
strategy_plot_data$Strategy_Label <- factor(
  strategy_plot_data$Strategy_Label,
  levels = c("Non Contributor", "Free Rider", "Loss Averse",
             "Conditional Cooperator", "Moral Cooperator",
             "Worthwhile Cooperator")
)

# Plot
ggplot(strategy_plot_data, aes(x = Strategy_Label, y = Proportion, fill = Communication)) +
  geom_col(width = 0.55, position = position_dodge(0.55)) +
  scale_y_continuous(limits = c(0, 0.5), expand = c(0, 0)) +
  scale_fill_manual(values = c("#355C7D", "#F67280")) +
  labs(x = "Strategy", y = "Proportion of Sample", fill = "Communication") +
  theme_bw() +
  theme(strip.text = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, color = "black"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 12),
        legend.position = c(.85, .8))
ggsave("PlayerStrategies.pdf", width = 8, height = 5)
