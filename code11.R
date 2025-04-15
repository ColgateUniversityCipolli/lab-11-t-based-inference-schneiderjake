########################################################################
########################################################################
#Lab 11
#Jake Schneider
########################################################################
########################################################################
library(pwr)
library(tidyverse)
library(patchwork)
library(effectsize)
library(ggpubr)


pwr.t.test(
  d = 0.65, 
  sig.level = 0.05, 
  power = 0.80, 
  type = "one.sample", 
  alternative = "two.sided"
)


########################
#load data finch data
finch.dat <- read.csv("data.csv")

#edit finch data and add a difference
finch.dat <- finch.dat |>
  rename( Farther.vals = g.Farther_vals , 
          Closer.vals = g.Closer_vals)|>
  mutate(difference = Closer.vals - Farther.vals)


#######################################################################
#3
#summarize the finch data
############
#a
sum.farther.vals <- finch.dat |>
  summarise(
    mean = mean(Farther.vals),
    sd = sd(Farther.vals),
    median = median(Farther.vals),
    min = min(Farther.vals),
    max = max(Farther.vals)
  )

############
#b
sum.closer.vals <- finch.dat |>
  summarise(
    mean = mean(Closer.vals),
    sd = sd(Closer.vals),
    median = median(Closer.vals),
    min = min(Closer.vals),
    max = max(Closer.vals)
  )

############
#c
sum.diff.vals <- finch.dat |>
  summarise(
    mean = mean(difference),
    sd = sd(difference),
    median = median(difference),
    min = min(difference),
    max = max(difference)
  )



finch_long <- finch.dat |>
  pivot_longer(cols = c(Closer.vals, Farther.vals, difference),
               names_to = "Condition",
               values_to = "Dopamine") |>
  mutate(Condition = fct_recode(Condition,
                                "Near Song" = "Closer.vals",
                                "Far Song" = "Farther.vals",
                                "Difference" = "difference"
  ))
  


boxplot.findat <- ggplot(finch_long, aes(x = Condition, y = Dopamine, fill = Condition)) +
  geom_boxplot(width = 0.6, outlier.shape = 21, outlier.size = 2) +
  labs(
    title = "Level of Dopamine Change by Singing Distance",
    x = "Condition",
    y = "Dopamine"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("boxplot.finchdat.pdf", plot = boxplot.findat, height = 5, width =6  )

#######################################################################
#4

#a T-test and hedges g for closer values 
t.closer <- t.test(finch.dat$Closer.vals, mu = 0)
g.closer <- hedges_g(finch.dat$Closer.vals, mu = 0)


parenthetical.closer <- paste0(
  "(t = ", round(t.closer$statistic, 2),
  ", p = ", ifelse(t.closer$p.value < 0.0001, "< 0.0001", signif(t.closer$p.value, 3)),
  "; g = ", round(g.closer, 2),
  "; 95% CI: ", round(t.closer$conf.int[1], 2), ", ", round(t.closer$conf.int[2], 2), ")"
)

#b T-test and hedges g for Farther values
t.farther <- t.test(finch.dat$Farther.vals, mu = 0)
g.farther <- hedges_g(finch.dat$Farther.vals, mu =0)


parenthetical.farther <- paste0(
  "(t = ", round(t.farther$statistic, 2),
  ", p = ", ifelse(t.farther$p.value < 0.0001, "< 0.0001", signif(t.farther$p.value, 3)),
  "; g = ", round(g.farther, 2),
  "; 95% CI: ", round(t.farther$conf.int[1], 2), ", ", round(t.farther$conf.int[2], 2), ")"
)

#c T-test and hedges g for difference (Closer - Farther)
t.diff <- t.test(finch.dat$difference, mu = 0)
g.diff <- hedges_g(finch.dat$difference, mu = 0)

parenthetical.diff <- paste0( 
  "(t = ", round(t.diff$statistic, 2),
  ", p = ", ifelse(t.diff$p.value < 0.0001, "< 0.0001", signif(t.diff$p.value, 3)),
  "; g = ", round(g.diff, 2),
  "; 95% CI: ", round(t.diff$conf.int[1], 2), ", ", round(t.diff$conf.int[2], 2), ")"
)

parenthetical.closer[1]
parenthetical.farther[1]
parenthetical.diff[1]



#######################################################################
#5
#create a fucntion to plot all 3 of our graphs 
plot.2tailed.function <- function(x, title_sub = expression(H[0] == 0~";"~H[a] != 0), R = 1000) {
  mu0 <- 0
  xbar <- mean(x)
  s <- sd(x)
  n <- length(x)
  t.stat <- (xbar - mu0) / (s / sqrt(n))
  
  # Null distribution
  ggdat.null <- tibble(t = seq(-5, 5, length.out = 1000)) |>
    mutate(pdf.null = dt(t, df = n - 1))
  
  # Resampling under observed distribution
  resamples <- tibble(t = numeric(R))
  for (i in 1:R) {
    samp <- sample(x, size = n, replace = TRUE)
    resamples$t[i] <- (mean(samp) - mu0) / (sd(samp) / sqrt(n))
  }
  
  # Create axis breaks
  t.breaks <- c(-5, qt(0.025, df = n - 1), 0, qt(0.975, df = n - 1), 5, t.stat)
  xbar.breaks <- t.breaks * s / sqrt(n) + mu0
  
  ggplot() +
    # Null distribution
    geom_line(data = ggdat.null, aes(x = t, y = pdf.null, color = "Null Distribution"), linewidth = 1) +
    
    # Rejection region shading (left)
    geom_ribbon(data = subset(ggdat.null, t <= qt(0.025, df = n - 1)),
                aes(x = t, ymin = 0, ymax = pdf.null, fill = "Rejection Region"), alpha = 0.4) +
    # Rejection region shading (right)
    geom_ribbon(data = subset(ggdat.null, t >= qt(0.975, df = n - 1)),
                aes(x = t, ymin = 0, ymax = pdf.null, fill = "Rejection Region"), alpha = 0.4) +
    
    # Resampled distribution (alternative)
    stat_density(data = resamples, aes(x = t, color = "Alternative Distribution"), geom = "line", linewidth = 1) +
    
    # Observed t-statistic
    geom_point(aes(x = t.stat, y = 0, color = "Observed t-statistic"), size = 3) +
    geom_vline(xintercept = t.stat, color = "red", linetype = "dotted", linewidth = 1) +
    
    # Aesthetic tweaks
    geom_hline(yintercept = 0) +
    scale_color_manual(name = "Legend", 
                       values = c("Null Distribution" = "black", 
                                  "Alternative Distribution" = "blue", 
                                  "Observed t-statistic" = "red")) +
    scale_fill_manual(name = "Legend", 
                      values = c("Rejection Region" = "grey")) +
    scale_x_continuous("t", 
                       breaks = round(t.breaks, 2),
                       sec.axis = sec_axis(~ ., name = expression(bar(x)),
                                           breaks = t.breaks,
                                           labels = round(xbar.breaks, 2))) +
    ylab("Density") +
    ggtitle("Two-Tailed T-Test with Bootstrapped Sampling Distribution", subtitle = title_sub) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "right"
    )
}


#use the function to graph the plots
close.2tailed.plot <- plot.2tailed.function(finch.dat$Closer.vals, title_sub = expression(H[0] == 0 ~ ";" ~ H[a] != 0))
far.2tailed.plot  <- plot.2tailed.function(finch.dat$Farther.vals, title_sub = expression(H[0] == 0 ~ ";" ~ H[a] != 0))
diff.2tailed.plot  <- plot.2tailed.function(finch.dat$difference, title_sub = expression(H[0] == 0 ~ ";" ~ H[a] != 0))


##############################################################################################
##############################################################################################
#one tailed test

mu0 <- 0

# One-tailed test: Closer.vals (greater)
t.closer.one <- t.test(finch.dat$Closer.vals, mu = mu0, alternative = "greater")
g.closer.one <- hedges_g(finch.dat$Closer.vals, mu = mu0)

# One-tailed test: Farther.vals (less)
t.farther.one <- t.test(finch.dat$Farther.vals, mu = mu0, alternative = "less")
g.farther.one <- hedges_g(finch.dat$Farther.vals, mu = mu0)

# One-tailed test: difference (greater)
t.diff.one <- t.test(finch.dat$difference, mu = mu0, alternative = "greater")
g.diff.one <- hedges_g(finch.dat$difference, mu = mu0)

##########   parentheticals   #############
parenthetical.closer.1tailed <- paste0(
  "(t = ", round(t.closer.one$statistic, 2),
  ", p = ", ifelse(t.closer.one$p.value < 0.0001, "< 0.0001", signif(t.closer.one$p.value, 3)),
  "; d = ", round(g.closer.one, 2),
  "; 95% CI: ", round(t.closer.one$conf.int[1], 2), ", ", round(t.closer.one$conf.int[2], 2), ")"
)

parenthetical.farther.1tailed <- paste0(
  "(t = ", round(t.farther.one$statistic, 2),
  ", p = ", ifelse(t.farther.one$p.value < 0.0001, "< 0.0001", signif(t.farther.one$p.value, 3)),
  "; d = ", round(g.farther.one, 2),
  "; 95% CI: ", round(t.farther.one$conf.int[1], 2), ", ", round(t.farther.one$conf.int[2], 2), ")"
)

parenthetical.diff.1tailed <- paste0(
  "(t = ", round(t.diff.one$statistic, 2),
  ", p = ", ifelse(t.diff.one$p.value < 0.0001, "< 0.0001", signif(t.diff.one$p.value, 3)),
  "; d = ", round(g.diff.one, 2),
  "; 95% CI: ", round(t.diff.one$conf.int[1], 2), ", ", round(t.diff.one$conf.int[2], 2), ")"
)

parenthetical.closer.1tailed[1]
parenthetical.farther.1tailed[1]
parenthetical.diff.1tailed[1]


#create a function for plotting the 3 graphs in our 1 tailed test
plot.1tailed.function <- function(x, alt = "greater", 
                                  title_sub = expression(H[0] == 0 ~ ";" ~ H[a] > 0), 
                                  R = 1000) {
  mu0 <- 0
  xbar <- mean(x)
  s <- sd(x)
  n <- length(x)
  t.stat <- (xbar - mu0) / (s / sqrt(n))
  
  # Null distribution
  ggdat.null <- tibble(t = seq(-5, 5, length.out = 1000)) |>
    mutate(pdf.null = dt(t, df = n - 1))
  
  # Resampling under observed distribution
  resamples <- tibble(t = numeric(R))
  for (i in 1:R) {
    samp <- sample(x, size = n, replace = TRUE)
    resamples$t[i] <- (mean(samp) - mu0) / (sd(samp) / sqrt(n))
  }
  
  # Set shading and subtitle depending on alternative
  if (alt == "greater") {
    shade <- subset(ggdat.null, t >= qt(0.95, df = n - 1))
    title_sub <- expression(H[0] == 0 ~ ";" ~ H[a] > 0)
  } else {
    shade <- subset(ggdat.null, t <= qt(0.05, df = n - 1))
    title_sub <- expression(H[0] == 0 ~ ";" ~ H[a] < 0)
  }
  
  # Breaks for axes
  t.breaks <- c(-5, qt(0.05, df = n - 1), 0, qt(0.95, df = n - 1), 5, t.stat)
  xbar.breaks <- t.breaks * s / sqrt(n) + mu0
  
  # Combine all elements with mapping to aesthetics for legend
  ggplot() +
    geom_line(data = ggdat.null, aes(x = t, y = pdf.null, color = "Null Distribution"), linewidth = 1) +
    geom_ribbon(data = shade, aes(x = t, ymin = 0, ymax = pdf.null, fill = "Rejection Region"), alpha = 0.4) +
    stat_density(data = resamples, aes(x = t, color = "Alternative Distribution"), geom = "line", linewidth = 1) +
    geom_point(aes(x = t.stat, y = 0, color = "Observed t-statistic"), size = 3) +
    geom_vline(xintercept = t.stat, color = "red", linetype = "dotted", linewidth = 1) +
    geom_hline(yintercept = 0) +
    scale_color_manual(name = "Legend", 
                       values = c("Null Distribution" = "black", 
                                  "Alternative Distribution" = "blue", 
                                  "Observed t-statistic" = "red")) +
    scale_fill_manual(name = "Legend", 
                      values = c("Rejection Region" = "grey")) +
    scale_x_continuous("t", 
                       breaks = round(t.breaks, 2),
                       sec.axis = sec_axis(~ ., name = expression(bar(x)),
                                           breaks = t.breaks,
                                           labels = round(xbar.breaks, 2))) +
    ylab("Density") +
    ggtitle("One-Tailed T-Test with Bootstrapped Sampling Distribution", subtitle = title_sub) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "right"
    )
}

#create our 3 plots 
close.1tailed.plot <- plot.1tailed.function(finch.dat$Closer.vals, alt = "greater", title_sub = "H[0] == 0 ; H[a] > 0")
far.1tailed.plot   <- plot.1tailed.function(finch.dat$Farther.vals, alt = "less", title_sub = "H[0] == 0 ; H[a] < 0")
diff.1tailed.plot  <- plot.1tailed.function(finch.dat$difference, alt = "greater", title_sub = "H[0] == 0 ; H[a] > 0")





close.plots <- (close.2tailed.plot / close.1tailed.plot)
farther.plots <- (far.2tailed.plot / far.1tailed.plot)
diff.plots <- (diff.2tailed.plot / diff.1tailed.plot)


ggsave("close.hyp.plots.pdf", plot = close.plots, height = 4.5, width = 7)

ggsave("farther.hyp.plots.pdf", plot = farther.plots, height = 4.5, width = 7)

ggsave("diff.hyp.plots.pdf", plot = diff.plots, height = 4.5, width = 7)










#####################################################################################################################3
#####################################################################################################################3
#####################################################################################################################3
#optional coding 
# Reshape the data to long format
# Create Subject IDs before pivoting
finch.plot <- finch.dat |>
  mutate(Subject = row_number()) |>
  pivot_longer(cols = c(Farther.vals, Closer.vals), 
               names_to = "Condition", 
               values_to = "Dopamine") |>
  mutate(Condition = recode(Condition,
                            "Farther.vals" = "Further",
                            "Closer.vals" = "Closer"),
         Condition = factor(Condition, levels = c("Further", "Closer"))
         )


# Calculate summary statistics for error bars
summary.stats <- finch.plot |>
  group_by(Condition) |>
  summarise(
    mean = mean(Dopamine),
    sd = sd(Dopamine)
  )

# Significance annotation (optional static version for plotting)
sig.annot <- data.frame(
  x = 1, xend = 2, y = 0.36, yend = 0.36,
  label = "***"
)

# Final paired-line plot
optional.graph <- ggplot(finch.plot, aes(x = Condition, y = Dopamine)) +
  geom_line(aes(group = Subject), color = "gray60", linewidth = 0.5, alpha = 0.8) +  # group moved here
  geom_point(aes(color = Condition), size = 3, alpha = 0.7) +  # individual dots
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "black") +  # mean
  geom_errorbar(data = summary.stats, aes(x = Condition, ymin = mean - sd, ymax = mean + sd),
                width = 0.15, color = "black", inherit.aes = FALSE) +
  geom_segment(data = sig.annot, aes(x = x, xend = xend, y = y, yend = yend), linewidth = 0.6) +
  geom_text(data = sig.annot, aes(x = (x + xend) / 2, y = y + 0.015, label = label), size = 5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Further" = "red", "Closer" = "blue")) +
  labs(x = NULL, y = expression(Delta*F/F~" (%)")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

ggsave("optionalgraph.pdf", plot = optional.graph, height = 5, width = 6)


#######################################################################
###########################################################################
