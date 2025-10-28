# Hypothesis test -> starting from the null distribution (H0)
# H0 assumes NO DIFFERENCE between p_hat1 and p_hat2
# If there's no difference, toss them all in the same bag

# Let's simulate what a null distribution looks like
p <- 0.5
n1 <- 600
n2 <- 400

# Simulate samples from that null distribution
# We don't use for resampling! 
# rbinom using for generating new number (use sample function for existing)
n_samples <- 1e4
x1 <- rbinom(n = n_samples,
             size = n1,
             prob = p)

x2 <- rbinom(n = n_samples,
             size = n2,
             prob = p)

# Calculate statistic for each sample
p1_hat <- x1 / n1
p2_hat <- x2 / n2
diff_prop <- p2_hat - p1_hat

# Standard error for the null hypothesis
# SE(p_hat)
p_hat <- (x1 + x2) / (n1 + n2)
hist(p_hat)

se_null <- sqrt(p_hat * (1 - p_hat) * (1/ n1 + 1 / n2))

# pnorm() is the area under the curve
# Why is mean 0?
# Why is sd se_null?
# Why is the first argument = diff_prop?
pval <- pnorm(diff_prop,
              mean = 0,
              sd = se_null,
              lower.tail = FALSE)

mean(pval <= 0.05)

hist(p2_hat - p1_hat)
