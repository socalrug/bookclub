# Sampling Simulation ###################################

pop_size = 250000000
parameter = 0.88
n = 1000

# run first simulation
possible_entries <- c(rep("support", parameter * pop_size), rep("not", (1 - parameter) * pop_size))
sampled_entries <- sample(possible_entries, size = n)
estimates <- sum(sampled_entries == 'support')/n

# run simulation 10000 more times
for(i in 1:10000) {
  sampled_entries <- sample(possible_entries, size = n)
  estimates <- append(estimates, sum(sampled_entries == 'support')/n)
}

# create a histogram plot below
hist(estimates, freq = F, main = "Sampling Distribution", ylim = c(0,40))
summary(estimates)

est_mean <- mean(estimates)
est_sd <- sd(estimates)

x <- seq(from = 0.84, to = 0.95, by = 0.0005)
y <- dnorm(x, est_mean, est_sd)
lines(x = x, y = y, col = 'tomato')

### Central Limit Theorem Success-Failure Conditions ##################

# Observations must be Independent
# Sufficiently Large sample size n

(success <- (n * parameter) >= 10)
(failure <- (n * (1 - parameter)) >= 10)
(success & failure)

## Smaller Sample Size ##################################################
pop_size = 250000000
parameter = 0.25
n = 500

# run first simulation
possible_entries <- c(rep("yes", parameter * pop_size), rep("no", (1 - parameter) * pop_size))
sampled_entries <- sample(possible_entries, size = n)
estimates <- sum(sampled_entries == 'yes')/n

# run simulation 10000 more times
for(i in 1:10000) {
  sampled_entries <- sample(possible_entries, size = n)
  estimates <- append(estimates, sum(sampled_entries == 'yes')/n)
}

# create a histogram plot below
hist(estimates, freq = F, main = "Sampling Distribution")
summary(estimates)

est_mean <- mean(estimates)
est_sd <- sd(estimates)

x <- seq(from = 0, to = 1, by = 0.0005)
y <- dnorm(x, est_mean, est_sd)
lines(x = x, y = y, col = 'tomato')
