# Hypothesis Testing ------------------------------------------------------------

##### ------- learnings today ------- #####
# - The five parts of a significance test (assumptions, hypotheses, test statistic, P-value, conclusion), done in R
# - One-sample t-test: by hand with pt()/qt(), then with t.test(); one-sided vs two-sided alternatives
# - How a 95% confidence interval and a two-sided test at alpha = 0.05 always agree
# - Two-sample tests: independent groups (Welch vs pooled) and paired samples
# - Median tests (wilcox.test) and why they are more robust to outliers than the t-test
# - What alpha really means (Type I errors), and what power / Type II errors are, through simulation
# - Statistical vs practical significance


##### ------- setup ------- #####

library(tidyverse)
library(patchwork) # to put several ggplots together (we used it in week 4)

set.seed(123) # reproducibility

# How to use this script:
# - before class: read through the toolbox, run it line by line, and note what's unclear
# - in class: we stop at the "# >> guess first" lines -- write down your guess BEFORE running the code!
# - as before, the ggplot code is there to illustrate the concepts, you don't need to learn it (yet)


##### ------- toolbox ------- #####

##### ------- part 1: from z to t ------- #####

# last week we used the normal distribution: pnorm(), qnorm(), dnorm()
# when we estimate sigma with the sample sd (s), we use the t distribution instead: pt(), qt(), dt()
# they work exactly the same way, with one extra argument: the degrees of freedom, df = n - 1

qnorm(0.975)          # 1.96, the z-value for a two-sided 95%

# >> guess first: will qt(0.975, df) be larger or smaller than 1.96? and for which df is it closest?
qt(0.975, df = 4)
qt(0.975, df = 30)
qt(0.975, df = 1000)

# the t distribution has "fatter tails" than the normal: with few observations, we are less sure about sigma,
# so we need to go further away from the mean to be 95% sure. With large n, t and z are basically the same.

grid <- seq(-4, 4, length.out = 200)
dens <- bind_rows(
  tibble(x = grid, density = dnorm(grid),       distribution = "normal (z)"),
  tibble(x = grid, density = dt(grid, df = 3),  distribution = "t, df = 3"),
  tibble(x = grid, density = dt(grid, df = 30), distribution = "t, df = 30")
)

ggplot(dens, aes(x = x, y = density, colour = distribution)) +
  geom_line(linewidth = 1) +
  labs(title = "t vs normal distribution", x = "t (or z)", y = "Density") +
  theme_minimal()


##### ------- part 2: one-sample t-test, the five parts ------- #####

# Example 6.4 from the book: an intervention to increase reading time for young people (12-17 years old).
# For the 12 people who took part, the change in reading time (in minutes, after - before) was:
reading <- c(3, 6, 9, 2, 3, 10, 6, 3, 5, 3, 8, 3)

# 1) Assumptions: random sample, quantitative variable, (roughly) normal population -> always look at your data!
hist(reading)

# 2) Hypotheses: H0: mu = 0 (no change)  vs  Ha: mu > 0 (reading time increases)
mu0 <- 0

# 3) Test statistic, by hand: t = (ybar - mu0) / se, with se = s / sqrt(n)
n    <- length(reading)
ybar <- mean(reading)
s    <- sd(reading)
se   <- s / sqrt(n)
t    <- (ybar - mu0) / se
t

# 4) P-value: probability of a t at least this large, if H0 were true (right tail, because Ha: mu > 0)
P <- pt(t, df = n - 1, lower.tail = FALSE)
P

# 5) Conclusion: P < alpha = 0.05 -> we reject H0, the data give strong evidence that reading time increased

# now the same in one line:
t.test(reading, mu = 0, alternative = "greater")

# you can extract each part of the output with $ -- compare with our hand calculations:
res <- t.test(reading, mu = 0, alternative = "greater")
res$statistic # t
res$parameter # df
res$p.value   # P

# two-sided version: Ha: mu != 0 (the default of t.test)
# >> guess first: will P be larger or smaller than for the one-sided test? by how much?
res2 <- t.test(reading, mu = 0)
res2$p.value
res2$conf.int # the 95% CI for mu

# >> CI and test are two views of the same inference:
# the 95% CI excludes 0  <=>  two-sided P < 0.05
# ?> would we reject H0: mu = 4 ? and H0: mu = 7 ? check with the CI first, then with t.test(reading, mu = ___)

# --- twist (exercise 6.43): the participant with a change of 10 minutes was wrongly recorded, it was actually 46! ---
reading_46 <- reading
reading_46[6] <- 46 # the 10 is in position 6

# >> guess first: with a much LARGER increase for one person, does the evidence for "reading time increases" get stronger or weaker?
t.test(reading_46, mu = 0, alternative = "greater")

# >> what happened to the mean? to s? to t?
mean(reading_46); sd(reading_46)
# >> one extreme value is enough to change the t-test a lot -- we come back to this in part 4


##### ------- part 3: two-sample tests ------- #####

# --- independent samples: do two groups have the same mean? ---
# H0: mu1 = mu2 (difference = 0)   vs   Ha: mu1 != mu2

iris2 <- iris %>% filter(Species %in% c("versicolor", "virginica")) # we compare 2 of the 3 species

ggplot(iris2, aes(x = Species, y = Sepal.Width, fill = Species)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  theme_minimal()

# formula notation: outcome ~ group (we will see the same notation for regressions with lm() later)
t.test(Sepal.Width ~ Species, data = iris2)

# ?> the title says "Welch Two Sample t-test": check ?t.test -- what does the default assume about the variances?
t.test(Sepal.Width ~ Species, data = iris2, var.equal = TRUE) # the "classic" pooled t-test that we learn in class, assumes equal variances

# by hand (Welch): the se of a difference combines the se of both groups
summ <- iris2 %>%
  group_by(Species) %>%
  summarise(n = n(), mean = mean(Sepal.Width), sd = sd(Sepal.Width), se = sd / sqrt(n), df = n - 1)
summ

se_diff  <- sqrt(sum(summ$se^2))          # sqrt(se1^2 + se2^2)
diff_hat <- summ$mean[1] - summ$mean[2]   # versicolor - virginica
t_obs    <- diff_hat / se_diff
t_obs # same t as in the output above (the Welch df formula is a bit ugly, we let R compute it)

df_welch <- t.test(Sepal.Width ~ Species, data = iris2)$parameter

# --- visualising what a two-sample t-test does (illustration, no need to learn this code) ---
# p0: the data | p1: sampling distribution of each mean | p2: of the difference, with its 95% CI | p3: the test

p0 <- ggplot(iris2, aes(x = Sepal.Width, fill = Species)) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 15) +
  geom_vline(data = summ, aes(xintercept = mean, colour = Species), linetype = "dashed", linewidth = 1) +
  labs(title = "1. The data (two samples)", y = "Count")

tgrid <- tibble(t = seq(-4, 4, length.out = 400))

mean_dens <- summ %>%
  crossing(tgrid) %>%
  mutate(x = mean + t * se, density = dt(t, df) / se)

p1 <- ggplot(mean_dens, aes(x = x, y = density, colour = Species)) +
  geom_line(linewidth = 1) +
  labs(title = "2. Sampling distribution of each mean", x = "Mean Sepal.Width", y = "Density")

ci <- diff_hat + c(-1, 1) * qt(0.975, df_welch) * se_diff
diff_dens <- tgrid %>% mutate(x = diff_hat + t * se_diff, density = dt(t, df_welch) / se_diff)

p2 <- ggplot(diff_dens, aes(x = x, y = density)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = ci, linetype = "longdash") +
  labs(title = "3. Difference in means, with 95% CI", subtitle = "does the CI include 0?",
       x = "Difference in means", y = "Density")

null_dens <- tibble(t = seq(-5, 5, length.out = 1000)) %>% mutate(density = dt(t, df_welch))

p3 <- ggplot(null_dens, aes(x = t, y = density)) +
  geom_line(linewidth = 1) +
  geom_area(data = filter(null_dens, t >= abs(t_obs)), fill = "red", alpha = 0.3) +
  geom_area(data = filter(null_dens, t <= -abs(t_obs)), fill = "red", alpha = 0.3) +
  geom_vline(xintercept = t_obs, linetype = "dashed") +
  labs(title = "4. If H0 were true: distribution of t",
       subtitle = sprintf("t = %.2f, two-sided P = %.2g (red area)", t_obs, 2 * pt(-abs(t_obs), df_welch)),
       y = "Density")

wrap_plots(p0, p1, p2, p3, ncol = 2) & theme_minimal()

# --- paired samples: the same people measured twice ---
before <- c(3.2, 4.1, 3.8, 3.5, 4.0)
after  <- c(3.6, 4.4, 4.1, 3.7, 4.3)

t.test(after, before, paired = TRUE)
t.test(after - before, mu = 0) # identical! a paired test is a one-sample test on the differences (as in part 2)

# >> guess first: what if we (wrongly) treat them as two independent groups? larger or smaller P?
t.test(after, before)
# >> why? (hint: look at how much people differ from each other, vs. how much each person changes)


##### ------- part 4: median tests (Wilcoxon) ------- #####

# the t-test is about the mean, and assumes a (roughly) normal population (or a large n).
# Wilcoxon tests work on the ranks of the values instead of the values themselves:
# they are about the median (more precisely, the "location"), and need fewer assumptions.
# the syntax is the same as t.test()

# one sample (Wilcoxon signed-rank test), on the reading data from part 2:
wilcox.test(reading, mu = 0, alternative = "greater") # (a warning about ties is normal here, values repeat)

# >> guess first: what happens with the wrongly recorded 46 from part 2?
wilcox.test(reading_46, mu = 0, alternative = "greater")
# >> compare with the t-test on reading_46: why does the Wilcoxon test not change at all? (hint: ranks)

# two independent samples (Wilcoxon rank-sum / Mann-Whitney test), on the iris data from part 3:
wilcox.test(Sepal.Width ~ Species, data = iris2)

# paired samples work too: wilcox.test(after, before, paired = TRUE)
# >> so why not always use Wilcoxon? with normal-ish data the t-test has more power, and it tells you about the mean, with a CI


##### ------- part 5: what does alpha mean? simulation ------- #####

## >> Big underlying question: if there is NO effect at all, how often do we still find a "significant" one?

n_obs <- 30   # observations per group
alpha <- 0.05

# (a) 1000 studies where H0 is TRUE: both groups come from the same population
p_null <- replicate(1000, t.test(rnorm(n_obs), rnorm(n_obs))$p.value)

mean(p_null < alpha) # share of "significant" results = Type I errors -> close to alpha!
hist(p_null, breaks = 20) # ?> what shape do the P-values have when H0 is true?

sum(p_null < alpha) # how many "discoveries" did you make? we'll collect the numbers in class

# (b) power: now H0 is FALSE, there is a true difference between the groups
true_diff <- 0.5 # difference in means, in standard deviations
p_alt <- replicate(1000, t.test(rnorm(n_obs, mean = true_diff), rnorm(n_obs))$p.value)

power <- mean(p_alt < alpha) # share of studies that (correctly) reject H0
power
1 - power                    # P(Type II error): there is an effect, but we miss it

# >> guess first, then change the values above and re-run part (c):
# - what happens to the power if you double n_obs? if true_diff = 0.2? if alpha = 0.01?


##### ------- exercises ------- #####

#### 6.7 ####
# According to a union agreement, the mean income for all senior-level workers in a large service company
# equals $500 per week. For a random sample of nine female employees, ybar = $410 and s = 90.
# (a) Test whether the mean income of female employees differs from $500 per week.
# (b) Report the P-value for Ha: mu < 500. (c) and for Ha: mu > 500. (Hint: the two one-sided P-values sum to 1)

# we only have summary statistics (no raw data), so no t.test() here: we do it by hand
ybar <- 410
s    <- 90
n    <- 9
mu0  <- 500

se <- s / sqrt(n)
t  <- (ybar - mu0) / se
t

2 * pt(-abs(t), df = n - 1)          # (a) two-sided P
pt(t, df = n - 1)                    # (b) Ha: mu < 500, left tail
pt(t, df = n - 1, lower.tail = FALSE) # (c) Ha: mu > 500, right tail
# >> check: (b) + (c) = 1, and (a) = 2 * the smaller one

#### 6.23 ####
# Jones and Smith separately test H0: mu = 500 vs Ha: mu != 500, each with n = 1000 and se = 10.
# Jones gets ybar = 519.5, Smith gets ybar = 519.7.

t_jones <- (519.5 - 500) / 10
t_smith <- (519.7 - 500) / 10

2 * pt(-abs(t_jones), df = 999) # 0.051 -> "not significant"
2 * pt(-abs(t_smith), df = 999) # 0.049 -> "significant"

# >> almost identical results, but opposite conclusions at alpha = 0.05.
# >> this is why we report the actual P-value (and a CI), not only "P < 0.05" / "reject H0"


##### ------- exercice (your turn) ------- #####

# Using the ESS data (Switzerland, round 9): do men and women differ in their life satisfaction?
# stflife: "How satisfied with life as a whole" (0 = extremely dissatisfied, 10 = extremely satisfied)
# gndr: 1 = male, 2 = female

ESS9_CH <- read.csv("ESSData/ESS9_CH.csv")

# already done for you: keep the two variables, remove missing codes (77, 88 = refusal, don't know), label gender
ess <- ESS9_CH %>%
  select(stflife, gndr) %>%
  filter(stflife <= 10) %>%
  mutate(gender = ifelse(gndr == 1, "male", "female"))

# fill in the blanks below (replace the ___ ):

# 1) state H0 and Ha in a comment

# 2) descriptives per group: mean, sd and sample size
ess %>%
  group_by(___) %>%
  summarise(mean = ___,
            sd   = ___,
            n    = ___)

# 3) visualise the comparison (e.g. a boxplot or a bar chart of the distribution per gender)
ggplot(ess, aes(x = ___, y = ___)) +
  geom_boxplot()

# 4) run a two-sample t-test (formula notation: outcome ~ group)
t.test(___ ~ ___, data = ___)

# 5) bonus: run the median version of the test. Do you reach the same conclusion?
wilcox.test(___)

# 6) in a comment, write your conclusion:
#    - report the difference, the 95% CI and the P-value
#    - how large is the difference, compared to the 0-10 scale? would it matter in practice? (see 6.25 & 6.26)
#    - if the result is not significant, can you conclude that men and women are equally satisfied? (see 6.48)
#    - bonus: try the same with a variable from your group project!
