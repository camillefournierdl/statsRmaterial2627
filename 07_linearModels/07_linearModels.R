# Linear Models ------------------------------------------------------------

##### ------- learnings today ------- #####
# - Always look at the scatterplot before computing anything
# - Correlation: what r measures (and what it doesn't), with cor() and cor.test()
# - The regression line with lm(): intercept, slope, and how to interpret them
# - What "best fit" means: residuals (errors) and least squares -- lm() picks the line with the smallest squared errors
# - R-squared, and inference on the slope: reading summary() and confint() (same t-test logic as last week)
# - Pitfalls: how variables are coded, influential outliers, association is not causation


##### ------- setup ------- #####

library(tidyverse)
library(patchwork) # to put several ggplots together

set.seed(123) # reproducibility

# How to use this script:
# - before class: read through the toolbox, run it line by line, and note what's unclear
# - in class: we stop at the "# >> guess first" lines -- write down your guess BEFORE running the code!
# - as before, the ggplot code is there to illustrate the concepts, you don't need to learn all of it

# ESS data, Switzerland, round 9 (check the codebook in ESSData/ for how each variable is coded)
ESS9_CH <- read.csv("ESSData/ESS9_CH.csv")

ess <- ESS9_CH %>%
  select(stflife, hinctnta, health, aesfdrk, trstprt, agea, eduyrs, imwbcnt) %>%
  rename(lifeSat = stflife,   # life satisfaction, 0 (extremely dissatisfied) - 10 (extremely satisfied)
         income  = hinctnta,  # household income decile, 1 (lowest 10%) - 10 (highest 10%)
         trustPol = trstprt,  # trust in political parties, 0 - 10
         age = agea)

# missing values are coded as 7/8/9, 77/88/99 or 999 (refusal, don't know, no answer): we remove them
# variable by variable later on, so that we don't lose people who only skipped one question


##### ------- toolbox ------- #####

##### ------- part 1: look before you compute ------- #####

# --- guess the correlation ---
# we simulate 3 clouds of 200 points with rnorm() (as in week 4)
# recipe: start from x, then y = x + some random noise
#   -> little noise: the points stay close to a line (strong correlation)
#   -> a lot of noise: the line disappears in the cloud (weak correlation)

n <- 200

# cloud A: y goes up with x, but with a lot of noise
x_A <- rnorm(n)
y_A <- x_A + rnorm(n, sd = 3)

# cloud B: y goes DOWN with x (minus sign), with some noise
x_B <- rnorm(n)
y_B <- -x_B + rnorm(n, sd = 1.3)

# cloud C: y goes up with x, with little noise
x_C <- rnorm(n)
y_C <- x_C + rnorm(n, sd = 0.5)

# we put the 3 clouds in one table (long format), with a column saying which cloud each point belongs to
clouds <- bind_rows(
  tibble(x = x_A, y = y_A, cloud = "A"),
  tibble(x = x_B, y = y_B, cloud = "B"),
  tibble(x = x_C, y = y_C, cloud = "C")
)

ggplot(clouds, aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~cloud) + # one panel per cloud
  theme_minimal()

# >> guess first: what is r in each panel? write your 3 guesses down, then:
cor(x_A, y_A)
cor(x_B, y_B)
cor(x_C, y_C)
# ?> change the sd of the noise above (try 0.1, 10, ...) and re-run: from which value do you stop "seeing" a relationship?

# --- Anscombe's quartet: same numbers, very different data ---
# a built-in dataset with 4 small datasets (x1/y1, ..., x4/y4); we put it in long format for ggplot
anscombe_long <- anscombe %>%
  mutate(id = 1:n()) %>%
  pivot_longer(-id, names_to = c(".value", "set"), names_pattern = "(.)(.)")

anscombe_long %>%
  group_by(set) %>%
  summarise(mean_x = mean(x), mean_y = mean(y), r = cor(x, y))
# >> guess first: the 4 sets have the same means and the same r. Will they look the same?

ggplot(anscombe_long, aes(x = x, y = y)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm") +
  facet_wrap(~set) +
  theme_minimal()
# >> same r, same line... but only set 1 is appropriately described by a straight line. Always plot your data!


##### ------- part 2: correlation in the ESS ------- #####

# Research question: are richer people more satisfied with their life?
ess_inc <- ess %>%
  filter(lifeSat <= 10, income <= 10)
nrow(ess_inc)

# both variables only take whole numbers, so points overlap: geom_jitter() moves them a little bit
ggplot(ess_inc, aes(x = income, y = lifeSat)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.3) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal()

# >> guess first: is the correlation positive or negative? strong (close to 1) or weak?
cor(ess_inc$income, ess_inc$lifeSat)

# r has no unit, is between -1 and 1, and is symmetric:
cor(ess_inc$lifeSat, ess_inc$income) # same value

# is the correlation different from 0 in the population? H0: rho = 0
cor.test(ess_inc$income, ess_inc$lifeSat)


##### ------- part 3: the regression line ------- #####

# a linear model: lifeSat = a + b * income + error
# formula notation, as in t.test() last week: outcome ~ predictor
m_inc <- lm(lifeSat ~ income, data = ess_inc)
# summary(m_inc) # this what you'd use routinely, we look at this later
coef(m_inc) # this is just the coefficients

# the same coefficients by hand (formula sheet): b = cov(x, y) / var(x),  a = ybar - b * xbar
b <- cov(ess_inc$income, ess_inc$lifeSat) / var(ess_inc$income)
a <- mean(ess_inc$lifeSat) - b * mean(ess_inc$income)
c(a = a, b = b)

# another way to write b, with the correlation: b = r * sy / sx
cor(ess_inc$income, ess_inc$lifeSat) * sd(ess_inc$lifeSat) / sd(ess_inc$income)

# interpretation:
# - slope b: one income decile higher goes with about b more points of life satisfaction (on a 0-10 scale)
# - intercept a: predicted life satisfaction when income = 0 (decile 0 doesn't exist: not meaningful here!)

ggplot(ess_inc, aes(x = income, y = lifeSat)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.3) +
  geom_smooth(method = "lm") + # draws the lm() line (with its 95% CI band)
  scale_x_continuous(breaks = 1:10) +
  theme_minimal()

# >> guess first: what is the predicted life satisfaction of someone in the lowest decile? and in the highest?
a + b * 1
a + b * 10
# (predict() does this for you -- more on predicted values in a later class)


##### ------- part 3b: what does "best fit" mean? errors and least squares ------- #####

# with 1000+ people we can't see individual errors, so we take 15 people at random
set.seed(14) # so that we all get the same 15 people
small <- ess_inc %>% slice_sample(n = 15)

m_small <- lm(lifeSat ~ income, data = small)
# summary(m_small) # we look at this later

# for each person: the value predicted by the line (yhat), and the error (residual) = y - yhat
small <- small %>%
  mutate(yhat  = fitted(m_small),
         error = residuals(m_small))
small

# the errors are the vertical distances between each point and the line
ggplot(small, aes(x = income, y = lifeSat)) +
  geom_segment(aes(xend = income, yend = yhat), colour = "red", linewidth = 1) +
  geom_point(size = 2) +
  geom_abline(intercept = coef(m_small)[1], slope = coef(m_small)[2], colour = "blue", linewidth = 1) +
  theme_minimal()

# ?> why can't we just add up the errors to measure how good the line is?
sum(small$error) # = 0 (up to rounding): positive and negative errors cancel out
# -> so we square them, and add them up: SSE = sum of squared errors
sum(small$error^2)

# --- (your turn!) beat the computer: can you draw a line with a smaller SSE than lm()? ---
# >> guess first: change a_guess and b_guess, and re-run until you get the smallest SSE you can
# (the starting values give a flat line, close to just predicting the mean for everyone)
a_guess <- 6
b_guess <- 0.3 # see with 0

plot_line <- function(a_line, b_line, title) {
  d <- small %>% mutate(yhat_line = a_line + b_line * income)
  sse <- sum((d$lifeSat - d$yhat_line)^2)
  ggplot(d, aes(x = income, y = lifeSat)) +
    geom_segment(aes(xend = income, yend = yhat_line), colour = "red", linewidth = 1) +
    geom_point(size = 2) +
    geom_abline(intercept = a_line, slope = b_line, linewidth = 1) +
    labs(title = title, subtitle = sprintf("a = %.2f, b = %.2f, SSE = %.1f", a_line, b_line, sse)) +
    theme_minimal()
}

plot_line(a_guess, b_guess, "Your line") + plot_line(coef(m_small)[1], coef(m_small)[2], "lm() line")

# --- why does lm() always win? ---
# we try 200 slopes (for each slope, we take the best intercept), and compute the SSE of each line
sse_grid <- tibble(slope = seq(-0.4, 0.8, length.out = 200)) %>%
  mutate(intercept = mean(small$lifeSat) - slope * mean(small$income),
         SSE = map2_dbl(intercept, slope, ~ sum((small$lifeSat - (.x + .y * small$income))^2)))

ggplot(sse_grid, aes(x = slope, y = SSE)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = coef(m_small)[2], linetype = "dashed", colour = "blue") +
  labs(title = "SSE of each possible line", subtitle = "dashed: the slope chosen by lm()") +
  theme_minimal()
# >> lm() picks exactly the bottom of this curve: this is why it is called "(ordinary) least squares", OLS
# ?> compare coef(m_small) with coef(m_inc): with only 15 people, how far are we from the full-sample slope?
# ?> change the seed before slice_sample() (try 4, 5, 6) and re-run: b changes from sample to sample,
#    sometimes it is even negative! -> this is why b has a standard error (part 4)


##### ------- part 4: R-squared and inference on the slope ------- #####

# back to the full sample. How much better is the line than just predicting the mean (ybar) for everyone?
SST <- sum((ess_inc$lifeSat - mean(ess_inc$lifeSat))^2) # total error if we only use the mean
SSE <- sum(residuals(m_inc)^2)                          # error left with the line
1 - SSE / SST # R-squared: share of the variation in y "explained" by x

# in a bivariate regression, R-squared = r^2
cor(ess_inc$income, ess_inc$lifeSat)^2

# the full output:
summary(m_inc)
# how to read it:
# - Estimate:   a (Intercept) and b (income), as above
# - Std. Error: the se of b -- how much b would vary from sample to sample (as in week 4!)
# - t value:    t = b / se, the test statistic for H0: beta = 0 (no linear relationship in the population)
# - Pr(>|t|):   the two-sided P-value (as in week 6)
# - Multiple R-squared: as computed above
b_se <- summary(m_inc)$coefficients["income", "Std. Error"]
b / b_se # same t value

# 95% CI for the slope: b +- t * se
confint(m_inc)
# >> guess first: the CI excludes 0 -- what does this tell you about the P-value of the slope?

# ?> is income a "strong" predictor of life satisfaction? compare the P-value with R-squared: what do they each tell you?

# ?> r is symmetric. Is the slope symmetric too? guess first: is the slope of income ~ lifeSat equal to 1 / b ?
coef(lm(income ~ lifeSat, data = ess_inc))[2]
1 / b


##### ------- part 5: pitfalls ------- #####

# --- (a) how is the variable coded? ---
# health: subjective general health, 1 = very good ... 5 = very bad (higher = WORSE health!)
ess_health <- ess %>% filter(lifeSat <= 10, health <= 5)

lm(lifeSat ~ health, data = ess_health)
# >> negative slope: does bad health make people happier?? no: always check the codebook!
# it is easier to interpret if we reverse the scale, so that higher = better health:
ess_health <- ess_health %>% mutate(goodHealth = 6 - health) # 1 -> 5, 5 -> 1
lm(lifeSat ~ goodHealth, data = ess_health) # same size, opposite sign

# --- (b) one influential point ---
# 10 made-up people: hours of volunteering per month (x) and trust in others (y, 0-10)
volunteer <- tibble(hours = c(0, 1, 2, 2, 3, 4, 5, 5, 6, 8),
                    trust = c(4, 5, 4, 6, 5, 6, 6, 7, 6, 7))
summary(lm(trust ~ hours, data = volunteer))

# one more person: volunteers 40 hours a month, but has very low trust
volunteer_out <- bind_rows(volunteer, tibble(hours = 40, trust = 2))

# >> guess first: what happens to the slope? to r?
summary(lm(trust ~ hours, data = volunteer_out))

ggplot(volunteer_out, aes(x = hours, y = trust)) +
  geom_point(size = 2) +
  geom_smooth(data = volunteer, method = "lm", se = FALSE, colour = "blue") +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  labs(subtitle = "blue: without the outlier, red: with it") +
  theme_minimal()
# >> one point far away on x can pull the whole line (because errors are squared: see part 3b!)

# --- (c) association is not causation ---
# ?> in part 3, does income cause life satisfaction? name one variable that could affect both
# (we will see how to "control" for other variables with multiple regression next week)


##### ------- exercises (try this with your project question!) ------- #####

