# Multiple Regression (with Dummies & Predicted Values) -------------------------------------

##### ------- learnings today ------- #####
# - Why we "control" for other variables: confounders can create (or hide) a relationship
# - Multiple regression with lm(y ~ x1 + x2 + ...): each slope is the effect of x "holding the others constant"
# - Reading the output: one t-test per coefficient, R-squared vs adjusted R-squared, and checking N (missing values!)
# - Dummy and categorical predictors: what a coefficient means relative to a reference category
# - Predicted values with predict(): turning coefficients into concrete scenarios
# - Comparing several models side by side in one table (stargazer)


##### ------- setup ------- #####

library(tidyverse)
library(stargazer) # regression tables (install.packages("stargazer") if needed)

set.seed(123) # reproducibility

# How to use this script:
# - before class: read through the toolbox, run it line by line, and note what's unclear
# - in class: we stop at the "# >> guess first" lines -- write down your guess BEFORE running the code!
# - as before, the ggplot code is there to illustrate the concepts, you don't need to learn all of it

# same ESS data as last week (Switzerland, round 9), with 2 extra variables: gender and education level
ESS9_CH <- read.csv("ESSData/ESS9_CH.csv")

ess <- ESS9_CH %>%
  select(stflife, hinctnta, health, agea, eduyrs, gndr, eisced) %>%
  rename(lifeSat = stflife,   # life satisfaction, 0 (extremely dissatisfied) - 10 (extremely satisfied)
         income  = hinctnta,  # household income decile, 1 - 10
         age = agea,
         gender = gndr,       # 1 = male, 2 = female
         eduLevel = eisced)   # education level, 1 (less than lower secondary) - 7 (MA level or higher)

# reminder: missing values are coded as 7/8/9, 55, 77/88/99 or 999 -- we remove them below


##### ------- toolbox ------- #####

# the new functions of today, on the iris data again
m_iris <- lm(Petal.Width ~ Petal.Length + Sepal.Length, data = iris) # 2 predictors: just add them with +
summary(m_iris)  # one row (estimate, se, t, p) per predictor

# predicted value for a new flower: predict() + a data frame with one column per predictor, and one row per combination
predict(m_iris, newdata = data.frame(Petal.Length = 4, Sepal.Length = 6))

# a categorical predictor (a factor): lm() creates the dummy variables for you
class(iris$Species)
lm(Petal.Width ~ Species, data = iris) # "setosa" is the reference category, it doesn't get a coefficient
summary(lm(Petal.Width ~ Species, data = iris))

##### ------- part 1: why control? a simulated confounder ------- #####

# Policy question: do cities with more police officers have more crime?
# We simulate 300 cities where police REDUCES crime, but where city size affects both:
# bigger cities hire more police AND have more crime -> size is a "confounder"

n_cities <- 300
effect_police <- -1  # the TRUE effect of one more police officer on crime (we decide it!)

size   <- rnorm(n_cities, mean = 50, sd = 15)                            # population (in thousands)
police <- 2 * size + rnorm(n_cities, sd = 20)                            # bigger city -> more police
crime  <- 5 * size + effect_police * police + rnorm(n_cities, sd = 20)   # bigger city -> more crime

cities <- tibble(size, police, crime)

# >> guess first: in lm(crime ~ police), will the slope be positive, negative, or close to 0?
lm(crime ~ police, data = cities) %>% summary()

# now we add size as a second predictor ("we control for size")
m_cities <- lm(crime ~ police + size, data = cities)
summary(m_cities)
# >> the police slope is now close to the true effect (-1): the bivariate slope even had the WRONG SIGN,
#    because it mixed up the effect of police with the effect of size

# to see it: we split the cities into 5 groups of similar size (60 cities each)
cities <- cities %>%
  mutate(sizeGroup = factor(ntile(size, 5), labels = c("1 (smallest)", "2", "3", "4", "5 (largest)")))

# the model's line for each group: predicted crime at the group's average size, over the group's range of police
lines_m <- cities %>%
  group_by(sizeGroup) %>%
  summarise(size = mean(size), police = list(seq(min(police), max(police), length.out = 2))) %>%
  unnest(police) %>%
  mutate(crime_pred = predict(m_cities, newdata = .))

ggplot(cities, aes(x = police, y = crime)) +
  geom_point(aes(colour = sizeGroup), alpha = 0.6) +
  geom_smooth(method = "lm", colour = "black") +                                  # all cities together
  geom_line(data = lines_m, aes(y = crime_pred, colour = sizeGroup), linewidth = 1) + # model with size
  labs(subtitle = "black: crime ~ police, coloured: crime ~ police + size (at each group's average size)") +
  theme_minimal()
# >> overall, the line goes UP. Holding size constant, the lines go DOWN: more police, less crime.
#    the coloured lines are parallel: the model has ONE police slope, the same for all sizes;
#    size only shifts the line up (by b_size crimes for each 1000 more inhabitants)
#    (Simpson's paradox: the overall relationship can have the opposite sign of the one within groups)
# ?> set effect_police to 0 (police has no effect) and re-run part 1: what does the bivariate model say?
#    and the model with size?


##### ------- part 2: back to the ESS: does income still matter once we account for health? ------- #####

# last week: richer people are more satisfied with their life. But health could affect both
# (healthier people can work more and earn more, and are happier)
ess_h <- ess %>%
  filter(lifeSat <= 10, income <= 10, health <= 5) %>%
  mutate(goodHealth = 6 - health) # reversed as last week: 1 = very bad ... 5 = very good

m1 <- lm(lifeSat ~ income, data = ess_h)
m2 <- lm(lifeSat ~ income + goodHealth, data = ess_h)

summary(m1)
# >> guess first: when we add goodHealth, will the income slope get bigger, smaller, or stay the same?
summary(m2)

# interpretation of m2:
# - income: for two people with the SAME health, one decile higher goes with b_income more points of life satisfaction
# - goodHealth: for two people with the SAME income, one step better health goes with b_health more points
# - intercept: predicted lifeSat for income = 0 and goodHealth = 0 (not meaningful here, neither exists)

# what "holding health constant" looks like: m2 draws one line per health level, all with the same slope
ess_h <- ess_h %>% mutate(fit_m2 = fitted(m2))

ggplot(ess_h, aes(x = income, y = lifeSat, colour = factor(goodHealth))) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.2) +
  geom_line(aes(y = fit_m2), linewidth = 1) +
  scale_x_continuous(breaks = 1:10) +
  labs(colour = "good health\n(1-5)") +
  theme_minimal()
# >> the lines are parallel (same income slope), and shifted up by b_health for each step of health

# (optional) where does b_income come from? remove from income everything that health can predict,
# and regress lifeSat on what's left (the residuals, as last week):
ess_h$income_res <- residuals(lm(income ~ goodHealth, data = ess_h))
coef(lm(lifeSat ~ income_res, data = ess_h))["income_res"]
coef(m2)["income"] # same number! b_income only uses the part of income that is not "explained" by health


##### ------- part 3: reading the output of a multiple regression ------- #####

# we now add age and years of education. Each new variable has its own missing values...
ess_m <- ess %>%
  filter(lifeSat <= 10, income <= 10, health <= 5,
         age < 999, eduyrs < 77, gender <= 2, eduLevel <= 7) %>%
  mutate(goodHealth = 6 - health)

nrow(ess_h)
nrow(ess_m)
# >> pitfall: if each model uses a different sample, we shouldn't compare their coefficients!
# -> filter ONCE on all the variables you will use, and fit all models on the same data
m1 <- lm(lifeSat ~ income, data = ess_m)
m2 <- lm(lifeSat ~ income + goodHealth, data = ess_m)
m3 <- lm(lifeSat ~ income + goodHealth + age + eduyrs, data = ess_m)
nobs(m1); nobs(m3) # same N: good

summary(m3)
# how to read it (same as last week, but one row per predictor):
# - Estimate:  effect of one more unit of x, holding all the OTHER predictors constant
# - t value / Pr(>|t|): test of H0: beta = 0 for THIS predictor, given the others are in the model
# - Multiple R-squared: share of the variation in lifeSat explained by all predictors together
confint(m3)
# ?> which predictors are significant at 5%? is age's effect "large"? (think: 10 years older = ?)

# --- adding more variables to a model ---
# we add a variable that is pure noise (it has nothing to do with life satisfaction)
ess_m$noise <- rnorm(nrow(ess_m))
m3_noise <- lm(lifeSat ~ income + goodHealth + age + eduyrs + noise, data = ess_m)

# >> guess first: will R-squared go up, down, or stay the same? and adjusted R-squared?
summary(m3)$r.squared;     summary(m3_noise)$r.squared
summary(m3)$adj.r.squared; summary(m3_noise)$adj.r.squared
# >> R-squared never decreases when you add a variable, even useless ones.
#    Adjusted R-squared penalises extra predictors: use it to compare models with different numbers of predictors
#    AIC and BIC are indices that penalise extra predictors even more, use them!! 
AIC(m3);AIC(m3_noise) # smaller AIC is better!

##### ------- part 4: dummies and categorical predictors ------- #####

# --- (a) a dummy: a variable that is 0 or 1 ---
ess_m <- ess_m %>% mutate(female = ifelse(gender == 2, 1, 0)) # 1 = female, 0 = male
table(ess_m$gender, ess_m$female) # always check your recoding!

lm(lifeSat ~ female, data = ess_m)
# >> guess first: the slope of a 0/1 variable is the difference between which two numbers?
ess_m %>%
  group_by(female) %>%
  summarise(mean_lifeSat = mean(lifeSat))
# >> intercept = mean of men (female = 0), slope = mean of women - mean of men
#    (you could do the same comparison as a two-sample t-test, week 6!)

# --- (b) a categorical variable with more than 2 groups ---
table(ess_m$eduLevel)
# eduLevel is coded 1-7, but the steps are not "equal": we group it into 3 meaningful categories
ess_m <- ess_m %>%
  mutate(edu3 = case_when(eduLevel %in% c(1, 2)    ~ "lower secondary",
                          eduLevel %in% c(3, 4)    ~ "upper secondary",
                          eduLevel %in% c(5, 6, 7) ~ "tertiary"),
         edu3 = factor(edu3, levels = c("lower secondary", "upper secondary", "tertiary")))

table(ess_m$eduLevel, ess_m$edu3)

m4 <- lm(lifeSat ~ income + goodHealth + age + female + edu3, data = ess_m)
summary(m4)
# lm() made 2 dummies (upper secondary, tertiary). The first level, "lower secondary", is the reference:
# - edu3tertiary: difference in lifeSat between tertiary and lower secondary educated people,
#   holding income, health, age and gender constant
# ?> why is there no coefficient for "lower secondary"? (hint: what would a 3rd dummy add?)
# ?> compare the female coefficient here with the one in (a): it even changes sign! why? (think of part 1)

# changing the reference category
ess_m <- ess_m %>% mutate(edu3_refTert = relevel(edu3, ref = "tertiary"))
m4_refTert <- lm(lifeSat ~ income + goodHealth + age + female + edu3_refTert, data = ess_m)
# >> guess first: which coefficients will change? which will stay the same?
summary(m4)
summary(m4_refTert)
# >> only the education coefficients change (they are differences to another group); the model is the same

# ?> why not simply put eduLevel (1-7) in the model as a number? what would we assume about each step?


##### ------- part 5: predicted values ------- #####

# coefficients are hard to communicate: "what do we predict for a given person?" is often clearer
# two women aged 45 with upper secondary education: one poor with average health, one rich with very good health
two_people <- data.frame(income = c(2, 9),
                         goodHealth = c(3, 5),
                         age = c(45, 45),
                         female = c(1, 1),
                         edu3 = c("upper secondary", "upper secondary"))

# >> guess first: what life satisfaction do you predict for each of them?
predict(m4, newdata = two_people)

# the same by hand for the first person: a + b1*x1 + b2*x2 + ...
b <- coef(m4)
b["(Intercept)"] + b["income"] * 2 + b["goodHealth"] * 3 + b["age"] * 45 + b["female"] * 1 +
  b["edu3upper secondary"] * 1 + b["edu3tertiary"] * 0

# with a 95% CI for the AVERAGE life satisfaction of people like them
predict(m4, newdata = two_people, interval = "confidence")

# --- a prediction plot: lifeSat across income, for bad vs very good health ---
# we vary income (1-10) and health (2 or 5), and keep the other variables at a *meaningful* value:
# a typical age (the mean), and the most common gender / education group (check with table()!)
scenarios <- expand_grid(income = 1:10,
                         goodHealth = c(2, 5),
                         age = mean(ess_m$age),
                         female = 1,
                         edu3 = "upper secondary")

pred <- predict(m4, newdata = scenarios, interval = "confidence")
scenarios <- bind_cols(scenarios, as_tibble(pred)) # adds fit, lwr, upr

ggplot(scenarios, aes(x = income, y = fit, colour = factor(goodHealth), fill = factor(goodHealth))) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = 1:10) +
  labs(y = "predicted life satisfaction (0-10)", colour = "good health", fill = "good health",
       subtitle = "women, mean age, upper secondary education") +
  theme_minimal()
# ?> which matters more for life satisfaction here: going from the 1st to the 10th income decile,
#    or going from bad (2) to very good (5) health?
# ?> change female or edu3 in the scenarios: do the lines change shape, or only move up/down? why?
# we will talk about interactions in the next classes

##### ------- part 6: comparing models in one table ------- #####

# a common way to present results: one column per model, adding controls step by step
stargazer(m1, m2, m3, m4,
          type = "text",                          # "html" or "latex" for reports
          keep.stat = c("n", "rsq", "adj.rsq"),
          dep.var.labels = "Life satisfaction (0-10)")
# ?> follow the income coefficient from column (1) to (4): what happens to it, and why?
# ?> does adding controls prove that income CAUSES life satisfaction? what could still be missing?

# saving the table for a report: same call, with a different type and an out = file
# - LaTeX: a .tex file you can \input{} in your document (Overleaf, ...)
stargazer(m1, m2, m3, m4, type = "latex", out = "08_multipleRegressions/table_lifeSat.tex",
          keep.stat = c("n", "rsq", "adj.rsq"), dep.var.labels = "Life satisfaction (0-10)")
# - Word: save as html, then open the file with Word (right-click > open with) and copy the table
stargazer(m1, m2, m3, m4, type = "html", out = "08_multipleRegressions/table_lifeSat.html",
          keep.stat = c("n", "rsq", "adj.rsq"), dep.var.labels = "Life satisfaction (0-10)")


##### ------- exercice (your turn) ------- #####

# To answer the question from your project, draw some plots that consider more than 2 variables.
# Think of which variables would be important to include in your models, as predictors or controls.
# Compare different models in a stargazer table, to check whether your effects change based on model specifications. 
# What does that mean for your interpretations?
