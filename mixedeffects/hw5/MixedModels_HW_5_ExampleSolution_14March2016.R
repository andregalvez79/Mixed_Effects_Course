# Mixed-model Course 2015/2016
# Example solution Homework Week 5


# load necessary libraries (well, I might not need all of them)
library(lattice)
library(lme4)
library(pbkrtest)
library(ggplot2)
library(psych) # for the command describeBy()
library(car) # for the command scatterplot()
library(boot) # for the command boot.ci()
#library(multicore) # in the past, I have also used that package (for the bootstrapped confidence intervals), but now I think I can do everything with parallel (package below) 

library(parallel) # for the PBmodcomp, to be able to use more than 1 cpu

# set contrasts to sum-to-zero (for unordered factors) and polynomial (for ordered factors)
options(contrasts = c("contr.sum", "contr.poly"))


# ok, so this thing you were not supposed to do in the homework!
?ChickWeight


# but let's first have a look at this data frame
str(ChickWeight)

head(ChickWeight)
tail(ChickWeight)

# and the whole thing:
ChickWeight


# 1. Check the data frame whether there are any missing values
# Well, if I scroll/look through the whole data frame on the screen, I don't see any NA entries, suggesting that there are no missing values.
# I can check that also more formerly:
unique(is.na(ChickWeight))
  # weight  Time Chick  Diet
# 1  FALSE FALSE FALSE FALSE


#2. My grouping factor, i.e., the variable I am going to use as a random intercept variable is Chick.
# I want to turn Chick into an explicit factor (because the original variable Chick has numbers as entries, which could theoretically lead to problems in case it would be treated as a continuous variable instead of a factor)
# I first create a new copy of the data frame and add variables in that new data frame
ChickWeight2 <- ChickWeight

ChickWeight2$f_Chick <- as.factor(paste('chick', ChickWeight2$Chick, sep = '_'))


# 3. Fixed-effects structure of my model:
# I'm interested in investigating the effects of Diet, Time, and their interaction.
# Since Diet has numeric entries, I want to turn it into an explicit factor
# Since the research question asks me for the Diet effect in the middle of the experiment, I will center Time

ChickWeight2$f_Diet <- as.factor(paste('diet', ChickWeight2$Diet, sep = '_'))

# checking that I use indeed sum-to-zero coding (as this is needed for Type 3 errors)

contrasts(ChickWeight2$f_Diet)
       # [,1] [,2] [,3]
# diet_1    1    0    0
# diet_2    0    1    0
# diet_3    0    0    1
# diet_4   -1   -1   -1
# --> good!


# OK, I still have to center Time
ChickWeight2$c_Time <- scale(ChickWeight2$Time, center = TRUE, scale = FALSE)



# 4. Use crosstables to figure out which of the fixed effects can be modeled with random slopes (in addition to fixed effects)
# Thus, I know that my random intercept (i.e., grouping variable) is f_Chick, so I create 3 cross tables; each of them has f_Chick in there, plus:
# f_Diet (to check whether f_Diet can be modeled with random slopes varying over f_Chick)
# c_Time (to check whether c_Time can be modeled with random slopes varying over f_Chick)
# f_Diet, c_Time (to check whether the interaction can be modeled with random slopes varying over f_Chick)


# OK, so here's the first one
with(ChickWeight2, table(f_Chick, f_Diet))
# oops, that gives a very looooooong output
# what happens if I put the variables in in different order?
with(ChickWeight2, table(f_Diet, f_Chick)) # now I get a very wide table. hmm, the first one might be a bit easier to look through, as I can see for each chick which of the diets it was fed (but you can do the same also with the wide table, of course, doesn't matter much)

# In any case, I conclude that each chick only got 1 Diet; so diet was "between-chicks" and thus cannot be modeled with random slopes varying over chick (like it would be the case for gender, for example)

# second table
with(ChickWeight2, table(f_Chick, Time)) # note, I could also use the centered Time variable here, it doesn;t matter
# I could also have switched the order of variables: with(ChickWeight2, table(Time, f_Chick))

# What I can see: For most chicks, we have 1 observation per Time point. In total, we have for most chicks 12 observations. This means that we can model Time with a random slope varying over f_Chick (we need at least 3 observations per Chick to model a continuous predictor with a random slope).
# We notice a second thing: Some of the cells have 0 values. And it seems that if a chick has a 0 at a specific time point (let's say day 6), all the following time points are also 0.
# A pretty plausible (but sad) explanation is that that specific chick may have died and thus couldn't be fed anymore...
# Thus, in contrast to what we thought in question (a) above, it seems that there ARE missing data (they are just not present in the data).


# third table
with(ChickWeight2, table(f_Chick, f_Diet, Time))
# well, since we know that Diet is 'between-chicks' we don't actually have to test whether the interaction is within-chicks (because it won't), but for the fun of it, we can still create this table and conclude indeed that the interaction is not within-chicks

# hmm, I wonder which order of variables leads to the easiest-to-read table, so I try out different orders
with(ChickWeight2, table(f_Diet, Time, f_Chick)) # that's easier to read than the table from the command above



with(ChickWeight2, table(Time, f_Diet, f_Chick)) # that's also ok; though I think I like the previous one best



# (e) Write down maximal model
chick_m1 <- lmer(weight ~ f_Diet * c_Time + (1 + c_Time | f_Chick), data = ChickWeight2)


# (f) Run the model and check summary output
chick_m1 <- lmer(weight ~ f_Diet * c_Time + (1 + c_Time | f_Chick), data = ChickWeight2)
# no convergence warnings, good!

summary(chick_m1)

# Random effects:
 # Groups   Name        Variance Std.Dev. Corr
 # f_Chick  (Intercept) 624.70   24.994       
          # c_Time       10.92    3.305   1.00
 # Residual             163.37   12.782       
# Number of obs: 578, groups:  f_Chick, 50

# Fixed effects:
               # Estimate Std. Error t value
# (Intercept)    124.1498     3.7475   33.13
# f_Diet1        -23.2118     5.5339   -4.19
# f_Diet2         -3.2436     6.7794   -0.48
# f_Diet3         16.5307     6.7794    2.44
# c_Time           8.9602     0.4967   18.04
# f_Diet1:c_Time  -2.6832     0.7325   -3.66
# f_Diet2:c_Time  -0.3511     0.8986   -0.39
# f_Diet3:c_Time   2.4626     0.8986    2.74

# what do you notice: the correlation between the random intercept and random slope is a perfect 1.0, i.e., these random effects are redundant. but since we didn't get any convergence warnings, I conclude that I don't have to change my model.

# Also, we have 578 observations and we have 14 parameters. If we go with the rule of thumb that we need at least 10 observations per parameter, this would say we need at least 140 observations, and we have many more, so that should all be fine.

# BTW: How did I get the number of parameters? Well, either you can count them yourself from the summary output (yes, I also counted the residual variance), or you can do a more formal approach like this:

# The function getMe() is a function to extract all kinds of information from a fitted object; for example:

getME(chick_m1, "beta") # this gives me the fixed effects (similar to fixef(mymodel), but just the numbers without the names of the estimates

# so to get the *number* of fixed effects in the model, we can do this:
length(getME(chick_m1, "beta")) # this give me 8

# to extract the random effects:
getME(chick_m1, "theta")

# to get the number
length(getME(chick_m1, "theta")) # this gives me 3


# to get the residual variance
getME(chick_m1, "sigma")
length(getME(chick_m1, "sigma")) # well, that will be always 1...



# diagnostic plots
densityplot(resid(chick_m1, scaled = TRUE)) # looks pretty ok
qqPlot(resid(chick_m1, scaled = TRUE)) # looks pretty ok (it looks like there might be some outliers, but I'll check when I compute the proportions below)
plot(chick_m1, type = c('p', 'smooth')) # fitted vs. residual --> looks like non-OK! might be that there is a strong quadratic effect going on? as a matter of fact, see chick_m2 below: yes, once the quadratic is fitted as well, that pattern looks ok! (But you didn't have to do that for the homework): Scroll to the end of the script to see how I fitted both a linear and quadratic effect of time


# compute the proportions of residuals for +/- 2, 2.5, 3
sum(abs(resid(chick_m1, scaled = TRUE)) > 2) / length(resid(chick_m1)) # 0.0467128
sum(abs(resid(chick_m1, scaled = TRUE)) > 2.5) / length(resid(chick_m1)) # 0.01211073
sum(abs(resid(chick_m1, scaled = TRUE)) > 3) / length(resid(chick_m1)) # 0.003460208




# (h) determine p values using mixed with PB and 500 simulations

MyCluster <- makeCluster(rep("localhost", detectCores() - 1))


chick_m1_mixed_PB <- mixed(weight ~ c_Time * Diet + (1 + c_Time | Chick), data = ChickWeight2, type = 3, method = 'PB', cl = MyCluster, progress = TRUE, args.test = list(nsim = 500, cl = MyCluster))
anova(chick_m1_mixed_PB)
# Mixed Model Anova Table (Type 3 tests)

# Model: weight ~ c_Time * Diet + (1 + c_Time | Chick)
# Data: ChickWeight2
              # Chisq Chi Df Pr(>Chisq)  Pr(>PB)   
# c_Time      104.128      1 0.00000000 0.002004 **
# Diet         18.692      3 0.00031662 0.002004 **
# c_Time:Diet  15.850      3 0.00121732 0.004008 **


stopCluster(MyCluster)


# posthocs to answer the following questions
# Which diets differ from each other with respect to the chicks weight in the middle of the experiment?
# In which diets is there significant evidence of change in weight over time?
# Which diets differ from each other significantly in the change in weight over time?

# for the first question, I use this command
lsmeans(chick_m1, pairwise ~ f_Diet)
# NOTE: Results may be misleading due to involvement in interactions
# $lsmeans
 # f_Diet   lsmean       SE    df lower.CL upper.CL
 # diet_1 100.9381 5.761563 47.69  89.3517 112.5244
 # diet_2 120.9063 7.989505 45.98 104.8240 136.9885
 # diet_3 140.6806 7.989505 45.98 124.5984 156.7628
 # diet_4 134.0744 7.991997 46.04 117.9877 150.1611

# Confidence level used: 0.95 

# $contrasts
 # contrast          estimate        SE    df t.ratio p.value
 # diet_1 - diet_2 -19.968201  9.850269 46.55  -2.027  0.1929
 # diet_1 - diet_3 -39.742520  9.850269 46.55  -4.035  0.0011
 # diet_1 - diet_4 -33.136361  9.852291 46.59  -3.363  0.0081
 # diet_2 - diet_3 -19.774319 11.298866 45.98  -1.750  0.3103
 # diet_2 - diet_4 -13.168160 11.300629 46.01  -1.165  0.6515
 # diet_3 - diet_4   6.606159 11.300629 46.01   0.585  0.9362

# P value adjustment: tukey method for comparing a family of 4 estimates


# to answer the second question, I use this command:
lstrends(chick_m1, ~ f_Diet, var = "c_Time")
 # f_Diet c_Time.trend        SE    df lower.CL  upper.CL
 # diet_1     6.276995 0.7616225 48.24 4.745847  7.808143
 # diet_2     8.609136 1.0591356 46.54 6.477871 10.740402
 # diet_3    11.422871 1.0591356 46.54 9.291605 13.554137
 # diet_4     9.531971 1.0600298 46.69 7.399095 11.664847

# Confidence level used: 0.95 



# to answer the third question, I use this command

pairs(lstrends(chick_m1, ~ f_Diet, var = "c_Time"))
 # contrast          estimate       SE    df t.ratio p.value
 # diet_1 - diet_2 -2.3321415 1.304545 47.11  -1.788  0.2919
 # diet_1 - diet_3 -5.1458762 1.304545 47.11  -3.945  0.0015
 # diet_1 - diet_4 -3.2549760 1.305271 47.21  -2.494  0.0739
 # diet_2 - diet_3 -2.8137347 1.497844 46.54  -1.879  0.2512
 # diet_2 - diet_4 -0.9228345 1.498476 46.61  -0.616  0.9265
 # diet_3 - diet_4  1.8909002 1.498476 46.61   1.262  0.5914

# P value adjustment: tukey method for comparing a family of 4 estimates 


# (j) Do follow-up models to compare Diets 1 and 3 (in principle we could do it for more comparisons, but I didn't want to give you even more homework)

# first create a new data frames that contains only the data from Diet 1 and 3
ChickWeight2_Diets1_3 <- droplevels(subset(ChickWeight2, Diet == 1 | Diet == 3))

# let's make sure that c_Time is also centered in the new data frame
ChickWeight2_Diets1_3$c_Time <- scale(ChickWeight2_Diets1_3$Time, center = TRUE, scale = FALSE)

# OK, so here's my follow-up model
chick_m1_Diets1_3 <- lmer(weight ~ c_Time * Diet + (1 + c_Time | Chick), data = ChickWeight2_Diets1_3)
summary(chick_m1_Diets1_3)
# Random effects:
 # Groups   Name        Variance Std.Dev. Corr
 # Chick    (Intercept) 662.43   25.738       
          # c_Time       11.28    3.359   1.00
 # Residual             182.61   13.513       
# Number of obs: 340, groups:  Chick, 30

# Fixed effects:
             # Estimate Std. Error t value
# (Intercept)  120.0812     5.0751  23.661
# c_Time         8.8506     0.6639  13.332
# Diet1        -19.6547     5.0751  -3.873
# c_Time:Diet1  -2.5722     0.6639  -3.875


# In principle, it would be good to do some model diagnostics of this model also, but I didn't want to give you even more homework.

# But here's what you could do (the same things we did for the full model with all diets)
# diagnostic plots
densityplot(resid(chick_m1_Diets1_3, scaled = TRUE)) # looks pretty ok
qqPlot(resid(chick_m1_Diets1_3, scaled = TRUE)) # looks pretty ok (it looks like there might be some outliers, but I'll check when I compute the proportions below)
plot(chick_m1_Diets1_3, type = c('p', 'smooth')) # fitted vs. residual --> still looks like non-OK (look s pretty much the same as what we saw for the full data set)


# compute the proportions of residuals for +/- 2, 2.5, 3
sum(abs(resid(chick_m1_Diets1_3, scaled = TRUE)) > 2) / length(resid(chick_m1_Diets1_3)) # 0.04117647
sum(abs(resid(chick_m1_Diets1_3, scaled = TRUE)) > 2.5) / length(resid(chick_m1_Diets1_3)) # 0.008823529
sum(abs(resid(chick_m1_Diets1_3, scaled = TRUE)) > 3) / length(resid(chick_m1_Diets1_3)) # 0




MyCluster <- makeCluster(rep("localhost", detectCores() - 1))

chick_m1_Diets1_3_mixed_PB <- mixed(weight ~ c_Time * Diet + (1 + c_Time | Chick), data = ChickWeight2_Diets1_3, type = 3, method = 'PB', cl = MyCluster, progress = TRUE, args.test = list(nsim = 500, cl = MyCluster))
anova(chick_m1_Diets1_3_mixed_PB)

# Mixed Model Anova Table (Type 3 tests)

# Model: weight ~ c_Time * Diet + (1 + c_Time | Chick)
# Data: ChickWeight2_Diets1_3
             # Chisq Chi Df Pr(>Chisq)  Pr(>PB)   
# c_Time      59.542      1 0.00000000 0.002004 **
# Diet        12.882      1 0.00033183 0.006012 **
# c_Time:Diet 12.902      1 0.00032821 0.006012 **




# (k) more follow-ups, now separate models for Diet 1 and Diet 3

# only Diet 1
ChickWeight2_Diets1 <- droplevels(subset(ChickWeight2, Diet == 1))

ChickWeight2_Diets1$c_Time <- scale(ChickWeight2_Diets1$Time, center = TRUE, scale = FALSE)

chick_m1_Diets1 <- lmer(weight ~ c_Time + (1 + c_Time | Chick), data = ChickWeight2_Diets1)
summary(chick_m1_Diets1)
# Random effects:
 # Groups   Name        Variance Std.Dev. Corr
 # Chick    (Intercept) 637.02   25.239       
          # c_Time       10.97    3.312   0.98
 # Residual             117.98   10.862       
# Number of obs: 220, groups:  Chick, 20

# Fixed effects:
            # Estimate Std. Error t value
# (Intercept)  99.3031     5.7989   17.12
# c_Time        6.2613     0.7598    8.24

chick_m1_Diets1_mixed_PB <- mixed(weight ~ c_Time + (1 + c_Time | Chick), data = ChickWeight2_Diets1, type = 3, method = 'PB', cl = MyCluster, progress = TRUE, args.test = list(nsim = 500, cl = MyCluster))
anova(chick_m1_Diets1_mixed_PB)
# Mixed Model Anova Table (Type 3 tests)

# Model: weight ~ c_Time + (1 + c_Time | Chick)
# Data: ChickWeight2_Diets1
       # Chisq Chi Df Pr(>Chisq)  Pr(>PB)   
# c_Time 30.31      1 3.6818e-08 0.002004 **


# only Diet 3
ChickWeight2_Diets3 <- droplevels(subset(ChickWeight2, Diet == 3))

ChickWeight2_Diets3$c_Time <- scale(ChickWeight2_Diets3$Time, center = TRUE, scale = FALSE)

chick_m1_Diets3 <- lmer(weight ~ c_Time + (1 + c_Time | Chick), data = ChickWeight2_Diets3)
summary(chick_m1_Diets3)
# Random effects:
 # Groups   Name        Variance Std.Dev. Corr
 # Chick    (Intercept) 727.27   26.968       
          # c_Time       12.14    3.485   1.00
 # Residual             285.10   16.885       
# Number of obs: 120, groups:  Chick, 10

# Fixed effects:
            # Estimate Std. Error t value
# (Intercept)  142.950      8.666   16.50
# c_Time        11.423      1.125   10.15


chick_m1_Diets3_mixed_PB <- mixed(weight ~ c_Time + (1 + c_Time | Chick), data = ChickWeight2_Diets3, type = 3, method = 'PB', cl = MyCluster, progress = TRUE, args.test = list(nsim = 500, cl = MyCluster))
anova(chick_m1_Diets3_mixed_PB)

# Mixed Model Anova Table (Type 3 tests)

# Model: weight ~ c_Time + (1 + c_Time | Chick)
# Data: ChickWeight2_Diets3
        # Chisq Chi Df Pr(>Chisq)  Pr(>PB)   
# c_Time 25.201      1 5.1657e-07 0.002004 **




# increase number of sims to see whether p value differs now (as it was a bit odd that we so often got the same p value of 0.002004 despite different Chisquare values)

chick_m1_Diets3_mixed_PB_moresims <- mixed(weight ~ c_Time + (1 + c_Time | Chick), data = ChickWeight2_Diets3, type = 3, method = 'PB', cl = MyCluster, progress = TRUE, args.test = list(nsim = 5000, cl = MyCluster))
anova(chick_m1_Diets3_mixed_PB_moresims)
# Model: weight ~ c_Time + (1 + c_Time | Chick)
# Data: ChickWeight2_Diets3
        # Chisq Chi Df Pr(>Chisq) Pr(>PB)    
# c_Time 25.201      1 5.1657e-07   2e-04 ***
# --> yes, now the p value is smaller










############################################################################################################################
# Additional things that were not part of the homework, but about which we talked in the class on March 14, 2016


#............................................................................................................................
# More visual approach (compared to cross tables) to check what fixed-effects predictors can be modeled additionally as random slopes

install.packages("plotrix")
library(plotrix)
sizetree(ChickWeight2[, c('f_Chick', 'f_Diet')])
sizetree(ChickWeight2[, c('f_Chick', 'Time')])


# in the plots above, the fonts are a bit small and in particular for the class slides it's difficult to see much. So I create the same plots, but using only the first 30 rows of data because that makes it easier to see the details (again, particularly for the class slides)
sizetree(ChickWeight2[1:30, c('f_Chick', 'f_Diet')])
sizetree(ChickWeight2[1:30, c('f_Chick', 'Time')])




#............................................................................................................................
# OK, further above, we looked at the funny-looking plot showing the fitted vs. residuals that showed a strong curvilinear pattern in the smoothed line and also evidence of heteroscedasticity

# It turns out, that there is quite some curvilinear relationship in the data

# If we had done some plots to understand the data *before* running the model, we probably would have seen this in at least some of the plots
# look at the distribution of the DV: overall, as a function of Diet, as a function of Time (the differences in the distributions as a function of Time are quite striking!!)
with(ChickWeight2, densityplot(weight))
with(ChickWeight2, densityplot(~weight | f_Diet))
with(ChickWeight2, densityplot(~weight | Time))

# we can also look at xyplots to understand the relationship between weight and Time better; first overall, then per Diet, then also per Chick
xyplot(weight ~ Time, data = ChickWeight2)
xyplot(weight ~ Time | f_Diet, data = ChickWeight2)

xyplot(weight ~ Time | f_Chick, data = ChickWeight2) # there is evidence that at least some chicks show non-linear effects of Time on weight
xyplot(weight ~ Time | f_Chick, data = ChickWeight2, type = c('p', 'r')) # here I added regression lines, this makes it a bit easier to see deviations from linearity

# we can also use a smoothed line to show the relationship; scatterplot also shows a straight regression line in green, so it's easy to see the differences between the straight green line and the smoothed red line
scatterplot(weight ~ Time, smooth = TRUE, boxplots = FALSE, data = ChickWeight2)

# OK, so all these plots suggest that we might miss something relevant if we model Time only as a linear effect.
# One simple way to capture more curvilinear aspects, is to model a continuous predictor such as Time with not only a linear predictor, but also a quadratic one




# How to use poly()

head(poly(ChickWeight2$Time, 2))

# In principle, there are at least two useful ways how to use poly():
# (a) create separate predictors for the linear and quadratic term (this is what I usually do for my analysis, to get separate p values for the linear and quadratic effects when using a model-comparison approach like KR F tests, PB, or LRTs)
# (b) use the poly() command in the lmer syntax; I only use that to create an effects plot that shows jointly the linear and quadratic effect (with (a) you cannot do that, you can ask only two plots, one for the linear, one for the quadratic effect; the one for the quadratic effect is difficult to interpret, in my opinion). For now, we'll use approach (a); later in the script, we'll come back to (b) where I show you how you can use that for an effects plot

ChickWeight2$poly_Time_lin <- poly(ChickWeight2$Time, 2)[, 1]
ChickWeight2$poly_Time_quad <- poly(ChickWeight2$Time, 2)[, 2]

# let's create a plot to visualize linear relationship
plot(ChickWeight2$poly_Time_lin[1:12], xlab = "Time (Days 0 to 21)", xaxt = 'n') # actually, the plot has a little 'bug:' the last data point (representing Day 21 should be a bit further to the left, so that it lies on the straight line; since this plot is a bit of a hack anyway, I didn't manage to make it nice and pretty within reasonable time...)


# same plot to visualize the quadratic relationship
plot(ChickWeight2$poly_Time_quad[1:12], xlab = "Time (Days 0 to 21)", xaxt = 'n') # the same about the little 'bug' applies also here, but since it's not a straight line, but a curve, it's a bit more difficult to see that the last data point a bit too far to the right...


# Are the linear and quadratic predictors centered? What's their SD (is it 1?)?
mean(ChickWeight2$poly_Time_lin) # mean is 0!
sd(ChickWeight2$poly_Time_lin) # SD is rather small (0.04)

mean(ChickWeight2$poly_Time_quad) # mean is 0!
sd(ChickWeight2$poly_Time_quad) # SD is rather small (0.04)


# what happens if we make these numbers a bit bigger?
ChickWeight2$poly_Time_lin_10 <- ChickWeight2$poly_Time_lin * 10
ChickWeight2$poly_Time_quad_10 <- ChickWeight2$poly_Time_quad * 10

mean(ChickWeight2$poly_Time_lin_10) # mean is still 0!
sd(ChickWeight2$poly_Time_lin_10) # SD is 10 times larger (0.4)

mean(ChickWeight2$poly_Time_quad_10) # mean is still 0!
sd(ChickWeight2$poly_Time_quad) # SD is 10 times larger (0.4)


# or you could use the scale command, I guess
ChickWeight2$s_poly_Time_lin <- scale(ChickWeight2$poly_Time_lin, center = FALSE, scale = TRUE))
ChickWeight2$s_poly_Time_quad <- scale(ChickWeight2$poly_Time_quad, center = FALSE, scale = TRUE))



# so how would our model with linear and quadratic predictors look like?

chick_m2 <- lmer(weight ~ f_Diet * (poly_Time_lin + poly_Time_quad) + (1 + poly_Time_lin + poly_Time_quad | f_Chick), data = ChickWeight2)

summary(chick_m2)


# diagnostic plots
densityplot(resid(chick_m2, scaled = TRUE)) # looks pretty ok
qqPlot(resid(chick_m2, scaled = TRUE)) # looks pretty ok (it looks like there might be some outliers, but I'll check when I compute the proportions below)
plot(chick_m2, type = c('p', 'smooth')) # fitted vs. residual --> looks better than the one from the linear-only model

# let's use the same range on the y axis for both plots
plot(chick_m1, type = c('p', 'smooth'), ylim = c(-50, 50)) 

plot(chick_m2, type = c('p', 'smooth'), ylim = c(-50, 50)) # fitted vs. residual --> looks better than the one from the linear-only model



# compute the proportions of residuals for +/- 2, 2.5, 3
sum(abs(resid(chick_m2, scaled = TRUE)) > 2) / length(resid(chick_m2)) # 0.04844291
sum(abs(resid(chick_m2, scaled = TRUE)) > 2.5) / length(resid(chick_m2)) # 0.01903114
sum(abs(resid(chick_m2, scaled = TRUE)) > 3) / length(resid(chick_m2)) # 0.01038062 # that number is a bit worse than what we had in the linear only model (but not much, I'd say)





MyCluster <- makeCluster(rep("localhost", detectCores() - 1))
chick_m2_mixed_PB <- mixed(weight ~ f_Diet * (poly_Time_lin + poly_Time_quad) + (1 + poly_Time_lin + poly_Time_quad | f_Chick), data = ChickWeight2, type = 3, method = 'PB', cl = MyCluster, progress = TRUE, args.test = list(nsim = 500, cl = MyCluster))
# that took a looong time!! (I didn't check my watch, but it felt like 30 min or so)

anova(chick_m2_mixed_PB)
# Mixed Model Anova Table (Type 3 tests)

# Model: weight ~ f_Diet * (poly_Time_lin + poly_Time_quad) + (1 + poly_Time_lin + 
# Model:     poly_Time_quad | f_Chick)
# Data: ChickWeight2
                         # Chisq Chi Df Pr(>Chisq)  Pr(>PB)   
# f_Diet                 18.1587      3  0.0004079 0.002004 **
# poly_Time_lin         100.3354      1  0.0000000 0.002004 **
# poly_Time_quad         15.3378      1  0.0000899 0.004008 **
# f_Diet:poly_Time_lin   15.0922      3  0.0017395 0.002004 **
# f_Diet:poly_Time_quad   9.3811      3  0.0246310 0.026052 * 


stopCluster(MyCluster)





# OK, I promised to show you how to use poly() in the lmer command so that you can later show the joint linear and quadratic effect with plot(effect())

# I run again the lmer model, but now I use the command poly() in the formula:

chick_m2b <- lmer(weight ~ f_Diet * poly(Time, 2) + (1 + poly(Time, 2) | f_Chick), data = ChickWeight2)
summary(chick_m2b)
# Random effects:
 # Groups   Name           Variance  Std.Dev. Corr     
 # f_Chick  (Intercept)       660.31  25.696           
          # poly(Time, 2)1 308976.88 555.857  0.98     
          # poly(Time, 2)2  40082.63 200.206  0.48 0.66
 # Residual                    43.08   6.564           
# Number of obs: 578, groups:  f_Chick, 50

# Fixed effects:
                       # Estimate Std. Error t value
# (Intercept)             123.918      3.826   32.39
# f_Diet1                 -23.426      5.667   -4.13
# f_Diet2                  -3.057      6.916   -0.44
# f_Diet3                  16.661      6.916    2.41
# poly(Time, 2)1         1445.415     82.856   17.44
# poly(Time, 2)2          124.648     30.564    4.08
# f_Diet1:poly(Time, 2)1 -439.200    122.949   -3.57
# f_Diet2:poly(Time, 2)1  -51.275    149.723   -0.34
# f_Diet3:poly(Time, 2)1  401.069    149.723    2.68
# f_Diet1:poly(Time, 2)2  -76.401     45.522   -1.68
# f_Diet2:poly(Time, 2)2   -1.583     55.154   -0.03
# f_Diet3:poly(Time, 2)2  155.227     55.154    2.81


# for comparison again the model with the 2 predictors
summary(chick_m2) # looks the same to me!
# Random effects:
 # Groups   Name           Variance  Std.Dev. Corr     
 # f_Chick  (Intercept)       660.31  25.696           
          # poly_Time_lin  308976.88 555.857  0.98     
          # poly_Time_quad  40082.63 200.206  0.48 0.66
 # Residual                    43.08   6.564           
# Number of obs: 578, groups:  f_Chick, 50

# Fixed effects:
                       # Estimate Std. Error t value
# (Intercept)             123.918      3.826   32.39
# f_Diet1                 -23.426      5.667   -4.13
# f_Diet2                  -3.057      6.916   -0.44
# f_Diet3                  16.661      6.916    2.41
# poly_Time_lin          1445.415     82.856   17.44
# poly_Time_quad          124.648     30.564    4.08
# f_Diet1:poly_Time_lin  -439.200    122.949   -3.57
# f_Diet2:poly_Time_lin   -51.275    149.723   -0.34
# f_Diet3:poly_Time_lin   401.069    149.723    2.68
# f_Diet1:poly_Time_quad  -76.401     45.522   -1.68
# f_Diet2:poly_Time_quad   -1.583     55.154   -0.03
# f_Diet3:poly_Time_quad  155.227     55.154    2.81



# now let's create a plot that shows in one single line jointly the linear and quadratic effect

plot(effect("poly(Time, 2)", chick_m2b))

# If you would use the model with the separate linear and quadratic predictors:
plot(effect("poly_Time_lin", chick_m2)) # ok, so that's a straight line
plot(effect("poly_Time_quad", chick_m2)) # now this is also a straight line! but you have to remember that this is a quadratic predictor: so both very low and very high values of the predictor are on the right side of the x axis, but medium values are on the left on the x axis. at least for my head, this is too complicated to figure out *and* combine it in my head with the linear effect of the linear plot. That's why I like to use for plotting the poly() command in the lmer formula...







