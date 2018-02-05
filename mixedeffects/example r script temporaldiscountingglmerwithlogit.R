# Example Script "Generalized Linear Mixed Effects Models"
# for use with the intertemporal choice data frame
# created by Bernd Figner (April 7/8, 2013); updated March 22, 2014 and March 30, 2015


# this script reads the "pre-processed" intertemp choice data file ICT_stacked_reduced_forBB_22March2014a.csv and then does a little bit more prepping and runs a bunch of glmer models (and graphs etc)




library(lme4) # for lmer, i.e., mixed models
library(dataframes2xls) # to write xls file
library(psych) # for describeBy
library(car) # for recode() and Anova()
library(effects) # for plots
library(afex)
library(coefplot2)
library(parallel)
# some more libraries are loaded below


options(scipen=5) # this has the effect that the serials are shown in regular, not scientific notation

# set contrasts to sum-to-zero (aka deviation coding) for unordered factors and polynomial for ordered factors
options(contrasts=c("contr.sum", "contr.poly"))

setwd('~/GoogleDrive/Radboud/Teaching/MultilevelClass/2016/Week_8/RScripts_Data/')

itc2 <- read.csv('ICT_stacked_Demographics.csv')
head(itc2)

# get rid of first column ('X'); and save data frame under shorter name
itc3 <- droplevels(itc2[,-1])

head(itc3)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PREDICTORS ~ PREDICTORS ~ PREDICTORS ~ PREDICTORS ~ PREDICTORS ~ PREDICTORS ~ PREDICTORS ~ PREDICTORS ~ PREDICTORS ~ PREDICTORS ~ PREDICTORS

# first look at what different values are in some of the variables

unique(itc3$SS_Time) #0 and 14 --> ok, the 0 trials are 'now' trials, the 14 trials are the 'not-now' trials

unique(itc3$LL_Time) #3, 14, 17, 28, 42


# let's compute the time difference between SS and LL

itc3$TimeDiff <- itc3$LL_Time - itc3$SS_Time

unique(itc3$TimeDiff) # 3, 14, 28

# what combinations of SS Time and Time Difference were presented? 
table(itc3$SS_Time, itc3$TimeDiff) # this is a command to show a 'cross-tabulation' with SS Time as 2 different rows; Time Difference as 3 different columns; and the numbers show how often each combination is in the data frame (hmm, odd that the 0/28 combination is in there more often than the others; perhaps partly due to the 'missing' 31 trials? not sure; would require a bit more detective work to figure that out; which I'm not going to do right now...)

# well, it's good that mixed models can handle well unbalanced designs, isn't it?

# so, how should we code things?
# As ususal, there are different ways how to do this (ordered/unordered factors, continuous, centered etc)


# Based on SS Time, I create a variable now/not-now that is coded as -0.5/+0.5
itc3$t_Now_Notnow <- recode(itc3$SS_Time, "0 = -0.5; 14 = 0.5")

# well, I code it also as an ordered factor (it shouldn't matter for the model which we use)
itc3$o_Now_Notnow <- ordered(recode(itc3$SS_Time, "0 = 'now'; 14 = 'notnow'"), levels = c('now', 'notnow'))


# For time difference, I'm going to create 2 variables (since I'm not sure which might make more sense); a centered continous variable (that treats time as a linear effect; which might be a lot to expect from participants...), and an ordered factor (which, since it has 3 levels, it will check for linear and quadratic effects, which is quite plausible)

itc3$c_TimeDiff <- itc3$TimeDiff - mean(itc3$TimeDiff)

# i also create a scaled version (i think i'm going to use this one for the glmer model, actually)
itc3$s_TimeDiff <- scale(itc3$TimeDiff, center = TRUE, scale = TRUE)


# here, i first recode the numerical 3, 14, 28 entries into categories (3days etc); then i turn it into an ordered factor; this I'll use for plotting only, I think
itc3$o_TimeDiff <- ordered(recode(itc3$TimeDiff, "3 = '3days'; 14 = '14days'; 28 = '28days'"), levels = c('3days', '14days', '28days'))



# SS Amount is relevant in the model to capture the 'magnitude' effect in intertemporal choice
# we can use it as continuous predictor, but it should be centered
itc3$c_SS_Amount <- itc3$SS_Amount - mean(itc3$SS_Amount)

# again a scaled version
itc3$s_SS_Amount <- scale(itc3$SS_Amount, center = TRUE, scale = TRUE)



# now, another very important predictor is the difference in amounts between the SS and the LL
# this difference can be computed either as absolute difference in amounts or in the relative difference in amounts (i.e., how much larger is the LL compared to the SS?)


# absolute difference
itc3$abs_Diff_Amounts <- itc3$LL_Amount - itc3$SS_Amount

# let's check the distribution, there might be some "catch" trials in there (where the SS is larger than the LL!)
densityplot(itc3$abs_Diff_Amounts)
# yes, indeed, there's a bunch of trials, where the SS is approx. 10 Euros larger than the LL! this is to check whether participants paid attention
# we could either remove these catch trials for the glmer analyses, or we could just leave them in there (or do both and check whether it makes a difference)
# if i were to report these results in a paper, i would probably remove the catch trials and analyze them separately (to see who made the wrong choices on these catch trials and then perhaps remove these participants). for now, i leave them in there.

# just a quick check whether choosing the wrong option in these catch trials was very common:
with(itc3, table(abs_Diff_Amounts, choice_SS0_LL1)) # well, it wasn't very common it seems (only happened once)



# relative difference
itc3$rel_Diff_Amounts <- (itc3$LL_Amount - itc3$SS_Amount) / itc3$SS_Amount
densityplot(itc3$rel_Diff_Amounts)

# ok, so we see about 5 different 'bins' of relative differences:
# - -20% --> these are the catch trials
# - 5% --> everybody should be patient here
# - 10% --> there might be quite some individual (and frame) differences here
# - 20% --> same as 10%
# - 30% --> same, probably
# - 50% --> here, we probably would expect most individuals to choose the LL


# for the analyses, it's fine to use the exact relative differences in the model
# but: when we want to plot the choices as a function of the relative differences, it might be nice to 'bin' them in these, well, bins
# the recode command could again come in handy...

# to find the cutoffs for the different categories, i first look at the actual values that occur
sort(unique(itc3$rel_Diff_Amounts))


itc3$rounded_rel_Diff_Amounts <- ordered(recode(itc3$rel_Diff_Amounts, "-0.3:-0.1 = '1_negative'; 0.02:0.06 = '2_5%'; 0.07:0.12 = '3_10%'; 0.18:0.22 = '4_20%'; 0.28:0.32 = '5_30%'; 0.45:0.52 = '6_50%'"), levels = c('1_negative', '2_5%', '3_10%', '4_20%', '5_30%', '6_50%'))

# check whether our bin boundaries make sense, i.e., whether we created categories that contain equal number of trials
table(itc3$rounded_rel_Diff_Amounts, itc3$SS_Time)
# yep, looks good!

table(itc3$rounded_rel_Diff_Amounts, itc3$TimeDiff) # yep, looks still good! actually now this shows also that the design is balanced (except the catch trials)


# created centered and scaled version of reldiff amounts
itc3$c_rel_Diff_Amounts <- scale(itc3$rel_Diff_Amounts, center = TRUE, scale = FALSE)

itc3$s_rel_Diff_Amounts <- scale(itc3$rel_Diff_Amounts, center = TRUE, scale = TRUE)



# What else do we need for our models?

# We already have a pp_code, but we can make it explicitly into a factor (which it probably already is, but still...) 
itc3$f_pp_code_ITC <- as.factor(itc3$pp_code_ITC)

# recode d_smoke into a factor with nicer labels
itc3$f_smoking <- recode(itc3$d_smoke, "0 = 'non-smoker'; 1 = 'smoker'; else = NA")

# OK, I think we're ready to do some graphs and some models (there might be more variables that need to be brough in shape, but I'm sure you can do that on your own)


# let's save this final data frame
write.csv(itc3, file = 'ICT_stacked_Demographics_b.csv')



# some figures
head(itc3)

with(itc3, interaction.plot(rounded_rel_Diff_Amounts, o_TimeDiff, choice_SS0_LL1))


# hmm, I wonder whether the one negative rel diff might lead to problems later in the modeling

# thus, I create a new data frame from which these catch trials have been removed. which means that i have to re-center/re-scale all continuous predictors, ugh...

itc4 <- droplevels(itc3[itc3$abs_Diff_Amounts > 0,])

# let's check whether that worked
unique(itc4$abs_Diff_Amounts) # looks good

# check number of rows and columns before and after
nrow(itc3) #992
nrow(itc4) #960 --> thus 32 less, that looks about right to me

# now let's do the recentering/rescaling
itc4$c_rel_Diff_Amounts <- scale(itc4$rel_Diff_Amounts, center = TRUE, scale = FALSE)
itc4$s_rel_Diff_Amounts <- scale(itc4$rel_Diff_Amounts, center = TRUE, scale = TRUE)
itc4$c_TimeDiff <- itc4$TimeDiff - mean(itc4$TimeDiff)
itc4$s_TimeDiff <- scale(itc4$TimeDiff, center = TRUE, scale = TRUE)
itc4$c_SS_Amount <- itc4$SS_Amount - mean(itc4$SS_Amount)
itc4$s_SS_Amount <- scale(itc4$SS_Amount, center = TRUE, scale = TRUE)

head(itc4)

# ok, i think that's all of them now that have to be re-centered/scaled

# let's save that data file
write.csv(itc4, file = 'ICT_stacked_Demographics_c_NoCatchTrials.csv')


min(itc4$SS_Amount)
max(itc4$SS_Amount)

# let's do some plots



# proportion of LL choice as function of the rounded relative differences and the time difference between SS and LL (I use the rounded/categorical relative differences only for the plots, not the actual models)
with(itc4, interaction.plot(rounded_rel_Diff_Amounts, o_TimeDiff, choice_SS0_LL1))

# let's try some plots that show the individual differences

# let's try our usual xyplot first
xyplot(choice_SS0_LL1 ~ rounded_rel_Diff_Amounts | pp_code_ITC, groups = pp_code_ITC, data = itc4, type = c('p', 'r'), auto.key = FALSE)
# well, since the DV is always either 0 or 1, the data points are not very informative; at least the regression line shows a bit of the relationship between rel diff and choice (although this is linear in the probability space, which is not how it's modeled, but still, for some quick visual impression for ourselves it might be ok)

# same using the exact relative differences: that's actually better, because the circles are not exactly on top of each other
xyplot(choice_SS0_LL1 ~ rel_Diff_Amounts | pp_code_ITC, groups = pp_code_ITC, data = itc4, type = c('p', 'r'), auto.key = FALSE)

xyplot(choice_SS0_LL1 ~ rel_Diff_Amounts | pp_code_ITC, groups = pp_code_ITC, data = itc4, type = c('p', 'smooth'), auto.key = FALSE)


# we could also show it separately for the different time differences, perhaps: ok, a bit complex, but still that's informative
xyplot(choice_SS0_LL1 ~ rel_Diff_Amounts | pp_code_ITC, groups = TimeDiff, data = itc4, type = c('p', 'r'), auto.key = TRUE)

# separate for now and notnow
xyplot(choice_SS0_LL1 ~ rel_Diff_Amounts | pp_code_ITC, groups = o_Now_Notnow, data = itc4, type = c('p', 'r'), auto.key = TRUE)


# I also played around a bit with the function scatterplot, but couldn't produce results that looked that good; feel free to have a look and perhaps you can tweak it to make it more useful
scatterplot(itc4$choice_SS0_LL1 ~ itc4$rel_Diff_Amounts | itc4$o_Now_Notnow, smoother = loessLine, boxplots = FALSE)
scatterplot(itc4$choice_SS0_LL1 ~ itc4$rel_Diff_Amounts | itc4$o_Now_Notnow, smoother = loessLine,  jitter = list(x = 0, y = 0.5), boxplots = FALSE, ylim = c(-0.1, 1.1))

# same, but remove regression lines
scatterplot(itc4$choice_SS0_LL1 ~ itc4$rel_Diff_Amounts | itc4$o_Now_Notnow, smoother = loessLine,  jitter = list(x = 0, y = 0.5), boxplots = FALSE, ylim = c(-0.1, 1.1), reg.line = FALSE)


head(itc4)


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...
# glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...

# some explanations: I first tried a bunch (well, a lot) of models, but they never converged. I tried to do these models with the data frame in which the catch trials were still in there (itc3). The next day, when I started working on it again, I realized that the problem might have been the catch trials. So I then created a data frame without the catch trials (itc4) and tried the models again.


# .........................................................
# models using data frame *without* catch trials


# so, how should our model look like?
# well, we're interested in investigating the effects of o_Now_Notnow, s_TimeDiff, s_SS_Amount, and s_rel_Diff_Amounts
# as created this task, I know that all of these factors are varied within-subject, so I know that they need to be modeled as random slopes as well. but what if you had to figure it out from the data?

# we check with table for each of the fixed effects, how many observations we have per participant

# first let's check o_Now_Notnow
with(itc4, table(f_pp_code_ITC, o_Now_Notnow))
# I'm plotting not all of it, just the first few rows
             # o_Now_Notnow
# f_pp_code_ITC now notnow
        # pp_01  15     15
        # pp_02  15     15
        # pp_03  15     15
        # pp_04  15     15


# for s_TimeDiff (for the table, we can use either the standardized or non-standardized variable; I'm using here the non-standardized simply because it has nice integer values, so the table looks a bit prettier)
with(itc4, table(f_pp_code_ITC, TimeDiff))
             # TimeDiff
# f_pp_code_ITC  3 14 28
        # pp_01 10 10 10
        # pp_02 10 10 10
        # pp_03 10 10 10
        # pp_04 10 10 10


# for s_SS_Amount
with(itc4, table(f_pp_code_ITC, SS_Amount))
             # SS_Amount
# f_pp_code_ITC 16 19 20 22 25 28 29 32 33 34 35 41 42 43 44 45 46 52 54 56 62 70
        # pp_01  2  1  1  1  2  1  2  1  2  1  1  1  2  1  1  2  1  1  1  3  1  1
        # pp_02  2  1  1  1  2  1  2  1  2  1  1  1  2  1  1  2  1  1  1  3  1  1
        # pp_03  2  1  1  1  2  1  2  1  2  1  1  1  2  1  1  2  1  1  1  3  1  1
        # pp_04  2  1  1  1  2  1  2  1  2  1  1  1  2  1  1  2  1  1  1  3  1  1
        # pp_05  2  1  1  1  2  1  2  1  2  1  1  1  2  1  1  2  1  1  1  3  1  1

# for s_rel_Diff_Amounts
with(itc4, table(rel_Diff_Amounts, f_pp_code_ITC))
# this is very difficult to paste in here, so I'm not doing it





# a simple model investigating the effects of frame, now/notnow, time difference, ss amount (magnitude effect!?), and relative amount difference on choice; Note: actually, it is NOT necessary to specify the link function
m2_1 <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4)
summary(m2_1)
# Warning messages:
# 1: In (function (fn, par, lower = rep.int(-Inf, n), upper = rep.int(Inf,  :
  # failure to converge in 10000 evaluations
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.605248 (tol = 0.001, component 1)


# same, but with more iterations
m2_1b <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9)))
summary(m2_1b)
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.60525 (tol = 0.001, component 1)
  
  # hmm, one warning less...


m2_1bobyqa <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9), optimizer = "bobyqa"))
summary(m2_1bobyqa)
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # unable to evaluate scaled gradient
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge: degenerate  Hessian with 1 negative eigenvalues


# I can also try to save the estimates from a model with convergence warning and then run the model again with these estimates as starting values I use m2_1c for that

estims_m2_1b <- getME(m2_1b, c("theta", "fixef"))

m2_1b_2 <- update(m2_1b, start = estims_m2_1b, control = glmerControl(optCtrl = list(maxfun = 1e+9)))
summary(m2_1b_2)
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # unable to evaluate scaled gradient
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
  
# OK, none of this helped so far...

# I try also allFit

m2_1b_allFit <- allFit(m2_1b, maxfun = 1e+9)
# ok, here's the printout I get while R is trying one optimizer after the other:
# bobyqa. : [OK]
# Nelder_Mead. : [OK]
# optimx.nlminb : [OK]
# optimx.L-BFGS-B : [OK]
# nloptwrap.NLOPT_LN_NELDERMEAD : Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # unable to evaluate scaled gradient
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
# 3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.57753 (tol = 0.001, component 1)
# [OK]
# nloptwrap.NLOPT_LN_BOBYQA : Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # unable to evaluate scaled gradient
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
# [OK]
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # unable to evaluate scaled gradient
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge: degenerate  Hessian with 1 negative eigenvalues


# I'm curious to see why it seems like bobyqa worked here...
# so I'm trying again...
m2_1bobyqa_2 <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9), optimizer = "bobyqa"))
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # unable to evaluate scaled gradient
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
# nope, that didn't work now


# hmm, I'm never sure whether the max is 1e+9 or 10e+9, so I try the second one here:
m2_1bobyqa_2b <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 10e+9), optimizer = "bobyqa"))
# ok, that ended very quickly with these warnings:
# Warning messages:
# 1: In (function (par, fn, lower = -Inf, upper = Inf, control = list(),  :
  # NAs introduced by coercion to integer range
# 2: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
  # convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded
# 3: In (function (par, fn, lower = -Inf, upper = Inf, control = list(),  :
  # NAs introduced by coercion to integer range
# 4: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
  # convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # unable to evaluate scaled gradient
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge: degenerate  Hessian with 3 negative eigenvalues
  
 # from that I assume that the max is 1e+9! 


# let's try Nelder_Mead
m2_1_NM <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9), optimizer = "Nelder_Mead"))
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.57753 (tol = 0.001, component 1)

# let's compare the estimates of the two models with warnings
summary(m2_1bobyqa_2)
summary(m2_1_NM)


# now I compare the random effect estimates and the fixed effect estimates of the two models
# first the random effects (I put them next to each other so it's easier to read)
cbind(lme4::getME(m2_1bobyqa_2, "theta"), getME(m2_1_NM, "theta"))
                                                       # [,1]        [,2]
# f_pp_code_ITC.(Intercept)                        3.12352640  3.19483000
# f_pp_code_ITC.o_Now_Notnow.L.(Intercept)         0.21779239  0.21247239
# f_pp_code_ITC.s_TimeDiff.(Intercept)             0.33176759  0.33607360
# f_pp_code_ITC.s_SS_Amount.(Intercept)            0.05422672  0.07392245
# f_pp_code_ITC.s_rel_Diff_Amounts.(Intercept)     0.83751674  0.89986524
# f_pp_code_ITC.o_Now_Notnow.L                     0.00000000  0.06696749
# f_pp_code_ITC.s_TimeDiff.o_Now_Notnow.L          0.64795959 -0.71651863
# f_pp_code_ITC.s_SS_Amount.o_Now_Notnow.L        -0.08472664  0.15302148
# f_pp_code_ITC.s_rel_Diff_Amounts.o_Now_Notnow.L  0.14654270  0.14578490
# f_pp_code_ITC.s_TimeDiff                         0.23261396  0.12623990
# f_pp_code_ITC.s_SS_Amount.s_TimeDiff            -0.17534359 -0.02887136
# f_pp_code_ITC.s_rel_Diff_Amounts.s_TimeDiff     -0.55627363 -0.05241007
# f_pp_code_ITC.s_SS_Amount                        0.17406689  0.23842089
# f_pp_code_ITC.s_rel_Diff_Amounts.s_SS_Amount     0.73130511  0.96017385
# f_pp_code_ITC.s_rel_Diff_Amounts                 0.00000000  0.36742600

# some of them look very different from each other (like the correlation between the TimeDiff and the NowNotnow slopes, which is estimated as 0.647... by bobyqa and as -.7165... by Nelder_Mead)

# ALSO: note that these numbers are different than the numbers we get in the summary output! the reason is that they are on a different scale than what is shown in the summary output. I guess for the comparison between 2 models, it doesn't matter, but it might be less confusing to have the same numbers as in the summary output.
# For more information, see here:
# http://stats.stackexchange.com/questions/154293/r-lmer-confint-theta-values-not-the-same-as-summary-values
# also: look at ?getME and search for "theta" --> they are parametrized as the "relative Cholesky factors of each random effect"

# We can get the numbers in the summary output with the function VarCorr
VarCorr(m2_1bobyqa_2) # ok, that looks good; but what if we want to show them next to each other as I did above using getME?

# let's try this:
as.data.frame(VarCorr(m2_1bobyqa_2)) # ok, that might work

cbind(as.data.frame(VarCorr(m2_1bobyqa_2)), as.data.frame(VarCorr(m2_1_NM))) # Hmm, now I get all the info twice (i.e., the variable names), making it somewhat unwiedly, so I try the following:

cbind(as.data.frame(VarCorr(m2_1bobyqa_2)), as.data.frame(VarCorr(m2_1_NM))[,4:5]) # ok, that worked, and the numbers look more similar than on the whatever-getME-uses scale, but some are still not that super-similar (like still the correlation between TimeDiff and NowNotnow, and the second-to-last row for example)


# and now the fixefs
cbind(getME(m2_1bobyqa_2, "fixef"), getME(m2_1_NM, "fixef"))
                         # [,1]       [,2]
# (Intercept)         1.3915419  1.4597594
# o_Now_Notnow.L      0.1497879  0.1698873
# s_TimeDiff         -1.3589967 -1.3836434
# s_SS_Amount         0.8762724  0.9009059
# s_rel_Diff_Amounts  2.6053900  2.6889673
# These look pretty similar to each other (which is re-assuring and ultimately we care about the fixed effects here, not about the random effects), but the differences in the random effects make me still feel uncomfortable about using any of these two models for inference statistics



# OK, so I have to try something else

# as one can see in the summary output of (nearly) all the models above, the correlation between the random intercept and the random slope for not/now is always perfectly correlated, therefore--and because NotNotNow is nowhere near significant (thus, I will never make the mistake of claiming it is significant based on this model), I remove the random slope for now/notnow; I leave the number of iterations at this silly high number (I could reduce it, I guess...)
m2_1d <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9)))
summary(m2_1d)
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.0508957 (tol = 0.001, component 1)

# ok, still not converging; so i take out the random correlation terms

m2_1e <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts +
(1 |f_pp_code_ITC) + 
(0 + s_TimeDiff |f_pp_code_ITC) +
(0 + s_SS_Amount |f_pp_code_ITC) + 
(0 + s_rel_Diff_Amounts|f_pp_code_ITC),
family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9)))
summary(m2_1e)
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.00899002 (tol = 0.001, component 1)
 # --> still not converging, darn (but this max|grad| term is getting closer to the tolerance of 0.001...)


# btw, this above is how I wrote the model two years ago. with the more recent lme4 version, it can be simply written like this:
m2_1e_ <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts||f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9)))
summary(m2_1e_)
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.00899002 (tol = 0.001, component 1)


# OK, I try different optimizers now
m2_1e_bobyqa <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts +
(1 |f_pp_code_ITC) + 
(0 + s_TimeDiff |f_pp_code_ITC) +
(0 + s_SS_Amount |f_pp_code_ITC) + 
(0 + s_rel_Diff_Amounts|f_pp_code_ITC),
family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9), optimizer = "bobyqa"))
summary(m2_1e_bobyqa)

# that worked!!! ok, I'll try the more complicated starting model then with bobyqa
# also, note that NowNotnow is still nowhere near significant; so there's no danger that we could claim that NowNotNow is significant (which we should not and must not based on a model that doesn't model it as a random slope)


m2_1_bobyqa <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9), optimizer = "bobyqa"))
summary(m2_1_bobyqa)
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # unable to evaluate scaled gradient
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
    
# ok, that didn't work, but I'll go through the same simplification steps as before.
# Also: Note that the summary output always suggests the same results in all non-converged (and the one converged) model; so the whole procedure that I'm going through here is more for my conscience and probably won't change the results

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# OK, but perhaps it's sufficient to the remove random slope for o_Now_Notnow, but put the random correlations back in?
m2_1d_bobyqa <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9), optimizer = "bobyqa"))
summary(m2_1d_bobyqa)
# great, that worked! hooray, so it seems to me that is the closest we can get to a maximal model. note that the only thing missing from being it a maximal model is the random slope for NowNotnow; since the fixed effect of NowNotnow is non-significant anyway, this model here should be fine for our confirmatory hypothesis test framework then


# ??????????????????????????????????????
# diagnostic plots?
# this is surprisingly(?) difficult for GLMMs...
# for those interested, see some links here (more on the slides...)
http://stats.stackexchange.com/questions/70783/how-to-assess-the-fit-of-a-binomial-glmm-fitted-with-lme4-1-0
http://www.r-bloggers.com/model-validation-interpreting-residual-plots/


plot(m2_1d_bobyqa) # for such logistic models, this is not helpful... (this is also discussed in some of the links/posts that I gave you in the slides of week 8) --> but go a bit further below for the section "more useful plots" (approx on line 500)

plot(m2_1d_bobyqa, type = c('p', 'smooth')) # this is not super helpful either


densityplot(resid(m2_1d_bobyqa, scaled = TRUE)) # ok, looks symmetric, but quite leptokurtic, probably boostrapping for p values is a good idea. BUT at least so far I haven't found clear advice whether looking at the residuals like this for a glmer doesn't make even sense or not... (I think it does)

# we can also do this (but again, not 100% whether this is relevant, though I think it is)
qqPlot(resid(m2_1d_bobyqa, scaled = TRUE)) # hmm, there is some deviation


# hmm, is that even relevant/useful (again, I haven't found any advice on this so far): we can of course compute the proportions of residuals for +/- 2, 2.5, 3
sum(abs(resid(m2_1d_bobyqa, scaled = TRUE)) > 2) / length(resid(m2_1d_bobyqa)) # 0.0125
sum(abs(resid(m2_1d_bobyqa, scaled = TRUE)) > 2.5) / length(resid(m2_1d_bobyqa)) # 0.001041667
sum(abs(resid(m2_1d_bobyqa, scaled = TRUE)) > 3) / length(resid(m2_1d_bobyqa)) # 0
sum(abs(resid(m2_1d_bobyqa, scaled = TRUE)) > 3.5) / length(resid(m2_1d_bobyqa)) # 0

# so these numbers look alright, *if it were an lmer model* -- whether this is also true for glmer, I'm not super sure (I think it is useful)



# .................. "more useful plots" ..................
# here are some plots that I think are useful; this is based on info from, for example, here:
# http://rpubs.com/bbolker/glmmchapter
# http://www.ashander.info/posts/2015/04/D-RUG-mixed-effects-viz/
# http://www.r-bloggers.com/model-validation-interpreting-residual-plots/


# so this is our usual model plot, which is not very helpful per se (the odd pattern seen here is the usual pattern one gets with a binary DV, as the DV has only 0's and 1's)
plot(m2_1d_bobyqa)

# here we can add a bit more information at least: Note the use here of the arguments id =  and idLabels = )
plot(m2_1d_bobyqa, id = 0.05, idLabels = ~f_pp_code_ITC)
# this identifies outliers (based on p < .05) and labels them with the participant code

# to get some more info about this type of plot, have a look here:
?plot.merMod

# I also found the advice that binning the residuals is a good idea (this is also true for lmers, of course)
# we can do things like this, showing the distribution of the residuals for different values of our predictors (to see whether the model is somewhere performing particularly badly)
plot(m2_1d_bobyqa, o_Now_Notnow ~ resid(., type = "pearson") )
plot(m2_1d_bobyqa, s_SS_Amount ~ resid(., type = "pearson") )
plot(m2_1d_bobyqa, s_TimeDiff ~ resid(., type = "pearson") )
plot(m2_1d_bobyqa, s_rel_Diff_Amounts ~ resid(., type = "pearson") )


# In one of the posts that I show in the class slide, they discuss binnedplots from the library arm, so let's try that
#install.packages("arm")
library(arm)
binnedplot(fitted(m2_1d_bobyqa), resid(m2_1d_bobyqa)) # yep, should be in that order, first the fitted, then the residuals
?binnedplot # 95% of the points should be within the lines; this doesn't really seem to be the case

# In the help files, it mentions the nclass argument, where one can determine the number of "bins"
# what if I use 100?
binnedplot(fitted(m2_1d_bobyqa), resid(m2_1d_bobyqa), nclass = 100) # that seems to be a lot of bins...
binnedplot(fitted(m2_1d_bobyqa), resid(m2_1d_bobyqa), nclass = 50) # that seems to be a lot of bins...
binnedplot(fitted(m2_1d_bobyqa), resid(m2_1d_bobyqa), nclass = 20) # hmm, that's better, or too few?
binnedplot(fitted(m2_1d_bobyqa), resid(m2_1d_bobyqa), nclass = 10) # that seems to too few...
# so, I guess without any more concrete guidance, this is not soo superhelpful...

# One piece of advice is also to see whether our model is doing ok in predicting the DV, so how can we look at that?

plot(itc4$choice_SS0_LL1, fitted(m2_1d_bobyqa)) # since the original DV is either 0 or 1, this is also not so helpful...


boxplot(fitted(m2_1d_bobyqa) ~ itc4$choice_SS0_LL1) # that's more helpful and looks pretty ok, actually, the model seems to be able to predict the choices
densityplot(~ fitted(m2_1d_bobyqa) | itc4$choice_SS0_LL1) # it seems the model might be better at predicting LL choice than SS choice?

densityplot(~ fitted(m2_1d_bobyqa), group = itc4$choice_SS0_LL1) # it seems the model might be better at predicting LL choice than SS choice?


# ok, that whole diagnostic plot business is not 100% satisfying, i'm stopping for now.....
# some argue actually that it doesn't make sense to do model diagnostics when the goal is not to improve the model but just do confirmatory testing... and here we want to do confirmatory model tests, and are less interested whether this is the best possible model

# Some good news is that the packages influence.ME and HLMdiag also work for glmers (I tested it only for influence.ME, but at least the instructions of HLMdiag say it also works for glmer objects)

# so let's try influence.ME
library(influence.ME)
infl_m2_1d_bobyqa <- influence(m2_1d_bobyqa, group = "f_pp_code_ITC")
cooks.distance(infl_m2_1d_bobyqa)
plot(infl_m2_1d_bobyqa, which = "cook", sort = TRUE)
# I have 32 participants (if I'm not mistaken), so one of the criteria to use as threshold is 4/32, which is 0.125 --> it looks like nobody has a value getting even close to that


# dfbetas
dfbetas(infl_m2_1d_bobyqa)
plot(infl_m2_1d_bobyqa, which = "dfbetas")
# I can tell it also to sort the entries; but since always several panels (per predictor) are shown, I have to tell it which predictor to use for the sorting
plot(infl_m2_1d_bobyqa, which = "dfbetas", sort = TRUE, to.sort = "s_SS_Amount")
plot(infl_m2_1d_bobyqa, which = "dfbetas", sort = TRUE, to.sort = "o_Now_Notnow.L") # yep, I have to specify it like it's in the summary output (i.e., I have to add the .L after the o_Now_Notnow)
plot(infl_m2_1d_bobyqa, which = "dfbetas", sort = TRUE, to.sort = "s_TimeDiff")
# you get the idea...
# the criteria for dfbetas (or one of the criteria, I should say) is 2/sqrt(N), which means 2/sqrt(32), which is 0.3535534

# hmm, from the plot, it seems like some might come perhaps close to that value
# here's a quick check
abs(dfbetas(infl_m2_1d_bobyqa)) > 0.3535534

# ok, there seem to be some greater than the criterion (but note: the other common criterion is +/-1 and nobody is getting close to that)
max(abs(dfbetas(infl_m2_1d_bobyqa))) # 0.526697 ok, this value is quite a bit larger than the criterion
# so, based on that, we could remove some participants and re-run the main analysis; however, I think I wouldn't do it, since I think all the observations are from the same well-controlled lab experiment... If you want to be very thorough, you could run the analysis with and without these potentially problematic participants included and report both



# OK, enough model diagnostics

# let's try to visualize our data with some figures

# first, let's use the raw data
# let's try the good old xyplots with a smoothed line; this means we're plotting the raw data!

xyplot(choice_SS0_LL1 ~ rel_Diff_Amounts, data = itc4, type = c('p', 'smooth')) # ok, not the prettiest, but it shows the relationship nicely
xyplot(choice_SS0_LL1 ~ rel_Diff_Amounts, group = TimeDiff, data = itc4, type = c('p', 'smooth'), auto.key = TRUE)
# it's funny that this plot (compared to the interaction plot) shows the 3 day trials as always choosing the LL; I guess this is an artifact of the smoothing algorithm

# however, this has to do with the exact versus rounded relative differences, as we can see in the plot below
xyplot(choice_SS0_LL1 ~ rounded_rel_Diff_Amounts, group = TimeDiff, data = itc4, type = c('p', 'smooth'), auto.key = TRUE)




# ok, next, let's do some effects plots

plot(effect("o_Now_Notnow", m2_1d_bobyqa))  # notice the y axis!
plot(effect("s_rel_Diff_Amounts", m2_1d_bobyqa)) # notice the y axis! this looks very odd
plot(effect("s_TimeDiff", m2_1d_bobyqa))  # notice the y axis!

# to have the plots on a more helpful y axis, we can use the type = argument
plot(effect("s_rel_Diff_Amounts", m2_1d_bobyqa), type = "response") # this shows the relationship on the 'probability scale' (i.e., in the range between 0 and 1, i.e., the original response scale)
plot(effect("s_rel_Diff_Amounts", m2_1d_bobyqa), type = "link") # this shows the relationship on the logit scale (i.e., the scale defined by the link function)


# let's check out the other effects with the same "response" trick
plot(effect("o_Now_Notnow", m2_1d_bobyqa), type = "response") # ok, that worked; notice the asymmetric CI bars
plot(effect("s_TimeDiff", m2_1d_bobyqa), type = "response") # ok, that worked; notice the asymmetric CI bars



# BTW, in older versions of these effects plots, you had to use the rescale argument (and you had to set it to FALSE); for those interested, here are these examples (it still works, but it tells you that it's outdated...)
?effect
plot(effect("s_rel_Diff_Amounts", m2_1d_bobyqa), rescale = TRUE) # that still looks kind of funny and not so helpful, actually


plot(effect("s_rel_Diff_Amounts", m2_1d_bobyqa), rescale = FALSE) # that looks much better!
# ok, so let's do the ones from above again with this rescale = FALSE thing added

plot(effect("o_Now_Notnow", m2_1d_bobyqa), rescale = FALSE)  # ok, that looks better; also note the asymmetric confidence indicators
plot(effect("s_rel_Diff_Amounts", m2_1d_bobyqa), rescale = FALSE)
plot(effect("s_TimeDiff", m2_1d_bobyqa), rescale = FALSE)  

# We don't have any interaction terms in this model, but if we had, we could plot them also, of course



# there's also the function from the package sjPlot (sjp.glmer can do several different plots, not just these effects plots; have a look here, for example: http://www.strengejacke.de/sjPlot/sjp.glmer/ or, more generally, here: http://www.strengejacke.de/sjPlot/)

install.packages("sjPlot")
library(sjPlot)

sjp.glmer(m2_1d_bobyqa, type = "eff")
sjp.glmer(m2_1d_bobyqa, type = "eff", show.ci = TRUE) # with this argument, CIs are shown



# ok, next, let's try out showing the model coeffs
coefplot2(m2_1d_bobyqa)

# sjp.glmer can also that kind of plot (it also does some diagnostic plots, but I haven't (yet?) found information how to interpret them)

sjp.glmer(m2_1d_bobyqa, type = "coef")


sjp.glmer(m2_1d_bobyqa, type = "fe")
# hmm, not sure what the x scale is here? it seems very non-linear and surely is different from what coefplot is showing
?sjp.glmer
# ah, it plots the Odd's ratios! and, although it doesn't say so, I guess the plotted number is the actual odd's ratio and the asterisks indicate the p value from the summary output? anyway, showing these asterisks is probably not such a fantastic idea, as these are from Wald Z tests (I assume)




#####
# so now we want to get p values

# Anova with KR-adjustment and F tests won't work, because that's only for gaussian models
# just to prove my point:
Anova_3F_m2_1d_bobyqa <- Anova(m2_1d_bobyqa, type = 3, test = 'F')
# Error in UseMethod("vcovAdj") : 
  # no applicable method for 'vcovAdj' applied to an object of class "c('glmerMod', 'merMod')"


# the Anova with Chisquares should work (purely for educational purposes, see comment below)
Anova_3Chisq_m2_1d_bobyqa <- Anova(m2_1d_bobyqa, type = 3, test = 'Chisq')
# Analysis of Deviance Table (Type III Wald chisquare tests)

# Response: choice_SS0_LL1
                     # Chisq Df Pr(>Chisq)    
# (Intercept)         4.8915  1    0.02699 *  
# o_Now_Notnow        0.2990  1    0.58450    
# s_TimeDiff         42.7547  1  6.205e-11 ***
# s_SS_Amount        32.1991  1  1.392e-08 ***
# s_rel_Diff_Amounts 48.1681  1  3.912e-12 ***

# however, these are the Wald chisquare tests and they are not trustworthy (they are the same tests as we get in the summary statement, except the ones in the summary statement are type 1 if I'm not mistaken and this here is type 3)


# we still have the following options: drop1, bootMer, and PBmodcomp

# I do drop1 to get LRT tests (we never talked about this in the class, as I think there are now much better ways how to get p values; namely the approaches we *did* discuss in the class)
drop1_m2_1d_bobyqa <- drop1(m2_1d_bobyqa, ~., test = 'Chisq') # btw, using drop1 means you'll get Type II tests (not Type III)

# as they mention in Barr et al; there can be problems: some of the simpler ("smaller") models didn't seem to converge, thus we got an error warning
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 1.15532 (tol = 0.001, component 1)
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model is nearly unidentifiable: large eigenvalue ratio
 # - Rescale variables?

# accordingly, the p values are probably not that trustworthy
drop1_m2_1d_bobyqa
# well, they still tell the same story: Time Diff, SS amount, and rel Diff are significant, now/notnow is not significant
# Single term deletions

# Model:
# choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + 
    # (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts | f_pp_code_ITC)
                   # Df    AIC    LRT   Pr(Chi)    
# <none>                715.95                     
# o_Now_Notnow        1 714.25  0.300    0.5842    
# s_TimeDiff          1 738.78 24.832 6.255e-07 ***
# s_SS_Amount         1 735.67 21.727 3.144e-06 ***
# s_rel_Diff_Amounts  1 756.20 42.249 8.034e-11 ***


# below is the script that shows the status of 2 year ago (2014), so it's a bit, well, historical
#  as you know, there's all the other newer and nice(r) methods to get p values, such as mixed() and confint(), as you know how these work, I'm not going to demonstrate those, but I'll leave in the somewhat historical stuff below from 2014 (it's still fine of course!)


# but let's do bootMer (that's what I would prefer for a paper anyway)
FUN_bootMer <- function(fit) {
	return(fixef(fit))
}

# I try it first with 3 simulations
t1 <- Sys.time()
boot_m2_1d_bobyqa <- bootMer(m2_1d_bobyqa, FUN_bootMer, nsim = 3, .progress = "txt", PBargs = list(style = 3), type = "parametric", parallel = 'multicore', ncpus = 3)
t2 <- Sys.time()
t2 - t1 # Time difference of 9.484615 secs
# ok, so i expect this to take around 1000/3*10/60 --> 55 minutes for 1000 simulations; which is more than I can afford right now, as I have to keep preparing the class...


# we could of course also use confint (with profiling or with bootstrapping)


m2_1d_bobyqa_profile <- profile(m2_1d_bobyqa, method = 'profile', signames = FALSE)
# hmm, I get this error:
# Error in profile.merMod(m2_1d_bobyqa, method = "profile", signames = FALSE) : 
  # Profiling over both the residual variance and
# fixed effects is not numerically consistent with
# profiling over the fixed effects only

# what if I try confint directly, rather than profile?

m2_1d_bobyqa_profileCI95 <- confint(m2_1d_bobyqa, method = "profile")
# same error:
# Computing profile confidence intervals ...
# Error in profile.merMod(object, which = parm, signames = oldNames, ...) : 
  # Profiling over both the residual variance and
# fixed effects is not numerically consistent with
# profiling over the fixed effects only

# does the same occur with boot?
m2_1d_bobyqa_bootCI95 <- confint(m2_1d_bobyqa, method = "boot", nsim = 3, .progress="txt", PBargs=list(style=3))
#There were 15 warnings (use warnings() to see them)
warnings()
# Warning messages:
# 1: In norm.inter(t, alpha) : extreme order statistics used as endpoints
# 2: In norm.inter(t, alpha) : extreme order statistics used as endpoints
# 3: In norm.inter(t, alpha) : extreme order statistics used as endpoints
# 4: In norm.inter(t, alpha) : extreme order statistics used as endpoints
# 5: In norm.inter(t, alpha) : extreme order statistics used as endpoints
# 6: In norm.inter(t, alpha) : extreme order statistics used as endpoints
# 7: In norm.inter(t, alpha) : extreme order statistics used as endpoints
# 8: In norm.inter(t, alpha) : extreme order statistics used as endpoints
# 9: In norm.inter(t, alpha) : extreme order statistics used as endpoints
# 10: In norm.inter(t, alpha) : extreme order statistics used as endpoints
# 11: In norm.inter(t, alpha) : extreme order statistics used as endpoints
# 12: In norm.inter(t, alpha) : extreme order statistics used as endpoints
# 13: In norm.inter(t, alpha) : extreme order statistics used as endpoints
# 14: In norm.inter(t, alpha) : extreme order statistics used as endpoints
# 15: In norm.inter(t, alpha) : extreme order statistics used as endpoints

# OK, so these warnings are only there because I did only 3 simulations... but otherwise it seems to work. So I could use that to get CIs



# since we have main effects only (so Type 2 vs. Type 3 tests don't matter), we can use PBmodcomp() just for fun
# I'm not going to use it for all the different effects, just for some, to demonstrate how we would do that

# so, our "Large" model was this here:
m2_1d_bobyqa <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9), optimizer = "bobyqa"))
summary(m2_1d_bobyqa)


# For the "Small" model, I'm going to remove the fixed effect of s_TimeDiff, so that we can compare the two models for a significance test

m2_1d_bobyqa_noTimeDiff <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_SS_Amount + s_rel_Diff_Amounts + (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9), optimizer = "bobyqa"))
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 1.15532 (tol = 0.001, component 1)
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model is nearly unidentifiable: large eigenvalue ratio
 # - Rescale variables?

summary(m2_1d_bobyqa_noTimeDiff)

# hmm, this rescale command actually makes me thing whether o_Now_Notnow might be causing the convergence problems, whether it might make more sense to just code it numerically or as a regular factor (since that's what it's doing anyway)

unique(itc4$o_Now_Notnow)

head(itc4)

# I do have t_Now_Notnow in my data frame, with now coded as -0.5 and notnow coded as +0.5, so I could try that


m2_1d_bobyqa_noTimeDiff_2 <- glmer(choice_SS0_LL1 ~ t_Now_Notnow + s_SS_Amount + s_rel_Diff_Amounts + (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9), optimizer = "bobyqa"))
# ok, that worked!
#hmm, that makes me wonder, whether I can add it back as a random slope?

m2_1d_bobyqa_2 <- glmer(choice_SS0_LL1 ~ t_Now_Notnow + s_SS_Amount + s_rel_Diff_Amounts + (1 + t_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9), optimizer = "bobyqa"))

# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # unable to evaluate scaled gradient
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
# ok, that's still a problem


# what about this,but without random correls?
m2_1d_bobyqa_3 <- glmer(choice_SS0_LL1 ~ t_Now_Notnow + s_SS_Amount + s_rel_Diff_Amounts + (1 + t_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts||f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9), optimizer = "bobyqa"))
# that worked!
summary(m2_1d_bobyqa_3)




# well, so I guess I could go either of two possible routes: fully maximal model except no NowNotnow random slope; or fully maximal except without the covariance terms. I think both would be acceptable

# I'll use mixed() to see which of these works (submodels might give convergence warnings...)

# to save time, I'm going to use LRTs

my3cluster <- makeCluster(rep("localhost", 3))


m2_1d_bobyqa_2_mixed <- mixed(choice_SS0_LL1 ~ t_Now_Notnow + s_SS_Amount + s_rel_Diff_Amounts + (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9), optimizer = "bobyqa"), type = 3, method = 'LRT', cl = my3cluster)
# that worked without giving me a problem

anova(m2_1d_bobyqa_2_mixed)

# Model: choice_SS0_LL1 ~ t_Now_Notnow + s_SS_Amount + s_rel_Diff_Amounts + 
# Model:     (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts | f_pp_code_ITC)
# Data: itc4
# Df full model: 14
                   # Df   Chisq Chi Df Pr(>Chisq)    
# t_Now_Notnow       13  0.2884      1   0.591264    
# s_SS_Amount        13  7.4721      1   0.006266 ** 
# s_rel_Diff_Amounts 13 24.4275      1  7.716e-07 ***

# and again the same conclusions (not too surprising...)


# what if we try the model without the covariances?
m2_1d_bobyqa_3_mixed <- mixed(choice_SS0_LL1 ~ t_Now_Notnow + s_SS_Amount + s_rel_Diff_Amounts + (1 + t_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts||f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1e+9), optimizer = "bobyqa"), type = 3, method = 'LRT', cl = my3cluster)
# that worked also without problems
anova(m2_1d_bobyqa_3_mixed)
# Model: choice_SS0_LL1 ~ t_Now_Notnow + s_SS_Amount + s_rel_Diff_Amounts + 
# Model:     (1 + t_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts || 
# Model:         f_pp_code_ITC)
# Data: itc4
# Df full model: 9
                   # Df   Chisq Chi Df Pr(>Chisq)    
# t_Now_Notnow        8  0.2381      1     0.6256    
# s_SS_Amount         8 32.9852      1  9.286e-09 ***
# s_rel_Diff_Amounts  8 40.7146      1  1.762e-10 ***
# and again the same conclusions (well, SS amount went from ** to ***, and actually the chisquare values for both SS and relDiff are quite a bit larger here, and the residual Dfs are smaller...)

# So in this case, it doesn;t really matter which model we use for our inference statistics. But in some cases, the conclusions might differ of course. So how should one choose the model? It is clear that one must NOT choose based on the p values of the fixed effects. But how could one choose? This is a difficult question, but one option might be to consult the AIC or BIC to pick one of the models *before* one looks at p values of fixed effects, and then stick to the chosen model.
# Another option is to run and report both models and discuss in the paper, why one did both, and what the different conclusions are. Unfortunately, there is so far little guidance in the literature what the best approach is.
# yet another option would be to go the Bates, Baayen, Kliegl et al route and first simplify the random effects structure *before* obtaining any p values.

stopCluster(my3cluster)



# Actually, given that the residuals didn't look universally fantastic, I would tend to prefer a bootstrap-based method, i.e., either bootstrapped CIs or the bootstrapped LRT (for example mixed() with PB)

# after all this work, I think I should save my workspace...
save.image("ICT_glmer_Example_2April2016a.RData")
