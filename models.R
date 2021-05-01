###MODEL RESPONSES###
#Mixed Effect Models=a mixed effects model yields a variance associated with each random factor and 
#the residual variance, so its not entirely clear which to use when calculating the Rsquared
#########Shinichi Nakagawa and Holger Schielzeth-method:
##marginal R2=describes the proportion of variance explained by the fixed factor(s) alone
##conditional R2=describes the proportion of variance explained by both the fixed and random factors

#Helpful webpage:
#  https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#singular-models-random-effect-variances-estimated-as-zero-or-correlations-estimated-as---1

#Binomial GLMM:
#  https://aosmith.rbind.io/2020/08/20/simulate-binomial-glmm/

library("sjPlot")#visualize model results
  #http://www.strengejacke.de/sjPlot/reference/plot_model.html

###COEFFICIENTS
#type = "est"
#-->Forest-plot of estimates. If the fitted model only contains one predictor, slope-line is plotted.
#type = "re"
#-->For mixed effects models, plots the random effects.
#type = "std"
#-->Forest-plot of standardized beta values.
#type = "std2"
#-->Forest-plot of standardized beta values, however, standardization is done by dividing by two sd (see 'Details').

###MARGINAL EFFECTS
#type = "pred"
#-->Predicted values (marginal effects) for specific model terms. See ggpredict for details.
#type = "eff"
#-->Similar to type = "pred", however, discrete predictors are held constant at their proportions (not reference level). See ggeffect for details.
#type = "int"
#-->Marginal effects of interaction terms in model.

###DIAGNOSTICS
#type = "slope"
#-->Slope of coefficients for each single predictor, against the response (linear relationship between each model term and response).
#type = "resid"
#-->Slope of coefficients for each single predictor, against the residuals (linear relationship between each model term and residuals).
#type = "diag"
#-->Check model assumptions.



#Nice tutorial for model evaluation/representation:
#https://www.flutterbys.com.au/stats/tut/tut10.5a.html 

# FOR DREDGE:
options(na.action = "na.fail")

### APHID DENSITY#############################################################################
glimpse(Aphid_density)#check data structure

hist(Aphid_density$N_aphid.mm) 
#---> Data does not look normal, so I suggest a transformation, which you can explore with a box-cox test (just FYI):
boxcox(N_aphid.mm~date_s+meanAnt.mean_s+Seal_500_s+
         date_s:meanAnt.mean_s+ 
         date_s:Seal_500_s+
         meanAnt.mean_s:Seal_500_s, 
       data=Aphid_density,
       lambda = seq(-0.8, 0.8, len = 20))
#---> the lambda value is close to 0.5 => square root transformation 
# box cox tests are a bit of a recipe, not to be taken too literally, but it can help to decide between log or sqrt transformations.
hist(sqrt(Aphid_density$N_aphid.mm)) #square root transformation does looks better


# Fit the model with square root transformation:
Aph<-lmer(sqrt(N_aphid.mm) ~ date_s + meanAnt.mean_s + Seal_500_s +
            date_s:meanAnt.mean_s +
            date_s:Seal_500_s +
            meanAnt.mean_s:Seal_500_s + 
                             (1|plantPop),
                           data=Aphid_density)

#Look at diagnostics:
plot(Aph)
qqnorm(resid(Aph))
hist(residuals(Aph)) # I like to look at the histogram as well :)
res <- DHARMa::simulateResiduals(Aph) # using package DHARMa for testing residuals
plot(res) # Looks all good, no significant deviations anywhere

#Look at the model summary:
# MAUD: function "summary" Gives you more interesting stuff with package lmerTest:
summary(Aph) # P values are consistent with the partial R2 confidence intervals below
lmerTest::ranova(Aph) # Shows that the random effect does not improve your model much... (not essential, just FYI)

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
r.squaredGLMM(Aph)
#########marginal:0.5276438    ----> now 0.467 with square root transformation
#########conditional:0.6599617 ----> now 0.691 with square root transformation

#Calculate partial R2 for each predictor (only fixed effects):
r2beta(Aph, method="nsj")

# Represent coefs with confidence intervals:
coefs <- broom.mixed::tidy(Aph, conf.int = TRUE)
dw <- dwplot(Aph,by_2sd=FALSE) 
print(dw+geom_vline(xintercept=0,lty=2)) # nice graph of coefficients
#alternative:
plot_model(Aph, type="est", show.values = TRUE, value.offset = .3, title="",
           vline.color="grey")+
  theme_sjplot2()

#Represent some relevant marginal effects:
plot(effects::allEffects(Aph)) # to have an (ugly) visual of interactions
plot_model(Aph, type="pred", back.transform=FALSE,
           terms=c("date_s", "Seal_500_s[-1.35, -0.43, 0.45, 0.93, 2.16])"),
           pred.type="re", #predictions are conditioned on random effects ; alternative: fixed effects only "fe"
           title="",
           axis.title=c("date", "sqrt (aphid / mm)"),
           legend.title="Level of sealing (low-->high)",
           #axis.lim=(2 vectors for x and y),
           #show.data = TRUE
           se=TRUE)+
  theme_sjplot2()

plot_model(Aph, type="pred", back.transform=FALSE,
           terms=c("date_s", "meanAnt.mean_s"),
           pred.type="re", #predictions are conditioned on random effects ; alternative: fixed effects only "fe"
           title="",
           axis.title=c("date", "sqrt (aphid / mm)"),
           legend.title="Number of ants (low-->high)",
           #axis.lim=(2 vectors for x and y),
           #show.data = TRUE
           se=TRUE)+
  theme_sjplot2()

# dredge + model avg to check if the same variables come out:
d.Aph <- dredge(Aph, rank = "AICc", REML = FALSE)
model.avg(d.Aph, subset = delta <2) # all variables kept in best 3 models
# We could do this for each model to provide an alternative method
# in an appendix if needed...

### Ant ATTENDANCE############################################################################
glimpse(Ant_attendance)#check data structure
hist(Ant_attendance$AntperAphid.mean)
library(bestNormalize)#-->this package masks boxcox() function (thats why we load it now)
bestNormalize(Ant_attendance$AntperAphid.mean) #=> log10 transform

# Fit LMER:
AntAtt<-lmer(log(AntperAphid.mean) ~ date_s + N_aphid_s + Seal_500_s +  
               date_s:N_aphid_s + date_s:Seal_500_s +
               N_aphid_s:Seal_500_s +
              (1|plantPop),
             data=Ant_attendance)

#Look at diagnostics:
plot(AntAtt)
qqnorm(resid(AntAtt))
res <- simulateResiduals(AntAtt)
plot(res) # ok for normality, but still a slight curved trend in residuals... 

#Look at the model summary:
summary(AntAtt) # not the same results as the partial R2 here...
lmerTest::ranova(AntAtt) # Shows that the random effect does improve model.
# FYI: I also tested the model without random effect to see, and the Sealing effect really was not significant at all. so exactly the same results without random.

# represent coefs with confidence intervals
coefs <- broom.mixed::tidy(AntAtt, conf.int = TRUE)
dw <- dwplot(AntAtt,by_2sd=FALSE) 
print(dw+geom_vline(xintercept=0,lty=2))
#alternative:
plot_model(AntAtt, type="est", show.values = TRUE, value.offset = .3, title="",
           vline.color="grey")+
  theme_sjplot2()

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
r.squaredGLMM(AntAtt)
######### marginal:0.54
######### conditional:0.83

#Calculate partial R2 for each predictor (only fixed effects):
r2beta(AntAtt, method="nsj") 
# sealing appears to explain a little bit here (r2 = 0.06), but I would ignore it 

#Represent some relevant marginal effects:
plot_model(AntAtt, type="pred", back.transform=FALSE,
           terms=c("date_s", "N_aphid_s[-1.05, 0.10, 1.17 , 2.90]"),
           pred.type="re", #predictions are conditioned on random effects ; alternative: fixed effects only "fe"
           title="",
           axis.title=c("date", "log (ant / aphid)"),
           legend.title="Number of aphids (low-->high)",
           #axis.lim=,
           #show.data = TRUE
           se=TRUE)+
  theme_sjplot2()

plot_model(AntAtt, type="pred", back.transform=FALSE,
           terms=c("Seal_500_s"),
           pred.type="re", #predictions are conditioned on random effects ; alternative: fixed effects only "fe"
           title="",
           axis.title=c("Seal_500", "log (ant / aphid)"),
           #axis.lim=,
           #show.data = TRUE,
           se=TRUE)+
  theme_sjplot2()

# dredge check
d.AntAtt <- dredge(AntAtt, REML = FALSE, rank = "AICc")
subset(d.AntAtt, subset  = delta <2)
# => consistent result, though sealing pops up again.. not in the top model though.

###Ant attendance considering replicated ant measurement: 5 measurements per sample session
#-->Total of 264 observation (1 samplig session with only 4 measurements)
glimpse(Ant_attendance_repl)#check data structure
hist(Ant_attendance_repl.test$AntperAphid)

#Normalize data via transformation
bestNormalize(Ant_attendance_repl$AntperAphid)#-->ordered quantile normalizing transformation(best)
normalize<-orderNorm(Ant_attendance_repl$AntperAphid)
Ant_attendance_repl$AntperAphid.norm<-predict(normalize)
hist(Ant_attendance_repl$AntperAphid.norm)
#But we will perform log-transformation (easier and =1.8333):
hist(log(Ant_attendance_repl$AntperAphid))

# Fit LMER:
AntAtt_repl<-lmer(log(AntperAphid) ~ date_s + N_aphid_s + Seal_500_s + 
               date_s:N_aphid_s + date_s:Seal_500_s +
               N_aphid_s:Seal_500_s + 
               (1|plantPop/date),
             data=Ant_attendance_repl)

#Look at diagnostics:
plot(AntAtt_repl)
qqnorm(resid(AntAtt_repl))
res <- simulateResiduals(AntAtt_repl)
plot(res) # PROBLEM!

#2nd try: ordered quantile normalizing transformation
AntAtt_repl1<-lmer(AntperAphid.norm ~ date_s + N_aphid_s + Seal_500_s + 
                     date_s:N_aphid_s + date_s:Seal_500_s + 
                     N_aphid_s:Seal_500_s +
                    (1|plantPop/date), data=Ant_attendance_repl)
#Look at diagnostics:
plot(AntAtt_repl)
qqnorm(resid(AntAtt_repl))
res <- simulateResiduals(AntAtt_repl1)
plot(res) #PROBLEM!

### TENDING TIME#############################################################################
glimpse(Tending_Time)#check data structure

hist(Tending_Time$aphid_IA.sum)
# THIS is not actually a proportion which can be modeled by a binomial because you cannot express it in counts of success vs. failures
# "proportions" of time periods in minutes do not correspond to a binomial model, in particular since you then summed and averaged them per population.
# You need beta regression for this kind of proportions:
# https://rcompanion.org/handbook/J_02.html

#### Trying out : BETA REGRESSIONS using the glmmTMB package:
library(glmmTMB)

tmp <-  na.omit(Tending_Time) # to be able to apply dredge later
Tend.betareg <- glmmTMB(aphid_IA.sum ~ date_s + N_aphid_s + Seal_500_s + 
                 date_s:N_aphid_s + date_s:Seal_500_s +
                 N_aphid_s:Seal_500_s +
                 (1|plantPop),
               data= tmp,
              family=beta_family)

# Test residuals:
hist(residuals(Tend.betareg)) # looks pretty good and normal
res <- DHARMa::simulateResiduals(Tend.betareg)
plot(res) # looks good

# Test fixed effects:
car::Anova(Tend.betareg, type="III") # one way to test for significance (probably the best)
summary(Tend.betareg) # another way which gives similar p-values
# sealing is signif (decreasing)

# another way to look at fixed effects is calculating confidence intervals for the predictor coefficients:
coefs <- broom.mixed::tidy(Tend.betareg, conf.int = TRUE)
# same thing presented in a graph:
dw <- dotwhisker::dwplot(Tend.betareg,by_2sd=FALSE) 
print(dw+geom_vline(xintercept=0,lty=2))

# R squared: DOES NOT WORK with the glmm betaregression ... that's too bad!
# r.squaredGLMM()

# Just to try it: Normal version (lmer):
Tend<-lmer(aphid_IA.sum ~ date_s + N_aphid_s + Seal_500_s + 
              date_s:N_aphid_s + date_s:Seal_500_s +
              N_aphid_s:Seal_500_s +
              (1|plantPop),
            data= na.omit(Tending_Time))

summary(Tend) # no significant effects
plot(Tend)
res <- DHARMa::simulateResiduals(Tend)
plot(res) # looks not too bad either, but more trend in residuals

# Compare the AIC of betaregression and normal regression:
model.sel(Tend, Tend.betareg)
## The normal model appears to be much worse than the betareg
# Conclusion: use betaregression.

#Represent coefs with CI in a plot:
plot_model(Tend.betareg, type="est", show.values = TRUE, value.offset = .3, title="",
           vline.color="grey", axis.lim = c(0.1,5))+
  theme_sjplot2()

#Visualize marginal effects:
plot_model(Tend.betareg, type="pred", back.transform=FALSE,
           terms=c("Seal_500_s"),
           pred.type="re", #predictions are conditioned on random effects ; alternative: fixed effects only "fe"
           title="",
           axis.title=c("Seal_500", "tending time"),
           #axis.lim=,
           show.data = TRUE,
           se=TRUE)+
  theme_sjplot2()

plot_model(Tend.betareg, type="pred", back.transform=FALSE,
           terms=c("date_s"),
           pred.type="re", #predictions are conditioned on random effects ; alternative: fixed effects only "fe"
           title="",
           axis.title=c("date", "tending time"),
           #axis.lim=,
           show.data = TRUE,
           se=TRUE)+
  theme_sjplot2()

# dredge check
d.Tend <- dredge(Tend.betareg, rank = "AICc", REML = FALSE)
a <- model.avg(d.Tend, subset = delta <2)
# date and Sealing are the top two variables.
model.weights(Tend)

### GROUP DEFENSE#############################################################################
glimpse(Group_reaction)#check data structure
hist(Group_reaction$MAXREACT_prop)

#~date+N_aphid+Seal_500+date:N_aphid+date:Seal_500+N_aphid:Seal_500
#-->GLMM (family=binomial)
GroupDef<-glmer(MAXREACT_prop ~ date_s+N_aphid_s+Seal_500_s+date_s:N_aphid_s+date_s:Seal_500_s+
              N_aphid_s:Seal_500_s+(1|plantPop)
              , data=na.omit(Group_reaction),
              family=binomial,
            glmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 1000000)))
#In eval(family$initialize, rho) : non-integer #successes in a binomial glm!
####SINGULAR FIT!! 

## TRY with glmmTMB
library(glmmTMB)
Group_reaction$MIN_noreact <- Group_reaction$N_ant - Group_reaction$MAXREACT
GroupDef.binom <- glmmTMB(cbind(MAXREACT , MIN_noreact) ~
                          date_s + N_aphid_s + Seal_500_s + 
                          date_s:N_aphid_s + date_s:Seal_500_s +
                          N_aphid_s:Seal_500_s +
                          (1|plantPop),
                        data=na.omit(Group_reaction),
                        family=binomial)
# NO SINGULAR FIT with this package :D

# Test residuals:
hist(residuals(GroupDef.binom)) 
library(DHARMa)
res <- simulateResiduals(GroupDef.binom )
plot(res) # no problem with residuals = OK

# Test fixed effects:
summary(GroupDef.binom)
# Nothing is signif...

# confidence intervals for the predictor coefficients:
t1 <- broom.mixed::tidy(GroupDef.binom, conf.int = TRUE)
# same thing presented in a graph:
dw <- dwplot(GroupDef.binom,by_2sd=FALSE) 
print(dw+geom_vline(xintercept=0,lty=2))

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values: (works for binomials)
r.squaredGLMM(GroupDef.binom)
#########marginal: 0.11
#########conditional: 0.63

#Represent coefs with CI in a plot:
plot_model(GroupDef.binom, type="est", show.values = TRUE, value.offset = .3, title="",
           vline.color="grey")+
  theme_sjplot2()


### Ant AGGRESSIVITY###########################################################################
glimpse(Ant_aggressivity) #check data structure
hist(Ant_aggressivity$aggr_score)

#-->LME
Aggr<-lmer(aggr_score~context+date_s+N_aphid_s+Seal_500_s+context:date_s+context:N_aphid_s+
               context:Seal_500_s+date_s:N_aphid_s+date_s:Seal_500_s+N_aphid_s:Seal_500_s+(1|plantPop/date), 
             data=Ant_aggressivity)

# Test residuals:
hist(residuals(Aggr)) # nice!
library(DHARMa)
res <- simulateResiduals(Aggr )
plot(res) # NOT LOOKING GOOD! => cannot use these score in a linear model.

# Test fixed effects just to see:
summary(Aggr)
# sealing, context and date signif

#--->  ALTERNATIVE: dividing the data into two (or three) binomials:
# 1: probability of reaction (score >0)
# 2: probability of aggressive reaction given a reaction! (score = 2)
# 3: probability of aggressive reaction! (score = 2)

glimpse(Ant_aggressivity) #check data structure

# Fit Binomial 1
reaction.binom<-glmmTMB(reaction ~ context + date_s + N_aphid_s + Seal_500_s + 
                context:date_s + context:N_aphid_s + context:Seal_500_s +
                date_s:N_aphid_s + date_s:Seal_500_s +
                N_aphid_s:Seal_500_s + 
                (1|plantPop/date),
              family = binomial,
              data=Ant_aggressivity) 
## >>>>>> !!! using na.omit creates convergence problems, and it is not necessary here

reaction.binom<-glmmTMB(reaction ~  context + date_s + N_aphid_s + Seal_500_s + 
                          context:date_s + context:N_aphid_s + context:Seal_500_s +
                          date_s:N_aphid_s + date_s:Seal_500_s +
                          N_aphid_s:Seal_500_s + 
                          (1|plot.simple/plant/date),
                        family = binomial,
                        data=Ant_aggressivity) 
## Sealing becomes no longer signif, although mean effect is the same.


# Test residuals:
plot(res <- simulateResiduals(reaction.binom )) # Not too bad looking even if slight trend

# Test fixed effects:
summary(reaction.binom) # sealing, context and date are still signif

# confidence intervals for the predictor coefficients:
t1 <- broom.mixed::tidy(reaction.binom, conf.int = TRUE)
dw <- dwplot(reaction.binom,by_2sd=FALSE) 
print(dw+geom_vline(xintercept=0,lty=2))

# r2 values:
r.squaredGLMM(reaction.binom)
#########marginal:0.21
#########conditional:0.35

#Represent coefs with CI in a plot:
plot_model(reaction.binom, type="est", show.values = TRUE, value.offset = .3, title="",
           vline.color="grey")+
  theme_sjplot2()

#Visualize relevant marginal effects
plot_model(reaction.binom, type="pred", back.transform=FALSE,
           terms=c("Seal_500_s", "context"),
           pred.type="re", #predictions are conditioned on random effects ; alternative: fixed effects only "fe"
           title="",
           axis.title=c("Seal_500", "Probability of reaction"),
           #axis.lim=,
           show.data = TRUE,
           se=TRUE)+
  theme_sjplot2()

plot_model(reaction.binom, type="pred", back.transform=FALSE,
           terms=c("date_s", "context"),
           pred.type="re", #predictions are conditioned on random effects ; alternative: fixed effects only "fe"
           title="",
           axis.title=c("date", "Probability of reaction"),
           #axis.lim=,
           show.data = TRUE,
           se=TRUE)+
  theme_sjplot2()

#########################
# Fit Binomial 2
cond.attack.binom <- glmmTMB(attack.given.react ~ context + date_s + N_aphid_s + Seal_500_s + 
                          context:date_s + context:N_aphid_s + context:Seal_500_s +
                          date_s:N_aphid_s + date_s:Seal_500_s +
                          N_aphid_s:Seal_500_s + 
                          (1|plantPop/date),
                        family = binomial,
                        data= na.omit(Ant_aggressivity))

# Test residuals:
plot(res <- simulateResiduals(cond.attack.binom )) # looking OK, though slight hump-backed trend

# Test fixed effects:
summary(cond.attack.binom) # NOTHING SIGNIFICANT HERE

# confidence intervals for the predictor coefficients:
t1 <- broom.mixed::tidy(cond.attack.binom, conf.int = TRUE)
dw <- dwplot(cond.attack.binom,by_2sd=FALSE) 
print(dw+geom_vline(xintercept=0,lty=2))

# r2 values:
r.squaredGLMM(cond.attack.binom)
#########marginal:0.05
#########conditional:0.20

#################
# Fit Binomial 3
attack.binom <- glmmTMB(attack ~ context + date_s + N_aphid_s + Seal_500_s + 
                          context:date_s + context:N_aphid_s + context:Seal_500_s +
                          date_s:N_aphid_s + date_s:Seal_500_s +
                          N_aphid_s:Seal_500_s + 
                          (1|plantPop/date),
                        family = binomial,
                        data=Ant_aggressivity)

# Test residuals:
plot(res <- simulateResiduals(attack.binom )) # looking OK

# Test fixed effects:
summary(attack.binom) # marginal (but OK) effect of sealing

# confidence intervals for the predictor coefficients:
t1 <- broom.mixed::tidy(attack.binom, conf.int = TRUE)
dw <- dwplot(attack.binom,by_2sd=FALSE) 
print(dw+geom_vline(xintercept=0,lty=2))

# r2 values:
r.squaredGLMM(attack.binom)
performance::r2_nakagawa(attack.binom)
#########marginal:0.08
#########conditional:0.24

### PARASITISM ################################################################################
glimpse(Parasitism) #check data structure

### Here again, your parasitism proportions are not easily expressed as counts of success and failure, but they can be modeled with a beta-binomial
# Because zero inflated binomial

tmp = na.omit(Parasitism)
Paras <- glmmTMB(cbind(N_parasitised, N_not_parasitised) ~ 
                   date_s + meanAnt.mean_s + Seal_500_s +
                   aphid_IA.sum_s + aggr_score.mean_s +
                   date_s:Seal_500_s + date_s:aphid_IA.sum_s +
                   date_s:aggr_score.mean_s +
                   Seal_500_s:aphid_IA.sum_s + Seal_500_s:aggr_score.mean_s +
                   (1|plantPop/date),
                 data = tmp,
                 family = betabinomial)

# Test residuals:
plot(res <- simulateResiduals(Paras )) # looking OK-ish

# Test fixed effects:
summary(Paras) # nothing signif

# confidence intervals for the predictor coefficients:
t1 <- broom.mixed::tidy(Paras, conf.int = TRUE)
dw <- dwplot(Paras,by_2sd=FALSE) 
print(dw+geom_vline(xintercept=0,lty=2))

# r2 values : r2glmm does not handle beta-binomials, but other package does:.
performance::r2_nakagawa(Paras) 
# but may not be quite reliable (they seem too high to me for a non-signif model):
#########marginal:0.339
#########conditional:0.542

#Represent coefs with CI in a plot:
plot_model(Paras, type="est", show.values = TRUE, value.offset = .3, title="",
           vline.color="grey")+
  theme_sjplot2()
