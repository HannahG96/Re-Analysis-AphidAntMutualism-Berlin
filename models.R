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

# FOR DREDGE:
options(na.action = "na.fail")

### APHID DENSITY#############################################################################
glimpse(Aphid_density)#check data structure
hist(Aphid_density$N_aphid.mm) 

#---> Data does not look normal, so I suggest a transformation, which you can explore here:
boxcox(N_aphid.mm~date_s+meanAnt.mean_s+Seal_500_s+
         date_s:meanAnt.mean_s+ 
         date_s:Seal_500_s+
         meanAnt.mean_s:Seal_500_s, 
       data=Aphid_density,
       lambda = seq(-0.05, 0.45, len = 20))
#---> the lambda value is close to 0.5 => square root transformation 
hist(sqrt(Aphid_density$N_aphid.mm)) #square root transformation looks better


# Fit the model with square root transformation:
Aph<-lmer(sqrt(N_aphid.mm) ~ date_s + meanAnt.mean_s + Seal_500_s +
            #date_s:meanAnt.mean_s + #do we need this interaction ?
            date_s:Seal_500_s +
            meanAnt.mean_s:Seal_500_s + 
                             (1|plantPop),
                           data=Aphid_density)

#Look at diagnostics:
plot(Aph)
qqnorm(resid(Aph))
hist(residuals(Aph)) # I like to look at the histogram as well :)
res <- DHARMa::simulateResiduals(Aph) # using package DHARMa for testing residuals
plot(res) # Looks all good, no dignificant deviations anywhere

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

# represent coefs with confidence intervals
coefs <- broom.mixed::tidy(Aph, conf.int = TRUE)
dw <- dwplot(Aph,by_2sd=FALSE) 
print(dw+geom_vline(xintercept=0,lty=2))

# dredge + model avg to check if the same variables come out:
d.Aph <- dredge(Aph, rank = "AICc", REML = FALSE)
model.avg(d.Aph, subset = delta <2) # all variables kept in best 3 models

### Ant ATTENDANCE############################################################################
glimpse(Ant_attendance)#check data structure
hist(Ant_attendance$AntperAphid.mean)

#---> Data does not look normal, so I suggest a transformation, which you can explore here:
boxcox(AntperAphid.mean ~ date_s+N_aphid_s+Seal_500_s+
         date_s:N_aphid_s+date_s:Seal_500_s+
         N_aphid_s:Seal_500_s,
       data=Ant_attendance,
       lambda = seq(-0.05, 0.45, len = 20))
#---> the lambda value is close to O => log transformation 
hist(log(Ant_attendance$AntperAphid.mean)) #log transformation looks only vaguely better

# Fit LMER with log:
AntAtt<-lmer(log(AntperAphid.mean)~ date_s+N_aphid_s+Seal_500_s+
               date_s:N_aphid_s+date_s:Seal_500_s+
            N_aphid_s:Seal_500_s+(1|plantPop), data=Ant_attendance)

#Look at diagnostics:
plot(AntAtt)
qqnorm(resid(AntAtt))
res <- simulateResiduals(AntAtt)
plot(res) # ok but still a slight curved trend in residuals

#Look at the model summary:
summary(AntAtt) # not the same results as the partial R2 here...
# represent coefs with confidence intervals
coefs <- broom.mixed::tidy(AntAtt, conf.int = TRUE)
dw <- dwplot(AntAtt,by_2sd=FALSE) 
print(dw+geom_vline(xintercept=0,lty=2))

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
r.squaredGLMM(AntAtt)
#########marginal:0.5276438
#########conditional:0.8294665

#Calculate partial R2 for each predictor (only fixed effects):
r2beta(AntAtt, method="nsj")

### TENDING TIME#############################################################################
glimpse(Tending_Time)#check data structure

hist(Tending_Time$aphid_IA.sum)
# THIS is not actually a proportion which can be modeled by a binomial because you cannot express it in counts of success vs. failures
#  "proportions" of time periods in minutes do not correspond to a binomial model, in particular since you then summed and averaged them per population.

# You need beta regression for this:
# https://rcompanion.org/handbook/J_02.html

#### Trying out : BETA REGRESSIONS using the glmmTMB package:
library(glmmTMB)
Tend.betareg <- glmmTMB(aphid_IA.sum ~ date_s + N_aphid_s + Seal_500_s + 
                 date_s:N_aphid_s + date_s:Seal_500_s +
                 N_aphid_s:Seal_500_s +
                 (1|plantPop),
               data=Tending_Time,
              family=beta_family)

# Test residuals:
hist(residuals(Tend.betareg)) # looks pretty good and normal
library(DHARMa)
res <- simulateResiduals(Tend.betareg)
plot(res) # does not find any problem with residuals, all seem normal
#=> beta regression works well

# Test fixed effects:
car::Anova(Tend.betareg, type="III") # one way to test for significance (probably the best)
summary(Tend.betareg) # another way which gives similar p-values
# sealing is signif (decreasing)

# another way to look at fixed effects is calculating confidence intervals for the predictor coefficients:
library(broom.mixed)
t1 <- broom.mixed::tidy(Tend.betareg, conf.int = TRUE)
View(t1)

# same thing presented in a graph:
library(dotwhisker)
Tend.betareg$coefficients <- TRUE ## hack!
dw <- dwplot(Tend.betareg,by_2sd=FALSE) 
print(dw+geom_vline(xintercept=0,lty=2))

# R squared: DOES NOT WORK with the glmm betaregression ... that's too bad!
# r.squaredGLMM(Tend.betareg) 

# Normal version (lmer):
Tend<-lmer(aphid_IA.sum ~ date_s + N_aphid_s + Seal_500_s + 
              date_s:N_aphid_s + date_s:Seal_500_s +
              N_aphid_s:Seal_500_s +
              (1|plantPop),
            data=Tending_Time)

summary(Tend) # no significant effects

# Compare the AIC of betaregression and normal regression:
model.sel(Tend, Tend.betareg)
## The normal model appears to be worse than the betareg

# Conclusion: use the output form betaregression.

### GROUP DEFENSE#############################################################################
glimpse(Group_reaction)#check data structure
hist(Group_reaction$MAXREACT_prop)

#~date+N_aphid+Seal_500+date:N_aphid+date:Seal_500+N_aphid:Seal_500
#-->GLMM (family=binomial)
GroupDef<-glmer(MAXREACT_prop ~ date_s+N_aphid_s+Seal_500_s+date_s:N_aphid_s+date_s:Seal_500_s+
              N_aphid_s:Seal_500_s+(1|plantPop)
              , data=Group_reaction,
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
plot(res) # but in spite of skew, does not find any big problem with residuals = OK

# Test fixed effects:
summary(GroupDef.binom)
# Nothing is signif

# confidence intervals for the predictor coefficients:
library(broom.mixed)
t1 <- broom.mixed::tidy(GroupDef.binom, conf.int = TRUE)
# same thing presented in a graph:
library(dotwhisker)
GroupDef.binom$coefficients <- TRUE ## hack!
dw <- dwplot(GroupDef.binom,by_2sd=FALSE) 
print(dw+geom_vline(xintercept=0,lty=2))

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values: (works for binomials)
r.squaredGLMM(GroupDef.binom)
#########marginal: 0.12
#########conditional: 0.12


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
plot(res) # NOT LOOKING GOOD!

# Test fixed effects:
summary(Aggr)
# sealing, context and date signif

# confidence intervals for the predictor coefficients:
library(broom.mixed)
t1 <- broom.mixed::tidy(Aggr, conf.int = TRUE)
# same thing presented in a graph:
library(dotwhisker)
dw <- dwplot(Aggr,by_2sd=FALSE) 
print(dw+geom_vline(xintercept=0,lty=2))

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
r.squaredGLMM(Aggr)
#########marginal:0.09770429
#########conditional:0.2710607
#In date:plantPop :
#numerical expression has 332 elements: only the first used

#Calculate partial R2 for each predictor (only fixed effects):
r2beta(Aggr, method="nsj")

#--->  ALTERNATIVE: dividing the data into two binomials
# 1: probability of reaction (score >0)
# 2: probability of aggressive reaction! (score = 2)

glimpse(Ant_aggressivity) #check data structure

# Binomial 1 : reaction vs. no reaction
Ant_aggressivity$reaction <- as.numeric(Ant_aggressivity$aggr_score >0)

# Fit binomial
reaction.binom<-glmmTMB(reaction ~ context + date_s + N_aphid_s + Seal_500_s + 
                context:date_s + context:N_aphid_s + context:Seal_500_s +
                date_s:N_aphid_s + date_s:Seal_500_s +
                N_aphid_s:Seal_500_s + 
                (1|plantPop/date),
              family = binomial,
              data=Ant_aggressivity)

# Test residuals:
plot(res <- simulateResiduals(reaction.binom )) # Not too bad this time

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


# Binomial 2 : attack given reaction 
# (excluding cases of no reaction -> replaced by NA)
Ant_aggressivity$attack.given.react <- as.numeric(Ant_aggressivity$aggr_score ==2)
Ant_aggressivity$attack.given.react[Ant_aggressivity$aggr_score ==0] <- NA 

# Fit binomial
cond.attack.binom <- glmmTMB(attack.given.react ~ context + date_s + N_aphid_s + Seal_500_s + 
                          context:date_s + context:N_aphid_s + context:Seal_500_s +
                          date_s:N_aphid_s + date_s:Seal_500_s +
                          N_aphid_s:Seal_500_s + 
                          (1|plantPop/date),
                        family = binomial,
                        data=Ant_aggressivity)

# Test residuals:
plot(res <- simulateResiduals(cond.attack.binom )) # looking OK

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


# Binomial 3 : probability of attack
# (NOT excluding cases of no reaction)
Ant_aggressivity$attack <- as.numeric(Ant_aggressivity$aggr_score ==2)

# Fit binomial
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
                   (1|plantPop),
                 data = tmp,
                 family = betabinomial)
# Test residuals:
plot(res <- simulateResiduals(Paras )) # looking OK

# Test fixed effects:
summary(Paras) # nothing signif

# confidence intervals for the predictor coefficients:
t1 <- broom.mixed::tidy(Paras, conf.int = TRUE)
dw <- dwplot(Paras,by_2sd=FALSE) 
print(dw+geom_vline(xintercept=0,lty=2))

# r2 values:
r.squaredGLMM(Paras)
#########marginal:0.08
#########conditional:0.24

# Look at data
ggplot(Parasitism, aes(x=date, y=Prop_paras))+
  geom_point()+
  facet_wrap(~plantPop)#sometimes only 1 obs per plant
