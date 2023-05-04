###MODEL RESPONSE VARIABLES###
#Mixed Effect Models=a mixed effects model yields a variance associated with each random factor and 
#the residual variance, so its not entirely clear which to use when calculating the Rsquared
#########Shinichi Nakagawa and Holger Schielzeth-method:
##marginal R2=describes the proportion of variance explained by the fixed factor(s) alone
##conditional R2=describes the proportion of variance explained by both the fixed and random factors

###1-A. APHID DENSITY#############################################################################
#=nb of aphids/length of focal zone (mm)

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
hist(sqrt(Aphid_density$N_aphid.mm)) #square root transformation does looks better


# Fit the model with square root transformation:
Aph<-lmer(sqrt(N_aphid.mm) ~ date_s + meanAnt.mean_s + Seal_500_s +
            date_s:meanAnt.mean_s +
            date_s:Seal_500_s +
            meanAnt.mean_s:Seal_500_s + 
            (1|plantPop),
          data=Aphid_density)#N=53

#Look at diagnostics:
plot(Aph)
qqnorm(resid(Aph))
hist(residuals(Aph)) # I like to look at the histogram as well :)
res <- DHARMa::simulateResiduals(Aph) # using package DHARMa for testing residuals
plot(res) # Looks all good, no significant deviations anywhere

#Look at the model summary:
#-->t-tests use Satterthwaite's method
#=Satterthwaite's approximmation to degrees of freedom
summary(Aph) # P values are consistent with the partial R2 confidence intervals below

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
r.squaredGLMM(Aph)
#########marginal:0.467 
#########conditional:0.691 
#Calculate partial R2 for each predictor (only fixed effects):
r2beta(Aph, method="nsj")

###1-B. APHID DENSITY EXCLUDING OUTLIER PLOT: Nl-200
# Test effect of outlier plot with max sealing:
sub_df <- Aphid_density[-which(Aphid_density$plot.simple == "Nl-200"),]
sub_Aph<-lmer(sqrt(N_aphid.mm) ~ date_s + meanAnt.mean_s + Seal_500_s +
                date_s:meanAnt.mean_s +
                date_s:Seal_500_s +
                meanAnt.mean_s:Seal_500_s + 
                (1|plantPop),
              data=sub_df)
summary(sub_Aph)  # No more interaction effect
#Look at diagnostics:
res <- DHARMa::simulateResiduals(sub_Aph) # using package DHARMa for testing residuals
plot(res) # Looks all good, no significant deviations anywhere
#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
r.squaredGLMM(sub_Aph)
#Calculate partial R2 for each predictor (only fixed effects):
r2beta(sub_Aph, method="nsj")

###2. Ant NUMBER: RESPONSE AS DECIMAL VARIABLE ############################################################################

glimpse(Ant_attendance)#check data structure
hist(Ant_attendance$meanAnt.mean)

#Fit LMER with gaussian distribution:
#-->log-transformed response to normalize data
hist(log(Ant_attendance$meanAnt.mean))

Ant.nb<-lmer(log(meanAnt.mean)~date_s + N_aphid_s + Seal_500_s +  
               date_s:N_aphid_s + date_s:Seal_500_s +
               N_aphid_s:Seal_500_s +
               (1|plantPop), data=Ant_attendance)#N=53
#Look at diagnostics:
plot(Ant.nb)
qqnorm(resid(Ant.nb))
res <- simulateResiduals(Ant.nb)
plot(res)#ALL GOOD!

#Look at the model summary:
#-->t-tests use Satterthwaite's method
summary(Ant.nb) # P values are consistent with the partial R2 confidence intervals below
#-->Sealing is only marginally significant

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
r.squaredGLMM(Ant.nb)
#########marginal:0.3961412
#########conditional:0.7521842

#Calculate partial R2 for each predictor (only fixed effects):
r2beta(Ant.nb, method="nsj")
#-->partial Rsquared of sealing remains relatively high

###3-A. Ant ATTENDANCE: ANT-PER-APHID RATIO############################################################################
glimpse(Ant_attendance)#check data structure
hist(Ant_attendance$AntperAphid.mean) # Not looking good...
#library(bestNormalize)#-->this package masks boxcox() function (thats why we load it now)
#bestNormalize(Ant_attendance$AntperAphid.mean) #=> log10 transform

# Fit LMER of log-transformed response:
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
#-->t-tests use Satterthwaite's method
summary(AntAtt) # not the same results as the partial R2 here...
lmerTest::ranova(AntAtt) # Shows that the random effect does improve model.

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
r.squaredGLMM(AntAtt)
######### marginal:0.54
######### conditional:0.83

#Calculate partial R2 for each predictor (only fixed effects):
r2beta(AntAtt, method="nsj") 

###3-B. ANT ATTENDANCE: TENDING TIME#############################################################################
glimpse(Tending_Time)#check data structure

hist(Tending_Time$aphid_IA.sum)
# THIS is not actually a proportion which can be modeled by a binomial because you cannot express it in counts of success vs. failures
# "proportions" of time periods in minutes do not correspond to a binomial model, in particular since you then summed and averaged them per population.
# You need beta regression for this kind of proportions:
# https://rcompanion.org/handbook/J_02.html

#### Trying out : BETA REGRESSIONS using the glmmTMB package:

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
#-->Wald test (z-test)
summary(Tend.betareg) # another way which gives similar p-values
# sealing is signif (decreasing)

# another way to look at fixed effects is calculating confidence intervals for the predictor coefficients:
coefs <- broom.mixed::tidy(Tend.betareg, conf.int = TRUE)
# same thing presented in a graph:
dw <- dotwhisker::dwplot(Tend.betareg,by_2sd=FALSE) 
print(dw+geom_vline(xintercept=0,lty=2))

# R squared: DOES NOT WORK with the glmm betaregression ... that's too bad!

###4. Ant AGGRESSIVITY###########################################################################
glimpse(Ant_aggressivity) #check data structure
hist(Ant_aggressivity$aggr_score)

# Fit Linear Mixed Effect Model:
Aggr<-lmer(aggr_score~context+date_s+N_aphid_s+Seal_500_s+context:date_s+context:N_aphid_s+
             context:Seal_500_s+date_s:N_aphid_s+date_s:Seal_500_s+N_aphid_s:Seal_500_s+(1|plantPop/date), 
           data=Ant_aggressivity)

# Test residuals:
hist(residuals(Aggr)) # nice!
res <- simulateResiduals(Aggr)
plot(res) # NOT LOOKING GOOD! => cannot use these score in a linear model.

#--->  ALTERNATIVE: dividing the data into binomial:
#=probability of reaction (score >0): aggressive (1) vs. avoidance (0)
Ant_aggressivity$reaction<-as.factor(Ant_aggressivity$reaction)

# Fit GLMER:
#Model1: aggressive (1) vs. avoidance (0)
reaction.binom<-glmer(reaction ~ context + date_s + N_aphid_s + Seal_500_s + 
                        context:date_s + context:N_aphid_s + context:Seal_500_s +
                        date_s:N_aphid_s + date_s:Seal_500_s +
                        N_aphid_s:Seal_500_s + 
                        (1|plantPop/date),
                      family = binomial,
                      data=Ant_aggressivity,
                      glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
# Test residuals:
plot(res <- simulateResiduals(reaction.binom)) # Some trend but not signif

# Test fixed effects:
#-->Wald Test
#=coefficient estimates are expected to be normally distributed (z-test)
summary(reaction.binom) # sealing, context and date are still signif

# r2 values:
r.squaredGLMM(reaction.binom)
#########marginal:0.212
#########conditional:0.347

# partial r2
r2beta(reaction.binom, method="sgv")
#-->gives a different marginal r2=0.154
