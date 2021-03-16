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

###APHID DENSITY#############################################################################
glimpse(Aphid_density)#check data structure

#Fit the model:
#~date+N_ant+Seal_500+date:N_ant+date:Seal_500+N_ant:Seal_500
#-->LME
Aph<-lmer(N_aphid.mm~date_s+meanAnt.mean_s+Seal_500_s+date_s:meanAnt.mean_s+date_s:Seal_500_s+
             meanAnt.mean_s:Seal_500_s+(1|plantPop), data=Aphid_density)

#Look at diagnostics:
plot(Aph)
qqnorm(resid(Aph))

#Look at the model summary:
display(Aph)

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
r.squaredGLMM(Aph)
#########marginal:0.5276438
#########conditional:0.6599617

#Calculate partial R2 for each predictor (only fixed effects):
r2beta(Aph, method="nsj")


###Ant ATTENDANCE############################################################################
glimpse(Ant_attendance)#check data structure

#Fit the model:
#=log-transformed response
#~date+N_aphid+Seal_500+date:N_aphid+date:Seal_500+N_aphid:Seal_500
#-->LME
AntAtt<-lmer(log(AntperAphid.mean)~date_s+N_aphid_s+Seal_500_s+date_s:N_aphid_s+date_s:Seal_500_s+
            N_aphid_s:Seal_500_s+(1|plantPop), data=Ant_attendance)

#Look at diagnostics:
plot(AntAtt)
qqnorm(resid(AntAtt))

#Look at the model summary:
display(AntAtt)

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
r.squaredGLMM(AntAtt)
#########marginal:0.5276438
#########conditional:0.8294665

#Calculate partial R2 for each predictor (only fixed effects):
r2beta(AntAtt, method="nsj")


###TENDING TIME#############################################################################
glimpse(Tending_Time)#check data structure

#~date+N_aphid+Seal_500+date:N_aphid+date:Seal_500+N_aphid:Seal_500
#-->GLMM (family=binomial)
Tend<-glmer(aphid_IA.sum~date_s+N_aphid_s+Seal_500_s+date_s:N_aphid_s+date_s:Seal_500_s+
               N_aphid_s:Seal_500_s+(1|plantPop), data=Tending_Time, family=binomial,
            glmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 1000000)))
#In eval(family$initialize, rho) : non-integer #successes in a binomial glm!
####SINGULAR FIT!!


#Look at diagnostics:
#plot(Tend)
#qqnorm(resid(Tend))

#Look at the model summary:
#display(Tend)

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
#r.squaredGLMM(Tend)
#########marginal:
#########conditional:


###GROUP DEFENSE#############################################################################
glimpse(Group_reaction)#check data structure

#~date+N_aphid+Seal_500+date:N_aphid+date:Seal_500+N_aphid:Seal_500
#-->GLMM (family=binomial)
GroupDef<-glmer(MAXREACT_prop~date_s+N_aphid_s+Seal_500_s+date_s:N_aphid_s+date_s:Seal_500_s+
              N_aphid_s:Seal_500_s+(1|plantPop), data=Group_reaction, family=binomial,
            glmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 1000000)))
#In eval(family$initialize, rho) : non-integer #successes in a binomial glm!
####SINGULAR FIT!!

#Look at diagnostics:
#plot(GroupDef)
#qqnorm(resid(GroupDef))

#Look at the model summary:
#display(GroupDef)

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
#r.squaredGLMM(GroupDef)
#########marginal:
#########conditional:


###Ant AGGRESSIVITY###########################################################################
glimpse(Ant_aggressivity)#check data structure

#Fit the model:
#~context+date+N_aphid+Seal_500+context:date+context:N_aphid+context:Seal_500+
              #date:N_aphid+date:Seal_500+N_aphid:Seal_500
#-->LME
Aggr<-lmer(aggr_score~context+date_s+N_aphid_s+Seal_500_s+context:date_s+context:N_aphid_s+
               context:Seal_500_s+date_s:N_aphid_s+date_s:Seal_500_s+N_aphid_s:Seal_500_s+(1|plantPop/date), 
             data=Ant_aggressivity)

#Look at diagnostics:
plot(Aggr)
qqnorm(resid(Aggr))

#Look at the model summary:
display(Aggr)

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
r.squaredGLMM(Aggr)
#########marginal:0.09770429
#########conditional:0.2710607
#In date:plantPop :
#numerical expression has 332 elements: only the first used

#Calculate partial R2 for each predictor (only fixed effects):
r2beta(Aggr, method="nsj")



###PARASITISM################################################################################
glimpse(Parasitism)#check data structure

#Fit the model:
#~date+N_ant+Seal_500+TendingTime+Aggressivity+date:Seal_500+date:TendingTime+date:Aggressivity+
  #  Seal_500:TendingTime+Seal_500:Aggressivity

#-->GLMM (family=binomial)
Paras<-glmer(Prop_paras~date_s+meanAnt.mean_s+Seal_500_s+aphid_IA.sum_s+aggr_score.mean_s+
               date_s:Seal_500_s+date_s:aphid_IA.sum_s+date_s:aggr_score.mean_s+Seal_500_s:aphid_IA.sum_s+
               Seal_500_s:aggr_score.mean_s+(1|plantPop), data=Parasitism, family=binomial,
             glmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 1000000)))
#In eval(family$initialize, rho) : non-integer #successes in a binomial glm!
####SINGULAR FIT!!-->model too complex, not enough observation, data zero-inflated (???)

ggplot(Parasitism, aes(x=date, y=Prop_paras))+
  geom_point()+
  facet_wrap(~plantPop)#sometimes only 1 obs per plant


#Look at diagnostics:
#plot(Paras)
#qqnorm(resid(Paras))

#Look at the model summary:
#summary(Paras)

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
#r.squaredGLMM(Paras)
#########marginal:
#########conditional:


