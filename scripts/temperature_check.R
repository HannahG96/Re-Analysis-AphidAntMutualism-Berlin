### CHECK TEMPERATURE EFFECTS ALONG URBANISATION GRADIENT

#Check normality (transform data if necessary)
hist(Aphid_density$mean.temp)

### 1 ###
#Model temperature in response to urbanisation: ~Seal_500
TEMP1<-lmer(1/mean.temp ~ Seal_500_s + 
             (1|plantPop),
           data=Aphid_density)
plot(TEMP1)
qqnorm(resid(TEMP1))
res <- DHARMa::simulateResiduals(TEMP1) # using package DHARMa for testing residuals
plot(res)
summary(TEMP1)
# r2 values:
r.squaredGLMM(TEMP1)
#########marginal:0.016
#########conditional:0.227
# partial r2
r2beta(TEMP1, method="sgv")


### 2 ###
#Model temperature in response to urbanisation: ~date*Seal_500
TEMP2<-lmer(1/mean.temp ~ date_s*Seal_500_s + 
            (1|plantPop),
          data=Aphid_density)

#Model diagnostics
plot(TEMP2)
qqnorm(resid(TEMP2))
res <- DHARMa::simulateResiduals(TEMP2) # using package DHARMa for testing residuals
plot(res)
summary(TEMP2) 
# r2 values:
r.squaredGLMM(TEMP2)
#########marginal:0.343
#########conditional:0.353