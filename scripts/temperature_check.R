### CHECK TEMPERATURE EFFECTS
hist(Aphid_density$mean.temp)
hist(1/Aphid_density$mean.temp)
TEMP<-lmer(1/mean.temp ~ date_s*Seal_500_s + 
            (1|plantPop),
          data=Aphid_density)
plot(TEMP)
qqnorm(resid(TEMP))
res <- DHARMa::simulateResiduals(TEMP) # using package DHARMa for testing residuals
plot(res)
summary(TEMP)
plot(1/Aphid_density$mean.temp~Aphid_density$date)
