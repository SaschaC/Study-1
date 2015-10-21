################### Statistical Modeling - ALL ################################
########################################################################
library(languageR)
library(lme4)
library(Hmisc)

### backfit 1
m1 = glmer(correctness ~ (red*spelling
                         )  + (1|set) 
          + (1|subject), data = xp3.sub.03, family = binomial(logit))
summary(m1)
drop1(m1)
m2 = glmer(correctness ~ (red+spelling
)  + (1|set) 
+ (1|subject), data = xp3.sub.03, family = binomial(logit))
summary(m2)
drop1(m2)
m3 = glmer(correctness ~ (red
)  + (1|set) 
+ (1|subject), data = xp3.sub.03, family = binomial(logit))
summary(m3)
drop1(m3)

final_model<- glmer(correctness ~ (1|set) 
  + (1|subject), data = xp3.sub.03, family = binomial(logit))
summary(m4)
################################################RTs
m1<-lmer(log_rt ~ (red*spelling+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = xp3.sub.04)
d<-xp3.sub.04[which(residuals(m1) < mean(residuals(m1))
                    +2.5 * sd(residuals(m1)) & residuals(m1)
                    > mean(residuals(m1)) - 2.5 * 
                      sd(residuals(m1))),]
m1<-lmer(log_rt ~ (red*spelling+trial+
                       log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = d)
summary(m1)
drop1(m1)
m2<-lmer(log_rt ~ (red+spelling+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = xp3.sub.04)
d<-xp3.sub.04[which(residuals(m2) < mean(residuals(m2))
                    +2.5 * sd(residuals(m2)) & residuals(m2)
                    > mean(residuals(m2)) - 2.5 * 
                      sd(residuals(m2))),]
m2<-lmer(log_rt ~ (red+spelling+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = d)
summary(m2)
drop1(m2)
m3<-lmer(log_rt ~ (spelling+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = xp3.sub.04)
d<-xp3.sub.04[which(residuals(m3) < mean(residuals(m3))
                    +2.5 * sd(residuals(m3)) & residuals(m3)
                    > mean(residuals(m3)) - 2.5 * 
                      sd(residuals(m3))),]
m3<-lmer(log_rt ~ (spelling+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = d)
summary(m3)
drop1(m3)
m4<-lmer(log_rt ~ (trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = xp3.sub.04)
d<-xp3.sub.04[which(residuals(m4) < mean(residuals(m4))
                    +2.5 * sd(residuals(m4)) & residuals(m4)
                    > mean(residuals(m4)) - 2.5 * 
                      sd(residuals(m4))),]
m4<-lmer(log_rt ~ (trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = d)
summary(m4)
#forward fit 1
m4a<-lmer(log_rt ~ (trial+
                      log(duration)+previous_log_rt)+(1|set)
          + (1|subject), data = xp3.sub.04)
AIC(m4a)
m4b<-lmer(log_rt ~ (trial+
                      log(duration)+previous_log_rt)+(1+trial|set)
          + (1|subject), data = xp3.sub.04)
AIC(m4b)
m4c<-lmer(log_rt ~ (trial+
                      log(duration)+previous_log_rt)+(1+previous_log_rt|set)
          + (1|subject), data = xp3.sub.04)
AIC(m4c)
m4d<-lmer(log_rt ~ (trial+
                      log(duration)+previous_log_rt)+(1|set)
          + (1+trial|subject), data = xp3.sub.04)
AIC(m4d)
m4e<-lmer(log_rt ~ (trial+
                      log(duration)+previous_log_rt)+(1|set)
          + (1+trial+previous_log_rt|subject), data = xp3.sub.04)
AIC(m4e)

final_model<-lmer(log_rt ~ (trial+
                              log(duration)+previous_log_rt)+(1|set)
                  + (1|subject), data = xp3.sub.04)
d<-xp3.sub.04[which(residuals(final_model) < mean(residuals(final_model))
                    +2.5 * sd(residuals(final_model)) & residuals(final_model)
                    > mean(residuals(final_model)) - 2.5 * 
                      sd(residuals(final_model))),]
final_model<-lmer(log_rt ~ (trial+
                              log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = d)
summary(final_model)
