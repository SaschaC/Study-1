################### Statistical Modeling - ALL ################################
########################################################################
library(languageR)
library(lme4)
library(Hmisc)

### backfit 1
m1 = glmer(correctness ~ (red*spelling
                         )  + (1|set) 
          + (1|subject_oexp), data = xp1.sub.03, family = binomial(logit))
summary(m1)
#Backfit 1

m1a<- glmer(correctness ~ (red*spelling
)  + (1+red|set) 
+ (1|subject_oexp), data = xp1.sub.03, family = binomial(logit))
AIC(m1a)
m1b<- glmer(correctness ~ (red*spelling
)  + (1+red+spelling|set) 
+ (1|subject_oexp), data = xp1.sub.03, family = binomial(logit))
AIC(m1b)
m1c<- glmer(correctness ~ (red*spelling
)  + (1+red|set) 
+ (1+red|subject_oexp), data = xp1.sub.03, family = binomial(logit))
AIC(m1c)

final_model<-glmer(correctness ~ (red*spelling
)  + (1+red|set) 
+ (1|subject_oexp), data = xp1.sub.03, family = binomial(logit))
summary(final_model)
########## check interactions
d.red = subset(xp1.sub.03, red == "reduced")
d.full = subset(xp1.sub.03, red == "full")
d.spelling = subset(xp1.sub.03, spelling == "+spelling")
d.nospelling = subset(xp1.sub.03, spelling == "-spelling")
## reduced
m1.red = glmer(correctness ~ (spelling) + (1|set) + (1|subject_oexp), 
               data = d.red, family = binomial(logit));
summary(m1.red)
##full
m1.full = glmer(correctness ~ (spelling) + (1|set) + (1|subject_oexp), 
                data = d.full, family = binomial(logit));
summary(m1.full)
##spelling
m1.spelling<-glmer(correctness ~ (red) + (1+red|set) 
                   + (1|subject_oexp), data = d.spelling, family = binomial(logit));
summary(m1.spelling)
##nospelling
m1.nospelling<-glmer(correctness ~ (red) + (1+red|set) 
                   + (1|subject_oexp), data = d.nospelling, family = binomial(logit));
summary(m1.nospelling)
################################################RTs
#Backfit 1
m1<-lmer(log_rt ~ (red*spelling+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject_oexp), data = xp1.sub.04)
d<-xp1.sub.04[which(residuals(m1) < mean(residuals(m1))
                    +2.5 * sd(residuals(m1)) & residuals(m1)
                    > mean(residuals(m1)) - 2.5 * 
                      sd(residuals(m1))),]
m1<-lmer(log_rt ~ (red*spelling+trial+
                       log(duration)+previous_log_rt)+(1|set)
         + (1|subject_oexp), data = d)
summary(m1)
drop1(m1)
m2<-lmer(log_rt ~ (red+spelling+trial+
                     log(duration)+previous_log_rt
                     )+(1|set)
         +(1|subject_oexp), data = xp1.sub.04)
d<-xp1.sub.04[which(residuals(m2) < mean(residuals(m2))
                    +2.5 * sd(residuals(m2)) & residuals(m2)
                    > mean(residuals(m2)) - 2.5 * 
                      sd(residuals(m2))),]
m2<-lmer(log_rt ~ (red+spelling+trial+
                       log(duration)+previous_log_rt)+(1|set)
+(1|subject_oexp), data = d)
summary(m2)
drop1(m2)
m3<-lmer(log_rt ~ (red+trial+
                       log(duration)+previous_log_rt
)+(1|set)
+(1|subject_oexp), data = xp1.sub.04)
d<-xp1.sub.04[which(residuals(m3) < mean(residuals(m3))
                    +2.5 * sd(residuals(m3)) & residuals(m3)
                    > mean(residuals(m3)) - 2.5 * 
                        sd(residuals(m3))),]
m3<-lmer(log_rt ~ (red+trial+
                       log(duration)+previous_log_rt)+(1|set)
         +(1|subject_oexp), data = d)
summary(m3)
drop1(m3)
#forward fit 1
m3a<-lmer(log_rt ~ (red+trial+
                       log(duration)+previous_log_rt)+(1|set)
+(1|subject_oexp), data = xp1.sub.04)
AIC(m3a)
m3b<-lmer(log_rt ~ (red+trial+
                        log(duration)+previous_log_rt)+(1+red|set)
          +(1|subject_oexp), data = xp1.sub.04)
AIC(m3b)
m3c<-lmer(log_rt ~ (red+trial+
                        log(duration)+previous_log_rt)+(1+red+trial|set)
          +(1|subject_oexp), data = xp1.sub.04)
AIC(m3c)
m3d<-lmer(log_rt ~ (red+trial+
                        log(duration)+previous_log_rt)+(1+red+previous_log_rt|set)
          +(1|subject_oexp), data = xp1.sub.04)
AIC(m3d)
m3e<-lmer(log_rt ~ (red+trial+
                        log(duration)+previous_log_rt)+(1+red|set)
          +(1+red|subject_oexp), data = xp1.sub.04)
AIC(m3e)
m3f<-lmer(log_rt ~ (red+trial+
                        log(duration)+previous_log_rt)+(1+red|set)
          +(1+red+trial|subject_oexp), data = xp1.sub.04)
AIC(m3f)
m3f<-lmer(log_rt ~ (red+trial+
                        log(duration)+previous_log_rt)+(1+red|set)
          +(1+red+log(duration)|subject_oexp), data = xp1.sub.04)
AIC(m3f)
m3g<-lmer(log_rt ~ (red+trial+
                        log(duration)+previous_log_rt)+(1+red|set)
          +(1+red+previous_log_rt|subject_oexp), data = xp1.sub.04)
AIC(m3g)
#Backfit 2
m4<-lmer(log_rt ~ (red+trial+
                       log(duration)+previous_log_rt)+(1+red|set)
         +(1+red|subject_oexp), data = xp1.sub.04)
d<-xp1.sub.04[which(residuals(m4) < mean(residuals(m4))
                    +2.5 * sd(residuals(m4)) & residuals(m4)
                    > mean(residuals(m4)) - 2.5 * 
                        sd(residuals(m4))),]
m4<-lmer(log_rt ~ (red+trial+
                       log(duration)+previous_log_rt)+(1+red|set)
         +(1+red|subject_oexp), data = d)
summary(m4)
