################### Statistical Modeling - ALL ################################
########################################################################
library(languageR)
library(lme4)
library(Hmisc)

### backfit 1

m1 = glmer(correctness ~ (red*spelling*p_universally + trial
                         + voctest_correctness)  + (1|set) 
          + (1|subject_oexp), data = xp2.sub.03, family = binomial(logit))
summary(m1)
drop1(m1)
m2 = update(m1, ~. -red:spelling:p_universally)
summary(m2)
anova(m1, m2)
drop1(m2)
m3 = update(m2, ~. -spelling:p_universally)
summary(m3)
drop1(m3)
m4 = update(m3, ~. -red:spelling)
summary(m4)
drop1(m4)
m5 = update(m4, ~. -spelling)
summary(m5)
drop1(m5)
m6 = update(m5, ~. -red:p_universally)
summary(m6)
drop1(m6)
m7 = glmer(correctness ~ (trial+ voctest_correctness)  + (1|set) 
           + (1|subject_oexp), data = xp2.sub.03, family = binomial(logit))

summary(m7)
#Forward fit 1
m7a = glmer(correctness ~ (red + trial + voctest_correctness)
           + (1+red|set) + (1|subject_oexp),family=binomial(logit),
           data = xp2.sub.03);
AIC(m7a)
m7b = glmer(correctness ~ (red + trial + voctest_correctness)
            + (1+red+trial|set) + (1|subject_oexp),family=binomial(logit),
            data = xp2.sub.03);
AIC(m7b)
m7c = glmer(correctness ~ (red + trial + voctest_correctness)
            + (1+red+trial+voctest_correctness|set) + (1|subject_oexp),family=binomial(logit),
            data = xp2.sub.03);
AIC(m7c)
m7d = glmer(correctness ~ (red + trial + voctest_correctness)
            + (1+red+trial|set) + (1|subject_oexp),family=binomial(logit),
            data = xp2.sub.03);
AIC(m7d)
m7e = glmer(correctness ~ (red + trial + voctest_correctness)
            + (1+red+trial|set) + (1+red|subject_oexp),family=binomial(logit),
            data = xp2.sub.03);
AIC(m7e)
m7f = glmer(correctness ~ (red + trial + voctest_correctness)
            + (1+red+trial|set) + (1+red|subject_oexp),family=binomial(logit),
            data = xp2.sub.03);
AIC(m7f)
m7g = glmer(correctness ~ (red + trial + voctest_correctness)
            + (1+red+trial|set) + (1+red+trial|subject_oexp),family=binomial(logit),
            data = xp2.sub.03);
AIC(m7g)

final_model<-m7e
summary(final_model)
#######
################################################RTs
m1<-lmer(log_rt ~ (red*spelling*p_universally+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject_oexp), data = xp1.sub.04, REML = FALSE)
d<-xp1.sub.04[which(residuals(m1) < mean(residuals(m1))
                    +2.5 * sd(residuals(m1)) & residuals(m1)
                    > mean(residuals(m1)) - 2.5 * 
                      sd(residuals(m1))),]
m1<-lmer(log_rt ~ (red*spelling*p_universally+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject_oexp), data = d, REML = FALSE)
summary(m1)
drop1(m1)
m2<-lmer(log_rt ~ (red+spelling+p_universally+trial+
                     log(duration)+previous_log_rt+
                     red:spelling+red:p_universally+
                     spelling:p_universally)+(1|set)
         +(1|subject_oexp), data = xp1.sub.04, REML = FALSE)
d<-xp1.sub.04[which(residuals(m2) < mean(residuals(m2))
                    +2.5 * sd(residuals(m2)) & residuals(m2)
                    > mean(residuals(m2)) - 2.5 * 
                      sd(residuals(m2))),]
m2<-lmer(log_rt ~ (red+spelling+p_universally+trial+
                     log(duration)+previous_log_rt+
                     red:spelling+red:p_universally+
                     spelling:p_universally)+(1|set)
         +(1|subject_oexp), data = d, REML = FALSE)
summary(m2)
drop1(m2)
m3<-lmer(log_rt ~ (red+spelling+p_universally+trial+
                     log(duration)+previous_log_rt+
                     red:spelling+red:p_universally
                     )+(1|set)
         +(1|subject_oexp), data = xp1.sub.04, REML = FALSE)
d<-xp1.sub.04[which(residuals(m3) < mean(residuals(m3))
                    +2.5 * sd(residuals(m3)) & residuals(m3)
                    > mean(residuals(m3)) - 2.5 * 
                      sd(residuals(m3))),]
m3<-lmer(log_rt ~ (red+spelling+p_universally+trial+
                     log(duration)+previous_log_rt+
                     red:spelling+red:p_universally)+(1|set)
         +(1|subject_oexp), data = d, REML = FALSE)
summary(m3)
drop1(m3)
m4<-lmer(log_rt ~ (red+spelling+p_universally+trial+
                     log(duration)+previous_log_rt
                   +red:p_universally)+(1|set)
+(1|subject_oexp), data = xp1.sub.04, REML = FALSE)
d<-xp1.sub.04[which(residuals(m4) < mean(residuals(m4))
                    +2.5 * sd(residuals(m4)) & residuals(m4)
                    > mean(residuals(m4)) - 2.5 * 
                      sd(residuals(m4))),]
m4<-lmer(log_rt ~ (red+spelling+p_universally+trial+
                     log(duration)+previous_log_rt
                     +red:p_universally)+(1|set)
         +(1|subject_oexp), data = d, REML = FALSE)
summary(m4)
drop1(m4)
m5<-lmer(log_rt ~ (red+p_universally+trial+
                     log(duration)+previous_log_rt
                   +red:p_universally)+(1|set)
         +(1|subject_oexp), data = xp1.sub.04, REML = FALSE)
d<-xp1.sub.04[which(residuals(m5) < mean(residuals(m5))
                    +2.5 * sd(residuals(m5)) & residuals(m5)
                    > mean(residuals(m5)) - 2.5 * 
                      sd(residuals(m5))),]
m5<-lmer(log_rt ~ (red+p_universally+trial+
                     log(duration)+previous_log_rt
                   +red:p_universally)+(1|set)
         +(1|subject_oexp), data = d, REML = FALSE)
summary(m5)
drop1(m5)
#forward fit
m5a<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)+red:p_universally)+(1|set)
          +(1|subject_oexp), data = xp1.sub.04, REML = TRUE)
summary(m5a)
m6b<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)++red:p_universally)+(1+red|set)
          +(1|subject_oexp), data = xp1.sub.04, REML = TRUE)
summary(m6b)
m6c<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)++red:p_universally)+(1+p_universally|set)
          +(1|subject_oexp), data = xp1.sub.04, REML = TRUE)
summary(m6c)
m6d<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)++red:p_universally)+(1+trial|set)
          +(1|subject_oexp), data = xp1.sub.04, REML = TRUE)
summary(m6d)
m6d<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)++red:p_universally)+(1+previous_log_rt|set)
          +(1|subject_oexp), data = xp1.sub.04, REML = TRUE)
summary(m6d)
m6e<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)++red:p_universally)+(1|set)
          +(1+red|subject_oexp), data = xp1.sub.04, REML = TRUE)
summary(m6e)
m6f<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)++red:p_universally)+(1|set)
          +(1+p_universally|subject_oexp), data = xp1.sub.04, REML = TRUE)
summary(m6f)



