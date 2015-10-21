################### Statistical Modeling - ALL ################################
########################################################################
library(languageR)
library(lme4)
library(Hmisc)

### backfit 1

m1 = glmer(correctness ~ (red*spelling)  + (1|set) 
          + (1|subject_oexp), data = xp2.sub.03, family = binomial(logit))
summary(m1)
drop1(m1)
m2 = update(m1, ~. -red:spelling)
summary(m2)
anova(m1, m2)
drop1(m2)
m3 = update(m2, ~. -spelling)
summary(m3)
anova(m2, m3)
drop1(m3)
#Forward fit 1
m3a = glmer(correctness ~ (red)
           + (1+red|set) + (1|subject_oexp),family=binomial(logit),
           data = xp2.sub.03);
AIC(m3a)
m3a = glmer(correctness ~ (red)
            + (1|set) + (1+red|subject_oexp),family=binomial(logit),
            data = xp2.sub.03);
AIC(m3a)

final_model<-glmer(correctness ~ (red)
                   + (1|set) + (1+red|subject_oexp),family=binomial(logit),
                   data = xp2.sub.03);
summary(final_model)
#######
################################################RTs
m1<-lmer(log_rt ~ (red*spelling+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject_oexp), data = xp2.sub.04)
d<-xp2.sub.04[which(residuals(m1) < mean(residuals(m1))
                    +2.5 * sd(residuals(m1)) & residuals(m1)
                    > mean(residuals(m1)) - 2.5 * 
                      sd(residuals(m1))),]
m1<-lmer(log_rt ~ (red*spelling+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject_oexp), data = d)
summary(m1)
drop1(m1)
m2<-lmer(log_rt ~ (red*spelling+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject_oexp), data = xp2.sub.04)
d<-xp2.sub.04[which(residuals(m2) < mean(residuals(m2))
                    +2.5 * sd(residuals(m2)) & residuals(m2)
                    > mean(residuals(m2)) - 2.5 * 
                      sd(residuals(m2))),]
m2<-lmer(log_rt ~ (red*spelling+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject_oexp), data = d)
summary(m2)
drop1(m2)
#forward fit 1
m1a<-lmer(log_rt ~ (red*spelling+
                         log(duration)+previous_log_rt)+(1|set)
          + (1|subject_oexp), data = xp2.sub.04)
AIC(m1a)
m2a<-lmer(log_rt ~ (red*spelling+
                      log(duration)+previous_log_rt)+(1+red|set)
          + (1|subject_oexp), data = xp2.sub.04)
AIC(m2a)
m3a<-lmer(log_rt ~ (red*spelling+
                      log(duration)+previous_log_rt)+(1+red+spelling|set)
          + (1|subject_oexp), data = xp2.sub.04)
AIC(m3a)
m4a<-lmer(log_rt ~ (red*spelling+
                      log(duration)+previous_log_rt)+(1+red+previous_log_rt|set)
          + (1|subject_oexp), data = xp2.sub.04)
AIC(m4a)
m5a<-lmer(log_rt ~ (red*spelling+
                      log(duration)+previous_log_rt)+(1+red+log(duration)|set)
          + (1|subject_oexp), data = xp2.sub.04)
AIC(m5a)
m6a<-lmer(log_rt ~ (red*spelling+
                      log(duration)+previous_log_rt)+(1+red|set)
          + (1+red|subject_oexp), data = xp2.sub.04)
AIC(m6a)
m7a<-lmer(log_rt ~ (red*spelling+
                      log(duration)+previous_log_rt)+(1+red|set)
          + (1+red|subject_oexp), data = xp2.sub.04)
AIC(m7a)
m8a<-lmer(log_rt ~ (red*spelling+
                      log(duration)+previous_log_rt)+(1+red|set)
          + (1+red+log(duration)|subject_oexp), data = xp2.sub.04)
AIC(m8a)
m9a<-lmer(log_rt ~ (red*spelling+
                      log(duration)+previous_log_rt)+(1+red|set)
          + (1+red+previous_log_rt|subject_oexp), data = xp2.sub.04)
AIC(m9a)
###backfit 2
m3<-lmer(log_rt ~ (red*spelling+
                     log(duration)+previous_log_rt)+(1+red|set)
         + (1+red+previous_log_rt|subject_oexp), data = xp2.sub.04)
d<-xp2.sub.04[which(residuals(m3) < mean(residuals(m3))
                    +2.5 * sd(residuals(m3)) & residuals(m3)
                    > mean(residuals(m3)) - 2.5 * 
                        sd(residuals(m3))),]
m3<-lmer(log_rt ~ (red*spelling+
                     log(duration)+previous_log_rt)+(1+red|set)
         + (1+red+previous_log_rt|subject_oexp), data = d)
summary(m3)
final_model<-m3
## interactions
d.red = subset(d, red == "reduced")
d.full = subset(d, red == "full")
d.spelling = subset(d, spelling == "+spelling")
d.nospelling = subset(d, spelling == "-spelling")
#red
m1.red<-lmer(log_rt ~ (spelling+
                     log(duration)+previous_log_rt)+(1|set)
+ (1+previous_log_rt|subject_oexp), data = d.red)
summary(m1.red)
drop1(m1.red)
m2.red<-lmer(log_rt ~ (spelling+
                           previous_log_rt)+(1|set)
             + (1+previous_log_rt|subject_oexp), data = d.red)
summary(m2.red)
drop1(m2.red)
#full
m1.full<-lmer(log_rt ~ (spelling+
                           log(duration)+previous_log_rt)+(1|set)
             + (1+previous_log_rt|subject_oexp), data = d.full)
summary(m1.full)
#spelling
m1.spelling<-lmer(log_rt~(red+
                             log(duration)+previous_log_rt)+(1+red|set)
                 + (1+red+previous_log_rt|subject_oexp),data=d.spelling)
summary(m1.spelling)
#nospelling
m1.nospelling<-lmer(log_rt~(red+
                              log(duration)+previous_log_rt)+(1+red|set)
                  + (1+red+previous_log_rt|subject_oexp),data=d.nospelling)
summary(m1.nospelling)
drop1(m1.nospelling)
