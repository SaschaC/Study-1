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
m1<-lmer(log_rt ~ (red*spelling*p_universally+trial+
                     log(duration)+previous_log_rt+voctest_correctness)+(1|set)
         + (1|subject_oexp), data = xp2.sub.04)
d<-xp2.sub.04[which(residuals(m1) < mean(residuals(m1))
                    +2.5 * sd(residuals(m1)) & residuals(m1)
                    > mean(residuals(m1)) - 2.5 * 
                      sd(residuals(m1))),]
m1<-lmer(log_rt ~ (red*spelling*p_universally+trial+
                     log(duration)+previous_log_rt+voctest_correctness)+(1|set)
         + (1|subject_oexp), data = d)
summary(m1)
drop1(m1)
m2<-lmer(log_rt ~ (red*spelling*p_universally+
                       log(duration)+previous_log_rt+voctest_correctness)+(1|set)
         + (1|subject_oexp), data = xp2.sub.04)
d<-xp2.sub.04[which(residuals(m2) < mean(residuals(m2))
                    +2.5 * sd(residuals(m2)) & residuals(m2)
                    > mean(residuals(m2)) - 2.5 * 
                        sd(residuals(m2))),]
m2<-lmer(log_rt ~ (red*spelling*p_universally+
                       log(duration)+previous_log_rt+voctest_correctness)+(1|set)
         + (1|subject_oexp), data = d)
summary(m2)
drop1(m2)
#forward fit 1
m2a<-lmer(log_rt ~ (red*spelling*p_universally+
                         log(duration)+previous_log_rt+voctest_correctness)+(1|set)
          + (1|subject_oexp), data = xp2.sub.04)
AIC(m2a)
m2b<-lmer(log_rt ~ (red*spelling*p_universally+
                        log(duration)+previous_log_rt+voctest_correctness)+(1+red|set)
          + (1|subject_oexp), data = xp2.sub.04)
AIC(m2b)
m2c<-lmer(log_rt ~ (red*spelling*p_universally+
                        log(duration)+previous_log_rt+voctest_correctness)+(1+spelling|set)
          + (1|subject_oexp), data = xp2.sub.04)
AIC(m2c)
m2d<-lmer(log_rt ~ (red*spelling*p_universally+
                        log(duration)+previous_log_rt+voctest_correctness)+(1+previous_log_rt|set)
          + (1|subject_oexp), data = xp2.sub.04)
AIC(m2d)
m2e<-lmer(log_rt ~ (red*spelling*p_universally+
                        log(duration)+previous_log_rt+voctest_correctness)+(1+voctest_correctness|set)
          + (1|subject_oexp), data = xp2.sub.04)
AIC(m2e)
m2f<-lmer(log_rt ~ (red*spelling*p_universally+
                        log(duration)+previous_log_rt+voctest_correctness)+(1|set)
          + (1+red|subject_oexp), data = xp2.sub.04)
AIC(m2f)
m2f<-lmer(log_rt ~ (red*spelling*p_universally+
                        log(duration)+previous_log_rt+voctest_correctness)+(1|set)
          + (1+red+p_universally|subject_oexp), data = xp2.sub.04)
AIC(m2f)
m2g<-lmer(log_rt ~ (red*spelling*p_universally+
                        log(duration)+previous_log_rt+voctest_correctness)+(1|set)
          + (1+red+log(duration)|subject_oexp), data = xp2.sub.04)
AIC(m2g)
m2h<-lmer(log_rt ~ (red*spelling*p_universally+
                        log(duration)+previous_log_rt+voctest_correctness)+(1|set)
          + (1+red+previous_log_rt|subject_oexp), data = xp2.sub.04)
AIC(m2h)
m2i<-lmer(log_rt ~ (red*spelling*p_universally+
                        log(duration)+previous_log_rt+voctest_correctness)+(1|set)
          + (1+red+previous_log_rt+voctest_correctness|subject_oexp), data = xp2.sub.04)
AIC(m2i)
###backfit 2
m3<-lmer(log_rt ~ (red*spelling*p_universally+
                                 log(duration)+previous_log_rt+voctest_correctness)+(1|set)
                   + (1+red+previous_log_rt|subject_oexp), data = xp2.sub.04)
d<-xp2.sub.04[which(residuals(m3) < mean(residuals(m3))
                    +2.5 * sd(residuals(m3)) & residuals(m3)
                    > mean(residuals(m3)) - 2.5 * 
                        sd(residuals(m3))),]
m3<-lmer(log_rt~(red*spelling*p_universally+
                                 log(duration)+previous_log_rt+voctest_correctness)+(1|set)
                   + (1+red+previous_log_rt|subject_oexp), data = d)
summary(m3)
anova(m2,m3)
drop1(m3)
final_model<-m3
## interactions
d.red = subset(d, red == "reduced")
d.full = subset(d, red == "full")
d.legal = subset(d, p_universally == "L")
d.illegal = subset(d, p_universally == "I")
d.spelling = subset(d, spelling == "+spelling")
d.nospelling = subset(d, spelling == "-spelling")

#red
m1.red<-lmer(log_rt ~ (spelling*p_universally+
                     log(duration)+previous_log_rt+voctest_correctness)+(1|set)
+ (1+previous_log_rt|subject_oexp), data = d.red)
summary(m1.red)
drop1(m1.red)
m2.red<-lmer(log_rt ~ (spelling*p_universally+
                           log(duration)+previous_log_rt)+(1|set)
             + (1+previous_log_rt|subject_oexp), data = d.red)
summary(m2.red)
drop1(m2.red)
m3.red<-lmer(log_rt ~ (spelling*p_universally+
                           previous_log_rt)+(1|set)
             + (1+previous_log_rt|subject_oexp), data = d.red)
summary(m3.red)
#full
m1.full<-lmer(log_rt ~ (spelling*p_universally+
                           log(duration)+previous_log_rt+voctest_correctness)+(1|set)
             + (1+previous_log_rt|subject_oexp), data = d.full)
summary(m1.full)
drop1(m1.full)
m2.full<-lmer(log_rt ~ (spelling+p_universally+
                            log(duration)+previous_log_rt+voctest_correctness)+(1|set)
              + (1+previous_log_rt|subject_oexp), data = d.full)
summary(m2.full)
drop1(m2.full)
m3.full<-lmer(log_rt ~ (spelling+
                            log(duration)+previous_log_rt+voctest_correctness)+(1|set)
              + (1+previous_log_rt|subject_oexp), data = d.full)
summary(m3.full)
drop1(m3.full)
m4.full<-lmer(log_rt ~ (
                            log(duration)+previous_log_rt+voctest_correctness)+(1|set)
              + (1+previous_log_rt|subject_oexp), data = d.full)
summary(m4.full)
#legal
m1.legal<-lmer(log_rt~(red*spelling+
            log(duration)+previous_log_rt+voctest_correctness)+(1|set)
+ (1+red+previous_log_rt|subject_oexp),data=d.legal)
summary(m1.legal)
drop1(m1.legal)
m2.legal<-lmer(log_rt~(red*spelling+
                           log(duration)+previous_log_rt)+(1|set)
               + (1+red+previous_log_rt|subject_oexp),data=d.legal)
summary(m2.legal)
drop1(m2.legal)
m3.legal<-lmer(log_rt~(red+spelling+
                           log(duration)+previous_log_rt)+(1|set)
               + (1+red+previous_log_rt|subject_oexp),data=d.legal)
summary(m3.legal)
drop1(m3.legal)
m4.legal<-lmer(log_rt~(spelling+
                           log(duration)+previous_log_rt)+(1|set)
               + (1+red+previous_log_rt|subject_oexp),data=d.legal)
summary(m4.legal)
drop1(m4.legal)
m5.legal<-lmer(log_rt~(
                           log(duration)+previous_log_rt)+(1|set)
               + (1+red+previous_log_rt|subject_oexp),data=d.legal)
summary(m5.legal)
#illegal
m1.illegal<-lmer(log_rt~(red*spelling+
                           log(duration)+previous_log_rt+voctest_correctness)+(1|set)
               + (1+red+previous_log_rt|subject_oexp),data=d.illegal)
summary(m1.illegal)
#spelling
m1.spelling<-lmer(log_rt~(red*p_universally+
                             log(duration)+previous_log_rt+voctest_correctness)+(1|set)
                 + (1+red+previous_log_rt|subject_oexp),data=d.spelling)
summary(m1.spelling)
drop1(m1.spelling)
m2.spelling<-lmer(log_rt~(red+p_universally+
                              log(duration)+previous_log_rt+voctest_correctness)+(1|set)
                  + (1+red+previous_log_rt|subject_oexp),data=d.spelling)
summary(m2.spelling)
drop1(m2.spelling)
m3.spelling<-lmer(log_rt~(red+
                              log(duration)+previous_log_rt+voctest_correctness)+(1|set)
                  + (1+red+previous_log_rt|subject_oexp),data=d.spelling)
summary(m3.spelling)
drop1(m3.spelling)
m4.spelling<-lmer(log_rt~(red+
                              log(duration)+voctest_correctness)+(1|set)
                  + (1+red|subject_oexp),data=d.spelling)
summary(m4.spelling)
drop1(m4.spelling)
#nospelling
m1.nospelling<-lmer(log_rt~(red*p_universally+
                              log(duration)+previous_log_rt+voctest_correctness)+(1|set)
                  + (1+red+previous_log_rt|subject_oexp),data=d.nospelling)
summary(m1.nospelling)
drop1(m1.nospelling)
m2.nospelling<-lmer(log_rt~(red*p_universally+
                                +previous_log_rt+voctest_correctness)+(1|set)
                    + (1+red+previous_log_rt|subject_oexp),data=d.nospelling)
summary(m2.nospelling)
drop1(m2.nospelling)
m3.nospelling<-lmer(log_rt~(red*p_universally+
                                +previous_log_rt+voctest_correctness)+(1|set)
                    + (1+red+previous_log_rt|subject_oexp),data=d.nospelling)
summary(m3.nospelling)
drop1(m3.nospelling)
