################### Statistical Modeling - ALL ################################
########################################################################
library(languageR)
library(lme4)
library(Hmisc)

### backfit 1
m1 = glmer(correctness ~ (red*spelling*p_universally + trial
                         + voctest_correctness)  + (1|set) 
          + (1|subject_oexp), data = xp1.sub.03, family = binomial(logit))
summary(m1)
drop1(m1)
m2 = update(m1, ~. -red:spelling:p_universally)
summary(m2)
anova(m1, m2)
drop1(m2)
m3 = update(m2, ~. -voctest_correctness)
summary(m3)
# Random |set
m4a = glmer(correctness ~ (spelling + red + p_universally 
           + trial + red:p_universally + red:spelling + 
             spelling:p_universally)
           + (1+spelling|set) + (1|subject_oexp),family=binomial(logit),
           data = xp1.sub.03);
summary(m4a)
m4b = glmer(correctness ~ (spelling + red + p_universally 
                           + trial + red:p_universally + red:spelling
                           + spelling:p_universally)
            + (1+red|set) + (1|subject_oexp),family=binomial(logit),
            data = xp1.sub.03);
summary(m4b)
m4c = glmer(correctness ~ (spelling + red + p_universally 
                           + trial + red:p_universally + red:spelling
                           + spelling:p_universally)
            + (1+red+trial|set) + (1|subject_oexp),family=binomial(logit),
            data = xp1.sub.03);
summary(m4c)

#forward fit 1
m4d = glmer(correctness ~ (spelling + red + p_universally 
                           + trial + red:p_universally + red:spelling
                           + spelling:p_universally)
            + (1+red|set) + (1+red|subject_oexp),family=binomial(logit),
            data = xp1.sub.03);
summary(m4d)
m4e = glmer(correctness ~ (spelling + red + p_universally 
                           + trial + red:p_universally + red:spelling
                           + spelling:p_universally)
            + (1+red|set) + (1+p_universally|subject_oexp),family=binomial(logit),
            data = xp1.sub.03);
summary(m4e)
m4f = glmer(correctness ~ (spelling + red + p_universally 
                           + trial + red:p_universally + red:spelling
                           + spelling:p_universally)
            + (1+red|set) + (1+trial|subject_oexp),family=binomial(logit),
            data = xp1.sub.03);
summary(m4f)
# backfit 2
m5<-glmer(correctness ~ (trial+red+spelling+p_universally+
                           red:p_universally+spelling:red+
                           spelling:p_universally
                         )  + (1+red|set) 
          + (1|subject_oexp), data = xp1.sub.03, family = binomial(logit))
summary(m5)
drop1(m5)
m6 = update(m5, ~. -red:p_universally)
summary(m6)
#forwardfit 2
m6a<-glmer(correctness ~ (trial+red+spelling+p_universally+
                            +spelling:red+
                            spelling:p_universally
)  + (1+red+spelling|set) 
+ (1|subject_oexp), data = xp1.sub.03, family = binomial(logit))
summary(m6a)
m6b<-glmer(correctness ~ (trial+red+spelling+p_universally+
                            +spelling:red+
                            spelling:p_universally
)  + (1+red+trial|set) 
+ (1|subject_oexp), data = xp1.sub.03, family = binomial(logit))
summary(m6b)
m6c<-glmer(correctness ~ (trial+red+spelling+p_universally+
                            +spelling:red+
                            spelling:p_universally
)  + (1+red+trial|set) 
+ (1+red|subject_oexp), data = xp1.sub.03, family = binomial(logit))
summary(m6c)
m6d<-glmer(correctness ~ (trial+red+spelling+p_universally+
                            +spelling:red+
                            spelling:p_universally
)  + (1+red+trial|set) 
+ (1+trial|subject_oexp), data = xp1.sub.03, family = binomial(logit))
summary(m6d)
m6e<-glmer(correctness ~ (trial+red+spelling+p_universally+
                            +spelling:red+
                            spelling:p_universally
)  + (1+red+trial|set) 
+ (1+p_universally|subject_oexp), data = xp1.sub.03, family = binomial(logit))
summary(m6e)

final_model<-m6
summary(m6)
########## check interactions
d.red = subset(xp1.sub.03, red == "reduced")
d.full = subset(xp1.sub.03, red == "full")
d.legal = subset(xp1.sub.03, p_universally == "L")
d.illegal = subset(xp1.sub.03, p_universally == "I")
d.spelling = subset(xp1.sub.03, spelling == "+spelling")
d.nospelling = subset(xp1.sub.03, spelling == "-spelling")

## reduced
m1.red = glmer(correctness ~ (spelling*p_universally+trial) + (1|set) + (1|subject_oexp), 
               data = d.red, family = binomial(logit));
summary(m1.red)
m2.red = glmer(correctness ~ (spelling*p_universally) + (1|set) + (1|subject_oexp), 
               data = d.red, family = binomial(logit));
summary(m2.red)
m3.red = glmer(correctness ~ (spelling+p_universally) + (1|set) + (1|subject_oexp), 
               data = d.red, family = binomial(logit));
summary(m3.red)
##full
m1.full = glmer(correctness ~ (spelling*p_universally+trial) + (1|set) + (1|subject_oexp), 
                data = d.full, family = binomial(logit));
summary(m1.full)
m2.full = glmer(correctness ~ (spelling+p_universally+trial) + (1|set) + (1|subject_oexp), 
                data = d.full, family = binomial(logit));
summary(m2.full)
m3.full = glmer(correctness ~ (spelling+trial) + (1|set) + (1|subject_oexp), 
                data = d.full, family = binomial(logit));
summary(m3.full)
##legal
m1.legal<-glmer(correctness ~ (spelling+trial+red+red:spelling) + (1+red|set) + 
                    (1|subject_oexp), 
                data = d.legal, family = binomial(logit));
summary(m1.legal)
m2.legal<-glmer(correctness ~ (spelling+trial+red+red:spelling) + (1+red|set) + 
                    (1|subject_oexp), 
                data = d.legal, family = binomial(logit));
summary(m2.legal)
##illegal
m1.illegal<-glmer(correctness ~ (spelling+trial+red+red:spelling) + (1+red|set) + (1|subject_oexp), 
                data = d.illegal, family = binomial(logit));
summary(m1.illegal)
##spelling
m1.spelling<-glmer(correctness ~ (p_universally+trial+red) + (1+red|set) 
                   + (1|subject_oexp), data = d.spelling, family = binomial(logit));
summary(m1.spelling)
m1.nospelling<-glmer(correctness ~ (p_universally+trial+red) + (1+red|set) 
                   + (1|subject_oexp), data = d.nospelling, family = binomial(logit));
summary(m1.nospelling)

################################################RTs
#Backfit 1
m1<-lmer(log_rt ~ (red*spelling*p_universally+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject_oexp), data = xp1.sub.04)
d<-xp1.sub.04[which(residuals(m1) < mean(residuals(m1))
                    +2.5 * sd(residuals(m1)) & residuals(m1)
                    > mean(residuals(m1)) - 2.5 * 
                      sd(residuals(m1))),]
m1<-lmer(log_rt ~ (red*spelling*p_universally+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject_oexp), data = d)
summary(m1)
drop1(m1)
m2<-lmer(log_rt ~ (red+spelling+p_universally+trial+
                     log(duration)+previous_log_rt+
                     red:spelling+red:p_universally+
                     spelling:p_universally)+(1|set)
         +(1|subject_oexp), data = xp1.sub.04)
d<-xp1.sub.04[which(residuals(m2) < mean(residuals(m2))
                    +2.5 * sd(residuals(m2)) & residuals(m2)
                    > mean(residuals(m2)) - 2.5 * 
                      sd(residuals(m2))),]
m2<-lmer(log_rt ~ (red+spelling+p_universally+trial+
                     log(duration)+previous_log_rt+
                     red:spelling+red:p_universally+
                     spelling:p_universally)+(1|set)
         +(1|subject_oexp), data = d)
summary(m2)
drop1(m2)
m3<-lmer(log_rt ~ (red+spelling+p_universally+trial+
                     log(duration)+previous_log_rt+
                     red:spelling+red:p_universally
                     )+(1|set)
         +(1|subject_oexp), data = xp1.sub.04)
d<-xp1.sub.04[which(residuals(m3) < mean(residuals(m3))
                    +2.5 * sd(residuals(m3)) & residuals(m3)
                    > mean(residuals(m3)) - 2.5 * 
                      sd(residuals(m3))),]
m3<-lmer(log_rt ~ (red+spelling+p_universally+trial+
                     log(duration)+previous_log_rt+
                     red:spelling+red:p_universally)+(1|set)
         +(1|subject_oexp), data = d)
summary(m3)
drop1(m3)
m4<-lmer(log_rt ~ (red+spelling+p_universally+trial+
                     log(duration)+previous_log_rt
                   +red:p_universally)+(1|set)
+(1|subject_oexp), data = xp1.sub.04)
d<-xp1.sub.04[which(residuals(m4) < mean(residuals(m4))
                    +2.5 * sd(residuals(m4)) & residuals(m4)
                    > mean(residuals(m4)) - 2.5 * 
                      sd(residuals(m4))),]
m4<-lmer(log_rt ~ (red+spelling+p_universally+trial+
                     log(duration)+previous_log_rt
                     +red:p_universally)+(1|set)
         +(1|subject_oexp), data = d)
summary(m4)
drop1(m4)
m5<-lmer(log_rt ~ (red+p_universally+trial+
                     log(duration)+previous_log_rt
                   +red:p_universally)+(1|set)
         +(1|subject_oexp), data = xp1.sub.04)
d<-xp1.sub.04[which(residuals(m5) < mean(residuals(m5))
                    +2.5 * sd(residuals(m5)) & residuals(m5)
                    > mean(residuals(m5)) - 2.5 * 
                      sd(residuals(m5))),]
m5<-lmer(log_rt ~ (red+p_universally+trial+
                     log(duration)+previous_log_rt
                   +red:p_universally)+(1|set)
         +(1|subject_oexp), data = d)
summary(m5)
drop1(m5)
#forward fit 1
m5a<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)+red:p_universally)+(1|set)
          +(1|subject_oexp), data = xp1.sub.04)
AIC(m5a)
m6b<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)+red:p_universally)+(1+red|set)
          +(1|subject_oexp), data = xp1.sub.04)
AIC(m6b)
m6c<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)+red:p_universally)+(1+red+p_universally|set)
          +(1|subject_oexp), data = xp1.sub.04)
AIC(m6c)
m6d<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)+red:p_universally)+(1+red+trial|set)
          +(1|subject_oexp), data = xp1.sub.04)
AIC(m6d)
m6e<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)+red:p_universally)+(1+red+previous_log_rt|set)
          +(1|subject_oexp), data = xp1.sub.04)
AIC(m6e)
m6e<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)+red:p_universally)+(1+red|set)
          +(1+red|subject_oexp), data = xp1.sub.04)
AIC(m6e)
m6f<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)+red:p_universally)+(1|set)
          +(1+red+p_universally|subject_oexp), data = xp1.sub.04)
AIC(m6f)
m6g<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)+red:p_universally)+(1|set)
          +(1+red+trial|subject_oexp), data = xp1.sub.04)
AIC(m6g)
m6h<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)+red:p_universally)+(1|set)
          +(1+red+previous_log_rt|subject_oexp), data = xp1.sub.04)
AIC(m6h)
m6i<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +log(duration)+red:p_universally)+(1|set)
          +(1+red+log(duration)|subject_oexp), data = xp1.sub.04)
AIC(m6i)
#Backfit 2
m7<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                        +log(duration)+red:p_universally)+(1+red|set)
              +(1+red|subject_oexp), data = xp1.sub.04)
d<-xp1.sub.04[which(residuals(m7) < mean(residuals(m7))
                    +2.5 * sd(residuals(m7)) & residuals(m7)
                    > mean(residuals(m7)) - 2.5 * 
                        sd(residuals(m7))),]
m7<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                   +log(duration)+red:p_universally)+(1+red|set)
         +(1+red|subject_oexp), data = d)
summary(m7)
drop1(m7)
m8<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                   +red:p_universally)+(1+red|set)
         +(1+red|subject_oexp), data = xp1.sub.04)
d<-xp1.sub.04[which(residuals(m8) < mean(residuals(m8))
                    +2.5 * sd(residuals(m8)) & residuals(m8)
                    > mean(residuals(m8)) - 2.5 * 
                        sd(residuals(m8))),]
m8<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                   +red:p_universally)+(1+red|set)
         +(1+red|subject_oexp), data = d)
summary(m8)
#Forward fit 2
m8a<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +red:p_universally)+(1+red|set)
          +(1+red|subject_oexp), data = xp1.sub.04)
AIC(m8a)
m8b<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +red:p_universally)+(1+red+p_universally|set)
          +(1+red|subject_oexp), data = xp1.sub.04)
AIC(m8b)
m8c<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +red:p_universally)+(1+red+trial|set)
          +(1+red|subject_oexp), data = xp1.sub.04)
AIC(m8c)
m8d<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +red:p_universally)+(1+red+previous_log_rt|set)
          +(1+red|subject_oexp), data = xp1.sub.04)
AIC(m8d)
m8d<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +red:p_universally)+(1+red|set)
          +(1+red+p_universally|subject_oexp), data = xp1.sub.04)
AIC(m8d)
m8e<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +red:p_universally)+(1+red|set)
          +(1+red+trial|subject_oexp), data = xp1.sub.04)
AIC(m8e)
m8f<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                    +red:p_universally)+(1+red|set)
          +(1+red+previous_log_rt|subject_oexp), data = xp1.sub.04)
AIC(m8f)

final_model<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                            +red:p_universally)+(1+red|set)
                  +(1+red|subject_oexp), data = xp1.sub.04)
d<-xp1.sub.04[which(residuals(final_model) < mean(residuals(final_model))
                    +2.5 * sd(residuals(final_model)) & residuals(final_model)
                    > mean(residuals(final_model)) - 2.5 * 
                        sd(residuals(final_model))),]
final_model<-lmer(log_rt ~ (red+p_universally+trial+previous_log_rt
                   +red:p_universally)+(1+red|set)
         +(1+red|subject_oexp), data = d)
summary(final_model)
#interactions
d.red<-subset(d, red=="reduced")
d.full<-subset(d, red=="full")
d.legal<-subset(d,p_universally=="L")
d.illegal<-subset(d,p_universally=="I")

m1.red<-lmer(log_rt ~ (p_universally+trial+previous_log_rt
                       )+(1|set)
             +(1|subject_oexp), data = d.red)
summary(m1.red)
m1.full<-lmer(log_rt ~ (p_universally+trial+previous_log_rt
)+(1|set)
+(1|subject_oexp), data = d.full)
summary(m1.full)
m1.legal<-lmer(log_rt ~ (red+trial+previous_log_rt
)+(1+red|set)
+(1+red|subject_oexp), data = d.legal)
summary(m1.legal)
m1.illegal<-lmer(log_rt ~ (red+trial+previous_log_rt
)+(1+red|set)
+(1+red|subject_oexp), data = d.illegal)
summary(m1.illegal)
