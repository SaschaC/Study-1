library(languageR)
library(lme4)
library(Hmisc)

# 1. Comparison between +spelling and -spelling group:
# vocabulary test, average proficiency from questionnaires,
# years of French in school, lextale
## vocabulary test
table(xp1_voctest$voctest_correctness,xp1_voctest$spelling)
prop.table(table(xp1_voctest$voctest_correctness,xp1_voctest$spelling),2)
prop.table(table(xp1_voctest$voctest_correctness))
chisq.test(table(xp1_voctest$voctest_correctness,xp1_voctest$spelling))
## average proficiency from questionnaires
mean(xp1_questinf$av_prof[xp1_questinf$spelling=="-spelling"])
mean(xp1_questinf$av_prof[xp1_questinf$spelling=="+spelling"])
mean(xp1_questinf$av_prof)
range(xp1_questinf$av_prof)
t.test(xp1_questinf$av_prof[xp1_questinf$spelling=="-spelling"], 
       xp1_questinf$av_prof[xp1_questinf$spelling=="+spelling"])
## years of French
mean(xp1_questinf$jaar[xp1_questinf$spelling=="-spelling"])
mean(xp1_questinf$jaar[xp1_questinf$spelling=="+spelling"])
t.test(xp1_questinf$jaar[xp1_questinf$spelling=="-spelling"], 
       xp1_questinf$jaar[xp1_questinf$spelling=="+spelling"])
## lextale
get_score<-function(d) {
  correct.words<-length(which(d$Stimulus.ACC == 1 & d$CorrectRight == 5))
  incorrect.pseudo<-length(which(d$Stimulus.ACC == 0 & d$CorrectRight == 1))
  score<-correct.words - (2*incorrect.pseudo)
  score
}
spelling<-xp1_lextale[xp1_lextale$spelling=="+spelling",]
nospelling<-xp1_lextale[xp1_lextale$spelling=="-spelling",]
spelling$subject_oexp<-factor(spelling$subject_oexp)
nospelling$subject_oexp<-factor(nospelling$subject_oexp)
score_all<-sapply(split(xp1_lextale,
                        xp1_lextale$subject_oexp), get_score)
mean(score_all)
range(score_all)
score1<-sapply(split(nospelling,
                     nospelling$subject_oexp), get_score)
mean(score1)
score2<-sapply(split(spelling,
                     spelling$subject_oexp), get_score)
mean(score2)
t.test(score1, score2)
### backfit 1
m1 = glmer(correctness ~ (red*spelling
                         )  + (1|set) 
          + (1|subject_oexp), data = xp1.sub.03, family = binomial(logit))
summary(m1)
# 2. Linear regressions: Accuracy
## Backfit 1
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
## Final model
final_model<-glmer(correctness ~ (red*spelling
)  + (1+red|set) 
+ (1|subject_oexp), data = xp1.sub.03, family = binomial(logit))
summary(final_model)
# Linear regression: RTs
## Backfit 1
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
## Forward fit 1
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
## Backfit 2
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
final_model<-m4
summary(final_model)
