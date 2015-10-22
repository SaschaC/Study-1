library(lme4)
library(LMERConvenienceFunctions)
library(lmtest)
#### Read in data ############
setwd(paste("W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/",
      "main experiment/Results - Compiled/nexp",sep=""))
setwd("~/Arbeit/Study 1/nexp/nexp")
pp

props_redPhontac<-get_error_proportions(xp3.sub.03,xp3.sub.03$red, xp3.sub.03$p_universally)
confints1<-get_prop_confints(xp3.sub.03$red,xp3.sub.03$p_universally,
                             xp3.sub.03$correctness,"reduced","wrong")
confints2<-get_prop_confints(xp3.sub.03$red,xp3.sub.03$p_universally,
                             xp3.sub.03$correctness,"full","wrong")
confints_props<-cbind(confints1,confints2)    
props_redPhontac
draw_plot(props_redPhontac,confints_props,c(0,1),"Error Proportions")

props_phontacSpelling<-get_error_proportions(xp3.sub.03,xp3.sub.03$p_universally, xp3.sub.03$spelling)
confints1<-get_prop_confints(xp3.sub.03$p_universally,xp3.sub.03$spelling,
                             xp3.sub.03$correctness,"I","wrong")
confints2<-get_prop_confints(xp3.sub.03$p_universally,xp3.sub.03$spelling,
                             xp3.sub.03$correctness,"L","wrong")
confints_props<-cbind(confints1,confints2)    
props_phontacSpelling
draw_plot(props_phontacSpelling,confints_props,c(0,1),"Error Proportions")

# RTs
rts_redSpelling<-get_rts(xp3.sub.03,xp3.sub.03$red, xp3.sub.03$spelling)
draw_plot(rts_redSpelling[[1]], rts_redSpelling[[2]],c(800,2000),label=("RT (ms)"))

rts_redPhontac<-get_rts(xp3.sub.03,xp3.sub.03$red, xp3.sub.03$p_universally)
rts_redPhontac[[1]]

rts_phontacSpelling<-get_rts(xp3.sub.03,xp3.sub.03$p_universally, xp3.sub.03$spelling)
rts_phontacSpelling[[1]]
### stats, group comparison
#age
mean(sapply(split(xp3_lexdec$age,xp3_lexdec$subject),unique))
range(sapply(split(xp3_lexdec$age,xp3_lexdec$subject),unique))
# vocabulary test
table(xp3_voctest$voctest_correctness,xp3_voctest$spelling)
prop.table(table(xp3_voctest$voctest_correctness))
prop.table(table(xp3_voctest$voctest_correctness,xp3_voctest$spelling),2)
chisq.test(table(xp3_voctest$voctest_correctness,xp3_voctest$spelling))
# av_prof
mean(xp3_questinf$av_prof)
range(xp3_questinf$av_prof)
mean(xp3_questinf$av_prof[xp3_questinf$spelling=="-spelling"])
mean(xp3_questinf$av_prof[xp3_questinf$spelling=="+spelling"])
t.test(xp3_questinf$av_prof[xp3_questinf$spelling=="-spelling"], 
       xp3_questinf$av_prof[xp3_questinf$spelling=="+spelling"])
#years French
mean(xp3_questinf$jaar)
range(xp3_questinf$jaar)
mean(xp3_questinf$jaar[xp3_questinf$spelling=="-spelling"])
mean(xp3_questinf$jaar[xp3_questinf$spelling=="+spelling"])
t.test(xp3_questinf$jaar[xp3_questinf$spelling=="-spelling"], 
       xp3_questinf$jaar[xp3_questinf$spelling=="+spelling"])
#lextale
get_score<-function(d) {
  correct.words<-length(which(d$Stimulus.ACC == 1 & d$CorrectRight == 5))
  incorrect.pseudo<-length(which(d$Stimulus.ACC == 0 & d$CorrectRight == 1))
  score<-correct.words - (2*incorrect.pseudo)
  score
}
spelling<-xp3_lextale[xp3_lextale$spelling=="+spelling",]
spelling$subject<-factor(spelling$subject)
nospelling<-xp3_lextale[xp3_lextale$spelling=="-spelling",]
nospelling$subject<-factor(nospelling$subject)

score_all<-sapply(split(xp3_lextale,
                        xp3_lextale$subject), get_score)

score_all["23"]<--1.56
score_all["3007"]<--1.56
str(score_all)
mean(score_all)
range(score_all)

score1<-sapply(split(nospelling,
                     nospelling$subject), get_score)
score1["23"]<--1.56
score1["3007"]<--1.56
mean(score1)
score2<-sapply(split(spelling,
                     spelling$subject), get_score)
mean(score2)
t.test(score1, score2)

### stats lexical decision
#props
m0 = glmer(correctness ~ (spelling*red*p_universally+
                            voctest_correctness + trial)  + (1|set) + 
             (1|subject), family = binomial(logit), data = xp3.sub.03)
summary(m0)
m1<- bfFixefLMER_t.fnc(m0, reset.REML.TRUE = FALSE,t.threshold=1.97)
summary(m1)
m2 <- ffRanefLMER.fnc(model = m1, ran.effects = 
                        list(ran.intercepts = c("set","subject"), slopes = c(
                          "red", "p_universally","spelling","trial"), 
                          corr = c(1, 1,1,1), by.vars = c("set","subject")
                        )) 
summary(m2)
test<-glmer(correctness ~ spelling + red + p_universally + trial + (1+red|set) + 
              (1 | subject) + spelling:red + spelling:p_universally + 
              red:p_universally, family = binomial(logit), data = xp3.sub.03)
#RTs
m0 = lmer(log_rt ~ (spelling*red*p_universally + 
                      voctest_correctness + trial+previous_log_rt)  + (1|set) +  
            (1|subject), data = xp3.sub.04)
summary(m0)

m1<- bfFixefLMER_F.fnc(m0, reset.REML.TRUE = FALSE)

summary(m1)
m2 <- ffRanefLMER.fnc(model = m1, ran.effects = 
                        list(ran.intercepts = c("set", "subject"), slopes = c(
                          "red", "p_universally","previous_log_rt","trial"), 
                          corr = c(1, 1,1,1,1), by.vars = c("set", "subject")
                        )) 
summary(m2)
m3<-bfFixefLMER_F.fnc(m2, reset.REML.TRUE = FALSE)
summary(m3)
