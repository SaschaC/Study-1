library(lme4)
library(LMERConvenienceFunctions)
library(lmtest)
# 1. Read in data
setwd(paste("W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/",
            "main experiment/Results - Compiled/nexp/data/xp3",sep=""))
## Read in lexicald decision(lexdec), vocabulary, (voctest),
## lextale, data from participant questionnaires (questinf),
## and phonotactic legality for each word (phonotactics)
xp3_lexdec <- read.delim("./day2_part1(lex_dec)/compiled_4.txt")
xp3_voctest <- read.delim("./day2_part2(voctest)/results_voctest_xp3.txt",head=FALSE)
xp3_lextale <- read.delim("./day2_part4(lextale)/results_lextale_xp3_all.txt")
xp3_questinf <- read.delim("pp_questinfo_xp3.txt", dec=",")
phonotactics<-read.delim("phonotactics.txt")
# 2. Data preparation
## remove last column of vocabulary test data (not needed) 
xp3_voctest = xp3_voctest[,-1]
## column names for vocabulary test
colnames(xp3_voctest) = c("file", "a1", "a2", "a3", "a4", "correct_answer", "assistant", "participant", "subject", "group", "response", "response_word", "voctest_correctness" )
## change 'Subject' column names of questionnaire and lextale data
colnames(xp3_questinf)[1]<-"subject"
colnames(xp3_lextale)[1]<-"subject"
##only leave those subjects in questinf and voctest, and 
##lextale that are also in the lexdec and verify the correct 
##number of subjects (=54)
xp3_questinf<-xp3_questinf[xp3_questinf$subject%in%xp3_lexdec$subject,]
xp3_questinf$subject<-factor(xp3_questinf$subject)
length(unique(xp3_questinf$subject))
xp3_voctest<-xp3_voctest[xp3_voctest$subject%in%xp3_lexdec$subject,]
xp3_voctest$subject<-factor(xp3_voctest$subject)
length(unique(xp3_voctest$subject))
xp3_lextale<-xp3_lextale[xp3_lextale$subject%in%xp3_lexdec$subject,]
xp3_lextale$subject<-factor(xp3_lextale$subject)
length(unique(xp3_lextale$subject))
unique(xp3_lexdec$subject)[which(!(unique(xp3_lexdec$subject)%in%xp3_lextale$subject))]
## add 'Spelling' columns
xp3_lexdec$spelling<-as.factor(ifelse(xp3_lexdec$group==1,"-spelling","+spelling"))
xp3_voctest$spelling<-ifelse(xp3_voctest$group==1,"-spelling","+spelling")
xp3_lextale<-merge(xp3_lextale,unique(xp3_voctest[,c("subject","group")]), by.x="subject",by.y="subject",all.x=TRUE)
xp3_lextale$spelling<-ifelse(xp3_lextale$group==1,"-spelling","+spelling")
xp3_questinf<-merge(xp3_questinf,unique(xp3_voctest[,c("subject","group")]), by.x="subject",by.y="subject",all.x=TRUE)
xp3_questinf$spelling<-ifelse(xp3_questinf$group==1,"-spelling","+spelling")
## rename levels of Response Correctness, and Reduction for lexical decision task
xp3_lexdec$correctness<-as.factor(xp3_lexdec$correctness)
levels(xp3_lexdec$correctness)<-c("wrong","correct")
xp3_lexdec$red<-as.factor(xp3_lexdec$red)
levels(xp3_lexdec$red)<-c("reduced","full")
### add "set"-column
xp3_voctest$set <- sapply(as.character(xp3_voctest$file), function(x) 
  strsplit(x, split = ("_"))[[1]][1])
xp3_lexdec$set <- sapply(as.character(xp3_lexdec$file), function(x) 
  strsplit(x, split = ("_"))[[1]][1])
## merge lexical decision data with vocabulary test, 
## questionnare info, and phonotactics
xp3_lexdec$order <- seq(1:nrow(xp3_lexdec))
xp3_voctest$subject_set <- paste(xp3_voctest$subject, xp3_voctest$set, 
                                 sep = "_")
xp3_lexdec$subject_set <- paste(xp3_lexdec$subject, xp3_lexdec$set, 
                                sep = "_")
xp3_lexdec <- merge(xp3_lexdec, 
                    xp3_voctest[,c("subject_set","voctest_correctness")],
                    by.x = "subject_set", by.y = "subject_set", all.x = TRUE)
xp3_lexdec<- merge(xp3_lexdec, 
                   xp3_questinf[,c("subject","jaar","av_prof")], 
                   by.x = "subject", by.y = "subject", all.x = TRUE)
xp3_lexdec<-merge(xp3_lexdec, phonotactics[,c("set","dutch","p_universally")], 
                  by.x = "set", by.y = "set", all.x = TRUE)
xp3_lexdec <- xp3_lexdec[order(xp3_lexdec$order),]
## add 'log_rt' column
xp3_lexdec$log_rt = log(xp3_lexdec$rt)
## add 'previous_rt' columns
previous_rt<-c()
for (i in 1:nrow(xp3_lexdec)) {
  if (xp3_lexdec$trial[i] == 1){
    previous_rt[i]<-round(mean(xp3_lexdec$rt[1:3]))
  }
  else{ 
    previous_rt[i]<-xp3_lexdec$rt[i-1]
  }
}
xp3_lexdec$previous_rt<-previous_rt
xp3_lexdec$previous_log_rt<-log(previous_rt)
## sub.01: remove high error targets
table.sets.correct<-prop.table(table(
  xp3_lexdec$correctness[xp3_lexdec$is_target=="yes"], 
  xp3_lexdec$set[xp3_lexdec$is_target=="yes"]),2)
table.sets.correct
table.sets.correct[,order(table.sets.correct[2,])]
xp3.sub.01<-subset(xp3_lexdec, !(set %in% c("t07", "t04")))
xp3.sub.01$set<-factor(xp3.sub.01$set)
## sub.02: remove participants with an error rate higher 
## than 40 %
table.subj.correct<-prop.table(table(
  xp3.sub.01$correctness, 
  xp3.sub.01$subject),2)
table.subj.correct[,order(table.subj.correct[2,])]
xp3.sub.02<-subset(xp3.sub.01, !(subject %in% c(
  "31","38")))
xp3.sub.02$subject<-factor(xp3.sub.02$subject)
# sub.03 (final data for accuracy analyses): only targets (no fillers)
xp3.sub.03 = subset(xp3.sub.02, is_target == "yes")
xp3.sub.03$set<-factor(xp3.sub.03$set)
## sub.04 ((final data for RT analyses): remove outliers and false responses
xp3.sub.04 = subset(xp3.sub.03, xp3.sub.03$log_rt < mean(xp3.sub.03$log_rt) + 
                      (2.5 * sd(xp3.sub.03$log_rt)) & xp3.sub.03$log_rt 
                    > mean(xp3.sub.03$log_rt) - (2.5 * sd(xp3.sub.03$log_rt)))
xp3.sub.04 = subset(xp3.sub.04, correctness == "correct")
# 3. Descriptive Analyses of results
## 3.1 Define functions for plotting and tabulating results
get_prop_confints<-function(f,v1,v2,flevel,v2level) {
  
  t_sum<-table(v1[f==flevel])
  t_contingency<-table(v1[f==flevel],v2[f==flevel])
  confints<-matrix(c(prop.test(t_contingency[1,v2level],t_sum[1])
                     $conf.int,prop.test(t_contingency[2,v2level],t_sum[2])
                     $conf.int),2,2)
  dimnames(confints)<-list(c("bottom","top"),dimnames(t_contingency)[[1]])
  confints
}

error.bar <- function (x.coordinate, middle, interval, ...) {
  if (is.vector(interval)) { 
    # if only one vector is provided (because the interval is symmetric)
    arrows(x.coordinate, middle -(middle - interval), x.coordinate, middle + 
             (middle - interval), angle=90, code=3, length = 0.1, lwd =2, ...)
  } else {                   
    # if a data frame or matrix is provided (because the interval is not symmetric)
    arrows(x.coordinate, interval[1,], x.coordinate, interval[2,], 
           angle=90, code=3, length = 0.1, lwd =2)
  }
}

get_acc_proportions <- function(d, v1, v2){
  
  props_1<-prop.table(table(
    d$correctness,v1,v2)[,1,],2)
  props_1<-round(props_1,2)
  props_2<-prop.table(table(
    d$correctness,v1,v2)[,2,],2)
  props_2<-round(props_2,2)
  error_props<-matrix(c(props_1["correct",],props_2["correct",]),
                      nrow=2,ncol=2,dimnames=list(levels(v2),levels(v1)))
  error_props
}

get_rts <- function(d,v1,v2){
  
  rts <- round(tapply(d$rt, list(v2, v1), mean))
  ttests<-tapply(d$rt, list(v2,v1), t.test)
  confints_rts<-sapply(ttests, function(x){x$conf.int}) 
  list(rts,confints_rts)
  
}

bar.col=c("red", "blue")

draw_plot<-function(d, confints, ylimits, label){
  mids<-barplot(d, col = bar.col, beside=T, ylab = label,
                cex.axis =  1.5, cex.main = 2, cex.lab = 2, 
                cex.names = 2, ylim = ylimits, lwd = 2,xpd=FALSE)
  text(mids, d-0.05, labels = d, pos=1, cex = 1)
  legend(2.75, 1.0, dimnames(d)[[1]],fill = bar.col)
  error.bar(mids,d,confints)
}
## 3.2 Tabulate and plot Accuracy
### reduction vs. spelling
props_redSpelling<-get_acc_proportions(xp3.sub.03,xp3.sub.03$red, xp3.sub.03$spelling)
confints1<-get_prop_confints(xp3.sub.03$red,xp3.sub.03$spelling,
                             xp3.sub.03$correctness,"reduced","correct")
confints2<-get_prop_confints(xp3.sub.03$red,xp3.sub.03$spelling,
                             xp3.sub.03$correctness,"full","correct")
confints_props<-cbind(confints1,confints2)    
props_redSpelling
draw_plot(props_redSpelling,confints_props,c(0,1),"Accuracy")
### reduction vs. phonotactics
props_redPhontac<-get_acc_proportions(xp3.sub.03,xp3.sub.03$red, xp3.sub.03$p_universally)
confints1<-get_prop_confints(xp3.sub.03$red,xp3.sub.03$p_universally,
                             xp3.sub.03$correctness,"reduced","correct")
confints2<-get_prop_confints(xp3.sub.03$red,xp3.sub.03$p_universally,
                             xp3.sub.03$correctness,"full","correct")
confints_props<-cbind(confints1,confints2)    
props_redPhontac
draw_plot(props_redPhontac,confints_props,c(0,1),"Error Proportions")
### phonotactics vs. spelling
props_phontacSpelling<-get_acc_proportions(xp3.sub.03,xp3.sub.03$p_universally, xp3.sub.03$spelling)
confints1<-get_prop_confints(xp3.sub.03$p_universally,xp3.sub.03$spelling,
                             xp3.sub.03$correctness,"I","correct")
confints2<-get_prop_confints(xp3.sub.03$p_universally,xp3.sub.03$spelling,
                             xp3.sub.03$correctness,"L","correct")
confints_props<-cbind(confints1,confints2)    
props_phontacSpelling
draw_plot(props_phontacSpelling,confints_props,c(0,1),"Error Proportions")
## 3.3 Tabulate and Plot RTs
### reduction vs. spelling
rts_redSpelling<-get_rts(xp3.sub.04,xp3.sub.04$red, xp3.sub.04$spelling)
draw_plot(rts_redSpelling[[1]], rts_redSpelling[[2]],c(800,2000),label=("RT (ms)"))
### reduction vs. phonotactics
rts_redPhontac<-get_rts(xp3.sub.04,xp3.sub.04$red, xp3.sub.04$p_universally)
rts_redPhontac[[1]]
### phonotactics vs. spelling
rts_phontacSpelling<-get_rts(xp3.sub.04,xp3.sub.04$p_universally, xp3.sub.04$spelling)
rts_phontacSpelling[[1]]