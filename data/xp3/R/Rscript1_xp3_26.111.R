rm(list=ls(all=TRUE))
###################################

install.packages("aod"); install.packages("amap"); install.packages("car"); install.packages("cluster"); install.packages("Hmisc"); install.packages("lattice"); install.packages("qcc"); install.packages("plotrix"); install.packages("rms"); install.packages("rms"); install.packages("rpart"); install.packages("vcd"); 
install.packages("gvlma"); install.packages("effects")
install.packages("multcomp"); install.packages("rgl")

library("car"); library("effects"); library("gvlma"); library("multcomp"); library("rgl"); library(MASS); library(languageR)

#### Read in data ############

#("/data/corpora/MPI_workspace/clsm/work/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/ExperimentalRoom/results/results_voctest.txt", header=F)
d <- read.delim("W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/day2_part1(lex_dec)/compiled_4.txt")
phon <- read.delim("W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/phonotactics.txt")
phon = phon[,-c(2:4)]  # spalten  entfernen, da schon in compiled_4
voctest <- read.delim("W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/day2_part2(voctest)/results_voctest_xp3.txt", header=F)
lextale <- read.delim("W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/day2_part4(lextale)/results_lextale_xp3_all.txt", header=T)
self_scores <- read.delim("W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/pp_questinfo_xp3.txt")

#############################################
########## voctest ##########################
#############################################

voctest = voctest[,-1] # aus irgendeinem Grund ist die erste Spalte dupliziert
colnames(voctest) = c("file", "a1", "a2", "a3", "a4", "correct_answer", "assistant", "participant", "subject", "group", "response", "response_word", "voctest_correctness" )

    # add "set"-column

filelist.voctest = strsplit(as.character(voctest$file), "_", fixed = T, perl = FALSE, useBytes = FALSE)

for (i in 1:length(filelist.voctest)) { # spalte fuer set hinzufuegen
  voctest$set[i] = filelist.voctest[[i]][1]
} 
    # add subject set column + correctness column

voctest.results = data.frame(voctest$subject, voctest$set, voctest$group, paste(voctest$subject, voctest$set), voctest$voctest_correctness); colnames(voctest.results) = c("subject", "set", "group", "subject_set", "voctest_correctness")

######################################################
############ transcription task ######################
######################################################
transcr_task <- read.delim("W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp2/day2_part3(transcription)/compiled_day2_part3_xp2.txt")

transcr_task_levels <- read.delim("W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp1/day2_part3(transcription)/day2_part3_levels.txt", header = T)

# add group column, set column, phonotactics column

subject_group = unique(data.frame(d$subject, d$group))
colnames(subject_group) = c("subject", "group")
transcr_task = merge(transcr_task, subject_group, by.x = "Subject", by.y = "subject")

transcr_task = merge(transcr_task, transcr_task_levels , by.x = "Correct", by.y = "stimulus") # set column
transcr_task = merge(transcr_task, phon, by.x = "set", by.y = "set")


#####################################################
############# lextale scores per participant ########
#####################################################

scores = vector()

for (i in 1:length(unique(lextale$Subject))) {
  temp.subset = subset(lextale, Subject == unique(lextale$Subject)[i])
  temp.correct.words = length(which(temp.subset$Stimulus.ACC == 1 & temp.subset$CorrectRight == 5))
  temp.incorrect.pseudo = length(which(temp.subset$Stimulus.ACC == 0 & temp.subset$CorrectRight == 1))
  temp.score = temp.correct.words - (2*temp.incorrect.pseudo)
  scores = append(scores, temp.score)   
} 

lextale.scores = data.frame(unique(lextale$Subject), scores); lextale.scores
colnames(lextale.scores) = c("subject", "lextale.scores"); lextale.scores

##################################################
################### d = compiled4 ################
##################################################

#### d.sub.01 = remove participants 1 - 4

d.sub.01 = d[,-12]

    #### add log_rt column #####################
    
d.sub.01$log_rt = log(d.sub.01$rt)

    #### add preceding rts #####################

d.sub.01.temp = data.frame()

for (i in 1:length(unique(d.sub.01$subject))) {
  participant = unique(d.sub.01$subject)[i]
  temp.subset = subset(d.sub.01, subject==participant)
  
  temp.subset$preceding_rt[1] = mean(temp.subset$rt[1:3]) # fuer ersten trial
  for (j in 2:nrow(temp.subset)) {       # erster trial hat keine preceding_rt
    temp.subset$preceding_rt[j] = temp.subset$rt[j-1]
  }
  d.sub.01.temp = rbind(d.sub.01.temp, temp.subset)
}

d.sub.01 = d.sub.01.temp

d.sub.01$log_preceding_rt = log(d.sub.01$preceding_rt)

    ########### add set and subject-set column ##################

file_list = strsplit(as.character(d.sub.01$file), "_", fixed = T, perl = FALSE, useBytes = FALSE)

for (i in 1:length(file_list)) { 
  d.sub.01$set[i] = file_list[[i]][1]
} 

d.sub.01$subject_set = paste(d.sub.01$subject, d.sub.01$set)

    ########## add voctest_correctness column

d.sub.01 = merge(d.sub.01, voctest.results[,4:5], by.x = "subject_set", by.y = "subject_set", all.x = TRUE)


    ########## add self_scores columns ##############

d.sub.01 = merge(d.sub.01, self_scores, by.x = "subject", by.y = "pp", all.x = TRUE)

    ########## add lextale_scores column ########
d.sub.01 = merge(d.sub.01, lextale.scores, by.x = "subject", by.y = "subject", all.x = TRUE)

    ######### add phonotactic columns ###########

d.sub.01 = merge(d.sub.01, phon, by.x = "set", by.y = "set", all.x = TRUE) # in phon sind die targets und die corresponding pseudowords


    ######### d.sub.02: remove high error sets 

d.sub.01.targets = subset(d.sub.01, is_target == "yes")
table.sets.correct = xtabs(~correctness + set, data = d.sub.01.targets);table.sets.correct
p_table.sets.correct = data.frame(prop.table(table.sets.correct,2)); p_table.sets.correct = subset(p_table.sets.correct, p_table.sets.correct$correctness == "yes"); p_table.sets.correct = p_table.sets.correct[order(p_table.sets.correct$Freq),]; p_table.sets.correct

d.sub.02 = subset(d.sub.01, !(set %in% c("t04", "t07")))
d.sub.02$set = factor(d.sub.02$set)

    ######### d.sub.03: remove high error participants

table.subj.correct = xtabs(~correctness + subject, data = d.sub.02);table.subj.correct
p_table.subj.correct = data.frame(prop.table(table.subj.correct,2)); p_table.subj.correct = subset(p_table.subj.correct, correctness == "yes"); p_table.subj.correct = p_table.subj.correct[order(p_table.subj.correct$Freq),]; p_table.subj.correct

d.sub.03 = subset(d.sub.02, !(subject %in% c(31,38,4,40,19,44)))    # participants

######### d.sub.04: only targets #########

d.sub.04 = subset(d.sub.03, is_target == "yes")
d.sub.04$set = factor(d.sub.04$set)
d.sub.04$targettype = factor(d.sub.04$targettype)

######### d.sub.05: remove outliers #####

d.sub.05 = subset(d.sub.04, d.sub.04$log_rt < mean(d.sub.04$log_rt) + (2.5 * sd(d.sub.04$log_rt)) & d.sub.04$log_rt > mean(d.sub.04$log_rt) - (2.5 * sd(d.sub.04$log_rt)))
d.sub.05$group = factor(d.sub.05$group)

######### d.sub.06: only correct responses #####

d.sub.06 = subset(d.sub.05, correctness == "yes")

###################################################################
###################################################################
###################################################################

###################################################################
######################## voctest.results, transc_task, lextale: ########### 
######################## only participants from final lex. dec. subset

voctest.results.sub = subset(voctest.results, (subject %in% unique(d.sub.05$subject)))

transcr_task.sub = subset(transcr_task, ((Subject %in% unique(d.sub.05$subject) & (set %in% levels(d.sub.05$set)))))
transcr_task.sub$set = factor(transcr_task.sub$set)

lextale.scores.sub = subset(lextale.scores, (subject %in% unique(d.sub.05$subject)))

######################################################################################

######################################################################################

######################################################################################
write.table(d.sub.01, 'W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/R/output/d.sub01.txt', sep = "\t", row.names = FALSE)
write.table(d.sub.02, 'W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/R/output/d.sub02.txt', sep = "\t", row.names = FALSE)
write.table(d.sub.03, 'W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/R/output/d.sub03.txt', sep = "\t", row.names = FALSE)
write.table(d.sub.04, 'W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/R/output/d.sub04.txt', sep = "\t", row.names = FALSE)

write.table(d.sub.05, 'W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/R/output/d.sub05.txt', sep = "\t", row.names = FALSE)
write.table(d.sub.06, 'W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/R/output/d.sub06.txt', sep = "\t", row.names = FALSE)
write.table(transcr_task.sub, 'W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp2/day2_part3(transcription)/xp3.transcr.all.sub.txt', sep = "\t", row.names = FALSE, quote = F)
write.table(subset(transcr_task.sub, group == 1)[c(1,2,3,10,7)], 'W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/day2_part3(transcription)/results grouped/xp3.transrc.sub.group1.txt', sep = "\t", row.names = FALSE, quote = F)
write.table(subset(transcr_task.sub, group == 2)[c(1,2,3,10,7)], 'W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/day2_part3(transcription)/results grouped/xp3.transrc.sub.group2.txt', sep = "\t", row.names = FALSE, quote = F)
write.table(subset(transcr_task.sub, group == 2 & p_dutch == "I")[c(1,2,3,10,7)], 'W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/day2_part3(transcription)/results grouped/xp3.transcr.sub.group2.ill.txt', sep = "\t", row.names = FALSE, quote = F)
write.table(subset(transcr_task.sub, group == 2 & p_dutch == "L")[c(1,2,3,10,7)], 'W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/day2_part3(transcription)/results grouped/xp3.transcr.sub.group2.l.txt', sep = "\t", row.names = FALSE, quote = F)
write.table(subset(transcr_task.sub, group == 1 & p_dutch == "I")[c(1,2,3,10,7)], 'W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/day2_part3(transcription)/results grouped/xp3.transcr.sub.02.group1.ill.txt', sep = "\t", row.names = FALSE, quote = F)
write.table(subset(transcr_task.sub, group == 1 & p_dutch == "L")[c(1,2,3,10,7)], 'W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/main experiment/Results - Compiled/xp3/day2_part3(transcription)/results grouped/xp3.transcr.sub.02.group1.l.txt', sep = "\t", row.names = FALSE, quote = F)
