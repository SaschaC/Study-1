#### Read in data ############

oxp1_lexdec <- read.delim("../oexp/xp1/day2_part1(lex_dec)/compiled_4.txt")
oxp1_voctest <- read.delim("../oexp/xp1/day2_part2(voctest)/results_voctest_xp1_all.txt", header = F)
oxp1_lextale <- read.delim("../oexp/xp1/day2_part4(lextale)/results_lexTale_all.txt")
oxp1_questinf <- read.delim("../oexp/xp1/pp_questinf_xp1.txt", dec = ",")

oxp2_lexdec <- read.delim("../oexp/xp2/day2_part1(lex_dec)/compiled_4.txt")
oxp2_voctest <- read.delim("../oexp/xp2/day2_part2(voctest)/results_voctest_xp2_all.txt", header = F)
oxp2_lextale <- read.delim("../oexp/xp2/day2_part4(lextale)/results_lexTale_all_xp2.txt")
oxp2_questinf <- read.delim("../oexp/xp2/pp_questinf_xp2.txt", dec = ",")


### lexdec

oxp1_lexdec <- oxp1_lexdec[,-12]
oxp1_lexdec <- subset(oxp1_lexdec, !(subject %in% c(1,2,3,4, 1003, 1008)))
oxp1_lexdec$oexperiment <- "1"
oxp1_lexdec$spelling <- "-spelling"
oxp1_lexdec$subject_oexp <- paste(oxp1_lexdec$subject, "1", sep = "_")

oxp2_lexdec <- oxp2_lexdec[,-12]
oxp2_lexdec$oexperiment <- "2"
oxp2_lexdec$spelling <- "+spelling"
oxp2_lexdec$subject_oexp <- paste(oxp2_lexdec$subject, "2", sep = "_")

nxp1_lexdec <- rbind(oxp1_lexdec[oxp1_lexdec$group =="2",], oxp2_lexdec[oxp2_lexdec$group =="2",])
nxp2_lexdec <- rbind(oxp1_lexdec[oxp1_lexdec$group =="1",], oxp2_lexdec[oxp2_lexdec$group =="1",])

### voctest

colnames(oxp1_voctest) = c("file", "a1", "a2", "a3", "a4", "correct_answer", "assistant", "participant", "subject", "group", "response", "response_word", "voctest_correctness" )
oxp1_voctest <- subset(oxp1_voctest, !(subject %in% c(1,2,3,4, 1003, 1008)))
oxp1_voctest$oexperiment <- "1"
oxp1_voctest$spelling <- "-spelling"
oxp1_voctest$subject_oexp <- paste(oxp1_voctest$subject, "1", sep = "_")
oxp1_voctest<-oxp1_voctest[oxp1_voctest$subject_oexp%in%oxp1_lexdec$subject_oexp,]

colnames(oxp2_voctest) = c("file", "a1", "a2", "a3", "a4", "correct_answer", "assistant", "participant", "subject", "group", "response", "response_word", "voctest_correctness" )
oxp2_voctest$oexperiment <- "2"
oxp2_voctest$subject_oexp <- paste(oxp2_voctest$subject, "2", sep = "_")
oxp2_voctest$spelling <- "+spelling"
oxp2_voctest<-oxp2_voctest[oxp2_voctest$subject_oexp%in%oxp2_lexdec$subject_oexp,]

nxp1_voctest <- rbind(oxp1_voctest[oxp1_voctest$group =="2",], oxp2_voctest[oxp2_voctest$group =="2",])
nxp2_voctest <- rbind(oxp1_voctest[oxp1_voctest$group =="1",], oxp2_voctest[oxp2_voctest$group =="1",])

### lextale

oxp1_lextale <- subset(oxp1_lextale, !(Subject %in% c(1,2,3,4, 1003, 1008)))
oxp1_lextale$oexperiment <- "1"
oxp1_lextale$subject_oexp <- paste(oxp1_lextale$Subject, "1", sep = "_")
oxp1_lextale$spelling <- "-spelling"
oxp1_lextale<-merge(oxp1_lextale, unique(oxp1_lexdec[,(c("subject_oexp", "group"))]), by.x = "subject_oexp", by.y = "subject_oexp", all.x = FALSE)

oxp2_lextale$oexperiment <- "2"
oxp2_lextale$subject_oexp <- paste(oxp2_lextale$Subject, "2", sep = "_")
oxp2_lextale$spelling <- "+spelling"
oxp2_lextale<-merge(oxp2_lextale, unique(oxp2_lexdec[,(c("subject_oexp", "group"))]), by.x = "subject_oexp", by.y = "subject_oexp", all.x = FALSE)

nxp1_lextale <- rbind(oxp1_lextale[oxp1_lextale$group =="2",], oxp2_lextale[oxp2_lextale$group =="2",])
nxp2_lextale <- rbind(oxp1_lextale[oxp1_lextale$group =="1",], oxp2_lextale[oxp2_lextale$group =="1",])

### questionnaire

oxp1_questinf <- subset(oxp1_questinf, !(pp %in% c(1,2,3,4, 1003, 1008)))
oxp1_questinf$oexperiment <- "1"
oxp1_questinf$subject_oexp <- paste(oxp1_questinf$pp, "1", sep = "_")
oxp1_questinf$spelling <- "-spelling"
oxp1_questinf<-merge(oxp1_questinf, unique(oxp1_lexdec[,(c("subject_oexp", "group"))]), by.x = "subject_oexp", by.y = "subject_oexp", all.x = FALSE)

oxp2_questinf$oexperiment <- "2"
oxp2_questinf$subject_oexp <- paste(oxp2_questinf$pp, "2", sep = "_")
oxp2_questinf$spelling <- "+spelling"
oxp2_questinf<-merge(oxp2_questinf, unique(oxp2_lexdec[,(c("subject_oexp", "group"))]), by.x = "subject_oexp", by.y = "subject_oexp", all.x = FALSE)

nxp1_questinf <- rbind(oxp1_questinf[oxp1_questinf$group ==2,], oxp2_questinf[oxp2_questinf$group ==2,])
nxp2_questinf <- rbind(oxp1_questinf[oxp1_questinf$group ==1,], oxp2_questinf[oxp2_questinf$group ==1,])

##############
##############

write.table(nxp1_lexdec, 'nxp1_lexdec.txt', sep = "\t", row.names = FALSE)
write.table(nxp2_lexdec, 'nxp2_lexdec.txt', sep = "\t", row.names = FALSE)
write.table(nxp1_voctest, 'nxp1_voctest.txt', sep = "\t", row.names = FALSE)
write.table(nxp2_voctest, 'nxp2_voctest.txt', sep = "\t", row.names = FALSE)
write.table(nxp1_lextale, 'nxp1_lextale.txt', sep = "\t", row.names = FALSE)
write.table(nxp2_lextale, 'nxp2_lextale.txt', sep = "\t", row.names = FALSE)
write.table(nxp1_questinf, 'nxp1_questinf.txt', sep = "\t", row.names = FALSE)
write.table(nxp2_questinf, 'nxp2_questinf.txt', sep = "\t", row.names = FALSE)
