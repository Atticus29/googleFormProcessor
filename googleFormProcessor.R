#Assumptions: 1) comes from a google form and takes an email address
# 2) if there's a key, it's email address is key.key@key.com
# 3) all commas in numbers and answers have been removed before the file was converted to csv format
# 4) the file is in csv format
# 5) all email address are first.last@domain.com

################
##Read Data In##
################

require(data.table)
baseDir = "/Users/mf/Google\ Drive/PCC_BI112_Summer_2018/Quizzes/"
fileName = "quiz7.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

######################################
##Convert Email Addresses Into Names##
######################################

emailRemoved = sub('@.*', '', data$Email.Address)
nameSplitList = strsplit(emailRemoved, "\\.")
first = unlist(lapply(nameSplitList, function(l) l[[1]])) #TODO make robust to missing or additional names
last = unlist(lapply(nameSplitList, function(l) l[[2]])) #TODO make robust to missing or additional names

#######################################
##Extract Question-containing columns##
#######################################

questionContainingDf = data[,(which(colnames(data) == "Email.Address")+1): ncol(data)]

################################################################################
##Construct a new dataframe with a blank column corresponding to each question##
################################################################################
finalDf = data.frame(cbind(first,last))
blankCol = c()
for(r in c(1:nrow(questionContainingDf))){
  blankCol = c(blankCol, "")
}
for(i in c(1:ncol(questionContainingDf))){
  finalDf = data.frame(cbind(finalDf, questionContainingDf[,i], blankCol))
  print(paste0("question", i))
  print(paste0("points_q",i))
  colnames(finalDf)[ncol(finalDf)-1] = paste0("question",i)
  colnames(finalDf)[ncol(finalDf)] = paste0("points_q",i)
}
############################################
##Create a csv file with the new dataframe##
############################################

write.table(finalDf,file = paste0(baseDir, revisedFileName,"_processed.csv"), quote=FALSE, sep=",", col.names = TRUE, row.names = FALSE)