#Assumptions: 1) comes from a google form and takes an email address
# 2) if there's a key, it's email address is key.key@key.com
# 3) all commas in numbers and answers have been removed before the file was converted to csv format
# 4) the file is in csv format
# 5) all email address are first.last@domain.com

################
##Read Data In##
################

require(data.table)
require(stringr)
baseDir = "/Users/mf/Google\ Drive/PCC_BI112_Summer_2018/Quizzes/"
fileName = "quiz7.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

########################################
##Extract Point Vals For Each Question##
########################################

pointVals = extractPointVals("^.*(\\d+)\\.pt\\.", colnames(data)) #\\d+\\.pt.

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

#######################################
##Add the points in the points_q cols##
#######################################
df=finalDf
keyRowNum = which(finalDf[,1]=="key")
searchStringForPointColNames = "points_q"
pointsVector = pointVals
addPointsToKeyRow(finalDf, which(finalDf[,1]=="key"), "points_q", pointVals)


############################################
##Create a csv file with the new dataframe##
############################################

#write.table(finalDf,file = paste0(baseDir, revisedFileName,"_processed.csv"), quote=FALSE, sep=",", col.names = TRUE, row.names = FALSE)

#############
##Functions##
#############

extractPointVals= function(regexp, colnames){
  all = str_match(colnames, regexp)[,2]
  nonNAs = all[which(!is.na(all))]
  return(nonNAs)
  #results = regexpr(regexp, colnames) #, value=TRUE
  #return (regmatches(colnames, results))
}

addPointsToKeyRow = function(df, keyRowNum, searchStringForPointColNames, pointsVector){
  df[] = lapply(df, as.character)
  
  #Find columns which has searchString in it
  cols <- grepl(searchStringForPointColNames, colnames(df))
  
  #Check if the columns with searchString and length of pointsVector is the same
  if (sum(cols) == sum(!is.na(pointsVector))) {
    #Assign the value
    df[keyRowNum,cols] <- pointsVector
  }
  #Return the updated dataframe
  print(df)
  return(df)
}
