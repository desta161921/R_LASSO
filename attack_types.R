options(warn = -1)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(stats)

#######################################
# Read in the training dataset
#######################################
trainingData <- read.csv("../NSL-KDD/KDDTrainFullDataSet.csv", 
              header = FALSE, stringsAsFactors = FALSE)

#######################################
# Read in the testing dataset
#######################################

testingData <- read.csv("../NSL-KDD/KDDTestFullDataSet.csv", 
                         header = FALSE, stringsAsFactors = FALSE)
#######################################
# Read in the field names table
#######################################

fieldNames <- read.csv("../KDD-Datasets/FieldNames.csv", 
                       header=FALSE, stringsAsFactors=FALSE)

#######################################
# Read in the attack types table
#######################################
attackTypes <- read.csv("../KDD-Datasets/AttackTypes.csv", 
                        header = FALSE, stringsAsFactors = FALSE, col.names = c('name', 'type'))

#############################################
# Assign the field names to the main dataset
############################################

names(trainingData) <- fieldNames[, 1]
names(testingData) <- fieldNames[, 1]
names(trainingData)[42]="status"
colnames(trainingData)[43]="category"

# Missing names
names(trainingData)[42:43] <- c('name', 'code')
names(testingData)[42:43] <- c('name', 'code')


# Let's merge fields that the datasets have in common (i.e., "name") using the merge() function
attackTypesTrainingDataset <- merge(trainingData, attackTypes)
attackTypesTestingDataset <- merge(testingData, attackTypes)

head(attackTypesTrainingDataset)


# Convert symbolic to continuous values
trainingData$protocol_type <- as.integer(trainingData$protocol_type)
trainingData$service <- as.integer(trainingData$service)
trainingData$flag <- as.integer(trainingData$flag)

# Attack name preprocess
trainingData$name = as.character(trainingData$name)
trainingData$name[trainingData$name == "ipsweep."] = "probe"
trainingData$name[trainingData$name == "portsweep."] = "probe"
trainingData$name[trainingData$name == "nmap."] = "probe"
trainingData$name[trainingData$name == "satan."] = "probe"
trainingData$name[trainingData$name == "buffer_overflow."] = "u2r"
trainingData$name[trainingData$name == "loadmodule."] = "u2r"
trainingData$name[trainingData$name == "perl."] = "u2r"
trainingData$name[trainingData$name == "rootkit."] = "u2r"
trainingData$name[trainingData$name == "back."] = "dos"
trainingData$name[trainingData$name == "land."] = "dos"
trainingData$name[trainingData$name == "neptune."] = "dos"
trainingData$name[trainingData$name == "pod."] = "dos"
trainingData$name[trainingData$name == "smurf."] = "dos"
trainingData$name[trainingData$name == "teardrop."] = "dos"
trainingData$name[trainingData$name == "ftp_write."] = "r2l"
trainingData$name[trainingData$name == "guess_passwd."] = "r2l"
trainingData$name[trainingData$name == "imap."] = "r2l"
trainingData$name[trainingData$name == "multihop."] = "r2l"
trainingData$name[trainingData$name == "phf."] = "r2l"
trainingData$name[trainingData$name == "spy."] = "r2l"
trainingData$name[trainingData$name == "warezclient."] = "r2l"
trainingData$name[trainingData$name == "warezmaster."] = "r2l"
trainingData$name[trainingData$name == "normal."] = "normal"
trainingData = trainingData[!(trainingData$name=="0.00"),]
trainingData = trainingData[!(trainingData$name==""),]
trainingData$name = factor(trainingData$name)

# Summary of each attack type - category
# NumberOfDoSAttacks <- as.data.frame(table(attackTypes$type=="dos"))

totalAttackCategories <- function(attackName, type) 
  {
  switch(type,
         dos = as.data.frame(table(attackTypes$type=="dos")), # Number of DoS attack categories :[back, land,  neptune, pod, smurf, teardrop]
         r2l = as.data.frame(table(attackTypes$type=="r2l")), # Number of R2L attack categories : [ftp_write, guess_passwd, imap, multihop, phf, spy, warezclient, warezmaster]
         u2r = as.data.frame(table(attackTypes$type=="u2r")), # Number of U2R attack categories : [buffer_overflow, loadmodule, perl, rootkit]
         probe = as.data.frame(table(attackTypes$type=="probe")), # Number of attack categories: [ipsweep, portsweep, nmap, satan]
         normal = as.data.frame(table(attackTypes$type=="normal")) # Normal session : [normal]
         ) 
}
attackName <- attackTypes

# Summary of the attacks
summaryOfAttackTypes <- as.data.frame(table(attackTypes))

#######################################
# Total number of each attack category in the training dataset
#######################################

totalNumberOfDoSAttackTypesTraining <- sum(attackTypesTrainingDataset$type=="dos")
totalNumberOfR2LAttackTypesTraining <- sum(attackTypesTrainingDataset$type=="r2l")
totalNumberOfU2RAttackTypesTraining <- sum(attackTypesTrainingDataset$type=="u2r")
totalNumberOfProbeAttackTypesTraining <- sum(attackTypesTrainingDataset$type=="probe")
totalNumberOfNormalSessionsTraining <- sum(attackTypesTrainingDataset$type=="normal")

#######################################
# Total Number of Attacks in the training dataset
#######################################
totalNumberOfAttacksTrainingDataSet <- sum(totalNumberOfDoSAttackTypesTraining + totalNumberOfR2LAttackTypesTraining + 
                                            totalNumberOfU2RAttackTypesTraining + totalNumberOfProbeAttackTypesTraining + 
                                            totalNumberOfNormalSessionsTraining)
#######################################
# Percentage of each attack category in the training dataset
#######################################0
percentageOfDoSTrainingDataSet <- noquote(paste("DoS Training=", round((sum(totalNumberOfDoSAttackTypesTraining)/
                                                         totalNumberOfAttacksTrainingDataSet)*100,digits=2),"%", sep = ""))
percentageOfR2LTrainingDataSet <- noquote(paste("R2l Training=", round((sum(totalNumberOfR2LAttackTypesTraining)/
                                                         totalNumberOfAttacksTrainingDataSet)*100,digits=2),"%", sep = ""))
percentageOfU2RTrainingDataSet <- noquote(paste("U2R Training=", round((sum(totalNumberOfU2RAttackTypesTraining)/
                                                         totalNumberOfAttacksTrainingDataSet)*100,digits=2),"%", sep = ""))
percentageOfProbeTrainingDataSet <- noquote(paste("Probe Training=", round((sum(totalNumberOfProbeAttackTypesTraining)/
                                                         totalNumberOfAttacksTrainingDataSet)*100,digits=2),"%", sep = ""))
percentageOfNormalTrainingDataSet <- noquote(paste("Normal Training=", round((sum(totalNumberOfNormalSessionsTraining)/
                                                         totalNumberOfAttacksTrainingDataSet)*100,digits=2),"%", sep = ""))

# Total number of each attack category in the testing dataset
totalNumberOfDoSAttackTypesTesting <- sum(attackTypesTestingDataset$type=="dos")
totalNumberOfR2LAttackTypesTesting <- sum(attackTypesTestingDataset$type=="r2l")
totalNumberOfU2RAttackTypesTesting <- sum(attackTypesTestingDataset$type=="u2r")
totalNumberOfProbeAttackTypesTesting <- sum(attackTypesTestingDataset$type=="probe")
totalNumberOfNormalSessionsTesting <- sum(attackTypesTestingDataset$type=="normal")

#######################################
# Total Number of Attacks in the testing dataset
#######################################
totalNumberOfAttacksTestingDataSet <- sum(totalNumberOfDoSAttackTypesTesting + 
                                            totalNumberOfR2LAttackTypesTesting + 
                                           totalNumberOfU2RAttackTypesTesting + 
                                            totalNumberOfProbeAttackTypesTesting +
                                           totalNumberOfNormalSessionsTesting)

#######################################
# Percentage of each attack category in the testing dataset
#######################################0
percentageOfDoSTestingDataSet <- noquote(paste("DoS Testing=", 
                                               round((sum(totalNumberOfDoSAttackTypesTesting)/
                                                                totalNumberOfAttacksTestingDataSet)*100,digits=2),"%", sep = ""))
percentageOfR2LTestingDataSet <- noquote(paste("R2l Testing=", 
                                               round((sum(totalNumberOfR2LAttackTypesTesting)/
                                                                totalNumberOfAttacksTestingDataSet)*100,digits=2),"%", sep = ""))
percentageOfU2RTestingDataSet <- noquote(paste("U2R Testing=", 
                                               round((sum(totalNumberOfU2RAttackTypesTesting)/
                                                                totalNumberOfAttacksTestingDataSet)*100,digits=2),"%", sep = ""))
percentageOfProbeTestingDataSet <- noquote(paste("Probe Testing=", 
                                                 round((sum(totalNumberOfProbeAttackTypesTesting)/
                                                                    totalNumberOfAttacksTestingDataSet)*100,digits=2),"%", sep = ""))
percentageOfNormalTestingDataSet <- noquote(paste("Normal Testing=", 
                                                  round((sum(totalNumberOfNormalSessionsTesting)/
                                                                      totalNumberOfAttacksTestingDataSet)*100,digits=2),"%", sep = ""))

#######################################
# Plots of attack distribution in the training data set
#######################################
attackDistributionTraining <- function()
{
  attackDistributionSummaryTraining  <- attackTypesTrainingDataset %>% group_by(type) %>% summarise(Attack_Count = n())
  print(attackDistributionSummaryTraining)
#   plotDistribution <- ggplot(attackDistributionSummary, aes(type, Attack_Count)) + geom_bar(stat = 'identity') + 
#     labs(x="Attack Categories", y="Count") + ggtitle("Attack Distribution") 
  plotDistributionTraining <- ggplot(attackDistributionSummaryTraining, aes(type, Attack_Count, fill=type)) +
    scale_fill_discrete(guide = guide_legend(title = "Attack Categories")) + geom_bar(stat = 'identity') + 
    geom_text(aes(label = Attack_Count), position = position_dodge(width = 1), vjust = -1) +
    labs(x="Attack Categories", y="Attack Count") + ggtitle("Training Data Set: Attack Distribution")
  
  plot(plotDistributionTraining) 
  
}
 
attackDistributionTraining()

#######################################
# Plots of attack distribution in the testing data set
#######################################
attackDistributionTesting <- function()
{
  attackDistributionSummaryTesting  <- attackTypesTestingDataset %>% group_by(type) %>% summarise(Attack_Count = n())
  print(attackDistributionSummaryTesting)
  plotDistributionTesting <- ggplot(attackDistributionSummaryTesting, aes(type, Attack_Count, fill=type)) +
    scale_fill_discrete(guide = guide_legend(title = "Attack Categories")) + geom_bar(stat = 'identity') + 
    geom_text(aes(label = Attack_Count), position = position_dodge(width = 1), vjust = -1) +
    labs(x="Attack Categories", y="Attack Count") + ggtitle("Testing Data Set: Attack Distribution")
  
  plot(plotDistributionTesting) 
  
}

attackDistributionTesting()

# Each attack name distribution in the training dataset
attackNameDistributionTraining <- table(attackTypesTrainingDataset$name, attackTypesTrainingDataset$type)

# Print percentages of attacks in the training data set
print(percentageOfDoSTrainingDataSet)
print(percentageOfNormalTrainingDataSet)
print(percentageOfProbeTrainingDataSet)
print(percentageOfR2LTrainingDataSet)
print(percentageOfU2RTrainingDataSet)

# Print percentage of attacks in the testing data set

print(percentageOfDoSTestingDataSet)
print(percentageOfNormalTestingDataSet)
print(percentageOfProbeTestingDataSet)
print(percentageOfR2LTestingDataSet)
print(percentageOfU2RTestingDataSet)


