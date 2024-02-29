# ===========================================================================
# Proof-of-concept: replicating Sawtooth results in R using ChoiceModelR
# 
# Original conjoint: Full-profile conjoint conducted in spring 2017
#      # of attributes: 9
#      # of levels: 4
#      design: 18-block fractional factorial with 12 choice tasks per n
# ===========================================================================

# initial setup =============================================================

# clear workspace
rm(list = ls())

# load necessary packages
pkg.list <- c("openxlsx", "plyr", "ChoiceModelR",
              "bayesm", "reshape2")
#install.packages(pkg.list)
lapply(pkg.list, require, character.only = TRUE)

# folder paths for importing data and cleaning data
survey.data.path   <- "C:\\...\\data"

# set working directory to the data folder
setwd(survey.data.path)

# import requisite data
survey_data = read.xlsx("....xlsx", sheet = 1)
survey_info = read.csv("C....csv", stringsAsFactors = F, check.names = F)

choice_columns = c("choicecard_1_1", "choicecard_1_2", 
                   "choicecard_1_3", "choicecard_1_4",	
                   "choicecard_1_5", "choicecard_1_6", 
                   "choicecard_1_7", "choicecard_1_8", 
                   "choicecard_1_9", "choicecard_1_10", 
                   "choicecard_1_11", "choicecard_1_12")
other_key_columns = c("block",	"q9")

# data preprocessing ========================================================

key_data = survey_data[c("respid",choice_columns,other_key_columns)]
key_data$ID = key_data$respid
key_data$respid <- NULL
response_data = melt(key_data, 
                     idvar =  c("choicecard_1_1", "choicecard_1_2", 
                                "choicecard_1_3", "choicecard_1_4",
                                "choicecard_1_5", "choicecard_1_6",
                                "choicecard_1_7", "choicecard_1_8",
                                "choicecard_1_9", "choicecard_1_10",
                                "choicecard_1_11", "choicecard_1_12"), 
                     id = c("ID", other_key_columns))

response_data$variable = as.numeric(gsub("choicecard_1_", "", as.character(response_data$variable)))
response_data = response_data[order(response_data$ID, response_data$variable),]

# Merge in the survey design (options for each choice)
head(survey_info)
original_survey_headers = colnames(survey_info)[4:ncol(survey_info)]
colnames(survey_info)[4:ncol(survey_info)] = paste0("X",1:(ncol(survey_info) - 3))

# Merge in the survey design
merged_response_data = merge(x = response_data, y = survey_info, by.x = c("block", "variable"), by.y = c("Version", "Task"), all.x=T, all.y=T)
nrow(merged_response_data) / nrow(response_data) #Should be 3

# Prepare dataset for HB analysis
merged_response_data$block <- NULL
colnames(merged_response_data)[match("variable", colnames(merged_response_data))] = "Task"
merged_response_data = merged_response_data[order(merged_response_data$ID, merged_response_data$Task, merged_response_data$Concept), ]

# model fitting =============================================================

###
# TBD: Hold out sample
###

train_data = merged_response_data[,c("ID", "Task", "Concept", paste0("X",1:(ncol(survey_info) - 3)), "value")]
train_data$value[duplicated(train_data[,c("ID","Task")])] = 0 # Only the first concept for each task should have a value
# None is indeed left out

# Set up the constraints
nrow(train_data)


# Analysis
x_coding = c(1,0,0,0,0,0,0,0,0) #Monthly fee is numeric
mcmc = list(R = 30000, use = 10000)
options = list(none = TRUE, save = TRUE, keep = 1)
# constraints = 

out = choicemodelr(train_data, x_coding, mcmc = mcmc, options = options)

estbetas = apply(out$betadraw,c(1,2),mean)

ncol(estbetas)

# Create parameters
beta_tracker = 1
for (i in 1:(ncol(survey_info)-3)) {
  
  var = paste0("X",i)
  type = x_coding[i]
  
  if (type == 0) {
    num_levels = length(unique(survey_info[,var]))
    temp_beta = estbetas[,beta_tracker:(beta_tracker + num_levels - 2), drop = F]
    beta_tracker = beta_tracker + num_levels - 1
    new_coef = cbind(temp_beta, 0 - rowSums(temp_beta))
    colnames(new_coef) = paste0(original_survey_headers[i], ": ", 1:ncol(new_coef))
  } else {
    num_levels = 1
    temp_beta = estbetas[,beta_tracker, drop = F]
    beta_tracker = beta_tracker + 1
    new_coef = temp_beta
    colnames(new_coef) = original_survey_headers[i]
  }
  if (i == 1) {
    params = new_coef
  } else {
    params = cbind(params, new_coef)
  }
  
}

# There is a NONE column at the very right
new_coef = estbetas[,ncol(estbetas), drop = F]
colnames(new_coef) = "NONE"
params = cbind(params, new_coef)

params = cbind(survey_data[, "respid", drop = F], params)
rownames(params) = NULL

write.csv(params, "result_30k.csv", row.names = F)