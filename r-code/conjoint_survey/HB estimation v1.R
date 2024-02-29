require(openxlsx)
require(plyr)
require(ChoiceModelR)
require(bayesm)
require(reshape2)

######################
# File setup and input
######################

folder = "C:\\Users\\Daniel.Wang\\Documents\\owg-fnr-conjoint-maxdiff2\\R"

survey_data = read.xlsx(file.path(folder,"data\\sample data.xlsx"), sheet = 1)
survey_info = read.csv(file.path(folder, "data\\sample survey design.csv"), stringsAsFactors = F, check.names = F)

choice_columns = c("choicecard_1_1", "choicecard_1_2", 
                     "choicecard_1_3",	"choicecard_1_4",	"choicecard_1_5",	"choicecard_1_6", "choicecard_1_7", 
                     "choicecard_1_8", "choicecard_1_9", "choicecard_1_10", "choicecard_1_11", "choicecard_1_12")
other_key_columns = c("block",	"q9")

key_data = survey_data[c("respid",choice_columns,other_key_columns)]
key_data$ID = key_data$respid
key_data$respid <- NULL
response_data = melt(key_data, idvar =  c("choicecard_1_1", "choicecard_1_2", 
                        "choicecard_1_3",	"choicecard_1_4",	"choicecard_1_5",	"choicecard_1_6", "choicecard_1_7", 
                        "choicecard_1_8", "choicecard_1_9", "choicecard_1_10", "choicecard_1_11", "choicecard_1_12"), id = c("ID", other_key_columns))

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

######################
# Generating a hold out sample. Out of sample removes entire questions from individuals. Though it is unclear if this is possible...
######################

# Hold out rate
holdout_rate = 0.2 # Only holds out by individual. Cannot hold out tasks

if (holdout_rate > 0) {
  
  unique_ID = unique(merged_response_data[,"ID"])
  selected_ID = unique_ID[runif(length(unique_ID)) >= holdout_rate]
  holdout_ID = setdiff(unique_ID,selected_ID)
  
  train_data = merged_response_data[merged_response_data$ID %in% selected_ID,c("ID", "Task", "Concept", paste0("X",1:(ncol(survey_info) - 3)), "value")]
  holdout_data = merged_response_data[merged_response_data$ID %in% holdout_ID,c("ID", "Task", "Concept", paste0("X",1:(ncol(survey_info) - 3)), "value")]
  # unique_ID = unique(merged_response_data[,c("ID","Task")])
  # unique_ID$insample = (runif(length(unique_ID)) > holdout_rate)
  # 
  # ID_insample = unique(unique_ID$ID[unique_ID$insample])
  # 
  # temp_data = merged_response_data[,c("ID","Task")]
  # temp_data = merge(x = temp_data, y = unique_ID, by = c("ID","Task"), all.x = T, all.y = T)
  # temp_data = temp_data[order(temp_data$ID, temp_data$Task),]
  # insample = temp_data$insample
  # 
  # train_data = merged_response_data[insample,c("ID", "Task", "Concept", paste0("X",1:(ncol(survey_info) - 3)), "value")]
  
} else {
  train_data = merged_response_data[,c("ID", "Task", "Concept", paste0("X",1:(ncol(survey_info) - 3)), "value")]
  holdout_data = NULL
}

train_data$value[duplicated(train_data[,c("ID","Task")])] = 0 # Only the first concept for each task should have a value
# None is indeed left out

##########################
# Set up the constraints
##########################

# Nothing here right now


##########################
# Perform analysis
##########################


# Analysis
x_coding = c(1,0,0,0,0,0,0,0,0) #Monthly fee is numeric
mcmc = list(R = 30000, use = 10000)
# mcmc = list(R = 3000, use = 1000)
options = list(none = TRUE, save = TRUE, keep = 1)
# constraints = 

out = choicemodelr(train_data, x_coding, mcmc = mcmc, options = options)

##########################
# Parse the results
#########################

# Get results -- these are the results automatically saved by the function. This is insample only
outputted_results = read.csv("Rbetas.csv")

# Manually get the betas
estbetas = apply(out$betadraw,c(1,2),mean)

# Create parameters
beta_tracker = 1
variable_name = NULL
variable_level = NULL
variable_type = NULL

for (i in 1:(ncol(survey_info)-3)) {
  
  var = paste0("X",i)
  type = x_coding[i]
  
  if (type == 0) {
    num_levels = length(unique(survey_info[,var]))
    variable_name = c(variable_name, rep(paste0("X",i),num_levels))
    variable_level = c(variable_level, 1:num_levels)
    variable_type = c(variable_type, rep(type, num_levels))
    temp_beta = estbetas[,beta_tracker:(beta_tracker + num_levels - 2), drop = F]
    beta_tracker = beta_tracker + num_levels - 1
    new_coef = cbind(temp_beta, 0 - rowSums(temp_beta))
    colnames(new_coef) = paste0(original_survey_headers[i], ": ", 1:ncol(new_coef))
  } else {
    num_levels = 1
    variable_name = c(variable_name, rep(paste0("X",i),num_levels))
    variable_level = c(variable_level, 0)
    variable_type = c(variable_type, rep(type, num_levels))
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

var_mapping = data.frame("Var" = colnames(params), "X" = variable_name, "k" = variable_level, "Type" = variable_type, stringsAsFactors = F)

# There is a NONE column at the very right
new_coef = estbetas[,ncol(estbetas), drop = F]
colnames(new_coef) = "NONE"
params = cbind(params, new_coef)

if (holdout_rate > 0) {
  params = cbind(survey_data[survey_data$respid %in% selected_ID, "respid", drop = F], params)
} else {
  params = cbind(survey_data[, "respid", drop = F], params)  
}
rownames(params) = NULL

write.csv(params, file.path(folder, "data\\result_30k_IS.csv"), row.names = F)

# Generate summary partworth for the out-of-sample
# This doesn't use segments yet
if (holdout_rate > 0) {
  mean_partworths = colMeans(params[,-1])
  oos_params = merge(data.frame("respid" = holdout_ID), as.data.frame(t(mean_partworths)), all = T)
  params = rbind(params, oos_params)
}

# Score the responses
head(merged_response_data)

# Center numerical variables before scoring
if (any(variable_type == 1)) {
  for (var_name in unique(variable_name[variable_type==1])) {
    merged_response_data[,var_name] = merged_response_data[,var_name] - mean(merged_response_data[,var_name])
  }
}

sum_partworth = rep(NA, nrow(merged_response_data))
for (i in 1:nrow(merged_response_data)) {
  
  if (i %% 100 == 0) print(i)
  
  temp_response = merged_response_data[i,]
  temp_ID = temp_response$ID
  
  # Get partworth
  part_worth = params[params$respid == temp_ID,]
  
  # Sum the coefficients
  temp_sum_partworth = 0
  
  temp_intercept = as.numeric(part_worth["NONE"])
  temp_sum_partworth = temp_sum_partworth + temp_intercept
  
  var_index = grep(x = names(temp_response), pattern="X[0-9]")
  for (j in 1:length(var_index)) {
    temp_index = var_index[j]
    
    var_name = names(temp_response)[temp_index]
    var_type = var_mapping$Type[match(var_name, var_mapping$X)]
    var_level = as.numeric(temp_response[temp_index])
    
    
    if (var_type == 0) {
      full_var_name = var_mapping$Var[(var_mapping$X == var_name) & (var_mapping$k == var_level)]
      
      temp_sum_partworth = temp_sum_partworth + as.numeric(part_worth[full_var_name])
    } else {
      full_var_name = var_mapping$Var[(var_mapping$X == var_name)]
      temp_sum_partworth = temp_sum_partworth + as.numeric(part_worth[full_var_name])*var_level
    }
    
  }
  
  sum_partworth[i] = temp_sum_partworth
}


# Calculate likelihood of selecting the responses
merged_response_data$Insample = merged_response_data$ID %in% selected_ID
responses = merged_response_data[,c("Insample","ID","Task","Concept","value")]
responses$sum_partworth = sum_partworth
require(data.table)
responses = as.data.table(responses)
responses = responses[,c("prob", "none_prob") := list(exp(sum_partworth) / (1+sum(exp(sum_partworth))), 1/(1+sum(exp(sum_partworth)))), by = list(ID,Task)]

responses$final_prob = ifelse(responses$value == responses$Concept, responses$prob, 
                              ifelse((responses$value == 4) & (responses$Concept == 1), responses$none_prob, 0))

likelihood = responses[,list(likelihood = sum(final_prob)), by = list(Insample,ID, Task)]

total_likelihood = likelihood[, list(LH = exp(mean(log(likelihood)))), by = list(Insample,ID)] # TBD what likelihood is actually used by Sawtooth
hist(total_likelihood$LH[total_likelihood$Insample])
hist(total_likelihood$LH[!total_likelihood$Insample])



