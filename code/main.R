source("model.R")

set.seed(700)

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript main.R --fold n --train ../data/features.csv --report performance.csv", call.=FALSE)
} else {
  # Read arguments and store they in variables
  target <- input <- output <- c()
  for(arg in args){
    if(arg == "--fold" || arg == "--train" || arg == "--report") {
      curr_type <- arg
    } else {
      if(curr_type == "--fold") fold <- as.numeric(arg)
      else if(curr_type == "--train") train_path <- arg
      else if(curr_type == "--report") report_path <- arg
    }
  }
}

# Load Data
train_data <- read.csv(train_path, stringsAsFactors=F)

for(i in colnames(train_data)) {
  train_data[, i] <- as.numeric(train_data[, i])
}

# Random Shuffle Data
row_count = nrow(train_data)
train_data <- train_data[sample(1:row_count), ]

print(model1_acc(train_data))
print(model2_acc(train_data))
print(model3_acc(train_data))
print(model4_acc(train_data))