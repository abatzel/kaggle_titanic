
library(data.table)
library(ggplot2)
library(Amelia)
library(randomForest)

# define directories, inputs, outputs
dir = 'C:/Users/abatzel/Documents/kaggle_titanic/'

inFile_test = paste0(dir, 'test.csv')
inFile_train = paste0(dir, 'train.csv')

# load data
data = fread(inFile_train, stringsAsFactors = FALSE)

# initial exploratory analysis
data[, .(prop_survived = (sum(Survived)/.N)), by = "Sex"]

data[, rounded_age := round(Age, -1)]

dt = data[, .(prop_survived = (sum(Survived)/.N)), by = "rounded_age"]
setorderv(dt, 'rounded_age')

data[, .(prop_survived = (sum(Survived)/.N)), by = "Pclass"]

data[, .(prop_survived = (sum(Survived)/.N),
         tot_num = .N), by = "SibSp"]

data[, .(prop_survived = (sum(Survived)/.N),
         tot_num = .N), by = "Parch"]

# feature engineering
data[, title := gsub('(.*, )|(\\..*)', '', Name)]
data[, Surname := sapply(Name, function(x) strsplit(x, split = '[,.]')[[1]][1])]







