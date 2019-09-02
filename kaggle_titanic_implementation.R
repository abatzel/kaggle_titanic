
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


md.pattern(data)


# Set a random seed
set.seed(754)
set.seed(700)
# Make variables factors into factors
# factor_vars <- c('title','Sex')
# 
# data[factor_vars] = lapply(data[factor_vars], function(x) as.factor(x))

data[, Sex := as.factor(Sex)]
data[, title := as.factor(title)]

# Build the model (note: not all possible variables are used)
rf_model = randomForest(factor(Survived) ~ Pclass + Sex + title,
                         data = data)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_bw()
