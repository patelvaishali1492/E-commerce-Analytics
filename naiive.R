# Bayes Theorem ; naive bayes classifier

# concept of behind probability is bayes theorem

# P(A/B) = [P(B/A) * P(A) / (P(B))] 

# machine1: 30 wrenches/hr
# machine2: 20 wrenches/hr
# out of all produced parts we can see that 1% are defective 
# out of all defective parts:
# We can SEE that 50% came from machine1
# and 50%  came from machine2

# Question what is the prob that a part produced by machine 2 is defective = ?

# P(def/machine2) = [ (P(machine2/Defect) * P(Defect)) / P(machine2)]
# (0.5* 0.01)/ (20/50) = 0.0.125 = 1.25%

# Step1: 
# P(walks/X) = Posterior Probability
# P(x/walks) = likelihood
# P(walks) = prior probability
# P(x) = MArginal likelihood
# Naive bayes classifier


# Posterior Probability = (likelihood * prior probability)/ MArginal likelihood

# Step2:
# P(Drivers/x) 

# from there we anlyse in which side the new data point will go

# when we have only 2 options
# P(Walks) = walks / total = 10/30
# P(X) draw a circle around new datapoint # select a radius # what radius does is # any fall in this circle is considers as the same cluster or side
# P(X) is prob of any random point to fall in that circle, what is the likelyhood of any point to fall in circle
# P(x) = Total no. of observation in circle / total observation = 4/30
# P(x/walks) draw same circle again; = no. of similar observations / Total no. of walkers = 3/10

# if we plug all above values that gives probability is the new person will walk

# Step3: # P(Drivers/X) = Posterior Probability
# P(x/Drivers) = likelihood = 1/20
# P(Drivers) = prior probability = 20/30
# P(x) = MArginal likelihood = 4/30
# Naive bayes classifier

# Prob(walk) >Prob(drive); so the new data point will belong to walk.

# Naive Bayes Classifier additional comment
# Why called naive ?
# Independence assumptions 
# from the above two equations denominator are same and will be cancelled. 
# We only need to compare likelihood and prior probabilties

# ###  Code for Naiive bayes

# Importing data

data = read.csv('Final.csv')
#data = data[3:5]
data <- data[c(1,2,59)]
# Encoding target featore as factor 
data$Hit_Miss = factor(data$Hit_Miss, levels = c(0, 1))

# splitting the dataset
library(caTools)
set.seed(123)
split = sample.split(data$Hit_Miss, SplitRatio = 0.7)
train = subset(data, split == T)
test = subset(data, split == F)

# Feature Scaling
#train[, 1:2] = scale(train[, 1:2])
#test[, 1:2] = scale(test[, 1:2])

# install packages
# install.packages("e1071")
library(e1071)

# Fitting the model
classifier = naiveBayes(x = train[, -3], y = train$Hit_Miss)
classifier

# A-priori probabilities:
#   train$Hit_Miss
# 0          1 
# 0.91711192 0.08288808 
# 
# Conditional probabilities:
#   Quantity
# train$Hit_Miss      [,1]      [,2]
# 0  8.908524 130.29006
# 1 27.594647  83.33831
# 
# UnitPrice
# train$Hit_Miss     [,1]     [,2]
# 0 4.211492 30.96096
# 1 0.000000  0.00000

# Predicting the test result
y_pred = predict(classifier, newdata = test[, -3])
y_pred

# confusion mtrix
cm = table(test[, 3], y_pred)
cm

# y_pred
# 0      1
# 0 145924    246
# 1     63  13148

# visualizing the training set result
library(ElemStatLearn)
set = train
x1 = seq(min(set[,1])-1, max(set[,1])+1, 0.01)
x2 = seq(min(set[,2])-1, max(set[,2])+1, 0.01)
grid_set = expand.grid(x1,x2)
colnames(grid_set) = c('Age','EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'Naive Bayes for Training Set',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))
contour(x1,x2, matrix(as.numeric(y_grid), length(x1),length(x2)), add =T)
points(grid_set, pch = '.', col= ifelse(y_grid ==1, 'springgreen3','tomato'))
points(set, pch =21, bg = ifelse(set[,3]==1, 'green4','red3'))

# visualizing test set result

library(ElemStatLearn)
set = test
x1 = seq(min(set[,1])-1, max(set[,1])+1, 0.01)
x2 = seq(min(set[,2])-1, max(set[,2])+1, 0.01)
grid_set = expand.grid(x1,x2)
colnames(grid_set) = c('Age','EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'Naive Bayes for Test Set',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))
contour(x1,x2, matrix(as.numeric(y_grid), length(x1),length(x2)), add =T)
points(grid_set, pch = '.', col= ifelse(y_grid ==1, 'springgreen3','tomato'))
points(set, pch =21, bg = ifelse(set[,3]==1, 'green4','red3'))

