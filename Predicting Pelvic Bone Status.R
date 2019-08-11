##### PROJECT:  Predicting Pelvic Bone Status   #####

library(readr)
library(ggplot2)
library(caret)
library(normalr)
library(neuralnet)

# Import dataset
ortho.data <- read_csv("Pelvic_Bone_Dataset.csv")
View(ortho.data)


# Check for missing values 
missing.values <- sum(is.na(ortho.data))
missing.values

# converting char to factor, since factors are better handled by R
str(ortho.data)
ortho.data$class <- as.factor(ortho.data$class)
str(ortho.data)

# Observing summary statisticsof all variables
summary(ortho.data) 

# Get a summary of how many entries are classified as Normal and Abnormal
table(ortho.data$class)


#### ANALYZING VARIABLES WITH VISUALIZATION , BEFORE WE NORMALIZE #####


###### variable 1: Pelvic Incidences  ######

# O/P variable against Variable 1
ggplot(ortho.data, aes(x = pelvic_incidence, fill = factor(class))) +
  geom_dotplot(width = 0.8) +
  xlab("Pelvic Incidences") +
  ylab("Count") +
  labs(fill = "class")

# Boxplot
boxplot(ortho.data$pelvic_incidence ~ ortho.data$class, xlab = "CLASS", ylab = "PELVIC INC")
hist(ortho.data$pelvic_incidence, xlab = "PELVIC INC")



######   Variable 2: pelvic_tilt numeric  ######

# O/P variable against Vaariable 2
ggplot(ortho.data, aes(x = ortho.data$`pelvic_tilt_numeric`, fill = factor(class))) +
  geom_dotplot(width = 0.8) +
  xlab("pelvic_tiltnumeric") +
  ylab("Count") +
  labs(fill = "class")


# Boxplot
boxplot(ortho.data$`pelvic_tilt_numeric` ~ ortho.data$class, xlab = "CLASS", ylab = "PELVIC TILT")
hist(ortho.data$pelvic_tilt_numeric, xlab = "PELVIC TILT")



######  Variable 3:  lumbar_lordosis_angle  ######

# O/P variable against Vaariable 3
ggplot(ortho.data, aes(x = ortho.data$lumbar_lordosis_angle, fill = factor(class))) +
  geom_dotplot(width = 0.8) +
  xlab("lumbar_lordosis_angle") +
  ylab("Count") +
  labs(fill = "class")


# Boxplot
boxplot(ortho.data$lumbar_lordosis_angle ~ ortho.data$class, xlab = "CLASS", ylab = "LUMBAR LORDOSIS ANGLE")
hist(ortho.data$lumbar_lordosis_angle, xlab = "LUMBAR LORDOSIS ANGLE")




######  Variable 4:  sacral_slope  ######

# O/P variable against Variable 4
ggplot(ortho.data, aes(x = ortho.data$sacral_slope, fill = factor(class))) +
  geom_dotplot(width = 0.8) +
  xlab("sacral_slope") +
  ylab("Count") +
  labs(fill = "class")


# Boxplot
boxplot(ortho.data$sacral_slope ~ ortho.data$class, xlab = "CLASS", ylab = "SACRAL SLOPE")
hist(ortho.data$sacral_slope, xlab = "SACRAL SLOPE")

# Removing the outlier observed in sacral slope histogram
x <- which.max(ortho.data$sacral_slope)
ortho.data[x,]
ortho.data$sacral_slope[x] <- mean(ortho.data$sacral_slope)

# No skewness in Sacral Slope
hist(ortho.data$sacral_slope, xlab = "SACRAL SLOPE")


###### Variable 5: pelvic_radius  ######

# O/P variable against Vaariable 5
ggplot(ortho.data, aes(x = ortho.data$pelvic_radius, fill = factor(class))) +
  geom_dotplot(width = 0.8) +
  xlab("pelvic_radius") +
  ylab("Count") +
  labs(fill = "class")


# Boxplot
boxplot(ortho.data$pelvic_radius ~ ortho.data$class, xlab = "CLASS", ylab = "PELVIC RADIUS")
hist(ortho.data$pelvic_radius, xlab = "PELVIC RADIUS")



###### Variable 6: degree_spondylolisthesis  ######

# O/P variable against Vaariable 6
ggplot(ortho.data, aes(x = ortho.data$degree_spondylolisthesis, fill = factor(class))) +
  geom_dotplot() +
  xlab("degree_spondylolisthesis") +
  ylab("Count") +
  labs(fill = "class")

# Boxplot, Histogram and Scater plot
boxplot(ortho.data$degree_spondylolisthesis ~ ortho.data$class, xlab = "CLASS", ylab = "DEGREE SPONDYLOLISTHESIS")
hist(ortho.data$degree_spondylolisthesis, xlab = "DEGREE SPONDYLOLISTHESIS")

# Removing the outlier observed in histogram
y <- which.max(ortho.data$degree_spondylolisthesis)
ortho.data[y,]
ortho.data$degree_spondylolisthesis[y] <- mean(ortho.data$degree_spondylolisthesis)

# Skewness reduced in degree spondylolisthesis
hist(ortho.data$degree_spondylolisthesis, xlab = "DEGREE SPONDYLOLISTHESIS")




########## HEATMAPS   ##########

#spliting the first 6 columns to make the heatmap
num.data <- ortho.data[,1:6]
heatmap(cor(num.data), Rowv = NA, Colv = NA)

# heatmap with values
library(gplots)
heatmap.2(cor(num.data), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(num.data),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))




########## SCATTER PLOT MATRIX   ############

library(GGally)
ggpairs(num.data[, c(1:6)])


#### DISTRIBUTION OF NORMAL N ABNORMAL  ####

percentage <- prop.table(table(ortho.data$class)) * 100
cbind(freq=table(ortho.data$class), percentage=percentage)


########################   REMOVING OUTLIERS   ##########################  

# We select values from each varibale which lie within quantile of 1% to 99%
selected.col = apply(ortho.data[,1:6],2,function(x) which( quantile(x, 0.01) < x & x < quantile(x, 0.99) ))

# WE ONLY SELECT INDEXES COMMON TO ALL 6 VARIABLES
final.index = Reduce(intersect, data.frame(selected.col))

# SELECT OBSERVATIONS FROM THE DATA SET, HAVING THE INDEXES PRODUCED ABOVE
ortho.new = ortho.data[final.index,]
View(ortho.new)


percentage <- prop.table(table(ortho.new$class)) * 100
cbind(freq=table(ortho.new$class), percentage=percentage)

####### PLOTTING VARIABLES IN NEW DATA SET ###############

boxplot(ortho.new$pelvic_incidence ~ ortho.new$class, xlab = "CLASS", ylab = "PELVIC INC")
boxplot(ortho.new$pelvic_tilt_numeric ~ ortho.new$class, xlab = "CLASS", ylab = "PELVIC TILT")
boxplot(ortho.new$lumbar_lordosis_angle ~ ortho.new$class, xlab = "CLASS", ylab = "LUMBAR LORDOSIS ANGLE")
boxplot(ortho.new$sacral_slope ~ ortho.new$class, xlab = "CLASS", ylab = "SACRAL SLOPE")
boxplot(ortho.new$pelvic_radius ~ ortho.new$class, xlab = "CLASS", ylab = "PELVIC RADIUS")
boxplot(ortho.new$degree_spondylolisthesis ~ ortho.new$class, xlab = "CLASS", ylab = "DEGREE SPONDYLOLISTHESIS")



############## NORMALIZING THE FILTERED DATA ########################

ortho.norm = scale(ortho.new[,1:6], center = TRUE, scale = TRUE)
ortho.norm = cbind(ortho.norm,ortho.new[,7])
is.data.frame(ortho.norm)
View(ortho.norm)



############### OVERSAMPLING DATA WITH OBSERVATION HAVING CLASS 'NORMAL'  ####################

# WE NEED TO DIVIDE THE DATA BEFORE SAMPLING 
set.seed(1)
train_partition<-sample(c(1:dim(ortho.norm)[1]),dim(ortho.norm)[1]*0.7)
ortho.train<-ortho.norm[train_partition,]
ortho.test<-ortho.norm[-train_partition,]

View(ortho.train)
View(ortho.test)

####  ANALYZING THE PERCENT OF ABNORMAL & NORMAL IN TRAIN SET ######

percentage <- prop.table(table(ortho.train$class)) * 100
cbind(freq=table(ortho.train$class), percentage=percentage)


### FILTER THE CLASS RECORDS FROM TRAIN DATA AND GET THE DIFFERENCE ###
normal.class <- ortho.train[ortho.train$class == 'Normal',]
norm.row = nrow(normal.class)

abnormal.class <- ortho.train[ortho.train$class == 'Abnormal',]
abnorm.row = nrow(abnormal.class)
class.number = abnorm.row - norm.row


##### OVERSAMPLING #####

### ADD class.number NORMAL CLASS OBSERVATION TO TRAIN DATA ###
ortho.train.data = rbind(ortho.train, normal.class[1:class.number,])
View(ortho.train.data)

## PERCENTAGE OF NORMAL AND ABNORMAL SHOULD BE 50-50 ##
percentage <- prop.table(table(ortho.train.data$class)) * 100
cbind(freq=table(ortho.train.data$class), percentage=percentage)


####################################    NEURAL NETWORKS     ##########################################################

########### CREATING DUMMY VARIABLES FOR CLASS IN TRAIN DATASET ###############

dummy.set <- model.matrix(~ 0 + class, data = ortho.train.data)
dummy.set <- as.data.frame(dummy.set)
t(t(names(dummy.set))) # check the names of the dummy variables
head(dummy.set)
View(dummy.set)    

train.neural <- cbind(dummy.set,ortho.train.data[,-c(7)])
View(train.neural)

########### CREATING DUMMY VARIABLES FOR CLASS IN VALIDATION DATASET ###############

num3 <- sample(row.names(ortho.test), 82)
ortho.test <- ortho.test[num3,]

dummy <- model.matrix(~ 0 + class, data = ortho.test)
dummy <- as.data.frame(dummy)
t(t(names(dummy))) # check the names of the dummy variables
head(dummy)
View(dummy)    

ortho.test.data <- cbind(dummy,ortho.test[,-c(7)])
test.neural = ortho.test.data[,3:8]


############# NEURAL NETWORK TRAINING ##################

set.seed(1)
nn <- neuralnet(classAbnormal + classNormal ~ pelvic_incidence + pelvic_tilt_numeric + lumbar_lordosis_angle+ pelvic_radius + degree_spondylolisthesis, data = train.neural, linear.output = F, hidden = 6)
plot(nn, rep="best")
nn$result.matrix  


#### TRAINING ####
predicted.train <- compute(nn, train.neural[,-c(1,2,6)])
predicted.tclass.N = apply(predicted.train$net.result,1,which.max)-1 
predicted.tclass.A <- predicted.tclass.N + 1

a <- sum(train.neural$classNormal == predicted.tclass.N & train.neural$classNormal == 1)
b <- sum(train.neural$classNormal == 1)
train.normal.error <- b - a
train.normal.error

c <- sum(train.neural$classAbnormal == predicted.tclass.A & train.neural$classAbnormal == 1)
d <- sum(train.neural$classAbnormal == 1)
train.abnormal.error <- d - c
train.abnormal.error

train.error = (train.normal.error + train.abnormal.error)/nrow(train.neural)
train.error


#### VALIDATION ####
predicted.valid <- compute(nn, test.neural[,-c(4)])
predicted.vclass.N <- apply(predicted.valid$net.result,1,which.max)-1
predicted.vclass.A <- predicted.vclass.N + 1

e <- sum(ortho.test.data$classNormal == predicted.vclass.N & ortho.test.data$classNormal == 1)
f <- sum(ortho.test.data$classNormal == 1)
valid.normal.error <- f - e
valid.normal.error

g <- sum(ortho.test.data$classAbnormal == predicted.vclass.A & ortho.test.data$classAbnormal == 1)
h <- sum(ortho.test.data$classAbnormal == 1)
valid.abnormal.error <- h - g
valid.abnormal.error

valid.error <- (valid.normal.error + valid.abnormal.error)/nrow(test.neural)
valid.error







################################################  LOGISTIC REGRESSION #########################################################

logit.reg <- glm(class ~ ., data = ortho.train, family = "binomial")
options(scipen=999)
summary(logit.reg)

# use predict() with type = "response" to compute predicted probabilities.
logit.reg.pred <- predict(logit.reg, ortho.test[,-7], type = "response")

# first 5 actual and predicted records
View(data.frame(actual = ortho.test$class, predicted = logit.reg.pred))

