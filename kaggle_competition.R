df = read.csv("train.csv")
table(df$Survived,df$Sex)


barplot(table(df$Survived),
       names.arg = c("Perished", "Survived"),
       main="Survived (passenger fate)", col="black")
barplot(table(df$Pclass), 
        names.arg = c("first", "second", "third"),
        main="Pclass (passenger traveling class)", col="firebrick")
barplot(table(df$Sex), main="Sex (gender)", col="darkviolet")
hist(df$Age, main="Age", xlab = NULL, col="brown")
barplot(table(df$SibSp), main="SibSp (siblings + spouse aboard)", 
        col="darkblue")
barplot(table(df$Parch), main="Parch (parents + kids aboard)", 
        col="gray50")
hist(df$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, 
     col="darkgreen")
barplot(table(df$Embarked), 
        names.arg = c("Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (port of embarkation)", col="sienna")

mosaicplot(df$Pclass ~ df$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

mosaicplot(df$Sex ~ df$Survived, 
           main="Passenger Fate by Gender", shade=FALSE, color=TRUE, 
           xlab="Sex", ylab="Survived")

boxplot(df$Age ~ df$Survived, 
        main="Passenger Fate by Age",
        xlab="Survived", ylab="Age")

mosaicplot(df$Embarked ~ df$Survived, 
           main="Passenger Fate by Port of Embarkation",
           shade=FALSE, color=TRUE, xlab="Embarked", ylab="Survived")

# replace missing age values
males = df$Sex == 'male'
females = df$Sex == 'female'

avg_male_age = mean(df[males,"Age"], na.rm=TRUE)
avg_female_age = mean(Af[females,"Age"], na.rm=TRUE)
avg_male_age = mean(df[males,"Age"], na.rm=TRUE)

age_nas = is.na(df$Age)

df$agefill = df$age

repl_males = males & age_nas
df$agefill[repl_males] = avg_male_age

repl_females = females & age_nas
df$agefill[repl_females] = avg_female_age

# create Gender field

df$Gender[males] = 0
df$Gender[females] = 1

# Create 'child' field

child = df$agefill <= 15
df$Child = child

# run linear regression model

mymodel = lm(Survived ~ Gender+Pclass+Child,data=df)
summary(mymodel)

###
# Read in test data set
###

df_test = read.csv("test.csv")

# replace missing age values
males = df_test$Sex == 'male'
females = df_test$Sex == 'female'

avg_male_age = mean(df_test[males,"Age"], na.rm=TRUE)
avg_female_age = mean(df_test[females,"Age"], na.rm=TRUE)

# first fill with age field

df_test$agefill = df_test$Age

# replace missing values

age_nas = is.na(df_test$Age)
repl_males = males & age_nas
df_test$agefill[repl_males] = avg_male_age

repl_females = females & age_nas
df_test$agefill[repl_females] = avg_female_age


# create Gender field

df_test$Gender[males] = 0
df_test$Gender[females] = 1

# Create 'child' field

child = df_test$agefill <= 15
df_test$Child = child


mymodel = glm(Survived ~ Gender+Pclass+Child,data=df, family=binomial)
summary(mymodel)
predictTrain = predict(mymodel,type="response",newdata=df_test)

selectTrain = predictTrain > 0.5
df_test$Survived = 0
df_test$Survived[selectTrain] = 1


###
# write out results
###

output_data = subset(df_test,select=c(Survived, PassengerId))
write.csv(output_data, "KaggleSubmissionR.csv")

