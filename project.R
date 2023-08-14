
```{r}
setwd("######/Project")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(randomForest)
library(tree)
```

```{r}
CD = read.csv("CountryData.csv")
dim(CD)
CD
CD12 = CD[!grepl("..",CD$X2012..YR2012.,fixed = TRUE),]
CD13 = CD12[!grepl("..",CD12$X2013..YR2013.,fixed = TRUE),]
CD14 = CD13[!grepl("..",CD13$X2014..YR2014.,fixed = TRUE),]
CD15 = CD14[!grepl("..",CD14$X2015..YR2015.,fixed = TRUE),]
CD16 = CD15[!grepl("..",CD15$X2016..YR2016.,fixed = TRUE),]
CD17 = CD16[!grepl("..",CD16$X2017..YR2017.,fixed = TRUE),]
CD18 = CD17[!grepl("..",CD17$X2018..YR2018.,fixed = TRUE),]
CD19 = CD18[!grepl("..",CD18$X2019..YR2019.,fixed = TRUE),]
CD19

write.csv(CD19,"######/Project/C19.csv", row.names = TRUE)
#From the written output, measures were selected
ML = read.csv("MeasuresList.csv")
ML_S = c("Access to clean fuels and technologies for cooking (% of population)", "Access to electricity (% of population)", "Age dependency ratio (% of working-age population)", "Agricultural land (% of land area)", "Agriculture, forestry, and fishing, value added (% of GDP)", "Annual freshwater withdrawals, agriculture (% of total freshwater withdrawal)", "Annual freshwater withdrawals, domestic (% of total freshwater withdrawal)", "Annual freshwater withdrawals, industry (% of total freshwater withdrawal)", "Arable land (% of land area)", "Arable land (hectares per person)", "Armed forces personnel (% of total labor force)", "Birth rate, crude (per 1, people)", "Broad money (% of GDP)", "CO2 emissions (metric tons per capita)", "Commercial bank branches (per 1, adults)", "Compulsory education, duration (years)", "Cost of business start-up procedures (% of GNI per capita)", "Current account balance (% of GDP)", "Current health expenditure per capita (current US$)", "Domestic credit to private sector (% of GDP)", "Domestic general government health expenditure (% of current health expenditure)", "Domestic private health expenditure (% of current health expenditure)", "Employment in agriculture (% of total employment) (modeled ILO estimate)", "Employment in industry (% of total employment) (modeled ILO estimate)", "Employment in services (% of total employment) (modeled ILO estimate)", "Fertilizer consumption (kilograms per hectare of arable land)", "Fixed telephone subscriptions (per 1 people)", "Foreign direct investment, net inflows (% of GDP)", "Foreign direct investment, net outflows (% of GDP)", "Forest area (% of land area)", "GDP per capita (current US$)", "Immunization, DPT (% of children ages 12-23 months)", "Immunization, HepB3 (% of one-year-old children)", "Immunization, measles (% of children ages 12-23 months)", "Industry (including construction), value added (% of GDP)", "Inflation, consumer prices (annual %)", "Labor force participation rate for ages 15-24, total (%) (modeled ILO estimate)", "Labor tax and contributions (% of commercial profits)", "Manufacturing, value added (% of GDP)", "Mobile cellular subscriptions (per 1 people)", "Monetary Sector credit to private sector (% GDP)", "Other taxes payable by businesses (% of commercial profits)", "Out-of-pocket expenditure (% of current health expenditure)", "People using at least basic drinking water services (% of population)", "People using at least basic sanitation services (% of population)", "Population ages -14 (% of total population)", "Population ages 15-64 (% of total population)", "Population ages 65 and above (% of total population)", "Population density (people per sq. km of land area)", "Population growth (annual %)", "Population in the largest city (% of urban population)", "Preprimary education, duration (years)", "Prevalence of anemia among children (% of children ages 6-59 months)", "Prevalence of undernourishment (% of population)", "Primary education, duration (years)", "Primary school starting age (years)", "Profit tax (% of commercial profits)", "Refugee population by country or territory of origin", "Renewable energy consumption (% of total final energy consumption)", "Rural population (% of total population)", "Secondary education, duration (years)", "Secure Internet servers (per 1 million people)", "Self-employed, total (% of total employment) (modeled ILO estimate)", "Services, value added (% of GDP)", "Statistical Capacity Score (Overall Average) (scale  - 1)", "Transport services (% of commercial service exports)", "Transport services (% of commercial service imports)", "Tuberculosis case detection rate (%, all forms)", "Vulnerable employment, female (% of female employment) (modeled ILO estimate)", "Wage and salaried workers, total (% of total employment) (modeled ILO estimate)", "Women Business and the Law Index Score (scale 1-1)", "Women's share of population ages 15+ living with HIV (%)")

CDC = CD19[is.element(CD19$Series.Name,  ML_S),]
#CDC
head(CDC)
write.csv(CDC,"######/Project/CleanCountryDataCSV.csv", row.names = TRUE)
```


```{r}
CD = read.csv("CleanCountryDataCSV.csv", header = TRUE)
dim(CD)
head(CD[,c(1:3)]) #just checking data
#Countries: 100, Year: 8, Measures: 50 (total initial)
```

**Data exploration and further cleansing**
*Checking for collinearity* 
```{r}
#Getting 2019 as a sample from CD to create data understanding plots. This speeds up the exploration process - using all data would take computationally too long, for little benefit, it is a fair assumption that one year relationships are similar compared to other years.
CD19 = CD[CD$Year == 2019,]
colnames(CD19)

#Splitting data into smaller chunks for pairs analysis, to make it readable with plot margins
pairs(CD19[,c(11:18)]) #We tested different ranges here which covered all possible interactions, removing highly correlated variables each time


#Acting on learnings, creating a new reference data set
CD=CD[,-c(3,18,21,26,31,33,45,48)] #Removing variables which caused obvious collinearity

```


**Getting our two main data sets ready, and final visualizations**
```{r}
#Splitting data into normal 'level' and 'log' transformations
#Log
colnames(CD)
CD_log = log(CD[,-c(1,2,14,34,35,38)]+2) #transforming all columns except Country Name, Year, and the 3 non-continuous variables
CD_log = data.frame(CD[,c(1,2,14,34,35,38)],CD_log) #re-merging all variables together
dim(CD_log)

#Extracting 2019 sample for final visualizations
CD19 = CD[CD$Year == "2019",]
CD19_log = CD_log[CD_log$Year == "2019",]
colnames(CD19)
colnames(CD19_log)

#Visualizing: all variables plotted independently against GDP per capita
CD19[,-c(1,2)] %>%
  gather(-GDP.per.capita..current.US.., key = "value", value = "Measure") %>%
  ggplot(aes(x = Measure, y = GDP.per.capita..current.US..)) +
    geom_point() +
    facet_wrap(~ value, scales = "free")

CD19_log[,-c(1,2)] %>%
  gather(-GDP.per.capita..current.US.., key = "value", value = "Measure") %>%
  ggplot(aes(x = Measure, y = GDP.per.capita..current.US..)) +
  geom_point() +
    facet_wrap(~ value, scales = "free")
```

**Splitting into Training and Testing Dataset**
```{r}
dim(CD)
colnames(CD)
attach(CD)
#Creating our class variable for Analysis: Part 1 Tree and Random Forest Models
Class = rep("LI", 800)
Class[GDP.per.capita..current.US.. > 1085 & GDP.per.capita..current.US.. <= 4256] = "LMI"
Class[GDP.per.capita..current.US.. > 4256 & GDP.per.capita..current.US.. <= 13206] = "UMI"
Class[GDP.per.capita..current.US.. > 13206] = "HI"
CD = data.frame(CD, Class)
summary(CD$Class) #Need to refactor Class, currently treated as characters
CD$Class = as.factor(CD$Class)
summary(CD$Class)
#Using again 2019 Country Data to get our Country List, to then split into training and testing data
CD19 = CD[CD$Year == 2019,]
CD19_ordered = CD19[order(CD19$GDP.per.capita..current.US.., decreasing = TRUE),]
CL = CD19_ordered$Country
CL

#High Income Country 80/20 Training/Testing Split 
HighIncomeCountryList = CL[1:33]
HighIncomeCountryList
sample(HighIncomeCountryList, round(length(HighIncomeCountryList)*0.8))
HICL_training = c("Israel","Austria","Japan","Germany","Denmark","Saudi Arabia","Belgium","Netherlands","Lithuania","Norway","Portugal","Estonia","Ireland","United Arab Emirates","New Zealand","Spain","Korea, Rep.","United States","Sweden","France","United Kingdom","Hungary","Panama","Chile","Kuwait","Finland")
#Set.seed() could also be used, but this helps better ensure given the project carried out over multiple weeks and ran multiple times
HICL_testing = HighIncomeCountryList[!(HighIncomeCountryList %in% HICL_training)]
HICL_training
HICL_testing

#Upper-Middle Income Country 80/20 Training/Testing Split 
UpperMiddleIncomeCountryList = CL[34:60]
UpperMiddleIncomeCountryList
sample(UpperMiddleIncomeCountryList, round(length(UpperMiddleIncomeCountryList)*0.8),0)
UMCL_training = c("Cuba","China","Azerbaijan","Dominican Republic","Gabon","Thailand","Belarus","Ecuador","Guatemala","Georgia","Armenia","Jamaica","Costa Rica","Colombia","Brazil","Serbia","North Macedonia","El Salvador","Paraguay","Mongolia","Namibia","Romania")
UMCL_testing = UpperMiddleIncomeCountryList[!(UpperMiddleIncomeCountryList %in% UMCL_training)]
UMCL_training
UMCL_testing

#Lower-Middle Income Country 80/20 Training/Testing Split 
LowerMiddleIncomeCountryList = CL[61:88]
LowerMiddleIncomeCountryList
sample(LowerMiddleIncomeCountryList, round(length(LowerMiddleIncomeCountryList)*0.8),0)
LMCL_training = c("Benin", "Egypt, Arab Rep.", "Indonesia", "Kenya", "Senegal", "Nigeria", "India","Angola","Algeria","Ukraine","Sri Lanka","Philippines","Uzbekistan", "Bangladesh","Pakistan", "Kyrgyz Republic","Ghana","Morocco","Cameroon","Congo, Rep.","Tunisia","Myanmar")    
LMCL_testing = LowerMiddleIncomeCountryList[!(LowerMiddleIncomeCountryList %in% LMCL_training)]
LMCL_training
LMCL_testing

#Low Income Country 80/20 Training/Testing Split
LowIncomeCountryList = CL[89:100]
LowIncomeCountryList
sample(LowIncomeCountryList, round(length(LowIncomeCountryList)*0.8),0)
LICL_training = c("Burkina Faso","Madagascar","Tanzania","Central African Republic","Rwanda","Gambia, The","Congo, Dem. Rep.","Malawi","Togo","Afghanistan")
LICL_testing = LowIncomeCountryList[!(LowIncomeCountryList %in% LICL_training)]
LICL_training
LICL_testing

CL_training = c(HICL_training, UMCL_training, LMCL_training, LICL_training)
CL_testing = c(HICL_testing, UMCL_testing, LMCL_testing, LICL_testing)
CL_training
CL_testing

CD_training = CD %>% filter(CD$Country %in% CL_training)
CD_testing = CD %>% filter(CD$Country %in% CL_testing)
#CD_training
#CD_testing
```


**Analysis 1: Tree and Random Forest Classification**
We don't have to use linearised data, as transformations do not impact tree and random forest ML models
```{r}
tree1 = tree(Class~.-GDP.per.capita..current.US.., data = CD_training[,-c(1,2)])
summary(tree1)
treecheck = cv.tree(tree1, FUN = prune.misclass)
treecheck$size
#Testing tree2$size confirms that our initial tree1 with 12 nodes is the CV-selected model, so no need to prune further
plot(tree1, col="blue", lwd = 1.5)
text(tree1, pretty = 0, cex= 1.5)

#Getting prediction
tree1Pred = predict(tree1, CD_training, type = "vector")
tree1Pred
summary(tree1Pred)

#Establishing CostBenefit Matrix matrix
CB = matrix(c(0,-1,-0.75,-0.5,-1,0,-0.5,-0.75,-0.75,-0.5,0,-0.5,-0.5,-0.75,-0.5,0), nrow = 4, byrow = T)
colnames(CB) = c("HI", "LI", "LMI","UMI")
rownames(CB) = c("HI", "LI", "LMI","UMI")
CB

#Optimizing model cut-off probability
alpha = seq(0.05, 0.9, 0.001)
CP = vector(length = length(alpha))
#CP = Cost Penalty
for (i in 1:length(alpha)) {
  predClass = ifelse((tree1Pred[,2] >= alpha[i]),"Low income"
                     , ifelse((tree1Pred[,3] >= alpha[i]), "Lower-middle income"
                              , ifelse((tree1Pred[,4] >= alpha[i]), "Upper-middle income"
                                       ,"High income")))
  
  tree1Confusion = table(predClass, CD_training$Class, deparse.level = 2)
  
  CP[i] = sum(CB * tree1Confusion)/sum(tree1Confusion)
}

plot(alpha, CP, lwd = 2, type="s", col = "darkred",xlab = "Cut-off Probabiltiy", ylab = "Cost Penalty (CP)")

alpha[CP == max(CP)] #We chose 0.5 cut-off as it was within this range

predClass_0.5 = ifelse((tree1Pred[,1] >= 0.5),"High-income", ifelse((tree1Pred[,3] >= 0.5), "Lower-middle income", ifelse((tree1Pred[,4] >= 0.5), "Upper-middle income","Low income")))

tree1Confusion_0.5 = table(predClass_0.5, CD_training$Class, deparse.level = 2)
tree1Confusion_0.5
CP_0.5= sum(CB * tree1Confusion_0.5)/sum(tree1Confusion_0.5)
CP_0.5
```

*Applying tree1 to Testing Data*
```{r}
tree1PredTest = predict(tree1, CD_testing, type = "vector")

predClassTest_0.5 = ifelse((tree1PredTest[,1] >= 0.5),"HI", ifelse((tree1PredTest[,3] >= 0.5), "LMI", ifelse((tree1PredTest[,4] >= 0.5), "UMI","LI")))
tree1ConfusionTest_0.5 = table(predClassTest_0.5, CD_testing$Class, deparse.level = 2)
tree1ConfusionTest_0.5

#Finding the one country case where the model was off by two classes.
CE = data.frame(CD_testing,predClassTest_0.5)
dim(CE)
colnames(CE)
CE[,c(1,2,21,43,44)]

CP_test_0.5 = sum(CB*tree1ConfusionTest_0.5)/sum(tree1ConfusionTest_0.5)
CP_test_0.5
```

*Creating Random Forest model*
```{r}
rf1 = randomForest(Class~.-GDP.per.capita..current.US.., data = CD_training)
rf1
predClassTest = predict(rf1, CD_testing)
#Cut of probability not applicable in the same way to random forest
Confusion = table(predClassTest, CD_testing$Class)
Confusion
PRFTest = sum(CB*Confusion)/sum(Confusion)
PRFTest
```


**Analysis Part 2: Linear Models**
*This following code section was not included in the report, as results were unimpressive compared to when using linearised data (code for that below). This was expected, but we still wanted to try both to compare.*
```{r}
colnames(CD_training)

linear1 = lm(GDP.per.capita..current.US..~.-GDP.per.capita..current.US.., data = CD_training[,-c(1,2,43)]) #removing country, year, and class variable
summary(linear1)

linear2 = update(linear1, ~.-Tuberculosis.case.detection.rate.....all.forms.)
summary(linear2)

linear3 = update(linear2, ~.-Manufacturing..value.added....of.GDP.)
summary(linear3)

linear4 = update(linear3, ~.-Population.density..people.per.sq..km.of.land.area.)
summary(linear4)

linear5 = update(linear4, ~.-Secondary.education..duration..years.)
summary(linear5)

linear6 = update(linear5, ~.-Prevalence.of.anemia.among.children....of.children.ages.6.59.months.)
summary(linear6)

linear7 = update(linear6, ~.-Age.dependency.ratio....of.working.age.population.)
summary(linear7)

linear8 = update(linear7, ~.-Population.growth..annual...)
summary(linear8)

linear9 = update(linear8, ~.-Primary.school.starting.age..years.)
summary(linear9)

l = linear4
pred = predict(l, newdata=CD_testing)
RMSE = sqrt(mean((CD_testing$GDP.per.capita..current.US.. - pred)^2))
RMSE
max(CD_testing$GDP.per.capita..current.US.. - pred)
#linear 4 the best 

pred4 = predict(linear4, newdata = CD_testing)
RMSE4 = sqrt(mean((CD_testing$GDP.per.capita..current.US.. - pred4)^2))
RMSE4

CD_testing_result = data.frame(CD_testing, pred4)
dim(CD_testing_result)
colnames(CD_testing_result)
CD_testing_result_mini = CD_testing_result[,c(1,2,21,44)] #mini to understand results more easily
```
**End of code excluded from report**


*Creating linear regression using linearized data*
```{r}
#First generating our LOG training and testing data
CD_training_log = CD_log %>% filter(CD_log$Country %in% CL_training)
CD_testing_log = CD_log %>% filter(CD_log$Country %in% CL_testing)
colnames(CD_training_log)
CD_training_log

linlinear1 = lm(GDP.per.capita..current.US.. ~.-GDP.per.capita..current.US.., data = CD_training_log[,-c(1,2)])
summary(linlinear1)

linlinear2 = update(linlinear1, ~.-Annual.freshwater.withdrawals..agriculture....of.total.freshwater.withdrawal.)
summary(linlinear2)

linlinear3 = update(linlinear2, ~.-Wage.and.salaried.workers..total....of.total.employment...modeled.ILO.estimate.)
summary(linlinear3)

linlinear4 = update(linlinear3, ~.-Rural.population....of.total.population.)
summary(linlinear4)

linlinear5 = update(linlinear4, ~.-People.using.at.least.basic.sanitation.services....of.population.)
summary(linlinear5)

linlinear6 = update(linlinear5, ~.-Prevalence.of.anemia.among.children....of.children.ages.6.59.months.)
summary(linlinear6)

linlinear7 = update(linlinear6, ~.-Population.in.the.largest.city....of.urban.population.)
summary(linlinear7)

linlinear8 = update(linlinear7, ~.-Manufacturing..value.added....of.GDP.)
summary(linlinear8)

linlinear9 = update(linlinear8, ~.-Women.s.share.of.population.ages.15..living.with.HIV....)
summary(linlinear9)

linlinear10 = update(linlinear9, ~.-Age.dependency.ratio....of.working.age.population.)
summary(linlinear10)
linlinear11 = update(linlinear10, ~.-Annual.freshwater.withdrawals..domestic....of.total.freshwater.withdrawal.)
summary(linlinear11)

linlinear12 = update(linlinear11, ~.-Population.density..people.per.sq..km.of.land.area.)
summary(linlinear12)

linlinear13 = update(linlinear12, ~.-Arable.land..hectares.per.person.)
summary(linlinear13)

linlinear14 = update(linlinear13, ~.-Population.growth..annual...)
summary(linlinear14)

linlinear15 = update(linlinear14, ~.-Prevalence.of.undernourishment....of.population.)
summary(linlinear15)

linlinear16 = update(linlinear15, ~.-Immunization..DPT....of.children.ages.12.23.months.)
summary(linlinear16)

ll = linlinear14
predl = predict(ll, newdata=CD_testing_log)
lRMSE = sqrt(mean((CD_testing_log$GDP.per.capita..current.US.. - predl)^2))
lRMSE
#Iteration 14 is the best


#Updating model 14, after evaluating remaining variables for further collinearity
linlinear17 = update(linlinear14, ~.-Agriculture..forestry..and.fishing..value.added....of.GDP.)
summary(linlinear17)


linlinear18 = update(linlinear17, ~.-Out.of.pocket.expenditure....of.current.health.expenditure.)
summary(linlinear18)

linlinear19 = update(linlinear18, ~.-Primary.education..duration..years.)
summary(linlinear19)

linlinear20 = update(linlinear19, ~.-Secondary.education..duration..years.)
summary(linlinear20)

linlinear21 = update(linlinear20, ~.-Services..value.added....of.GDP.)
summary(linlinear21)

linlinear22 = update(linlinear21, ~.-Armed.forces.personnel....of.total.labor.force.)
summary(linlinear22)

#linlinear 20 is the best in terms of mitigating collinearity from linlinear14
predl20 = predict(linlinear20, newdata = CD_testing_log)
RMSEl20 = sqrt(mean((CD_testing_log$GDP.per.capita..current.US.. - predl20)^2))
RMSEl20


CD_testing_log_result = data.frame(CD_testing_log, predl20)
dim(CD_testing_log_result)
colnames(CD_testing_log_result)
CD_testing_log_result_mini = CD_testing_log_result[,c(1,2,24,43)]


CD_testing_log[,1]
country = "Slovak Republic" #testing different countries here 
CD_testing_log_result_mini= CD_testing_log_result_mini[CD_testing_log_result_mini$Country == country,]

CD_testing_log_result_mini
CD_testing_rev_transformed_mini = 2.71828182845904^CD_testing_log_result_mini[,c(3,4)]-2
CD_testing_rev_transformed_mini

CD_final_testing = data.frame(CD_testing_log_result_mini[,c(1,2)],CD_testing_rev_transformed_mini)


plot(CD_final_testing$Year, CD_final_testing$GDP.per.capita..current.US.., type="b", ylim=c(0,90000), ylab = "GDP per Capita (current USD)", xlab = "Year", main = country)

lines(CD_final_testing$Year, CD_final_testing$predl20, type="b", col="red")
```
**END OF CODE SCRIPT**
