library("dplyr")
library("censusapi")
library("randomForest")
library("caTools")
library("smbinning")
library("cdlTools")
library("totalcensus")
library("ggplot2")
library("fastDummies")
library("forcats")
options(scipen=999)
df <- read.csv(file = "SCHOLASTICDATA_cleaning_V9_DKKL.csv", stringsAsFactors = FALSE)

apis <- listCensusApis()
View(apis)

df <- mutate_all(df, funs(na_if(.,"")))
hist(df$REVENUE)
### binning
dfUpdated$CH1Flag <- as.numeric(as.character(dfUpdated$CH1Flag))
bin <- smbinning(dfUpdated, "CH1Flag", "GENRE_REVENUE")
length(bin$bands)
df$CH2Flag <- as.numeric(as.character(df$CH2Flag))
bin <- smbinning(sample, "CH2Flag", "REVENUE")

### cuts
dfUpdated$revBins <- cut(dfUpdated$GENRE_REVENUE, bin$bands, 
                      labels=c("1","2","3","4", "5", "6", "7", "8", "9"),
    include.lowest=T)
str(df)

sample$revBins <- as.numeric(as.character(sample$revBins))

dfUpdated$CH1Flag <- as.factor(ifelse(dfUpdated$CHANNEL == "CHANNEL 1", 1,0))
dfUpdated$CH2Flag <- as.factor(ifelse(dfUpdated$CHANNEL == "CHANNEL 2", 1,0))

df$DIVIDE_CHANNEL <- as.factor(df$DIVIDE_CHANNEL) 
sample <- dfUpdated[sample(nrow(dfUpdated), 10000), ]

sample$CH1Flag <- as.factor(sample$CH1Flag)
sample$CH2Flag <- as.factor(sample$CH2Flag)
sample$HHI_BAND <- as.factor(sample$HHI_BAND)
split = sample.split(sample$EDU_HS_SOME_COLLEGE, SplitRatio = 0.7)
trainSparse = subset(sample, split==TRUE)
testSparse = subset(sample, split==FALSE)

fit = randomForest(CH2Flag ~ EDU_HS_SOME_COLLEGE, data=trainSparse, ntree = 500,
                        type="classification")
predictRF = predict(fit, newdata=testSparse)
table(testSparse$CH1Flag, predictRF)

# make dataframe from importance() output
feat_imp_df <- importance(fit) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

# middle school population
dfUpdated$middleSchoolFlag <- ifelse(dfUpdated$Lexile_LowestGrade == "from_grade6" | 
                                       dfUpdated$Lexile_LowestGrade == "from_grade7" |
                                       dfUpdated$Lexile_LowestGrade == "from_grade8", 1, 0)

middleSchools <- dfUpdated[dfUpdated$middleSchoolFlag == 1,]

middleSchools$CH1.GENRE.Split <- as.factor(middleSchools$CH1.GENRE.Split)
middleSchools$SERIES <- as.factor(middleSchools$SERIES)
middleSchools$HHI_BAND <- as.factor(middleSchools$HHI_BAND)
middleSchools$PROD_TYP <- as.factor(middleSchools$PROD_TYP)

linearMod <- lm(GENRE_TOTALUNITS ~ CH1.GENRE.Split + SERIES + PROD_TYP, data=middleSchools)  # build linear regression model on full data
print(linearMod)
summary(linearMod)
middleSchools$SERIES

# classifier
dfUpdated[dfUpdated==''] <- NA
nonNas <- dfUpdated[complete.cases(dfUpdated$Lexile_LowestGrade),]
nonNas <- dfUpdated[complete.cases(dfUpdated$CH1.GENRE.Split),]
nonNas <- dfUpdated[complete.cases(dfUpdated$CH2_CATEGORY),]
sample <- nonNas[sample(nrow(nonNas), 10000), ]

sample$STATE <- as.factor(sample$STATE)
sample$PROD_TYP <- as.factor(sample$PROD_TYP)
sample$SCHOOL_TYPE <- as.factor(sample$SCHOOL_TYPE)
sample$CH1.GENRE.Split <- as.factor(sample$CH1.GENRE.Split)
sample$CH1Flag <- as.factor(sample$CH1Flag)
sample$CH2Flag <- as.factor(sample$CH2Flag)
sample$HHI_BAND <- as.factor(sample$HHI_BAND)
sample$middleSchoolFlag <- as.factor(sample$middleSchoolFlag)
sample$CH2_CATEGORY <- as.factor(sample$CH2_CATEGORY)
split = sample.split(sample$middleSchoolFlag, SplitRatio = 0.7)
trainSparse = subset(sample, split==TRUE)
testSparse = subset(sample, split==FALSE)

fit = randomForest(middleSchoolFlag ~ CH1.GENRE.Split + CH2_CATEGORY, data=trainSparse, ntree = 500,
                   type="classification")
predictRF = predict(fit, newdata=testSparse)
table(testSparse$CH1Flag, predictRF)
summary(dfUpdated$CH1.GENRE.Split)
dfUpdated$CH2_CATEGORY


# dummy variables
dummyDf <- dummy_cols(dfUpdated, select_columns = "CH1.GENRE.Split", remove_first_dummy = TRUE)


# forcats
funny <- dfUpdated
funny$CH1.GENRE.Split <- fct_lump(funny$CH1.GENRE.Split, n = 7)
summary(funny$CH1.GENRE.Split)

# channel split
funny <- funny[complete.cases(funny),]
funny$CHANNEL <- as.factor(funny$CHANNEL)
funny$GENRE_TOTALUNITS <- scale(funny$GENRE_TOTALUNITS)
hist(funny$GENRE_TOTALUNITS)
linearMod <- lm(GENRE_REVENUE ~ CH1.GENRE.Split + CHANNEL, data=funny)  # build linear regression model on full data
print(linearMod)
summary(linearMod)
# plot dataframe
ggplot(feat_imp_df, aes(x = reorder(feature, MeanDecreaseGini), 
                        y = MeanDecreaseGini)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: <Model>"
  )

revenueByChannel <- df %>% 
  select(everything()) %>% 
  group_by(DIVIDE_CHANNEL, PROD_TYP) %>% 
  summarise(
    total = n(),
    revenue = sum(REVENUE, na.rm = TRUE),
    averageUnitPrice = mean(UNIT_PRICE, na.rm = TRUE)
  ) %>% 
  mutate(revenueByUnit = revenue / total,
         freqProdType = total / sum(total))

df$SCHOOL_TYPE

genreAnalysis <- df %>% 
  select(everything()) %>% 
  group_by(DIVIDE_CHANNEL, Lexile_GradeLevel_Category) %>% 
  summarise(
    total = n(),
    revenue = sum(REVENUE, na.rm = TRUE),
    averageUnitPrice = mean(UNIT_PRICE, na.rm = TRUE)
  ) %>% 
  mutate(revenueByUnit = revenue / total,
         freqProdType = total / sum(total))

genreAnalysis <- df %>% 
  select(everything()) %>% 
  group_by(DIVIDE_CHANNEL, Lexile_GradeLevel_Category) %>% 
  summarise(
    total = n(),
    revenue = sum(REVENUE, na.rm = TRUE),
    averageUnitPrice = mean(UNIT_PRICE, na.rm = TRUE)
  ) %>% 
  mutate(revenueByUnit = revenue / total,
         freqProdType = total / sum(total))

genreAnalysis2 <- df %>% 
  select(everything()) %>% 
  group_by(CHANNEL, Lexile_GradeLevels) %>% 
  summarise(
    total = n(),
    revenue = sum(REVENUE, na.rm = TRUE),
    averageUnitPrice = mean(UNIT_PRICE, na.rm = TRUE)
  ) %>% 
  mutate(revenueByUnit = revenue / total,
         freqProdType = total / sum(total))

genreAnalysisState <- df %>% 
  select(everything()) %>% 
  group_by(STATE, Lexile_GradeLevels) %>% 
  summarise(
    total = n(),
    revenue = sum(REVENUE, na.rm = TRUE),
    averageUnitPrice = mean(UNIT_PRICE, na.rm = TRUE)
  ) %>% 
  mutate(revenueByUnit = revenue / total,
         freqProdType = total / sum(total))

df1 <- df %>% 
  filter(Lexile_GradeLevels == "beyond_grade12")



library("PerformanceAnalytics")
sample$EDU_NO_HS <- as.numeric(sub("%", "", sample$EDU_NO_HS))
sample$EDU_HS_SOME_COLLEGE <- as.numeric(sub("%", "", sample$EDU_HS_SOME_COLLEGE))
sample$EDU_BACHELOR_DEG <- as.numeric(sub("%", "", sample$EDU_BACHELOR_DEG))
sample$EDU_GRADUATE_DEG <- as.numeric(sub("%", "", sample$EDU_GRADUATE_DEG))
sample$CH1Flag <- as.numeric(as.character(sample$CH1Flag))
sample$CH2Flag <- as.numeric(as.character(sample$CH2Flag))
sample$revBins <- as.numeric(as.character(sample$revBins))


my_data <- sample %>%
  select(REVENUE, EDU_NO_HS, EDU_HS_SOME_COLLEGE, EDU_BACHELOR_DEG, 
         EDU_GRADUATE_DEG, CH1Flag, CH2Flag, revBins)

chart.Correlation(my_data, histogram=TRUE, pch=19)


# census pull 

# listVars
sahie_vars <- listCensusMetadata(name = "timeseries/healthins/sahie", type = "variables")

acs_1yearVars <- listCensusMetadata(name = "acs/acs1/profile", type = "variables", vintage = 2018)

# demographic <- getCensus(name = "pep/charagegroups",
#           vars = c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"),
#           region = "us:*",
#           time = 2017)

# school enrollment estimates 1st-8th 2018
acs_1yearDF <- getCensus(name = "acs/acs1/profile", vars = c("DP02_0055E"), region = "county:*", vintage = 2018)


# central planning database
centralPlanningBlockgroupVars <- listCensusMetadata(name = "pdb/blockgroup", type = "variables", vintage = 2018)
centralPlanningBlockgroupDF <- getCensus(name = "pdb/blockgroup", vars = c("Othr_Lang_ACSMOE_13_17"), vintage = 2018, region = "block group:*", regionin = "state:01+county:001")

# Population 5 years and older that speak a language other than english at home
pdb <- getCensus(name = "pdb/blockgroup",
                 vintage = 2018,
                 vars = c("County_name", "State_name", "Othr_Lang_ACSMOE_12_16"),
                 region = "block group:*",
                 regionin = "state:01+county:*")
head(pdb)

# population 5-17 that speak another language at home
acs5yearVars <- listCensusMetadata(name = "acs/acs5", type = "variables", vintage = 2018)
acs5yearDF <- getCensus(name = "acs/acs5", vars = c("B16007_007E"), vintage = 2018, region = "county:*")
acs5yearDF$state <-  fips(acs5yearDF$state, to = "Name")
acs5yearDF$stateabbr <- state.abb[match(acs5yearDF$state, state.name)]

acs5yearDF <- acs5yearDF %>% 
  group_by(state, county)

acs5yearDF$countyName <- acs5yearDF$county 

data("dict_fips")
dict_fips <- totalcensus::dict_fips
for(i in 1:nrow(acs5yearDF)){
  acs5yearDF$countyName[i] <- totalcensus::convert_fips_to_names(acs5yearDF$county[i], states = acs5yearDF$stateabbr[i], geo_header = "COUNTY")
}

# time series data
timeseries5yearpopulation <- listCensusMetadata(name = "timeseries/idb/5year", type = "variables")

timeseries5yearpopulationDF <- getCensus(name = "timeseries/idb/5year",
                 vars = c("POP10_14", "YR"),
                 time = 2016,
                 FIPS = "US",
                 )

us_pop <- getCensus(name = "timeseries/idb/1year",
                        vars = c("NAME", "POP"),
                        time = 2050,
                        FIPS = "US",
                        AGE = "10:18")

timeseriespovertyschdist <- listCensusMetadata(name = "timeseries/poverty/saipe/schdist", type = "variables")

timeseriespovertyschdistDF <- getCensus(name = "timeseries/poverty/saipe/schdist",
                    vars = timeseriespovertyschdist$name[1:3],
                    region = "school district (unified):*",
                    regionin = "state:25",
                    time = 2017)

saipe_schools <- getCensus(name = "timeseries/poverty/saipe/schdist",
                           vars = c("SD_NAME", "SAEPOV5_17V_PT", "SAEPOVRAT5_17RV_PT", "GRADE", "YEAR"),
                           region = "school district (unified):*",
                           regionin = "state:*",
                           time = "from 2015 to 2017")
# name: the name of the API as defined by the Census, like "acs5" or "timeseries/bds/firms"
# vintage: the dataset year, generally required for non-timeseries APIs
# vars: the list of variable names to get
# region: the geography level to return, like state or county

# school districts
saipe_schools <- getCensus(name = "timeseries/poverty/saipe/schdist",
                           vars = c("SD_NAME", "SAEPOV5_17V_PT", "SAEPOVRAT5_17RV_PT"),
                           region = "school district (unified):*",
                           regionin = "state:25",
                           time = 2017)

saipe_schools$state <-  fips(saipe_schools$state, to = "Name")
saipe_schools$stateabbr <- state.abb[match(saipe_schools$state, state.name)]


# population by age, population over 5 speak english less than very well, population enrolled in school 1-8th
# under 18
acs1yearVars <- listCensusMetadata(name = "acs/acs1/cprofile", type = "variables", vintage = 2018)
acs1yearDF <- getCensus(name = "acs/acs1/cprofile", vars = c("CP04_2018_027E", 
                                                             "CP03_2018_087E",
                                                             "CP04_2018_143E", "CP03_2018_086E", "CP03_2018_088E", "CP05_2018_086E",
                                                             "CP03_2018_065E", 	"CP02_2018_039E",
                                                             "CP02_2018_152E", "CP02_2018_150E", "CP02_2018_055E", "CP04_2018_126E", "CP03_2018_009E",
                                                             "CP03_2018_008E", "CP02_2018_095E", "CP03_2018_128E"), vintage = 2018, region = "county:*")

acs1yearDF$state <-  fips(acs1yearDF$state, to = "Name")
acs1yearDF$stateabbr <- state.abb[match(acs1yearDF$state, state.name)]

dict_fips <- totalcensus::dict_fips
for(i in 1:nrow(acs1yearDF)){
  acs1yearDF$county[i] <- totalcensus::convert_fips_to_names(acs1yearDF$county[i], states = acs1yearDF$stateabbr[i], geo_header = "COUNTY")
}

acs1yearDF$county <- toupper(acs1yearDF$county)
acs1yearDF$county <- gsub(" COUNTY", "", acs1yearDF$county)

dfAnalysis <- dfUpdated
dfAnalysis <- df
dfAnalysis <- dummyDf

bye <- merge(dfAnalysis, acs1yearDF, by.x = c("COUNTY", "STATE"),
               by.y = c("county", "stateabbr"))

bye <- bye[,c(1:2,39:86)]
bye <- bye[,c(1:5, 9:33, 35:50)]

summarizer <- function(x) {
  if (grepl("split",x)) {
    sum(x, na.rm = TRUE)
  } else if(grepl("CP0", x)){
    mean(x, na.rm = TRUE)
  } else if(is.numeric(x)){
    sum(x, na.rm = TRUE)
  }
}


bye <- bye %>% group_by(STATE, COUNTY) %>%
  summarize_each(funs(summarizer))

bye2 <- bye %>% 
  group_by(STATE, COUNTY) %>% 
  summarise(revenueByCounty = sum(GENRE_REVENUE, na.rm = TRUE),
            TotalHousingUnits = mean(CP04_2018_027E, na.rm = TRUE),
            MeanFamilyIncome = mean(CP03_2018_087E, na.rm = TRUE),
            GrossRentAsPercentageHHI = mean(CP04_2018_143E, na.rm = TRUE),
            MedianFamilyIncome = mean(CP03_2018_086E, na.rm = TRUE),
            PerCapitaIncome = mean(CP03_2018_088E, na.rm = TRUE),
            MeanEarningsPerHouseHold = mean(CP03_2018_065E, na.rm = TRUE),
            WomenBetween15And50GaveBirthLastYear = mean(CP02_2018_039E, na.rm = TRUE),
            TotalHouseholdsWithInternetSubscription = mean(CP02_2018_152E, na.rm = TRUE),
            TotalHouseholdsWithComputerandInternet = mean(CP02_2018_150E, na.rm = TRUE),
            PopulationEnrolledInGrades1Through8 = mean(CP02_2018_055E, na.rm = TRUE),
            OccupiedUnitsPayingRent = mean(CP04_2018_126E, na.rm = TRUE),
            UnemploymentRate = mean(CP03_2018_009E, na.rm = TRUE),
            CivilianLaborForce = mean(CP03_2018_008E, na.rm = TRUE),
            ForeignBornPopulation = mean(CP02_2018_095E, na.rm = TRUE),
            percentageOfFamiliesBelowPoverty = mean(CP03_2018_128E, na.rm = TRUE)
            ) 

bye3 <- bye %>% 
  group_by(STATE, COUNTY) %>% 
  summarise_at(c("CH1.GENRE.Split_Advice & Inspiration", "CH1.GENRE.Split_Animal Stories",
                 "CH1.GENRE.Split_Biography & Autobiography", "CH1.GENRE.Split_Classics",
                 "CH1.GENRE.Split_Fairy Tales, Folk Tales & Fables", 
                 "CH1.GENRE.Split_Fantasy", "CH1.GENRE.Split_Historical Fiction",
                 "CH1.GENRE.Split_Horror & Supernatural", "CH1.GENRE.Split_How-To",
                 "CH1.GENRE.Split_Humor & Funny Stories", "CH1.GENRE.Split_Informative Nonfiction",
                 "CH1.GENRE.Split_Media", "CH1.GENRE.Split_Mystery & Suspense",
                 "CH1.GENRE.Split_Myths & Legends", "CH1.GENRE.Split_n/a",
                 "CH1.GENRE.Split_Narrative Nonfiction", "CH1.GENRE.Split_Poetry, Songs & Verse",
                 "CH1.GENRE.Split_Realistic Fiction", "CH1.GENRE.Split_Records, Fun Facts & Interactive",
                 "CH1.GENRE.Split_Reference", "CH1.GENRE.Split_Romance",
                 "CH1.GENRE.Split_School Stories", "CH1.GENRE.Split_Science Fiction",
                 "CH1.GENRE.Split_Spooky Stories", "CH1.GENRE.Split_Young Adult"), sum, na.rm = TRUE)

bye4 <- merge(bye2,bye3,by = c("STATE","COUNTY"))

bye4 <- bye4[,3:43]

bye4 <- bye4[,c(1,3:41)]

bye4 <- bye4[,c(1,16:40)]
linearMod <- lm(revenueByCounty ~ ., data=bye4)  # build linear regression model on full data
print(linearMod)
summary(linearMod)

dummy <- dummyDf[,c(41,45:69)]

linearMod <- lm(GENRE_TOTALUNITS ~ ., data=dummy)  # build linear regression model on full data
print(linearMod)
summary(linearMod)




dfUpdated$EDU_NO_HS <- as.numeric(sub("%", "", dfUpdated$EDU_NO_HS))
dfUpdated$EDU_HS_SOME_COLLEGE <- as.numeric(sub("%", "", dfUpdated$EDU_HS_SOME_COLLEGE))
dfUpdated$EDU_BACHELOR_DEG <- as.numeric(sub("%", "", dfUpdated$EDU_BACHELOR_DEG))
dfUpdated$EDU_GRADUATE_DEG <- as.numeric(sub("%", "", dfUpdated$EDU_GRADUATE_DEG))
dfUpdated$CHANNEL <- as.factor(dfUpdated$CHANNEL)
dfUpdated$HHI_BAND <- as.factor(dfUpdated$HHI_BAND)
dfUpdated$CH1.GENRE.Split <- as.factor(dfUpdated$CH1.GENRE.Split)
dfUpdated$CH2_CATEGORY <- as.factor(dfUpdated$CH2_CATEGORY)

linearMod <- lm(GENRE_REVENUE ~ CHANNEL + EDU_NO_HS + HHI_BAND + CH1.GENRE.Split + CH2_CATEGORY, data=dfUpdated)  # build linear regression model on full data

print(linearMod)
summary(linearMod)

fit1 <- lm(GENRE_REVENUE ~ CHANNEL + EDU_NO_HS + HHI_BAND + CH1.GENRE.Split + CH2_CATEGORY +
             PROD_TYP + SCHOOL_TYPE, data=sample)

fit1 <- lm(revenueByCounty ~ housingUnits, data=bye)

plot(GENRE_REVENUE ~ CHANNEL + EDU_NO_HS + HHI_BAND + CH1.GENRE.Split + CH2_CATEGORY +
       PROD_TYP + SCHOOL_TYPE, data=sample)
abline(fit1)

byehi <- merge(hello, bye, by = c("STATE", "COUNTY"))
byehi$CP02_2018_121E <- scale(byehi$CP02_2018_121E) 

linearMod <- lm(revenueByCounty.x ~ CP05_2018_019E + CP04_2018_027E + CP03_2018_087E + CP04_2018_081E, data=byehi)  # build linear regression model on full data
print(linearMod)
summary(linearMod)

fit1 <- lm(revenueByCounty ~ UnemploymentRate +
             OccupiedUnitsPayingRent + PopulationEnrolledInGrades1Through8 
           + TotalHousingUnits + 
             MeanFamilyIncome + PerCapitaIncome, data=bye)

fit1 <- lm(revenueByCounty ~ UnemploymentRate, data=bye)
plot(revenueByCounty ~ UnemploymentRate, data=bye)

plot(revenueByCounty ~ UnemploymentRate +
       OccupiedUnitsPayingRent + PopulationEnrolledInGrades1Through8 
     + TotalHousingUnits + 
       MeanFamilyIncome + PerCapitaIncome, data=bye)
fit1 <- lm(revenueByCounty ~ TotalHousingUnits, data=bye)
plot(revenueByCounty ~ TotalHousingUnits, data=bye)
abline(fit1)

dfUpdated <- read.csv(file = "SCHOLASTICDATA_cleaning_V11_DKKL_withGENREsplit_02062020.csv", stringsAsFactors = FALSE)

# var importance
sample <- dfUpdated[sample(nrow(dfUpdated), 300000), ]
sample <- sample[complete.cases(sample), ]
sample$CHANNEL <- as.factor(sample$CHANNEL) 
sample$CH1Flag <- as.factor(as.character(sample$CH1Flag))
sample$revenue_clusters <- as.factor(sample$revenue_clusters)
sample$revBins <- as.factor(sample$revBins)
sample$PROD_TYP <- as.factor(sample$PROD_TYP)
sample$CH2_CATEGORY <- as.factor(sample$CH2_CATEGORY)
sample$CH1.GENRE.Split <- as.factor(sample$CH1.GENRE.Split)
sample$HHI_BAND <- as.factor(sample$HHI_BAND)
sample$SCHOOL_TYPE <- as.factor(sample$SCHOOL_TYPE)
sample$SERIES <- as.factor(sample$SERIES)
sample$SCHOOL_TYPE <- as.factor(sample$SERIES)
sample$Lexile_GradeLevel_Category <- as.factor(sample$Lexile_GradeLevel_Category)
sample$STATE <- as.factor(sample$STATE)
sample <- sample %>% rename(ProductCategory = CH2_CATEGORY, ProductGenre = CH1.GENRE.Split, 
                            RevenueBins = revBins, PercentageWithoutHighSchool = EDU_NO_HS,
                            HouseholdIncomeBand = HHI_BAND, ProductType = PROD_TYP, SchoolLevel = Lexile_GradeLevel_Category,
                            SchoolType = SCHOOL_TYPE, PercentageSomeCollege = EDU_HS_SOME_COLLEGE,
                            PercentageBachelorDegree = EDU_BACHELOR_DEG, PercentageGraduateDegree = EDU_GRADUATE_DEG)
sample$EDU_NO_HS
summary(sample$revBins)
split = sample.split(sample$CH1Flag, SplitRatio = 0.7)
trainSparse = subset(sample, split==TRUE)
testSparse = subset(sample, split==FALSE)
sample$UNIT_PRICE
fit = randomForest(CH1Flag ~  + total_units + UNIT_PRICE +
                     SchoolLevel + STATE + SchoolType + ProductType, data=trainSparse, ntree = 500,
                   type="classification")
predictRF = predict(fit, newdata=testSparse)
table(testSparse$CH1Flag, predictRF)
12462 + 17412
# make dataframe from importance() output
feat_imp_df <- importance(fit) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

# plot dataframe
ggplot(feat_imp_df, aes(x = reorder(feature, MeanDecreaseGini), 
                        y = MeanDecreaseGini)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: <Model>"
  )

# roc curve
adult.rf.pred = ROCR::prediction(predictRF, testSparse)

#performance in terms of true and false positive rates
adult.rf.perf = performance(predictRF,"tpr","fpr")

#plot the curve
plot(adult.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

library(spatstat)
roc_full_resolution <- roc(testSparse$CH1Flag, as.numeric(predictRF))
rounded_scores <- round(as.numeric(predictRF), digits=1)
roc_rounded <- roc(testSparse$CH1Flag, rounded_scores)
plot(roc_full_resolution, print.auc=TRUE)

library(pROC)
roc_obj <- roc(testSparse$CH1Flag, as.numeric(predictRF))
auc(roc_obj)
## 
## Call:
## roc.default(response = test_set$bad_widget, predictor = glm_response_scores)
## 
## Data: glm_response_scores in 59 controls (test_set$bad_widget FALSE) < 66 cases (test_set$bad_widget TRUE).
## Area under the curve: 0.9037
lines(roc_rounded, col="red", type='b')
text(0.4, 0.43, labels=sprintf("AUC: %0.3f", auc(roc_rounded)), col="red")

#compute area under curve
library("ROCR")
auc <- performance(adult.rf.pred,"auc")

auc <- unlist(slot(auc, "y.values"))

minauc<-min(round(auc, digits = 2))
maxauc<-max(round(auc, digits = 2))
minauct <- paste(c("min(AUC) = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")

library("PerformanceAnalytics")
revenueByCounty ~ UnemploymentRate + CivilianLaborForce +
  OccupiedUnitsPayingRent + PopulationEnrolledInGrades1Through8 
+ TotalHouseholdsWithComputerandInternet + TotalHouseholdsWithInternetSubscription + TotalHousingUnits + 
  MeanFamilyIncome + PerCapitaIncome
my_data <- bye %>%
  select(STATE, COUNTY, revenueByCounty, UnemploymentRate, 
         OccupiedUnitsPayingRent, PopulationEnrolledInGrades1Through8, TotalHousingUnits, MeanFamilyIncome, 
         PerCapitaIncome)

chart.Correlation(my_data[,3:9], histogram=TRUE, pch=19)

# population 2017 1-8 grade enrolled in school
# 	CP02_2017_055E
library("stringr")
acs1yearDFEnrollment2017 <- getCensus(name = "acs/acs1/cprofile", vars = c("CP02_2017_055E", "CP02_2018_121E"), vintage = 2018, region = "county:*")

acs1yearDFEnrollment2017$state <-  fips(acs1yearDFEnrollment2017$state, to = "Name")
acs1yearDFEnrollment2017$stateabbr <- state.abb[match(acs1yearDFEnrollment2017$state, state.name)]

dict_fips <- totalcensus::dict_fips
for(i in 1:nrow(acs1yearDFEnrollment2017)){
  if(!is.na(acs1yearDFEnrollment2017$county[i])){acs1yearDFEnrollment2017$county[i] <- totalcensus::convert_fips_to_names(acs1yearDFEnrollment2017$county[i], states = acs1yearDFEnrollment2017$stateabbr[i], geo_header = "COUNTY")}
}

acs1yearDFEnrollment2017$county <- toupper(acs1yearDFEnrollment2017$county)
acs1yearDFEnrollment2017$county <- gsub(" COUNTY", "", acs1yearDFEnrollment2017$county)

dfAnalysis <- df 

hello <- merge(dfAnalysis, acs1yearDFEnrollment2017, by.x = c("COUNTY", "STATE"),
               by.y = c("county", "stateabbr"))

hello <- hello %>% 
  group_by(STATE, COUNTY, CP02_2017_055E, CP02_2018_121E) %>% 
  summarise(revenueByCounty = sum(REVENUE, na.rm = TRUE))

linearMod <- lm(revenueByCounty ~ CP02_2017_055E, data=hello)  # build linear regression model on full data
print(linearMod)
summary(linearMod)



Sys.setenv(CENSUS_KEY = "063f24ad08f55aec05ecb473bb26861ee48dbd74")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")


acs_comparison <- getCensus(name = "timeseries/healthins/sahie",
                            vintage = 2017, 
                            vars = c("NAME", "CP03_2013_025E", "CP03_2014_025E", "CP03_2015_025E", "CP03_2016_025E", "CP03_2017_025E"), 
                            region = "metropolitan statistical area/micropolitan statistical area:*")
head(acs_comparison)

sahie_counties <- getCensus(name = "timeseries/healthins/sahie",
                            vars = c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"), 
                            region = "county:*", 
                            regionin = "state:01,02", 
                            time = 2017)
head(sahie_counties, n=12L)




# convert zipcodes to counties
library("noncensus")
data(zip_codes)
data(counties)

state_fips  = as.numeric(as.character(counties$state_fips))
county_fips = as.numeric(as.character(counties$county_fips))    
counties$fips = state_fips*1000+county_fips    
zip_codes$fips =  as.numeric(as.character(zip_codes$fips))

# test
temp = subset(zip_codes, zip == "30329")    
subset(counties, fips == temp$fips)

# predictions linear
library(ISLR)
smp_siz = floor(.75*nrow(sample)) 
train_ind = sample(seq_len(nrow(sample)),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
train =sample[train_ind,] #creates the training dataset with row numbers stored in train_ind
test=sample[-train_ind,]  

linearMod <- lm(revenueByCounty ~ UnemploymentRate + CivilianLaborForce +
                  OccupiedUnitsPayingRent + PopulationEnrolledInGrades1Through8 
                + TotalHouseholdsWithComputerandInternet + TotalHouseholdsWithInternetSubscription + TotalHousingUnits + 
                  MeanFamilyIncome + PerCapitaIncome, data=train) 

linearMod <- lm(GENRE_REVENUE ~ CHANNEL + EDU_NO_HS + HHI_BAND + CH1.GENRE.Split + CH2_CATEGORY +
                  PROD_TYP + SCHOOL_TYPE, data=train)

# 0. Build linear model 
linearMod <- lm(revenueByCounty ~ UnemploymentRate + CivilianLaborForce +
                  OccupiedUnitsPayingRent + PopulationEnrolledInGrades1Through8 
                + TotalHousingUnits + 
                  MeanFamilyIncome + PerCapitaIncome, data=bye) 
# 1. Add predictions 
pred.int <- predict(linearMod, test, interval = "prediction")
mydata <- as.data.frame(cbind(test$GENRE_REVENUE, pred.int))
# 2. Regression line + confidence intervals
library("ggplot2")
p <- ggplot(mydata, aes(V1, fit)) +
  geom_point() +
  stat_smooth(method = lm)
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")




CHANNEL + EDU_NO_HS + HHI_BAND + CH1.GENRE.Split + CH2_CATEGORY +
  PROD_TYP + SCHOOL_TYPE
bye1 <- sample %>% 
  group_by(STATE, COUNTY) %>% 
  summarise(revenueByCounty = sum(GENRE_REVENUE, na.rm = TRUE),
            EDU_NO_HS = mean(EDU_NO_HS, na.rm = TRUE),
            MeanFamilyIncome = mean(CP03_2018_087E, na.rm = TRUE),
            GrossRentAsPercentageHHI = mean(CP04_2018_143E, na.rm = TRUE),
            MedianFamilyIncome = mean(CP03_2018_086E, na.rm = TRUE),
            PerCapitaIncome = mean(CP03_2018_088E, na.rm = TRUE),
            MeanEarningsPerHouseHold = mean(CP03_2018_065E, na.rm = TRUE),
            WomenBetween15And50GaveBirthLastYear = mean(CP02_2018_039E, na.rm = TRUE),
            TotalHouseholdsWithInternetSubscription = mean(CP02_2018_152E, na.rm = TRUE),
            TotalHouseholdsWithComputerandInternet = mean(CP02_2018_150E, na.rm = TRUE),
            PopulationEnrolledInGrades1Through8 = mean(CP02_2018_055E, na.rm = TRUE),
            OccupiedUnitsPayingRent = mean(CP04_2018_126E, na.rm = TRUE),
            UnemploymentRate = mean(CP03_2018_009E, na.rm = TRUE),
            CivilianLaborForce = mean(CP03_2018_008E, na.rm = TRUE),
            ForeignBornPopulation = mean(CP02_2018_095E, na.rm = TRUE)
  )