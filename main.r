require(data.table)
require(TunePareto)
require(glmnet)

testStart=as.Date('2018-11-16')
trainStart=as.Date('2012-07-15')
rem_miss_threshold=0.01 #parameter for removing bookmaker odds with missing ratio greater than this threshold

source('C:\\Users\\a-adsene\\Desktop\\ETM58D\\data_preprocessing.r')
source('C:\\Users\\a-adsene\\Desktop\\ETM58D\\feature_extraction.r')
source('C:\\Users\\a-adsene\\Desktop\\ETM58D\\performance_metrics.r')
source('C:\\Users\\a-adsene\\Desktop\\ETM58D\\train_models.r')


# read data
matches_raw=readRDS("C:\\Users\\a-adsene\\Desktop\\ETM58D\\df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")
odd_details_raw=readRDS('C:\\Users\\a-adsene\\Desktop\\ETM58D\\df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds')

# preprocess matches
matches=matches_data_preprocessing(matches_raw)

# preprocess odd data
odd_details=details_data_preprocessing(odd_details_raw,matches)

# extract open and close odd type features from multiple bookmakers
features=extract_features.openclose(matches,odd_details,pMissThreshold=rem_miss_threshold,trainStart,testStart)

# divide data based on the provided dates 
train_features=features[Match_Date>=trainStart & Match_Date<testStart] 
test_features=features[Match_Date>=testStart] 

# run glmnet on train data with tuning lambda parameter based on RPS and return predictions based on lambda with minimum RPS
predictions=train_glmnet(train_features, test_features,not_included_feature_indices=c(1:5), alpha=1,nlambda=50, tune_lambda=TRUE,nofReplications=2,nFolds=10,trace=T)


