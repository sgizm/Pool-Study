library(h2o)
h2o.init()
h2o.removeAll() # Clean slate - just in case the cluster was already running
df <- h2o.importFile(path = normalizePath("/Users/yaman/Desktop/IST.csv"))

mydata <- as.h2o(x=df, destination_frame= "mydata")

#split data 
splits <- h2o.splitFrame(mydata,           
                         c(0.6,0.2), seed=1234)     

#train <- h2o.assign(splits[[1]], key="train")  
train <- h2o.assign(splits[[1]], "train.hex")   
## assign the first result the R variable train
## and the H2O name train.hex
#valid <- h2o.assign(splits[[2]], key="valid") 
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[3]], "test.hex")     ## R test, H2O test.hex

train[1:5,]   ## rows 1-5, all columns

## run our first predictive model
rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = train,        ## the H2O frame for training
  validation_frame = valid,      ## the H2O frame for validation (not required)
  x=c(5:26, 28:41),                        ## the predictor columns, by column index (col:19 is roles, gender =22, 41 is company)
  y=27,                          ## the target index (what we are predicting)
  model_id = "rf_covType_v1",    ## name the model in H2O
  ##   not required, but helps use Flow
  ntrees = 200,                  ## use a maximum of 200 trees to create the
  ##  random forest model. The default is 50.

  max_depth = 30, 
  stopping_rounds = 2,           ## Stop fitting new trees when the 2-tree
  ##  average is within 0.001 (default) of 
 
  score_each_iteration = T,      ## Predict against training and validation for
  ##  each tree. Default will skip several.
  seed = 1000000)                ## Set the random seed so that this can be
##  reproduced.

summary(rf1)                     ## View information about the model.

rf1@model$validation_metrics     ## A more direct way to access the validation 
##  metrics. 

h2o.hit_ratio_table(rf1,valid = T)[1,2] ## Even more directly, the hit_ratio @ k=1 (Accuracy)

###############################################################################


gbm1 <- h2o.gbm(
  training_frame = train,        ## the H2O frame for training
  validation_frame = valid,      ## the H2O frame for validation (not required)
  x=c(4:18, 20:41),                        ## the predictor columns, by column index
  y=19,                          ## the target index (what we are predicting)
  model_id = "gbm_covType1",     ## name the model in H2O
  seed = 2000000)                ## Set the random seed for reproducability

###############################################################################
summary(gbm1)                   ## View information about the model.
h2o.hit_ratio_table(gbm1,valid = T)[1,2] ## Overall accuracy.

###############################################################################
# Improvements:
gbm2 <- h2o.gbm(
  training_frame = train,     ##
  validation_frame = valid,   ##
  x=c(4:18, 20:41),                     ##
  y=19,                       ## 
  ntrees = 20,                ## decrease the trees, mostly to allow for run time
  ##  (from 50)
  learn_rate = 0.2,           ## increase the learning rate (from 0.1)
  max_depth = 10,             ## increase the depth (from 5)
  stopping_rounds = 2,        ## 
  stopping_tolerance = 0.01,  ##
  score_each_iteration = T,   ##
  model_id = "gbm_covType2",  ##
  seed = 2000000)             ##


###############################################################################

summary(gbm2)
h2o.hit_ratio_table(gbm1,valid = T)[1,2]    ## review the first model's accuracy
h2o.hit_ratio_table(gbm2,valid = T)[1,2]    ## review the new model's accuracy
###############################################################################

# find importance
my_varimp <- h2o.varimp(rf1)
my_varimp

### All done, shutdown H2O    
h2o.shutdown(prompt=FALSE)