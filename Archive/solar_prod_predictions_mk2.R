path <- "/Users/sebastianmontero/Dropbox/IE MBD/Programming R/workgroup_project/"
setwd(path)
start <- Sys.time()
print(paste("Start time:",start))

#### PACKAGES ####
library(data.table)
library(caret)
library(e1071)

#### IMPORT DATA #### 
dt_all <- data.table(readRDS("solar_dataset.RData"))
dt <- dt_all[1:5113]

dt_to_predict <- dt_all[,1]

dt_stations <- dt[,1:99]
dt_pca <- dt[,100:456]

#### TRAIN/TEST SPLIT ####
train <- dt[1:3579,]
test  <- dt[3580:nrow(dt),]

#### MODEL FORM ####
f1 <- as.formula(paste("ACME ~", paste(colnames(dt_pca[,1:length(dt_pca)]), collapse="+")))

#### TEST SVM MODEL ####
#Define Control
control <- trainControl(method = "none")

degree <- as.vector(10^seq(from = -2, to = 2, by = 1))
scale <- as.vector(10^seq(from = -2, to = 2, by = 1))
C <-  as.vector(10^seq(from = -2, to = 2, by = 1))
grid <- expand.grid(degree=degree,scale=scale,C=C)

#Model
poly_model <- train(f1, data = dt, method = "svmPoly",
                             trControl=control,
                             preProcess = c("center","scale"),
                             metric="MAE",
                             tuneGrid = grid,
                             tuneLength = 5)
poly_model
prediction <- data.table(predict(poly_model, newdata = test))

#Viz
station <-data.table(test$ACME)
station$index <- as.numeric(row.names(station))
prediction$index <- as.numeric(row.names(prediction))

#b_sigma<-round(svm_model$bestTune$sigma,5)
b_C<-poly_model$bestTune$C
b_degree<-poly_model$bestTune$degree
b_scale<-poly_model$bestTune$scale

poly_model$bestTune

title <- paste(poly_model$method,
               " | metric=",poly_model$metric,
               " | C=",b_C,
               " | degree=",b_degree,
               " | scale=",b_scale,
               sep ="")
ggplot() +
  geom_point(mapping = aes(y=station$V1,station$index), color = 'black')+
  geom_point(mapping = aes(y=prediction$V1,x=prediction$index),color = 'white', shape = 21) +
  labs(title = title)+
  theme_dark()

#### RUN SVM OPTIMIZED MODELS ON ALL STATIONS ####

start1 <- Sys.time()
print(paste("Start time for station models:",start1))

prediction_data <- dt_to_predict

n=0
for (s in colnames(dt_stations[,-1])) {
  n = n+1
  print(paste("Training Station ",n,": ",s,sep=""))
  f_station <- as.formula(paste(s,"~", paste(colnames(dt_pca[,1:length(dt_pca)]), collapse="+")))

  print("Training Model...")
  grid <- expand.grid(C = c_values, sigma=sigma_values)
  
  optimized_model <- train(f_station, data = dt, method = "svmPoly",
                               trControl=control,
                               preProcess = c("center","scale"),
                               metric="MAE")
  
  print("Predicting...")
  prediction <- data.table(predict(optimized_model, newdata = dt_all))
  
  colnames(prediction)[1] <- s
  prediction_data <- data.table(prediction_data,prediction)
}

finish <- Sys.time()
run_time_stations <- difftime(finish, start1, units='mins')
print(paste("Runtime for station prediction is:",round(as.numeric(run_time_stations),0), "minutes"))

#### SAVING PREDICTIONS ####
final_dataset<- prediction_data
write.csv(final_dataset[5114:nrow(final_dataset)],"predictions_poly_mk3.csv", row.names = F)














