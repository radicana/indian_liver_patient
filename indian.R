
#instal required packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")



#############################----  DATA WRANGLING  ---- #######################################


 # importing data from github
   tmp <- tempfile()
   download.file("https://github.com/radicana/indian_liver_patient/raw/master/indian_liver_patient.csv", tmp)
   liver <- read.csv(tmp)
   file.remove(tmp)


 # general about liver dataset------------------------------------------------
   str(liver)

 # use abbreviations for column names
   colnames(liver) <- c("age", "gender", "TSB", "DSB", "ALP", "ALT",
                        "AST", "TP", "albumin", "A_G_ratio", "patient")

 # which is a label for disease
   liver %>% group_by(patient) %>% 
     summarise(TSB = mean(TSB), ALT = mean(ALT), AST = mean(AST)) 


 # Change the nonpatient label to "not sick" and patient label to "sick" (for the purpose of visualization) 
   liver$patient <- as.character(liver$patient)
   liver$patient[liver$patient == 2] <- "not sick"
   liver$patient[liver$patient == 1] <- "sick"


 # and turn as factor    
   liver$patient <- as.factor(liver$patient)  

 # first rows of data and brief summary
   head(liver)
   summary(liver)

 # missing values----------------------------------------------
   liver %>% summarise_all(~ sum(is.na(.))) %>% select_if(.!=0)

 # in the professional literature we can find formula A_G_ratio = albumin/(TP-albumin)
   liver %>% mutate(ratio_hat = albumin/(TP-albumin)) %>% ggplot(aes(ratio_hat, A_G_ratio)) +
     geom_point(na.rm=TRUE)+
     geom_abline(col="red")

 # filling the missing values by formula A_G_ratio = albumin/(TP-albumin)
   liver <- liver %>% 
   mutate(A_G_ratio = ifelse(is.na(A_G_ratio), round(albumin/(TP-albumin),digits= 2), A_G_ratio))


 
#############################----  DATA VISUELIZATION  ---- #######################################

# univariate analysis -------------------------------------------------------------------------  

 # PATIENT------------    
  # distribution of the patient variable 
    liver %>% ggplot(aes(patient)) +
      geom_bar( fill = "paleturquoise3", color = "gray40") +
      geom_text(aes(label=..count..), stat="count", vjust=-0.5) +
      ggtitle("Patient distribution")
   
     
 # Proportion of patients
   round(prop.table(table(liver$patient)),2)  

# Univariate distribution
  liver %>% select(-gender, -patient) %>% reshape2::melt() %>% 
    ggplot(aes(x = variable, y = value)) +
    geom_boxplot(na.rm = TRUE, fill = "paleturquoise3") +
    scale_y_continuous(trans = "log2") +
    ggtitle("Univariate distribution")


# distribution of each column with numerical results of blood analysis among patient column 
  liver %>% mutate(ALP=log2(ALP), ALT=log2(ALT), AST=log2(AST), DSB=log2(DSB), TSB=log2(TSB)) %>%
    gather(liver_column, count, -patient, -age, -gender) %>%
    ggplot(aes(patient, count, fill = patient)) +
    geom_boxplot() +
    facet_wrap(~liver_column, scales = "free", ncol = 4) +
    scale_fill_manual(values=c("khaki","paleturquoise3")) +
    theme(axis.text.x = element_blank(), legend.position="bottom") +
    ggtitle("Distribution of numerical variables among patient")



# GENDER-------------
 # distribution of gender among patient     
   liver %>% ggplot(aes(gender, group = patient)) +
     geom_bar(aes(y = ..prop.., fill= factor(..x..)), stat = "count", color= "gray40") +
     geom_text(aes(label = percent(..prop..), y = ..prop..), stat = "count", vjust = -0.5) +
     labs(y= "Percent") +
     facet_grid(~patient) +
     scale_fill_manual(values=c("khaki","paleturquoise3")) +
     theme(legend.position = "none") +
     scale_y_continuous(labels = percent) +
     ggtitle("Distribution of gender among patient")

 round(prop.table(table(liver$gender))*100,1) 

# AGE
 # distribution of age among patient     
   means_age <- liver %>% group_by(patient) %>% 
     summarise(age=round(mean(age),1)) 

   liver %>% ggplot(aes(patient, age, fill=patient)) +
     geom_boxplot() +
     theme(legend.position = "none") +
     scale_fill_manual(values=c("khaki","paleturquoise3")) + 
     geom_text(data = means_age, aes(label = age, y = age +3)) +
     ggtitle("Distribution of age among patient")



# Bivariate analysis -----------------------------------------------------------------------

# correlation
  round(liver %>% select(-patient, -gender) %>% cor(method = "spearman"),2) %>% 
    ggcorrplot(hc.order = TRUE, type = "upper",colors = c("paleturquoise3", "white", "khaki"), lab = TRUE)   


# features with high correlation coefficient   

 q1 <- liver %>% ggplot(aes(DSB,TSB, col = patient)) +  
   geom_point()+ 
   theme(legend.position ="none") 

 q2 <- liver %>% ggplot(aes(ALT,AST, col=patient)) + 
   geom_point() +
   scale_x_continuous(trans = "log10") +
   scale_y_continuous(trans = "log10") + 
   theme(legend.position = "none") 

 q3 <- liver %>% ggplot(aes(albumin,A_G_ratio, col=patient)) + 
   geom_point() + 
   theme(legend.position = "none") 

 q4 <- liver %>% ggplot(aes(albumin,TP, col=patient)) + 
   geom_point() + 
   theme(legend.position = c(0.9,0.25))
 
 grid.arrange(q1,q2,q3,q4,ncol=2, nrow=2, top = "High correlation coefficient")



# features with correlation coefficient about 0.5   

 g1 <- liver %>% ggplot(aes(ALP,log(AST), col = patient)) +  
   theme(legend.position ="none") +      
   geom_point()

 g2 <- liver %>% ggplot(aes(TSB,log(AST), col = patient)) +  
   geom_point()+ 
   theme(legend.position = "none") +
   ylab("") 

 g3 <- liver %>% ggplot(aes(DSB,log(AST), col = patient)) +  
   geom_point()+ 
   theme(legend.position ="none") +
   ylab("") 


 p1 <- liver %>% ggplot(aes(ALP,log(ALT), col = patient)) +   
   geom_point()+ 
   theme(legend.position ="none")

 p2 <- liver %>% ggplot(aes(TSB, log(ALT),  col = patient)) +   
   geom_point()+ 
   theme(legend.position = "none") +
   ylab("")

 p3 <- liver %>% ggplot(aes(DSB, log(ALT), col = patient)) +    
   geom_point()+ 
   theme(legend.position = "none") +
   ylab("")

 grid.arrange(g1, g2, g3, p1, p2, p3, nrow = 2, ncol = 3, 
              top = "Correlation coefficient of about 0.5")
   



#############################----  DATA TRANSFORMATION  ---- #######################################


 # for the purpose of modeling, we change the levels in the variable patient to 0 and 1
   liver$patient <- plyr::revalue(liver$patient, c("sick"= 1, "not sick" = 0))

 # transform gender variable into numeric
   liver$gender <- ifelse(liver$gender == "Male", 1, 0) %>% as.factor()

 # transform data using NORAMLIZATION for modeling purpose
   liver_norm <- liver %>% select(-patient, -gender) %>% 
     sapply(function(x) {(x-min(x))/(max(x)-min(x))}) %>% 
     data.frame(patient = liver$patient,
               gender = liver$gender)
 
 summary(liver_norm)


 # transform data using log function on ALT, ALP and AST for modeling purpose
   liver_log <- liver %>% mutate(ALT=log(ALT), 
                                 ALP= log(ALP), 
                                 AST=log(AST))
 summary(liver_log)

options(digits = 4)

#############################----  MODELING ---- #######################################

# metrics calculation functions

AUC <- function(model, test) {   
  probs  <- predict(model, test, type = "prob")[,2]      
  p <- prediction(probs,test$patient)   
  per <- performance(p,"tpr", "fpr")
  perf_auc <- performance(p,"auc") 
  perf_auc@y.values[[1]]
}

metrics <- function(y, x, auc, title){
  tibble(model = title,
         accuracy_test = confusionMatrix(y, x)$overall["Accuracy"],
         sensitivity = confusionMatrix(y, x)$byClass["Sensitivity"],
         specificity = confusionMatrix(y, x)$byClass["Specificity"],
         f_score = confusionMatrix(y, x)$byClass["F1"],
         AUC = auc)
}


 # data partition into train and test set
   set.seed(14, sample.kind = "Rounding")
   ind <- createDataPartition(liver$patient, time = 1, p = 0.2, list = FALSE)
 
 # liver data set
   train <- liver %>% slice(-ind)
   test <- liver %>% slice(ind)
 
 # transform liver data set with log
   trainl <- liver_log %>% slice(-ind)
   testl <- liver_log %>% slice(ind)

 # normalized
   trainn <- liver_norm %>% slice(-ind)
   testn <- liver_norm %>% slice(ind)

 

# glm with all predictors ###############################################################      

   fit_glm_all <- train(patient ~ ., 
                        method = "glm", 
                        data = train)

   y_hat <- predict(fit_glm_all, test) 
   tbl <- metrics(y_hat, 
                 test$patient,
                 AUC(fit_glm_all, test), 
                 "GLM (all predictors)")
 
   tbl %>% kable()  

# check if occured overfitting
  confusionMatrix(predict(fit_glm_all, train), train$patient)$overall["Accuracy"]

  varImp(fit_glm_all)

  
#-----------------------------------------------------------------------------------------
# We throw out features TSB, gender, AST and A_G_ratio based on variable importance and correlations

   fit_glm6 <- train(patient ~ age + TP  + ALT + DSB + ALP +albumin, 
                     method = "glm", 
                     data = train)

   y_hat <- predict(fit_glm6, test)
   tbl <- bind_rows(tbl,metrics(y_hat, 
                                test$patient,
                                AUC(fit_glm6, test),
                                "GLM (6 predictors)"))
  tbl %>% kable()

# check if occured overfitting
  confusionMatrix(predict(fit_glm6, train), train$patient)$overall["Accuracy"]

 
 
# KNN ####################################################################################

 # knn on liver   
   set.seed(14, sample.kind = "Rounding")
   fit_knn <- train(patient ~ ., 
                    method = "knn", 
                    data = train)

   plot(fit_knn)
   y_hat <- predict(fit_knn,test) 
   tbl <- bind_rows(tbl,metrics(y_hat, 
                                test$patient, 
                                AUC(fit_knn, test), 
                                "KNN on liver"))
  tbl %>% kable()

# check if occured overfitting
  confusionMatrix(predict(fit_knn, train), train$patient)$overall["Accuracy"]


# normalization-----------------------------------------------------------------------
  set.seed(14, sample.kind = "Rounding")
  fit_norm_knn <- train(patient ~ ., 
                         method = "knn", 
                         data = trainn)
  plot(fit_norm_knn)
  y_hat <- predict(fit_norm_knn, testn) 
  tbl <- bind_rows(tbl,metrics(y_hat, 
                               testn$patient, 
                               AUC(fit_norm_knn, testn),
                             "KNN on liver(norm)"))
 tbl %>% kable()

# check if occured overfitting
  confusionMatrix(predict(fit_norm_knn, train), train$patient)$overall["Accuracy"]


# log----------------------------------------------------------------------------------   
  set.seed(14, sample.kind = "Rounding")
  fit_log_knn <- train(patient ~ ., 
                       method = "knn", 
                       data = trainl)
  plot(fit_log_knn)
  y_hat <- predict(fit_log_knn, testl) 
  tbl <- bind_rows(tbl,metrics(y_hat, 
                               testl$patient, 
                               AUC(fit_log_knn, testl),
                               "KNN on liver(log)"))
 tbl %>% kable()

# check if occured overfitting
  confusionMatrix(predict(fit_norm_knn, train), train$patient)$overall["Accuracy"]



# classification tree ######################################################################
  # tree with dafault tunung 
  set.seed(14, sample.kind = "Rounding")
  fit_tree <- train(patient ~ . , 
                    method="rpart", 
                    data = train)

  y_hat <- predict(fit_tree, test)
  tbl <- bind_rows(tbl,metrics(y_hat, test$patient, AUC(fit_tree, test),"TREE on liver"))
  tbl %>% kable()

  # tree plot
  fancyRpartPlot(fit_tree$finalModel, sub = "")

# check if occured overfitting
  confusionMatrix(predict(fit_tree, train), train$patient)$overall["Accuracy"]


 # controling cross validation and tuning cp parameter
   set.seed(14, sample.kind = "Rounding")
   fit_tree <- train(patient ~ . , 
                     method="rpart", 
                     data = train,
                     trControl = trainControl(method= "cv", number = 10, p=0.9),
                     tuneGrid=data.frame(cp=seq(0.05,0.1, len=30)))

   y_hat <- predict(fit_tree, test)
   tbl <- bind_rows(tbl,metrics(y_hat, test$patient, AUC(fit_tree, test),
                             "TREE on liver(trainControl)"))
  tbl %>% kable()


# check if occured overfitting
  confusionMatrix(predict(fit_tree, train), train$patient)$overall["Accuracy"]
  
  # The goal is to improve prediction performance and reduce instability by averaging multiple
  # decision trees

#FOREST####################################################################

  # random forest with dafault tunung
    set.seed(14, sample.kind = "Rounding")
    fit_rf <- train(patient~., 
                      method="rf", 
                      data = train)

    y_hat <- predict(fit_rf,test)
    tbl <- bind_rows(tbl,metrics(y_hat, test$patient, AUC(fit_rf, test),"RANDOM FOREST on liver"))
    tbl %>% kable()

# check if occured overfitting
  confusionMatrix(predict(fit_rf, train), train$patient)$overall["Accuracy"]


  
  # random forest with tuning parameter nodesize
    m <- fit_rf$finalModel$mtry
    nodesize <- seq(50, 100, 3)
    set.seed(14, sample.kind = "Rounding")
    accu <- sapply(nodesize, function(ns){
    train(patient ~ ., 
          method = "rf", 
          data = train,
          tuneGrid = data.frame(mtry = m),
          nodesize = ns)$results$Accuracy
                  })
    

  qplot(nodesize, accu)
  
  # random forest with the optimized minimun nodesize
    set.seed(14, sample.kind = "Rounding")
    fit_rf_tune <- train(patient ~ ., 
                         data= train, 
                         method = "rf",
                         nodesize = nodesize[which.max(accu)],
                         trControl = trainControl(method= "cv", number = 10, p=0.9))

    y_hat <- predict(fit_rf_tune, test)
    tbl <- bind_rows(tbl,metrics(y_hat, test$patient, AUC(fit_rf_tune, test),"RANDOM FOREST tune"))
    tbl %>% kable()

# check if occured overfitting
  confusionMatrix(predict(fit_rf_tune, train), train$patient)$overall["Accuracy"]

  plot(varImp(fit_rf_tune))

  