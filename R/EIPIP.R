library(R6)

#' This class represents the EIPIP model, a R6 based class for model training and prediction using IPIP and EIPIP, its exhaustive variant.
#'
#' @param OUTPUT_VAR The variable that we want to predict
#' @param OUTPUT_MIN The value of the minoritary class in the output characteristic
#' @param OUTPUT_MAJ The value of the majoritary class in the output characteristic
#' @param conf A vector of algorithms to be executed calling the TRAIN_METRIC metric and the train set
#' @param metrics The metric function that calculates, given a prediction in the form of data[,obs] data[,pred]
#' @param TRAIN_METRIC The metric used to train the model
#' @param prop_maj The proportion of the majoritary class (default: 0.55)
#' @param exhaustive Boolean variable that checks if the training is made in sequential or exhaustive mode (default: FALSE)
#' @param alpha_p The alpha_p parameter for the model (default: 0.01)
#' @param alpha_b The alpha_b parameter for the model (default: 0.01)
#' @param SELECT_METRIC The second metric established in order to check if one ensemble is better than other (default: TRAIN_METRIC)
#' @return A list of trained models. Prediction over those models is to be made with the prediction.fold functions.


EIPIP <- R6Class("EIPIP",
                public = list(
                  OUTPUT_VAR = NULL,
                  OUTPUT_MIN = NULL,
                  OUTPUT_MAJ = NULL,
                  conf = NULL,
                  metrics = NULL,
                  TRAIN_METRIC = NULL,
                  prop_maj = 0.55,
                  exhaustive = FALSE,
                  alpha_p = 0.01,
                  alpha_b = 0.01,
                  SELECT_METRIC = NULL,
                  np = NULL,
                  p = NULL,
                  b = NULL,
                  ensemble_model = NULL,
                  percentage_per_sample = 0.7,
                  q = 0.7,
                  #' @description EIPIP initialization
                  initialize = function(OUTPUT_VAR, OUTPUT_MIN, OUTPUT_MAJ, conf, metrics,
                                        TRAIN_METRIC, prop_maj = 0.55, exhaustive = FALSE,
                                        alpha_p = 0.01, alpha_b = 0.01, SELECT_METRIC = TRAIN_METRIC) {
                    self$OUTPUT_VAR <- OUTPUT_VAR
                    self$OUTPUT_MIN <- OUTPUT_MIN
                    self$OUTPUT_MAJ <- OUTPUT_MAJ
                    self$conf <- conf
                    self$metrics <- metrics
                    self$TRAIN_METRIC <- TRAIN_METRIC
                    self$prop_maj <- prop_maj
                    self$exhaustive <- exhaustive
                    self$alpha_p <- alpha_p
                    self$alpha_b <- alpha_b
                    self$SELECT_METRIC <- SELECT_METRIC
                  },
                  #' @description Training method for EIPIP
                  train = function(train.set, val.set) {
                    nmin = sum(train.set[[self$OUTPUT_VAR]] == self$OUTPUT_MIN)
                    nmaj = sum(train.set[[self$OUTPUT_VAR]] == self$OUTPUT_MAJ)
                    self$np <- self$calculate_np(nmin, nmaj)
                    self$p <- self$calculate_p()


                    if(self$exhaustive == TRUE) {
                      self$ensemble_model <- self$train_IPIP_exhaustive(train.set, val.set)
                    } else {
                      self$b <- self$calculate_b(nmin)
                      if(self$b<=length(self$conf)){
                        self$ensemble_model <- self$train_IPIP_seq(train.set, val.set)
                      }
                      else{
                        print(sprintf("Not enough elements for the sequential approach. %d expected, %d given", self$b, length(self$conf)))
                      }
                    }
                    invisible(self)
                  },
                  #' @description Prediction method for EIPIP
                  #' @param x The input to make predictions on
                  predict = function(x) {
                    self$prediction.fold(self$ensemble_model, x)
                  },


                  #' @description Function to calculate number of minority examples to exist in subsets
                  #' @param nmin The count of minoritary class instances
                  #' @param nmaj The count of majoritary class instances
                  #Function to calculate number of positive samples, np
                  calculate_np= function(nmin, nmaj)
                  {
                    np <- round(nmin*self$percentage_per_sample)
                    return(np)
                  },
                  #' @description Function to calculate number of partitions, p. Must be called after calculating np
                  #Function to calculate number of partitions, p
                  calculate_p = function()
                  {
                    p <- ceiling(log(self$alpha_p)/(log(1-1/self$np)*self$np))
                    return(p)
                  },

                  #' @description Function to calculate maximum ensemble size, b
                  #' @param nmin The count of minoritary class instances
                  calculate_b = function( nmin)
                  {

                    b <- ceiling(log(self$alpha_b)/(log(1-1/nmin)*self$np))
                    return(b)
                  },


                  #Function to calculate maximum number of consecutive attempts to enlarge ensemble
                  mt = function(b, n) { ceiling((b-n) / 3) },



                  #' @description Function to update the ensemble with a new model and enlarge its prediction values
                  #' @param old_ensemble The existing ensemble of models
                  #' @param new_element The new model to be added
                  #' @param prediction_current The current prediction (default: FALSE)
                  #' @param val.test The validation set (default: FALSE)

                  prediction.add = function(old_ensemble, new_element, prediction_current = FALSE, val.test = FALSE){
                    # if no current prediction is passed, generate prediction using new model
                    if(prediction_current == FALSE){
                      prediction_current = as.numeric(unlist(predict(new_element, val.test)) == self$OUTPUT_MAJ)
                    }
                    # If the old ensemble is not empty, update the ensemble and the model count
                    if(length(old_ensemble$ensemble)!=0){
                      prev_count =  old_ensemble$model_count
                      l <- list("ensemble"= c(old_ensemble$ensemble,list(new_element)), "model_count"= prev_count + prediction_current)
                      return(l)
                    }
                    else{  # else initialize ensemble with the new model and the prediction count
                      l <- list("ensemble"= list(new_element), "model_count"=prediction_current)
                      return(l)
                    }
                  },


                  #' @description Function to get the final predicted class by comparing the model count with a threshold
                  #' @param predicted The predicted model count given by iterations of the prediction.add function
                  prediction.predicted = function(predicted){

                    nElems <- length(predicted$ensemble)
                    pred <- ifelse(predicted$model_count  < self$q*nElems, self$OUTPUT_MIN, self$OUTPUT_MAJ)
                    return(pred)
                  },

                  #Functions used to predict a final trained model
                  #Function to predict over a single ensemble

                  #' @description Function used to predict a final trained model over a single ensemble
                  #' @param conj.model The combined model ensemble
                  #' @param x The input data for prediction
                  prediction = function(conj.model, x){
                    pred <- data.frame(matrix(nrow=nrow(x),ncol=0))
                    for(model in conj.model) pred <- cbind(pred, predict(model,x))
                    nElems = ncol(pred)
                    nElems <- ncol(pred)
                    counts <- rowSums(pred == self$OUTPUT_MAJ)
                    pred <- counts
                    ifelse(is.na(pred) | pred<self$q*nElems, self$OUTPUT_MIN, self$OUTPUT_MAJ)
                  },



                  #' @description Function to predict over all ensembles
                  #' @param ensemble The ensemble of models
                  #' @param x The input data for prediction
                  prediction.fold = function(ensemble, x){
                    pred <- as.data.frame(lapply(ensemble, function(e) self$prediction(e,x)))
                    nElems = ncol(pred)
                    nElems <- ncol(pred)
                    counts <- rowSums(pred == self$OUTPUT_MAJ)
                    pred <- counts
                    ifelse(is.na(pred) | pred<self$q*nElems, self$OUTPUT_MIN, self$OUTPUT_MAJ)
                  },


                  #Functions to calculate the probability of the predictions

                  #' @description Function to calculate the probability of the predictions for an individual ensemble
                  #' @param ensemble The ensemble of models
                  #' @param x The input data for prediction
                  #' @return The data of the prediction probabilities
                  prediction.prob = function(ensemble, x){
                    pred <- data.frame(matrix(nrow=nrow(x),ncol=0))
                    for(model in ensemble) pred <- cbind(pred, predict(model,x,type="prob")[[self$OUTPUT_MAJ]])
                    pred<- rowSums(pred)
                    return(pred)
                  },

                  #Functions to calculate the probability of the predictions

                  #' @description Function to calculate the probability of the predictions for the full group of ensembles
                  #' @param ensemble The ensemble of models
                  #' @param x The input data for prediction
                  #' @return The data of the prediction probabilities
                  prediction.fold.prob = function(ensemble, x){
                    #Place all predictions for a sample in each row of a data set
                    pred <- as.data.frame(lapply(ensemble, function(e) self$prediction.prob(e,x)))
                    pred <- rowSums(pred)/sum(unlist(lapply(ensemble,length)))
                    return(pred)
                  },




                  #' @description Function to train a sequential IPIP model
                  #' @param train.set The training dataset
                  #' @param val.test The validation dataset
                  train_IPIP_seq = function(  train.set, val.test){

                    nmin = sum(train.set[[self$OUTPUT_VAR]] == self$OUTPUT_MIN)
                    nmaj = sum(train.set[[self$OUTPUT_VAR]] == self$OUTPUT_MAJ)



                    majSubSize <-  round(self$np * self$prop_maj /(1-self$prop_maj ))
                    minSubSize <-  self$np
                    #Icludes from 1 to p all ensembles of the set
                    dfs <- list()

                    minoritary <- subset(train.set, train.set[[self$OUTPUT_VAR]] == self$OUTPUT_MIN)
                    majoritary<- subset(train.set, train.set[[self$OUTPUT_VAR]] == self$OUTPUT_MAJ)

                    for(k in 1:self$p){
                      id.minoritary <- sample(x = 1:nmin, size = minSubSize, replace = TRUE) #Index for minoritary class for each subset
                      id.majoritary <- sample(x= 1:nmaj, size = majSubSize, replace = TRUE) #Indexes for majoritary class for each subset

                      dfs[[k]] <- rbind(minoritary[id.minoritary,],majoritary[id.majoritary,])
                    }


                    E <- list() #Final model (ensemble of ensembles)


                    for(k in 1:self$p){
                      cat(sprintf("Ensemble number %d of %d\n", k,self$p))
                      Ek <- list() # k-esim ensemble
                      i <- 0 #Counter for number of tries of enlarging the ensemble
                      metric.ensemble  = 0 #Variable for storing the metric of each ensemble accumulated.
                      #Balanced partition

                      df <- dfs[[k]]
                      model_i = 1

                      #We select the elements for the training
                      majoritary <- which(df[[self$OUTPUT_VAR]] == self$OUTPUT_MAJ)
                      minoritary <- which(df[[self$OUTPUT_VAR]] == self$OUTPUT_MIN)


                      while(length(Ek$ensemble)<=self$b && i<self$mt(self$b,length(Ek$ensemble))){
                        #We use replace TRUE to make some randomness over the seed learners
                        ind.train <- c(
                          sample(majoritary, size = majSubSize, replace = TRUE),
                          sample(minoritary, size = minSubSize, replace = TRUE)
                        )


                        #We train with the models in configuration with a sequential approach

                        model <- self$conf[[length(Ek$ensemble)+1]](df[ind.train,], self$metrics, self$OUTPUT_VAR, self$TRAIN_METRIC)


                        if (length(Ek$ensemble)==0){
                          u <- -Inf;
                          names(u) <- self$SELECT_METRIC;
                          metrics.ensemble <- u
                        }

                        new_Ek <- self$prediction.add(Ek, model, val.test = val.test[colnames(val.test)!=self$OUTPUT_VAR])
                        pred = self$prediction.predicted(new_Ek)
                        metrics.ensemble.new <- self$metrics(data.frame(
                          obs = val.test[[self$OUTPUT_VAR]],
                          pred= as.factor(pred)
                        ))
                        #We check if the new model changes the result. If it does not we start trying again
                        if(metrics.ensemble.new[self$SELECT_METRIC] <= metrics.ensemble[self$SELECT_METRIC]){
                          i <- i+1
                        } else{
                          #If the ensemble tries to enlarge again, we restart the enlarging posibilities.
                          Ek <- new_Ek
                          metrics.ensemble <- metrics.ensemble.new
                          i <- 0
                        }
                      } # End of the k-esim ensemble building
                      cat(sprintf("Max metric %s of ensemble %d is %f with length %d\n", self$SELECT_METRIC, k, metrics.ensemble[self$SELECT_METRIC], length(Ek$ensemble)))
                      E[[length(E)+1]] <- Ek$ensemble

                    }
                    return(E);
                  },

                  #' @description Function to generate powerset of the trained models
                  #' @param s The set of models for which powerset is to be generated
                  #' @param val.test The validation dataset
                  powerset = function(s, val.test){
                    len = length(s)
                    l = c()
                    vector(length=2^len);l[[1]]=numeric()
                    counter = 1L
                    for(x in 1L:len){
                      prediction_current =as.numeric(unlist(predict(s[x], val.test)) == self$OUTPUT_MAJ)
                      prev_count = 0
                      for(subset in 1L:counter){
                        counter=counter+1
                        if(subset == 1){
                          l[[counter]] = list("ensemble"= s[x], "model_count"= prediction_current)
                        }
                        else{
                          prev_count = l[[subset]]$model_count
                          l[[counter]] = list("ensemble"= c(l[[subset]]$ensemble,s[x]), "model_count"= prev_count + prediction_current)
                        }
                      }
                    }
                    return(tail(l, length(l)-1))
                  },

                  #' @description Function to train an exhaustive IPIP model
                  #' @param train.set The training dataset
                  #' @param val.test The validation dataset
                  train_IPIP_exhaustive = function(train.set, val.test){



                    nmin = sum(train.set[[self$OUTPUT_VAR]] == self$OUTPUT_MIN)
                    nmaj = sum(train.set[[self$OUTPUT_VAR]] == self$OUTPUT_MAJ)



                    majSubSize <-  round(self$np*self$prop_maj/(1-self$prop_maj))
                    minSubSize <-  self$np

                    #Incluye en cada posicion los valores de los elementos de dicha particion, de 1 a p
                    dfs <- list()

                    minoritary <- subset(train.set, train.set[[self$OUTPUT_VAR]] == self$OUTPUT_MIN)
                    majoritary <- subset(train.set, train.set[[self$OUTPUT_VAR]] == self$OUTPUT_MAJ)

                    for(k in 1:self$p){
                      id.minoritary <- sample(x = 1:nmin, size = minSubSize, replace = TRUE) #Index for minoritary class for each subset
                      id.majoritary <- sample(x= 1:nmaj, size = majSubSize, replace = TRUE) #Indexes for majoritary class for each subset

                      dfs[[k]] <- rbind(minoritary[id.minoritary,],majoritary[id.majoritary,])
                    }



                    E <- list() #Final model (ensemble of ensembles)


                    for(k in 1:self$p){

                      cat(sprintf("Ensemble number %d of %d\n", k,self$p))
                      Ek <- list() # k-esim ensemble
                      init = Sys.time()

                      #Balanced partition

                      df <- dfs[[k]]
                      majoritary <- which(df[[self$OUTPUT_VAR]] == self$OUTPUT_MAJ)
                      minoritary <- which(df[[self$OUTPUT_VAR]] == self$OUTPUT_MIN)
                      ind.train <- c(
                        sample(majoritary, size = majSubSize, replace = TRUE),
                        sample(minoritary, size = minSubSize, replace = TRUE)
                      )
                      train.test <- df[ind.train,]
                      Ek <- list()
                      for(conf in 1:length(self$conf)){

                        Ek[[length(Ek)+1]] <- self$conf[[conf]](train.test, self$metrics, self$OUTPUT_VAR, self$TRAIN_METRIC)

                      }

                      all_sets <- self$powerset(Ek, as.data.frame(val.test[, -which(names(val.test) == self$OUTPUT_VAR)]))

                      ##If we want to force bigger sets before smaller ones
                      #lengths <- sapply(all_sets, function(x) length(x))
                      # Reorder the list based on the lengths
                      # all_sets <- all_sets[order(lengths, decreasing = TRUE)]



                      max_metric = function(model){
                        pred = self$prediction.predicted(model)
                        met <- metrics(data.frame(
                          obs = val.test[[self$OUTPUT_VAR]],
                          pred = as.factor(pred)
                        ))[self$SELECT_METRIC]
                        return(met)
                      }



                      #Run the max metric check in parallel mode
                      max_metric_list <- lapply(all_sets, max_metric)

                      max_metric_value <- max(unlist(max_metric_list))
                      max_set <- all_sets[[which.max(unlist(max_metric_list))]]$ensemble
                      cat(sprintf("Max metric %s of ensemble is %f with length %d\n", self$SELECT_METRIC, max_metric_value, length(max_set)))
                      E[[length(E)+1]] <- max_set

                    }
                    return(E);
                  },
                #' @description Calculate the metrics in IPIP for evaluating ensemble model performance.
                #'
                #' @param setToTest The dataset for testing the ensemble model.
                #'
                #' @return A data frame with columns:
                #' - \code{obs}: Observed values in the test dataset.
                #' - \code{pred}: Predicted values by the ensemble model.
                #' - \code{prob}: Predicted probabilities for each class.
                #' - \code{obs.prob}: Observed probabilities (1 for majority class, 0 for others).
                #'

                metricTest = function(setToTest){
                  dataPrepared <- data.frame(
                    obs = setToTest[[self$OUTPUT_VAR]],
                    pred = as.factor(self$prediction.fold(self$ensemble_model,setToTest[,names(setToTest) != self$OUTPUT_VAR])),
                    prob= self$prediction.fold.prob(self$ensemble_model, setToTest[,names(setToTest) != self$OUTPUT_VAR]),
                    obs.prob = as.numeric(ifelse(setToTest[[self$OUTPUT_VAR]] == self$OUTPUT_MAJ, 1, 0))
                  )
                  return(self$metrics(dataPrepared))
                }
                ))

