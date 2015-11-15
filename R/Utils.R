# Utility functions

PrepData <- function(df){
    # Prepares data in final form; assumes that variables 'Ret_121:Ret_180, 
    # 'Ret_PlusOne, Ret_PlusTwo' are present in the data frame
    #
    # Args: df: data frame containing target variables
    #
    # Returns: for submission reformatted data frame with right headers 
    library(magrittr)
    library(dplyr)
    library(tidyr)
    
    target_vars <- c(paste0("Ret_", 121:180), "Ret_PlusOne", "Ret_PlusTwo")
    
    df %<>% 
        gather("var_name", "pred", 1:62) %>%
        mutate(ind = row_number() %% nrow(df),
               var = row_number() %/% nrow(df) + 1) 
    
    df$ind[df$ind == 0] <- 60000
    df$var[df$ind == 60000] <- df$var[df$ind == 60000] - 1
    
    df %<>% 
        mutate(Id = paste0(ind, "_", var)) %>%
        rename(Predicted = pred) %>%
        select(Id, Predicted)
    
    return(df)
}

ComputeMean <- function(df){
    # Computes some time series characteristics of the variables "Ret_2:Ret_120"
    #
    # Args: df: data frame containing the variables Ret_2:Ret_120
    #
    # Returns: data frame with additional variables defined in this function;
    #          can be easily modified
    library(dplyr)
    library(tidyr)
    
    return_variables <-
        paste0("Ret_", 2:120)
    
    df %>%
        select(Id, one_of(return_variables)) %>%
        gather(time, returns, 2:120) %>%
        select(-time) %>%
        group_by(Id) %>%
        summarise(mean_return = mean(returns),
                  sd_return = sd(returns),
                  median_return = median(returns)) %>%
        inner_join(df, by = "Id") %>%
        return
}

MAE <- function(data, lev = NULL, model = NULL){
    # Returns mean absolute error; to be used in caret package for choosing
    # optimization goal; 
    #
    # Args: data: data frame having two variables obs and pred
    #       lev: unimportant for regression problems
    #       model: unimportant for regression problems
    #
    # Returns: labeled vector with mean absolute error
    mae <- function(obs, pred){
        mean(abs(obs-pred))
    }
    return(c("MAE" = mae(data$obs, data$pred)))
}

BuildModel<- function(train, test, pred_var, target_var){
    # Build a quantile regression model for target variable target_var using the
    # variables pred_var as predictors; does median imputation of all other
    # target variables not contained in target_var so that progress on
    # individual variables can be monitored
    #
    # Args: df: data frame containig pred_var, target_var as column names
    #       pred_var: character vector of variables to be used as predictors
    #       target_var: target variable to be predicted
    #
    # Returns: data frame ready for submission
    library(dplyr)
    library(tidyr)
    library(magrittr)
    
    if (!all(c(target_var, pred_var) %in% colnames(train))){
        stop("Predictor and target variables must 
             be column names of data frame")
    }
    
    target_vars <- c(paste0("Ret_", 121:180), "Ret_PlusOne", "Ret_PlusTwo")
    
    # Build median impute model based on target variables
    impute_model <-
        preProcess(train %>% select(one_of(target_vars)),
                   method = "medianImpute"
                   )
    
    # Build empty data frame and predict median values
    preds <- matrix(data = NA, nrow = 60000, ncol = length(target_vars)) %>%
        data.frame
    colnames(preds) <- target_vars
    preds %<>% predict(impute_model, .)
    
    # Build model
    pred_model <- quantreg::rq(as.formula(paste0(target_var, " ~ .")), 
                               tau = 0.5, 
                               data = train %>% 
                                   select(one_of(c(target_var, pred_var)))
                               )
    
    # Predict
    preds[ , target_var] <- predict(pred_model, newdata = test)
    
    # Prepare for submission
    preds %<>% PrepData
    
    return(preds)
}