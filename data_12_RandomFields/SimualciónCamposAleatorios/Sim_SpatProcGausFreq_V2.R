#library(funData)
library("ggplot2")

simSpatFunData <- function(argvals, Ph=FALSE, ValPh, f, M, eFunType, ignoreDeg = NULL, trueVals, scores, N)
#argvals:   A numeric vector, containing the observation points (a fine grid on a real interval) of the functional data that is to be   #           simulated.
#Phase:     A vector with the phase shift of the generated curves (N).
#M          An integer, giving the number of univariate basis functions to use. 
#eFunType   A character string specifying the type of univariate orthonormal basis functions to use. For data on higher-dimensional    #            domains, eFunType can be a vector, specifying the marginal type of eigenfunctions to use in the tensor product. See
#            eFun for details.
#ignoreDeg  A vector of integers, specifying the degrees to ignore when generating the univariate orthonormal bases. Defaults to NULL.
#           For higher-dimensional data, ignoreDeg can be supplied as list with vectors for each marginal. See eFun for details.
#N          An integer, specifying the number of functions to be generated. This number correspondes to the number of sites. 

#The following arguments are deleted: 
#eValType   A character string, specifying the type of eigenvalues/variances used for the generation of the simulated functions based on            the truncated Karhunen-Loeve representation. See eVal for details.
#           N An integer, specifying the number of multivariate functions to be generated

#The following arguments are added:
#trueVals: Are the eigenvalues
#scores

{
    if (!is.numeric(M)) 
        stop("Parameter 'M' must be numeric.")
    if (!is.character(eFunType)) 
        stop("Parameter 'eFunType' must be passed as a string.")
    if (!(is.null(ignoreDeg) | all(is.numeric(ignoreDeg), ignoreDeg > 
        0))) 
        stop("Parameter 'ignoreDeg' must be either NULL or a vector of positive numbers.")
        
    #if (!all(is.character(eValType), length(eValType) == 1)) 
    #    stop("Parameter 'eValType' must be passed as a string.")
        
    if (!all(is.numeric(N), length(N) == 1, N > 0)) 
        stop("Parameter 'N' must be passed as a positive number.")
    if (!(is.list(argvals) & all(is.numeric(unlist(argvals))))) {
        if (is.numeric(argvals)) 
            argvals <- list(argvals)
        else stop("Parameter 'argvals' must be either passed as a list or as a vector of numerics.")
    }
    
    if (Ph==TRUE) 
       if (length(ValPh) != N)
          stop("Length of the phase vector must be equal to the number of curves generated(N)")
    
    
    p <- length(argvals)
    if (length(M) != p) {
        if (length(M) == 1) {
            warning("Simulation of tensor product data. The value of M will be used for all dimensions.")
            M <- rep(M, p)
        }
        else stop("M must have the same length as argvals or 1.")
    }
    if (length(eFunType) != p) {
        if (length(eFunType) == 1) {
            warning("Simulation of tensor product data. The value of eFunType will be used for all dimensions.")
            eFunType <- rep(eFunType, p)
        }
        else stop("eFunType must have the same length as argvals or 1.")
    }
    
    #The scores are realizations of a multivariate normal with mean vector zero and covariance matriz given by diag(eigenvalues_j)
    #The eigenvalues (called trueVals int his code) constitute the variance of the scores
    #The following two lines are replaced by the arguments added to the function
    #These arguments store the eigenvalues and the scores of the spatial process
    
    #trueVals <- eVal(prod(M), eValType)
    #scores <- t(replicate(N, stats::rnorm(prod(M), sd = sqrt(trueVals))))
        
    if (p == 1) 
    {
      if(Ph==FALSE) # Curves WITHOUT PHASE. argvals is common for all the curves        
      {
         resX <- matrix(0, nrow=N, ncol= length(argvals[[1]]))
         argX <- matrix(0, nrow=N, ncol= length(argvals[[1]]))
         trueFuns <- eFunFreq(argvals = argvals[[1]], f=f, M = M, ignoreDeg = ignoreDeg, type = eFunType)
         for (j in 1:N) 
            resX[j,] <- t(as.matrix(scores[j,])) %*% trueFuns@X
         simData <- funData(argvals, resX)
      }
      else #Ph=TRUE. CURVES WITH PHASE. Each curve has its domain given by argvals
      {
         for (j in 1:N) 
         {
           trueFuns <- eFunFreq(argvals = argvals[[1]]+ValPh[j], f=f, M = M, ignoreDeg = ignoreDeg, type = eFunType)
           if (j==1)
           {
              argX <- list(argvals[[1]]+ValPh[j])
              resX <- list(as.numeric(t(as.matrix(scores[j,])) %*% trueFuns@X))
           }
           else
           {
              argX <- c(argX, list(argvals[[1]]+ValPh[j]))
              resX <- c(resX, list(as.numeric( t(as.matrix(scores[j,])) %*% trueFuns@X)))
           }
        }
        simData <- irregFunData(argX, resX)             
      }
    }
    return(list(simData = simData, trueFuns = trueFuns, trueVals = trueVals))
}
