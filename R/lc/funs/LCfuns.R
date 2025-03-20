

## functions to fit Poisson LC model

## Starting values of LC model
LC_starting_pars <- function(Dth,Exp){
  ## dimensions
  m <- nrow(Dth)
  n <- ncol(Dth)
  ## starting values
  One <- matrix(1, nrow=n, ncol = 1)    
  Fit <- log((Dth + 1)/(Exp + 2))
  ## for Alpha, take mean of log death rates
  Alpha <- apply(Fit/n,1,sum,na.rm=T)
  ## for Beta, take alpha and normalize to sum to one
  Beta <- matrix(1 * Alpha, ncol = 1)
  sum.Beta <- sum(Beta) 
  Beta <- Beta / sum.Beta
  ## for Kappa, standardize a series from n to 1 
  Kappa <- matrix(seq(n, 1, by = -1), nrow = n, ncol = 1)
  Kappa <- Kappa - mean(Kappa)
  Kappa <- Kappa / sqrt(sum(Kappa * Kappa))
  ## return list
  out <- list(Alpha=Alpha,Beta=Beta,Kappa=Kappa,One=One)
}

## Update Alpha
Update.alpha <- function(Alpha, Beta, Kappa,
                         One, Dth, Exp, D.fit){
  difD <- Dth - D.fit
  Alpha <- Alpha + difD %*% One / ifelse((D.fit %*% One)==0,1e-8,D.fit %*% One) 
  Eta <- Alpha %*% t(One) + Beta %*% t(Kappa)
  D.fit <- Exp * exp(Eta)
  list(Alpha = Alpha, D.fit = D.fit)
}
## Update Beta
Update.beta <- function(Alpha, Beta, Kappa,
                        One, Dth, Exp, D.fit){
  difD <- Dth - D.fit
  Kappa2 <- Kappa * Kappa
  Beta <- Beta + difD %*% Kappa / ifelse((D.fit %*% Kappa2)==0,1e-8,D.fit %*% Kappa2) 
  Eta <- Alpha %*% t(One) + Beta %*% t(Kappa)
  D.fit <- Exp * exp(Eta)
  list(Beta = Beta, D.fit = D.fit)
  
}
## Update Kappa
Update.kappa <- function(Alpha, Beta, Kappa,
                         One, Dth, Exp, D.fit){
  difD <- Dth - D.fit
  Beta2 <- Beta * Beta
  Kappa <- Kappa + t(difD) %*% Beta / (t(D.fit) %*% Beta2) 
  Kappa <- Kappa - mean(Kappa)
  Kappa <- Kappa / sqrt(sum(Kappa * Kappa))
  Kappa <- matrix(Kappa, ncol = 1)
  Eta <- Alpha %*% t(One) + Beta %*% t(Kappa)
  D.fit <- Exp * exp(Eta)
  list(Kappa = Kappa, D.fit = D.fit)
}