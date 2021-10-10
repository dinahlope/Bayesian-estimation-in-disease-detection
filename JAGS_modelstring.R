#'' 
#' Bayesian Estimation in disease detection
#' 24 candidate models
#' 
#' @author Dinah Lope/ Haydar Demirhan
#' @created 17/09/2021
#''

# specify the model
# model string for 24 models
modelString = "
  model {
  for(i in 1:n){
  
    pi[i] <- ifelse(m == 1,  ilogit(b[1] + gp[i,1]*b[2] + gpc[i,1]*b[3]),
             ifelse(m == 2,  ilogit(b[1]                + gpc[i,1]*b[3]),
             ifelse(m == 3,  ilogit(b[1] + gp[i,1]*b[2]),
             
             ifelse(m == 4,  ilogit(b[1] + gp[i,1]*b[2] + gpc[i,1]*b[3]),
             ifelse(m == 5,  ilogit(b[1]                + gpc[i,1]*b[3]),
             ifelse(m == 6,  ilogit(b[1] + gp[i,1]*b[2]),
             
             ifelse(m == 7,  ilogit(b[1] + gp[i,1]*b[2] + gpc[i,1]*b[3]),
             ifelse(m == 8,  ilogit(b[1]                + gpc[i,1]*b[3]),
             ifelse(m == 9,  ilogit(b[1] + gp[i,1]*b[2]),
                                         
             ifelse(m == 10, ilogit(b[1] + gp[i,1]*b[2] + gpc[i,1]*b[3]),
             ifelse(m == 11, ilogit(b[1]                + gpc[i,1]*b[3]),
             ifelse(m == 12, ilogit(b[1] + gp[i,1]*b[2]),
              
                    
             ifelse(m == 13, ilogit(b[1] + gp[i,1]*b[2] + gpc[i,1]*b[3] + acdiff[i,1]*b[4]),
             ifelse(m == 14, ilogit(b[1]                + gpc[i,1]*b[3] + acdiff[i,1]*b[4]),
             ifelse(m == 15, ilogit(b[1] + gp[i,1]*b[2]                 + acdiff[i,1]*b[4]),
              
             ifelse(m == 16, ilogit(b[1] + gp[i,1]*b[2] + gpc[i,1]*b[3] + acdiff[i,1]*b[4]),
             ifelse(m == 17, ilogit(b[1]                + gpc[i,1]*b[3] + acdiff[i,1]*b[4]),
             ifelse(m == 18, ilogit(b[1] + gp[i,1]*b[2]                 + acdiff[i,1]*b[4]),
                                         
             ifelse(m == 19, ilogit(b[1] + gp[i,1]*b[2] + gpc[i,1]*b[3] + acdiff[i,1]*b[4]),
             ifelse(m == 20, ilogit(b[1]                + gpc[i,1]*b[3] + acdiff[i,1]*b[4]),
             ifelse(m == 21, ilogit(b[1] + gp[i,1]*b[2]                 + acdiff[i,1]*b[4]),
                    
             ifelse(m == 22, ilogit(b[1] + gp[i,1]*b[2] + gpc[i,1]*b[3] + acdiff[i,1]*b[4]),
             ifelse(m == 23, ilogit(b[1]                + gpc[i,1]*b[3] + acdiff[i,1]*b[4]),
                             ilogit(b[1] + gp[i,1]*b[2]                 + acdiff[i,1]*b[4])
    
                    )))))))))))))))))))))))
  
  
    log.lambda[i] <- ifelse(m == 1,  pop[i,1] + a[1] + hless[i,1]*a[2] + hsize[i,1]*a[4] + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]] + acdiff[i,1]*a[3],
                     ifelse(m == 2,  pop[i,1] + a[1] + hless[i,1]*a[2] + hsize[i,1]*a[4] + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]] + acdiff[i,1]*a[3],
                     ifelse(m == 3,  pop[i,1] + a[1] + hless[i,1]*a[2] + hsize[i,1]*a[4] + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]] + acdiff[i,1]*a[3],
                     
                     ifelse(m == 4,  pop[i,1] + a[1]                   + hsize[i,1]*a[4] + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]] + acdiff[i,1]*a[3],
                     ifelse(m == 5,  pop[i,1] + a[1]                   + hsize[i,1]*a[4] + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]] + acdiff[i,1]*a[3],
                     ifelse(m == 6,  pop[i,1] + a[1]                   + hsize[i,1]*a[4] + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]] + acdiff[i,1]*a[3],      
                     
                     ifelse(m == 7,  pop[i,1] + a[1] + hless[i,1]*a[2]                   + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]] + acdiff[i,1]*a[3],      
                     ifelse(m == 8,  pop[i,1] + a[1] + hless[i,1]*a[2]                   + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]] + acdiff[i,1]*a[3],
                     ifelse(m == 9,  pop[i,1] + a[1] + hless[i,1]*a[2]                   + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]] + acdiff[i,1]*a[3],
                     
                     ifelse(m == 10, pop[i,1] + a[1]                                     + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]] + acdiff[i,1]*a[3],
                     ifelse(m == 11, pop[i,1] + a[1]                                     + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]] + acdiff[i,1]*a[3],
                     ifelse(m == 12, pop[i,1] + a[1]                                     + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]] + acdiff[i,1]*a[3],
    
      
                     ifelse(m == 13, pop[i,1] + a[1] + hless[i,1]*a[2] + hsize[i,1]*a[4] + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]],
                     ifelse(m == 14, pop[i,1] + a[1] + hless[i,1]*a[2] + hsize[i,1]*a[4] + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]],
                     ifelse(m == 15, pop[i,1] + a[1] + hless[i,1]*a[2] + hsize[i,1]*a[4] + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]],
                                          
                     ifelse(m == 16, pop[i,1] + a[1]                   + hsize[i,1]*a[4] + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]],
                     ifelse(m == 17, pop[i,1] + a[1]                   + hsize[i,1]*a[4] + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]],
                     ifelse(m == 18, pop[i,1] + a[1]                   + hsize[i,1]*a[4] + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]],
                                          
                     ifelse(m == 19, pop[i,1] + a[1] + hless[i,1]*a[2]                   + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]],
                     ifelse(m == 20, pop[i,1] + a[1] + hless[i,1]*a[2]                   + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]],
                     ifelse(m == 21, pop[i,1] + a[1] + hless[i,1]*a[2]                   + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]],
                      
                     ifelse(m == 22, pop[i,1] + a[1]                                     + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]],
                     ifelse(m == 23, pop[i,1] + a[1]                                     + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]],
                                     pop[i,1] + a[1]                                     + irsd1[i,1]*a[5] + irsd2[i,1]*a[6] + year[i, 1]*a[7] + (year[i, 1]^2)*a[8] + theta[index[i]] + phi[index[i]]
    
                            )))))))))))))))))))))))
    
    
    z[i] ~ dpois(pi[i]*exp(log.lambda[i]))
    
  }
  mu.theta ~ dnorm(-1, 1000)
  sigma.theta ~ dnorm(0,0.00001)T(0,)
  
  for(j in 1:R){
    theta[j] ~ dnorm(mu.theta, 1/sigma.theta)
  }

  phi[1:R] ~ dmnorm(zrs[1:R], precMatrixCAR(adj[1:R, 1:R], rho.car, tau))

  a[1] ~ dnorm(-10, 1000)
  for(i in 2:8){
    a[i] ~ dnorm(0, 1/sigmaA)
  }
  sigmaA ~ dgamma(0.01,0.01)
  b[1] ~ dnorm(2, 600)
  for(i in 2:4){
    b[i] ~ dnorm(0, 1/sigmaB)
  }
  sigmaB ~ dgamma(0.01,0.01)
  nu ~ dnorm(0,500)T(0,)
  tau <- 1/(nu*nu) 
  rho.car <- 0.999 #added
  
  m ~ dcat(mPriorProb[])
  mPriorProb[1]  <- 0.04166667
  mPriorProb[2]  <- 0.04166667
  mPriorProb[3]  <- 0.04166667
  mPriorProb[4]  <- 0.04166667
  mPriorProb[5]  <- 0.04166667
  mPriorProb[6]  <- 0.04166667
  mPriorProb[7]  <- 0.04166667
  mPriorProb[8]  <- 0.04166667
  mPriorProb[9]  <- 0.04166667
  mPriorProb[10] <- 0.04166667
  mPriorProb[11] <- 0.04166667
  mPriorProb[12] <- 0.04166667
  mPriorProb[13] <- 0.04166667
  mPriorProb[14] <- 0.04166667
  mPriorProb[15] <- 0.04166667
  mPriorProb[16] <- 0.04166667
  mPriorProb[17] <- 0.04166667
  mPriorProb[18] <- 0.04166667
  mPriorProb[19] <- 0.04166667
  mPriorProb[20] <- 0.04166667
  mPriorProb[21] <- 0.04166667
  mPriorProb[22] <- 0.04166667
  mPriorProb[23] <- 0.04166667
  mPriorProb[24] <- 0.04166667

}
"
writeLines(modelString , con="TEMPmodel.txt")
