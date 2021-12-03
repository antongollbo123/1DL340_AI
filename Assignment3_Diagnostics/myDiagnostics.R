# "hist" IS A 100X9 MATRIX, IT CONTAINS VALUES FOR Pn (Pneumonia), Te (Temperature), 
#VTB (Visited a TB location), TB (Tuberculosis), Sm (Smoker), LC (Lung Cancer), Br (Bronchitis), 
#XR (X-Ray result), Dy (Dyspnea, or shortness of breath).
###############################################################################################
# "cases" is a 100x9 matrix, containing same parameters, which should be used to evaluate performance of model
#
#

#data = data(hist)

temp_given_pneumonia <- function(data){
  has_pneumonia = data[data$Pn > 0,]$Te
  #print(data)
  mean_temp_p = mean(has_pneumonia)
  sd_temp_p = sd(has_pneumonia)
  has_not_pneumonia = data[data$Pn == 0,]$Te
  mean_temp_noP = mean(has_not_pneumonia)
  sd_temp_noP = sd(has_not_pneumonia)
  
  return (c(mean_temp_p, sd_temp_p, mean_temp_noP, sd_temp_noP))
}


pneumonia <- function(data){
  #print(data)
  prob_pneumonia_one = length(which(data$Pn == 1)) / length(data$Pn)
  prob_pneumonia_zero = 1 - prob_pneumonia_one
  
  return(c(prob_pneumonia_one,prob_pneumonia_zero))
}

visited <- function(data){
  #print(data)
  prob_visited_one = length(which(data$VTB == 1)) / length(data$VTB)
  prob_visited_zero = 1 - prob_visited_one
  
  return(c(prob_visited_one,prob_visited_zero))
}

smokes <- function(data){
  #print(data)
  prob_smokes_one = length(which(data$Sm == 1)) / length(data$Sm)
  prob_smokes_zero = 1 - prob_smokes_one
  
  return(c(prob_smokes_one,prob_smokes_zero))
}

tuberc <- function(data){
  has_VTB = length(which(data$VTB == 1 & data$TB == 1))
  hasnt_VTB = length(which(data$VTB == 1 & data$TB == 0))
  prob_has_VTB = has_VTB/(has_VTB + hasnt_VTB)
  prob_hasnt_VTB = 1 - prob_has_VTB
  
  has_nVTB = length(which(data$VTB == 0 & data$TB == 1))
  hasnt_nVTB = length(which(data$VTB == 0 & data$TB == 0))
  prob_has_nVTB = has_nVTB /(has_nVTB + hasnt_nVTB)
  prob_hasnt_nVTB = 1- prob_has_nVTB
  return(c(prob_has_VTB, prob_hasnt_VTB, prob_has_nVTB, prob_hasnt_nVTB))
}

lung_cancer <- function(data){
  has_Sm = length(which(data$Sm == 1 & data$LC == 1))
  hasnt_Sm = length(which(data$Sm == 1 & data$LC == 0))
  prob_has_Sm = has_Sm/(has_Sm + hasnt_Sm)
  prob_hasnt_Sm = 1 - prob_has_Sm
  
  has_nSm = length(which(data$Sm == 0 & data$LC == 1))
  hasnt_nSm = length(which(data$Sm == 0 & data$LC == 0))
  prob_has_nSm = has_nSm /(has_nSm + hasnt_nSm)
  prob_hasnt_nSm = 1- prob_has_nSm
  return(c(prob_has_Sm, prob_hasnt_Sm, prob_has_nSm, prob_hasnt_nSm))
}

broncx <- function(data){
  has_Sm = length(which(data$Sm == 1 & data$Br == 1))
  hasnt_Sm = length(which(data$Sm == 1 & data$Br == 0))
  prob_has_Sm = has_Sm/(has_Sm + hasnt_Sm)
  prob_hasnt_Sm = 1 - prob_has_Sm
  
  has_nSm = length(which(data$Sm == 0 & data$Br == 1))
  hasnt_nSm = length(which(data$Sm == 0 & data$Br == 0))
  prob_has_nSm = has_nSm /(has_nSm + hasnt_nSm)
  prob_hasnt_nSm = 1- prob_has_nSm
  return(c(prob_has_Sm, prob_hasnt_Sm, prob_has_nSm, prob_hasnt_nSm))
}

dyspn <- function(data){
  has_LC_Br = length(which(data$Dy ==1 & data$LC == 1 & data$Br == 1))
  has_nLC_Br = length(which(data$Dy ==1 & data$LC == 0 & data$Br == 1))
  has_nLC_nBr = length(which(data$Dy ==1 & data$LC == 0 & data$Br == 0))
  has_LC_nBr = length(which(data$Dy ==1 & data$LC == 1 & data$Br == 0))
  
  hasnt_LC_Br = length(which(data$Dy == 0 & data$LC == 1 & data$Br == 1))
  hasnt_nLC_Br = length(which(data$Dy == 0 & data$LC == 0 & data$Br == 1))
  hasnt_nLC_nBr = length(which(data$Dy == 0 & data$LC == 0 & data$Br == 0))
  hasnt_LC_nBr = length(which(data$Dy == 0 & data$LC == 1 & data$Br == 0))
  
  prob_has_LC_Br = has_LC_Br /(has_LC_Br + hasnt_LC_Br)
  prob_hasnt_LC_Br = 1 - prob_has_LC_Br
  
  prob_has_nLC_Br = has_nLC_Br  / (has_nLC_nBr+hasnt_nLC_nBr)
  prob_hasnt_nLC_Br = 1- prob_has_nLC_Br
  
  prob_has_nLC_nBr = has_nLC_nBr / (has_nLC_nBr + hasnt_nLC_nBr)
  prob_hasnt_nLC_nBr =1- prob_has_nLC_nBr
  
  prob_has_LC_nBr = has_LC_nBr / (has_LC_nBr + hasnt_LC_nBr)
  prob_hasnt_LC_nBr  = 1-prob_has_LC_nBr
  
  return(c(prob_has_LC_Br, prob_hasnt_LC_Br, prob_has_nLC_Br, prob_hasnt_nLC_Br, prob_has_nLC_nBr, prob_hasnt_nLC_nBr, prob_has_LC_nBr, prob_hasnt_LC_nBr))
}

xRAY <- function(data){
  has_Pn_LC_TB = length(which(data$XR == 1 & data$Pn ==1 & data$LC == 1 & data$TB == 1))
  has_Pn_nLC_TB = length(which(data$XR == 1 & data$Pn ==1 & data$LC == 0 & data$TB == 1))
  has_Pn_nLC_nTB = length(which(data$XR == 1 & data$Pn ==1 & data$LC == 0 & data$TB == 0))
  has_Pn_LC_nTB = length(which(data$XR == 1 & data$Pn ==1 & data$LC == 1 & data$TB == 0))
  
  has_nPn_LC_TB = length(which(data$XR == 1 & data$Pn == 0 & data$LC == 1 & data$TB == 1))
  has_nPn_nLC_TB = length(which(data$XR == 1 & data$Pn == 0 & data$LC == 0 & data$TB == 1))
  has_nPn_nLC_nTB = length(which(data$XR == 1 & data$Pn == 0 & data$LC == 0 & data$TB == 0))
  has_nPn_LC_nTB = length(which(data$XR == 1 & data$Pn == 0 & data$LC == 1 & data$TB == 0))
  
  hasnt_Pn_LC_TB = length(which(data$XR == 0 & data$Pn ==1 & data$LC == 1 & data$TB == 1))
  hasnt_Pn_nLC_TB = length(which(data$XR == 0 & data$Pn ==1 & data$LC == 0 & data$TB == 1))
  hasnt_Pn_nLC_nTB = length(which(data$XR == 0 & data$Pn ==1 & data$LC == 0 & data$TB == 0))
  hasnt_Pn_LC_nTB = length(which(data$XR == 0 & data$Pn ==1 & data$LC == 1 & data$TB == 0))
  
  hasnt_nPn_LC_TB = length(which(data$XR == 0 & data$Pn == 0 & data$LC == 1 & data$TB == 1))
  hasnt_nPn_nLC_TB = length(which(data$XR == 0 & data$Pn == 0 & data$LC == 0 & data$TB == 1))
  hasnt_nPn_nLC_nTB = length(which(data$XR == 0 & data$Pn == 0 & data$LC == 0 & data$TB == 0))
  hasnt_nPn_LC_nTB = length(which(data$XR == 0 & data$Pn == 0 & data$LC == 1 & data$TB == 0))
  
  prob_has_Pn_LC_TB = has_Pn_LC_TB / (has_Pn_LC_TB+hasnt_Pn_LC_TB)
  prob_hasnt_Pn_LC_TB = 1 - prob_has_Pn_LC_TB
  
  prob_has_Pn_nLC_TB = has_Pn_nLC_TB / (has_Pn_nLC_TB+hasnt_Pn_nLC_TB)
  prob_hasnt_Pn_nLC_TB = 1 - prob_has_Pn_nLC_TB
  
  prob_has_Pn_nLC_nTB = has_Pn_nLC_nTB / (has_Pn_nLC_nTB+hasnt_Pn_nLC_nTB)
  prob_hasnt_Pn_nLC_nTB = 1 - prob_has_Pn_nLC_nTB
  
  prob_has_Pn_LC_nTB = has_Pn_LC_nTB / (has_Pn_LC_nTB+hasnt_Pn_LC_nTB)
  prob_hasnt_Pn_LC_nTB = 1 - prob_has_Pn_LC_nTB
  
  
  prob_has_nPn_LC_TB = has_nPn_LC_TB / (has_nPn_LC_TB+hasnt_nPn_LC_TB)
  prob_hasnt_nPn_LC_TB = 1 - prob_has_nPn_LC_TB
  
  prob_has_nPn_nLC_TB = has_nPn_nLC_TB / (has_nPn_nLC_TB+hasnt_nPn_nLC_TB)
  prob_hasnt_nPn_nLC_TB = 1 - prob_has_nPn_nLC_TB
  
  prob_has_nPn_nLC_nTB = has_nPn_nLC_nTB / (has_nPn_nLC_nTB+hasnt_nPn_nLC_nTB)
  prob_hasnt_nPn_nLC_nTB = 1 - prob_has_nPn_nLC_nTB
  
  prob_has_nPn_LC_nTB = has_nPn_LC_nTB / (has_nPn_LC_nTB+hasnt_nPn_LC_nTB)
  prob_hasnt_nPn_LC_nTB = 1 - prob_has_nPn_LC_nTB
  
  return(c(prob_has_Pn_LC_TB, prob_hasnt_Pn_LC_TB, 
           prob_has_Pn_nLC_TB, prob_hasnt_Pn_nLC_TB, 
           prob_has_Pn_nLC_nTB, prob_hasnt_Pn_nLC_nTB, 
           prob_has_Pn_LC_nTB, prob_hasnt_Pn_LC_nTB, 
           prob_has_nPn_LC_TB, prob_hasnt_nPn_LC_TB, 
           prob_has_nPn_nLC_TB, prob_hasnt_nPn_nLC_TB, 
           prob_has_nPn_nLC_nTB, prob_hasnt_nPn_nLC_nTB, 
           prob_has_nPn_LC_nTB,prob_hasnt_nPn_LC_nTB))
}


learn <- function(data){
  tmp <- temp_given_pneumonia(data)
  pn <- pneumonia(data)
  vtb <- visited(data)
  sm <- smokes(data)
  tu <- tuberc(data)
  lc <- lung_cancer(data)
  br <- broncx(data)
  dy <- dyspn(data)
  xr <- xRAY(data)
  network <- list(Te = tmp, Pn = pn, VTB = vtb, Sm = sm, TB = tu, LC = lc, Br = br, Dy = dy, XR = xr)
  return(network)
}

getProb <- function(case){
  # if statements here
  return(prob)
}

getSample <- function(rand, cCase){
  indexes <- c(1,4,6,7)
  cProb <- getProb(cCase)
  for (index in indexes){
    if (cCase[index] == 0){
      tCase <- cCase
      tCase[index] <- 1
      tProb <- getProb(tCase)
      if ((tProb/cProb) >= 1){
        cProb <- tProb
        cCase <- tCase
      } else if ((tProb/cProb) > rand)  {
        cProb <- tProb
        cCase <- tCase
      }
    } else if (cCase[index] == 1){
      tCase <- cCase
      tCase[index] <- 0
      tProb <- getProb(tCase)
      if ((tProb/cProb) >= 1){
        cProb <- tProb
        cCase <- tCase
      } else if ((tProb/cProb) > rand)  {
        cProb <- tProb
        cCase <- tCase
      }
    }
  }
  return(cCase)
}

getFinalProb <- function(data,index){
  #print(data)
  prob_sickn_one = length(which(data[index] == 1)) / length(data[index])
  prob_sickn_zero = 1 - prob_sickn_one
  if (prob_sickn_one >= prob_sickn_zero){
    return(1)
  } else if (prob_sickn_one < prob_sickn_zero){
    return(0)
  } 
}

metroGibbs <- function(initCase,rand,samples){
  sampleList <- list()
  initCase$Pn <- 0
  initCase$TB <- 0
  initCase$LC <- 0
  initCase$Br <- 0
  cCase <- initCase
  for(i in 1:length(samples)){
    bestSample <- getSample(rand[i], cCase)
    sampleList[i] <- bestSample
    cCase <- bestSample
  }
  burnIn <- 1:(length(samples)*0.1)
  burnIn <- floor(burnIn)
  finalSampleList <- sampleList[-c(burnIn)]
  pn <- getFinalProb(finalSampleList, 1)
  tb <- getFinalProb(finalSampleList, 4)
  lC <- getFinalProb(finalSampleList, 6)
  br <- getFinalProb(finalSampleList, 7)
  return(list(Pn = pn, Tb = tb, LC = lC, Br = br))
}

diagnose <- function(network, cases){
  samples = 10
  rand = runif(samples)
  results <- list()
  for(i in 1:length(cases)){
    results[i] <- metroGibbs(case[i,],rand,samples)
  }
  return(results)
}
