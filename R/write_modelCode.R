write_modelCod <- function(){
  
  code <- nimble::nimbleCode({
    
    # Four seasons for survival: 
    # Summer: S[1,t] ~ mO + rE
    # Autumn: S[2,t] ~ mO + mH + rE
    # Winter: S[3,t] ~ mO + mH + mSNO
    # Spring: S[4,t] ~ mO + mSNO
    
    # mO = natural mortality
    # mH = general harvest mortality (license & nuisance harvest)
    # mSNO = SNO harvest (may have to be split into den hunts vs. helicopter hunts)
    # rE = emigration hazard rate
  
    # Psi[t] = breeding/mating probability
    # eta[t] = probability of not aborting pregnancy/entire litter (we may not be able to estimate this)
    # rho[t] = litter size in utero
    # S_SNO[t] = probability of surviving den hunts (proportion of dens NOT taken out)
    # S_0[t] = "natural" survival from den to recruitment
    # Imm[t] = expected number of immigrants
    
    # Census: Start of June
    
    # Survival
    survN[t+1] ~ dbin(S[1,t]*S[2,t]*S[3,t]*S[4,t], N[t])
    
    # Breeding/mated females
    B[t] ~ dbin(S[1,t]*Psi[t], survN[t])
    
    # Reproductive events
    E[t+1] ~ dbin(S[2,t]*S[3,t]*eta[t+1], B[t])
    
    # Number of pups born
    L[t+1] ~ dpois(rho[t+1]*E[t+1])
    
    # Number of pups recruited
    R[t+1] ~ dbin(S_SNO[t+1]*S_0[t+1], L[t+1])
    
    # Immigrants
    survImm[t+1] ~ dpois(S[3,t]*S[4,t]*Imm[t])
    # Assumption: All immigration happens within December
    
    # Popualtion size
    N[t+1] <- survN[t+1] + R[t+1] + survImm[t+1]
  
  
  
  })
}