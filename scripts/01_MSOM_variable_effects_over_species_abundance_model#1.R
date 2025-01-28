### Load packages
library(here)
library(R2jags)

# -------- Script to perform the MSOM and verify the variable effects over species relative abundance  -----------
## -------- Load data -----------
data<-read.csv(file =  here("data","sampaio.data.csv"), sep = ",", fileEncoding  = "UTF-8")

##----- Specify model in JAGS language ----- adapted from Sampaio et al., 2023
sink(file = here("data","Richness.model.formula_A_int.txt"))
cat("model { 

      # prior distributions on community level estimates - hyperparameters
			psi ~ dunif(0,1)	# inclusion rate that generates psi
			
      # mean value (mu)
			# parameter related to occupancy
			mu.a0 ~ dnorm(0,0.5)  # intercept on lambda
      mu.a1 ~ dnorm(0,0.5)  # slope on lambda for Com dist
      mu.a2 ~ dnorm(0,0.5)  # slope on lambda for With/without dog
      mu.a3 ~ dnorm(0,0.5)  # slope for interaction
      
			# parameter related to detectability
			mu.r0 ~ dnorm(0,0.5)  # intercept on lambda
			#mu.r1 ~ dnorm(0,0.5)  # slope on detection for Com dist
      #mu.r2 ~ dnorm(0,0.5)  # slope on detection for With/without dog
      #mu.r3 ~ dnorm(0,0.5)  # slope on detection for Deforestation
    
			# standard deviation
			# parameter related to abundance
      sigma.a0 ~ dunif(0,10)	# intercept
      sigma.a1 ~ dunif(0,10)	# Com dist
			sigma.a2 ~ dunif(0,10)	# With/without dog
			sigma.a3 ~ dunif(0,10)	# Interaction 
			
			# parameter related to detectability
			sigma.r0 ~ dunif(0,10) # intercept
			#sigma.r1 ~ dunif(0,10) # Com dist
			#sigma.r2 ~ dunif(0,10) # With/without dog
			#sigma.r3 ~ dunif(0,10) # Deforestation
			
			# create precision
			# parameter related to abundance
			tau.a0 <- pow(sigma.a0,-2)
			tau.a1 <- pow(sigma.a1,-2)
			tau.a2 <- pow(sigma.a2,-2)
			tau.a3 <- pow(sigma.a3,-2)
			
			# parameter related to detectability
			tau.r0 <- pow(sigma.r0,-2)
			#tau.r1 <- pow(sigma.r1,-2)
			#tau.r2 <- pow(sigma.r2,-2)
			#tau.r3 <- pow(sigma.r3,-2)
			
			for(i in 1:(nspecies+nzeros)) {
				 # Create priors for species i from the community level prior distributions
				
				 w[i] ~ dbern(psi)  # inclusion indicators
   
         a0[i] ~ dnorm(mu.a0, tau.a0)  # intercept
         a1[i] ~ dnorm(mu.a1, tau.a1)  # treatment on Com.dist
         a2[i] ~ dnorm(mu.a2, tau.a2)  # treatment on Hunting dog
         a3[i] ~ dnorm(mu.a3, tau.a3)  # Interaction
         
         r0[i] ~ dnorm(mu.r0, tau.r0) # intercept
         #r1[i] ~ dnorm(mu.r1, tau.r1) # treatment on Com.dist
         #r2[i] ~ dnorm(mu.r2, tau.r2) # treatment on Hunting dog
         #r3[i] ~ dnorm(mu.r3, tau.r3) # treatment on Deforestation
	     	
     ## Likelihood
     #likelihood - Ecological model for latent abundance of species i in sites j
      for (j in 1:nSites){
      # population abundances.
      
        log(lambda[j,i]) <- a0[i] + a1[i]*Com.dist[j] + a2[i]*margem[j] + a3[i]*Com.dist[j]*margem[j]
        Z[j,i] ~ dpois(lambda[j,i]) # latent abundance of each species in each site
        A[j,i] <- Z[j,i] * w[i]		  # latent abundance only for extant species
        o[j,i] <- step(A[j,i]-1)  	# occupancy of each species in each site

		# detection process model
				r[j,i] <- 1/(1+exp(-(r0[i])))
				#r[j,i] <- 1/(1+exp(-(r0[i] + r1[i]*Com.dist[j]))) #+ r2[i]*margem[j] + r3[i]*Def[j]))) 
				
				p[j,i] <- 1-pow(1-r[j,i],A[j,i])	
        y[j,i] ~ dbin(p[j,i], k[j])  # model observation data as binomial outcome with prob p and k trials

				}#j
			  }#i
			  
		## counting species richness at site
      for(j in 1:nSites){ 
        SR[j]	<- sum(o[j,])	# whole species
    ## counting abundance at site
        AB[j]	<- sum(A[j,])	# whole species
    ## couting biomass at site  
        BI[j] <- sum(A[j,1:nspecies]*biomass)
    }}", fill=TRUE)
sink()

##----- Inputs to MSMOM ---- ##
Com.dist     <- as.vector(data$Com.D)
margem       <- as.vector(data$margem)
k            <- as.vector(data$trials.k)
nzeros       <- 10
nSites       <- dim(data)[1]
nspecies     <- dim(data[1:21])[2]
y            <- as.matrix(data[1:21])
yaug = cbind(y, matrix(0, ncol=nzeros, nrow=nSites))
yaug = as.matrix(yaug)
biomass<-c(9.50,25,30,4.5,1.2,6,18,160,1.2,1.2,1.28,5.1,30.5,7.750,0.420,1.088,4.85,15,6,0.75,4.5)

##----- create jags data ---- ##
jags_data <- list(y=as.matrix(yaug),nspecies=nspecies, nzeros = nzeros, nSites = nSites , k = k,
                  Com.dist=Com.dist, margem=margem, biomass=biomass)#, Def=Def)

##----- monitored parameters ---- ##
params = c("a0", "a1","a2", "a3",
           "mu.a0", "mu.a1","mu.a2", "mu.a3",
           "r0",#"r1",# "r2", "r3",
           "A","o", "SR", "AB","BI")

##----- initial values ---- ##
inits <- function()list(Z=cbind(matrix(1,nrow=nSites,ncol=(nspecies)), 
                                matrix(rpois(n=(nzeros)*nSites,lambda=0.05),nrow=nSites,ncol=(nzeros))),
                        psi=runif(1),
                        w=c(rep(1,nspecies), rbinom(nzeros,1,0.5)))

##----- perform the model ---- ##
out <- jags(jags_data, inits, params,  here("data","Richness.model.formula_A_int.txt"),
            n.chain=3, n.burnin=50000, n.iter=100000, n.thin=100)

##----- Save results ---- ##
saveRDS(out, here("data", "RN_multitaxa_AB_int.rds"))

##----- Load model ---- ##
out<-readRDS(here("data", "RN_multitaxa_AB_int.rds"))

##----- Model fit -----
##----- Check model convergence -----
source(file = here("data","convergence.r"))
sumario
rhat
