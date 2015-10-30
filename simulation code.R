######################################################################
##### DESCRIPTION SIMULATION STUDY COMMENT ON MAXWELL ET AL. (2015) 
##### GOAL: EXAMINE CONSEQUENCES FOR SCIENCE UNDER DIFFERENT PUBLISHING SCENARIOS
#
### INDEPENDENT VARIABLES: 4 X 3 X 3 = 36 CELLS
# POPULATION EFFECT SIZE (mus): 0; 0.2; 0.5; 0.8
# SAMPLE SIZE PER GROUP (n.specs): 25; 1750; 0 (= 1/72 PROBABILITY OF OBSERVING SAMPLE SIZE OF 
#                                  1750 AND 71/72 PROBABILITY OF OBSERVING SAMPLE SIZE OF 25)
# SCENARIOS (scens):
# - "new": ONLY THE REPLICATIONS ARE INCLUDED IN META-ANALYSIS (FIRST STUDY IS OMITTED)
# - "all.pub.1": NON-SIGNIFICANT STUDIES HAVE A PROBABILITY OF 0.1 TO BE INCLUDED IN META-ANALYSIS
# - "new.pub.1": ONLY THE REPLICATIONS AND NON-SIGNIFICANT STUDIES HAVE A PROBABILITY OF 0.1 TO BE INCLUDED IN META-ANALYSIS
#
### DEPENDENT VARIABELS
# MEAN OF META-ANALYTIC ESTIMATE
# SD OF META-ANALYTIC ESTIMATE
# MEAN OF SUM OF SAMPLE SIZE IN EACH META-ANALYSIS
# SD OF AVERAGE SAMPLE SIZE IN EACH META-ANALYSIS
# HOW OFTEN NULL-HYPOTHESIS OF NO EFFECT WAS REJECTED
# HOW OFTEN UPPER BOUND OF CONFIDENCE INTERVAL WAS LARGER THAN 0.1
##### END DESCRIPTION SIMULATION STUDY #####
######################################################################

rm(list = ls()) # Clean workspace

### Function for fixed-effect meta-analysis
fix <- function(yi, vi) {
  wi <- 1/vi # Weight per study
  est <- sum(yi*wi)/sum(wi) # FE meta-analytic estimate
  se <- sqrt(1/sum(wi)) # Standard error of meta-analytic estimate
  ci.lb <- est-qnorm(0.975)*se # Lower bound CI meta-analytical estimate
  ci.ub <- est+qnorm(0.975)*se # Upper bound CI meta-analytical estimate
  zval <- est/se # Z-value for test of no effect
  pval.0 <- pnorm(zval, lower.tail = FALSE) # Compute one-sided p-value
  
  return(data.frame(est = est, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval.0))
}

##################
### CONDITIONS ###
##################

mus <- c(0, 0.2, 0.5, 0.8) # Population effect size
n.specs <- c(25, 1750, 0) # Sample sizes (0 refers to 1/72 probability of observing a sample size of 1750 and 71/72 probability of a sample size of 25)
scens <- c("new", "all.pub.1", "new.pub.1") # Different scenarios
reps <- 10000 # Number of replications

### Empty objects for storing results
res.est <- res.est.sd <- res.ni <- res.ni.sd <- res.h1 <- res.ub <- 
  array(NA, dim = c(length(scens), length(mus), length(n.specs)), dimnames = list(scens, as.character(mus), as.character(n.specs)))
out.est <- out.ni <- out.h1 <- out.ub <- numeric(reps)

###################
### SIMULATIONS ###
###################

set.seed(22102015) # Set seed for simulations

for (mu in mus) {
  for (n.spec in n.specs) {
    for (scen in scens) {
      
      cat("mu = ", mu, "n.spec = ", n.spec, "scen = ", scen, fill = TRUE)
      
      for (i in 1:reps) {
        
        crit <- TRUE # Criterion for staying in while loop below
        
        if (scen == "new" | scen == "new.pub.1") { # If meta-analysis is based on only replications create empty objects
          yi <- sei <- ni.all <- numeric()
        } else { # If meta-analysis is based on all studies (scenario "all.pub.1")
          ni.all <- 25
          sei <- sqrt(2/25) # Compute standard error
          pcv <- pnorm(qnorm(.975)*sei, mean = mu, sd = sei) # Probability of observing significant result given mu and standard error
          yi <- qnorm(runif(1, min = pcv, max = 1), mean = mu, sd = sei) # Generated observed effect size given being significant
          fe <- fix(yi = yi, vi = sei^2) # Conduct fixed-effect meta-analysis    
        }
        
        while (crit == TRUE) { # Stay in while loop till width of CI is larger than 0.2
          
          if (n.spec == 0) { # Determine whether a study with a large or small sample size has to be generated 
            if (runif(1) < 1/72) { ni <- 1750 
            } else { ni <- 25 }
          } else { ni <- n.spec } # Use prespecified sample size
          
          if (scen == "new") {
            sei <- c(sei, sqrt(2/ni)) # Compute and store standard error
            ni.all <- c(ni.all, ni) # Store sample size
          }
          
          if (scen == "all.pub.1" | scen == "new.pub.1") { # Generate studies for scenario "all.pub.1" and "new.pub.1"              
            if (length(yi) == 0) { # Do not conduct meta-analysis till a study is included in meta-analysis ("new.pub.1")
              while(length(yi) == 0) { 
                yi.pub <- rnorm(1, mean = mu, sd = sqrt(2/ni)) # Generate effect size
                pval.pub <- pnorm(yi.pub/sqrt(2/ni), lower.tail = FALSE) # Compute p-value
                if (pval.pub < .025 | runif(1) < 0.1) { # Include study in meta-analysis if p-value is below .025 or randomly generated number is below 0.1
                  yi <- c(yi, yi.pub)
                  sei <- sqrt(2/ni)
                  ni.all <- c(ni.all, ni)
                } else { ni.all <- c(ni.all, ni) }
              }
            } else {
              yi.pub <- rnorm(1, mean = mu, sd = sqrt(2/ni)) # Generate effect size
              pval.pub <- pnorm(yi.pub/sqrt(2/ni), lower.tail = FALSE) # Compute p-value
              if (pval.pub < .025 | runif(1) < 0.1) { # Include study in meta-analysis if p-value is below .025 or randomly generated number is below 0.1
                yi <- c(yi, yi.pub)
                sei <- c(sei, sqrt(2/ni))
                ni.all <- c(ni.all, ni)
              } else { ni.all <- c(ni.all, ni) }      
            }
          } else { yi <- c(yi, rnorm(1, mean = mu, sd = sqrt(2/ni))) } # Generate effect size for scenario "new"
          
          fe <- fix(yi = yi, vi = sei^2) # Conduct fixed-effect meta-analysis
          
          crit <- fe$ci.ub - fe$ci.lb > 0.2 # Return FALSE if width of CI is lower than 0.2
          
        }
        
        out.est[i] <- fe$est # Store estimate of fixed-effect meta-analysis
        out.ni[i] <- sum(ni.all) # Store sum of sample size in each meta-analysis
        out.h1[i] <- fe$pval < .025 # Store whether the effect is significantly larger than 0 (two-tailed test and only tested in predicted direction)
        out.ub[i] <- fe$ci.ub > 0.1 # Store whether lower bound of CI is larger than 0.1
        
      }
      
      res.est[scen, as.character(mu), as.character(n.spec)] <- round(mean(out.est), 3) # Compute mean of meta-analytic estimates
      res.est.sd[scen, as.character(mu), as.character(n.spec)] <- round(sd(out.est), 3) # Compute standard deviation of meta-analytic estimates
      res.ni[scen, as.character(mu), as.character(n.spec)] <- round(mean(out.ni), 3) # Compute mean of sum of sample size in each meta-analysis
      res.ni.sd[scen, as.character(mu), as.character(n.spec)] <- round(sd(out.ni), 3) # Compute standard deviation of average sample size in each meta-analysis
      res.h1[scen, as.character(mu), as.character(n.spec)] <- round(mean(out.h1), 3) # Compute how often null-hypothesis of no effect was rejected
      res.ub[scen, as.character(mu), as.character(n.spec)] <- round(mean(out.ub), 3) # Compute how often lower bound of CI was larger than 0.1
      
    }
    dump(c("scens", "mus", "n.specs", "reps", "res.est", "res.est.sd", "res.ni", "res.ni.sd", "res.h1", "res.ub"), file = "results.maxwell.R")
  }
}