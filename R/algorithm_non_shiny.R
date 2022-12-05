library(hms)



# constants:
weight_ideal <- data.frame(Male=65, Female=61)
a <- 0.1
b <- 0.9
new_alpha <- log(0.5)/10


# inputs (scalar):
sex <- "Male"
actual_weight <- 87
UFH_dose_init <- 28000
time_of_intervention_init <- hms(hours=9, minutes = 16)

# calculate (one off):
algo_weight_M <- min(actual_weight, weight_ideal$Male)
algo_weight_F <- min(actual_weight, weight_ideal$Female)
algo_weight <- ifelse(test=sex=="Male", yes=algo_weight_M, no=algo_weight_F)

half_life <- 26+0.323*UFH_dose_init/algo_weight

beta <- log(0.5)/half_life

C0_init <- UFH_dose_init

prot_dose_init <- UFH_dose_init/100


# inputs (vector):
UFH_dose <- c(UFH_dose_init,
              10000,
              10000,
              0,
              rep(NA, 3))
time_of_intervention <- c(time_of_intervention_init,
                          hms(hours = 9, minutes = 55),
                          hms(hours = 11, minutes = 55),
                          hms(hours = 12, minutes = 15),
                          rep(NA, 3))


# calculate (vector):
time_elapsed <- as.numeric(time_of_intervention[-1] - time_of_intervention[-length(time_of_intervention)])/60

rows.minus.1 <- length(time_of_intervention)-1
C0 <- c(C0_init, rep(NA, rows.minus.1))
prot_dose <- c(prot_dose_init, rep(NA, rows.minus.1))
for(i in 2:rows.minus.1){
  C0[i] <- UFH_dose[i] + C0[i-1]*a*exp(new_alpha*time_elapsed[i-1]) + C0[i-1]*b*exp(beta*time_elapsed[i-1])
  prot_dose[i] <- C0[i]/100
}


prot_dose
