
#############################################
#########       decision model       ########
#############################################

library("lubridate")
library("dplyr")
library("reshape2")
library("ggplot2")
source("C:\\Users\\Giacomo Monti\\Desktop\\decision model\\final projects\\funzioni_decision_model.R")



#############################
#   funzione con parametri  #
#############################

sink("simulation_output.txt")

# for seed_n in c(12345, 1, 1991, 300, 38746, 94345){
#   for interarrivi in c(115.4885, 133, 98) {
#     for letti in c(seq(17,27)){
#       for attesa in c(50, 60, 65, 70, 90){


for (seed_n in c(12345)){
  for (interarrivi in c(115.4885)) {
    for (letti in c(17,27)){
      for (attesa in c(50, 90)){        
        
        
        #------------simulation
        simulation = simulate_arrival(start = "2018-01-01 00:05:00", n_days=45,
                                      lambda_interarrivi=1/interarrivi  , 
                                      lambda_surgery = 1/74.01397 ,
                                      lambda_los = 1/1838.647,
                                      seed = seed_n )
        
        
        simulation_full = simulate_recovery(data = simulation,
                                            n_letti = letti,
                                            max_attesa = attesa,
                                            each=10)
        
        
        #------------clean
        class(simulation_full$leave) <- "POSIXct"
        class(simulation_full$admission) <- "POSIXct"
        
        simulation_full$waiting_time = difftime(simulation_full$admission , simulation_full$start_waiting, 
                                                units = "mins")
        simulation_full = simulation_full [simulation_full$arrival_date_time >= ymd_hms("2018-02-01 00:00:00"), ]
        
        
        
        #------------periodi
        hour_series = seq(ymd_hms('2018-02-01 00:00:00'), ymd_hms('2018-03-01 23:00:00'), by = '10 min')  
        final = data.frame(date_time = hour_series)
        
        for (i in seq_along(final$date_time)){
          j = final$date_time[i]
          
          final[i,"n_patient_im_in"] = 
            length(simulation_full[simulation_full$admission <= j & simulation_full$leave > j &
                                     simulation_full$transfer == FALSE ,"patient_id"])
          
          final[i,"n_patient_im_out"] = 
            length(simulation_full[simulation_full$admission <= j & simulation_full$leave > j &
                                     simulation_full$transfer == TRUE ,"patient_id"])
        }
        
        #------------output
        a = sum(letti - final$n_patient_im_in  > 1, na.rm=T) #quante mezzore con più di 1 letto vuoto (Male)
        c = sum(final$n_patient_im_out == 1 , na.rm=T) #quante mezzore abbiamo occupato almeno 1 letto fuori
        d = sum(final$n_patient_im_out == 2 , na.rm=T) 
        e = sum(final$n_patient_im_out == 3 , na.rm=T)
        f = sum(final$n_patient_im_out >= 4 , na.rm=T)
        
        #------------print
        print(c(
          round(seed_n,0),
          round(interarrivi,0),
          round(letti,0),
          round(attesa,0),
          
          round(dim(simulation_full)[1],0),
          round(quantile(simulation_full[,"waiting_time"], 0.5),0),
          round(quantile(simulation_full[,"waiting_time"], 0.9),0),
          round(mean(simulation_full$waiting_time),0), #mean_waiting
          round(max(simulation_full$waiting_time),0), #max_waiting
          round(max(final$n_patient_im_in+final$n_patient_im_out),0), #max letti per periodo
          round(sum(simulation_full$transfer, na.rm=T),0), #trasferiti
          round(a,0),
          round(c,0),
          round(d,0),
          round(e,0),
          round(f,0),
          round(dim(final),0),
          round(sum(a+c+d+e+f),0))
        ) 
        
      }
    }
  }
}
sink()



