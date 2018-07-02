
#############################################
#########       decision model       ########
#############################################

library("lubridate")
library("dplyr")
library("reshape2")
library("ggplot2")

#importa le funzioni per simulare
source("C:\\Users\\Giacomo Monti\\Desktop\\decision model\\final projects\\funzioni_decision_model.R")

#lambda stimati da dati storici + los stimata in minuti
lambda_interarrivi = 1/115.4885
lambda_surgery = 1/74.01397
lambda_los=1/1838.647


########################################
#   funzione con parametri per arrivi  #
########################################

simulation = simulate_arrival(start = "2018-01-01 00:05:00", n_month=2,
                              lambda_interarrivi, lambda_surgery, lambda_los)


#----------------creo serie più lunga di un mese per evitare che la recovery 
#----------------sia vuota ma poi la taglio a 15 giorni
simulation = simulation [simulation$arrival_date_time > ymd_hms("2018-01-15 00:00:00"), ]


##########################################
#   funzione con parametri per recovery  #
##########################################

simulation_full = simulate_recovery(data = simulation,  n_letti = 19, max_attesa = 60,  each=15)

#----------------data cleaning
class(simulation_full$leave) <- "POSIXct"
class(simulation_full$admission) <- "POSIXct"

simulation_full$transfer = ifelse(is.na(simulation_full$transfer), FALSE, simulation_full$transfer)
simulation_full$waiting_time = difftime(simulation_full$admission , simulation_full$start_waiting, 
                                        units = "mins")
simulation_full = simulation_full [simulation_full$arrival_date_time >= ymd_hms("2018-02-01 00:00:00"), ]

#primi risultati
mean_waiting = mean(simulation_full$waiting_time)
max_waiting = max(simulation_full$waiting_time)
trasferiti = sum(simulation_full$transfer, na.rm=T)





##############################################################
############      conteggi per periodi            ############
##############################################################

#serie lunga quanto il mese che ho fatto girare prima
hour_series = seq(ymd_hms('2018-02-01 00:00:00'), ymd_hms('2018-03-01 23:00:00'), by = '30 min')  
final = data.frame(date_time = hour_series)

for (i in seq_along(final$date_time)){
  j = final$date_time[i]
  
  final[i,"n_patient_im_in"] = 
    length(simulation_full[simulation_full$admission <= j & simulation_full$leave > j &
                             simulation_full$transfer == FALSE ,"patient_id"])
  
  final[i,"n_patient_im_out"] = 
    length(simulation_full[simulation_full$admission <= j & simulation_full$leave > j &
                             simulation_full$transfer == TRUE ,"patient_id"])
  
  final[i,"mean_waiting"] = 
    quantile(simulation_full[simulation_full$admission <= j & simulation_full$leave > j &
                               simulation_full$transfer == FALSE ,"waiting_time"], 0.5)
  
  final[i,"p90_waiting"] = 
    quantile(simulation_full[simulation_full$admission <= j & simulation_full$leave > j &
                               simulation_full$transfer == FALSE ,"waiting_time"], 0.9)
  print(i)
}



######################################################
############       f obiettivo            ############
######################################################

b=19 #deve essere uguale  quello che ho usato nella simulazione
u=1 #emergenze
a = sum(b - final$n_patient_im_in  > u, na.rm=T) #quante mezzore con più di u letti vuoti (Male)
b = sum(final$p90_waiting  > 60, na.rm=T) #quanti mezzore con più di 60 minuti d attesa 90 perc
c = sum(final$n_patient_im_out == 1 , na.rm=T) #quante mezzore abbiamo occupato almeno 1 letto fuori
d = sum(final$n_patient_im_out == 2 , na.rm=T) #letti mezzora -- moltiplico per 2
e = sum(final$n_patient_im_out == 3 , na.rm=T)
f = sum(final$n_patient_im_out == 4 , na.rm=T)

sum(a+b+c+d+e+f) #minimizzare

