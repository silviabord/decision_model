#############################################
#####   decision model preprocessing   ######
#############################################

library("lubridate")
library("dplyr")
library("reshape2")
library("ggplot2")

historical_info <- read.csv("C:/Users/sdara/Decision Model/Progetto_Finale/decision_model-master/historical_info.csv", sep=";", stringsAsFactors=FALSE)
patients <- read.delim2("C:/Users/sdara/Decision Model/Progetto_Finale/decision_model-master/patients.csv", stringsAsFactors=FALSE)


#############################################
#########      data preparation      ########
#############################################

#format date time variables
historical_info$Date = dmy(historical_info$Date)

patients$arrival_date = dmy(patients$Arrival_Date)
patients$arrival_date_time = dmy_hms(paste(patients$Arrival_Date, patients$Arrival.Time))
patients$arrival_time = dmy_hms(paste("01 01 1991", patients$Arrival.Time))
patients$arrival_month = month(patients$arrival_date)
patients$arrival_dom = mday(patients$arrival_date)
patients$arrival_hour = as.numeric(hour(patients$arrival_time))

patients$waiting_time_round = round(patients$POST_ANESTHESIA_CARE_UNIT_Time_min)
patients$Arrival.Time=NULL

patients = patients[order(patients$arrival_date_time),] 
patients$arrival_date_time_t0 = lag(patients$arrival_date_time)
patients$hours_to_next = difftime(patients$arrival_date_time,patients$arrival_date_time_t0, units = "hour")
patients$minutes_to_next = difftime(patients$arrival_date_time,patients$arrival_date_time_t0, units = "min")


set.seed(1234)
#decidere parametro exp negativa >> 1/media
patients$rexp = rexp( dim(patients)[1] , 1/0.5)
patients$rexp_round = floor(patients$rexp)
patients$rexp_decimal = patients$rexp-patients$rexp_round
patients$los_estimate = (patients$Length_of_Stay_day*24*60)+
  round(patients$rexp_decimal*24*60,0)
patients$rexp = NULL
patients$rexp_round = NULL
patients$rexp_decimal = NULL


patients$leave_date_time = 
  ymd_hms(patients$arrival_date_time+ #ora ingresso in surgery
            minutes(
              round(patients$Surgery_Time_min,0) +
                round(patients$POST_ANESTHESIA_CARE_UNIT_Time_min,0) +
                patients$los_estimate))

patients = patients[order(patients$leave_date_time),] 
patients$leave_date_time_t0 = lag(patients$leave_date_time)
patients$hours_to_next_leave = difftime(patients$leave_date_time,patients$leave_date_time_t0, units = "hour")
patients$minutes_to_next_leave = difftime(patients$leave_date_time,patients$leave_date_time_t0, units = "min")




##############################################################
#########      statistiche su tempi intra arrivi      ########
##############################################################

#arrivando tutti ad ora tonda i minute sono tutti multipli di 60
temp = patients %>% group_by(arrival_time) %>%
  summarise(avg_minutes_to_next_arrival = mean(minutes_to_next),
            avg_minutes_to_next_leave = mean(minutes_to_next_leave),
            median_minutes_to_next_arrival = median(minutes_to_next , na.rm = T),
            median_minutes_to_next_leave = median(minutes_to_next_leave , na.rm = T),
            min_minutes_to_next_arrival = min(minutes_to_next , na.rm = T),
            min_minutes_to_next_leave = min(minutes_to_next_leave , na.rm = T),
            max_minutes_to_next_arrival = max(minutes_to_next , na.rm = T),
            max_minutes_to_next_leave = max(minutes_to_next_leave , na.rm = T))

#plot con minutes_to next_leave
plot(temp$arrival_time, temp$avg_minutes_to_next_leave, type="b", pch=19,
     lwd = 1, lambda=0.9, main = "Minutes to next leave vs minutes to next arrival",
     ylim=c(60,200), xlab = "Hour", ylab = "Minutes to next a/l")
lines (temp$arrival_time, temp$avg_minutes_to_next_arrival, type="b",  pch=19,
       lwd = 1, col= "blue")
legend(662688000, 110, legend=c("Leave", "Arrival"),
       col=c("black", "blue"), lty=1, cex=0.8, bty="n")

#min tempo intra arrivi sempre 0, max tempo intra arrivi varia
plot (temp$arrival_time, temp$max_minutes_to_next_leave, type="b",  pch=19,
      lwd = 1, lty=2)
lines (temp$arrival_time, temp$max_minutes_to_next_arrival, type="b",  pch=19,
       lwd = 1, col= "blue",  lty=2)
legend(662688000, 3500, legend=c("Leave", "Arrival"),
       col=c("black", "blue"), lty=1, cex=0.8, bty="n")

#distance between arrival
hist(as.numeric(patients$minutes_to_next), breaks = 50)
hist(as.numeric(patients$hours_to_next), breaks=50)

#as.numeric(mean(patients$hours_to_next, na.rm=T))

table(month(patients$arrival_date), as.numeric(patients$hours_to_next))

median(as.numeric(patients$minutes_to_next), na.rm = T)
mean(as.numeric(patients$minutes_to_next), na.rm = T)

#distance between leave
hist(as.numeric(patients$minutes_to_next_leave))
hist(as.numeric(patients$hours_to_next_leave))
median(as.numeric(patients$minutes_to_next_leave), na.rm = T)
mean(as.numeric(patients$minutes_to_next_leave), na.rm = T)



##############################################################
############           data exploration           ############
##############################################################

#serie storica ingressi nel reparto 
plot(historical_info$patients, type = "l")
###################secondo historical_info a mezzanotte ci sono in media 10 letti occupati###################
barplot(prop.table(table((historical_info$patients))))
summary(historical_info$patients)

###################historical_info has 1 month more###################
max(historical_info$Date)
#min(historical_info$Date)
max(patients$Arrival_Date)
#min(patients$Arrival_Date)

###################pazienti arrivano a qualunque ora equiprobabilmente###################
barplot(prop.table(table(hour(patients$arrival_time))),main = "arrival_time")

barplot(prop.table(table(hour(patients$leave_date_time))),main = "leave_time")

###################pazienti arrivano in qualunque giorno equiprobabilmente###################
patients$dow = factor(weekdays(patients$arrival_date), ordered=T,
                      levels=c("lunedì","martedì","mercoledì","giovedì","venerdì","sabato","domenica")) 
barplot(prop.table(table(patients$dow)),main = "dow")



################### arrivano gli stesi tutti i mesi ###################
barplot(prop.table(table(patients$arrival_month)))
barplot(prop.table(table(patients$arrival_dom)))


################### p di POST_ANESTHESIA_CARE_UNIT_Time_min dato ingresso in recovery ###################
temp = unclass(prop.table(table(hour(patients$arrival_time),
                                patients$POST_ANESTHESIA_CARE_UNIT_Time_min
), margin = 1)*100)



patients$arrival_hour_range = cut(patients$arrival_hour, b = c(-1,5,11,17,24))

ggplot(patients, aes(POST_ANESTHESIA_CARE_UNIT_Time_min,
                     colour = as.factor(arrival_hour_range))) +
  geom_density()

###################durata surgery esponenziale neg ###################
barplot(prop.table(table(round(patients$Surgery_Time_min,0))),
        main = "Surgery_Time_min")



###################attesa ingresso esponenziale neg ###################
barplot(prop.table(table(round(patients$POST_ANESTHESIA_CARE_UNIT_Time_min,0))),
        main = "POST_ANESTHESIA_CARE_UNIT_Time_min")
###################attesa ingresso in media 53 minuti ###################
mean(patients$POST_ANESTHESIA_CARE_UNIT_Time_min)
###################attesa ingresso in MEDIANA 48 minuti ###################
median(patients$POST_ANESTHESIA_CARE_UNIT_Time_min)

###################LOS ORARIA esponenziale negativa###################
barplot(prop.table(table(round(patients$Length_of_Stay_day,0))),
        main = "Length_of_Stay_day")
mean(patients$Length_of_Stay_day)



#################################
####       Simulazione       ####
#################################

hist(patients$minutes_to_next)
lambda_interarrivi = 1/as.numeric(mean(patients$minutes_to_next, na.rm=T))
hist(rexp(200,lambda_interarrivi))

hist(patients$Surgery_Time_min)
lambda_surgery = 1/as.numeric(mean(patients$Surgery_Time_min, na.rm=T))
hist(rexp(200,lambda_surgery))

hist(patients$los_estimate)
lambda_los = 1/as.numeric(mean(patients$los_estimate, na.rm=T))
hist(rexp(200,lambda_los))
