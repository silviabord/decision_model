#############################################
#########       decision model       ########
#############################################



library("lubridate")
library("dplyr")
library("reshape2")
library("ggplot2")

historical_info <- read.csv("C:/Users/Giacomo Monti/Desktop/decision model/final projects/historical_info.csv", sep=";", stringsAsFactors=FALSE)
patients <- read.delim2("C:/Users/Giacomo Monti/Desktop/decision model/final projects/patients.csv", stringsAsFactors=FALSE)





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
#patients$bed_date = patients$arrival_date
#patients[hour(patients$arrival_date_time)>12, "bed_date"] = patients[hour(patients$arrival_date_time)>12, "arrival_date"]+1

patients = patients[order(patients$arrival_date_time),] 
patients$arrival_date_time_t0 = lag(patients$arrival_date_time)
patients$hours_to_next = difftime(patients$arrival_date_time,patients$arrival_date_time_t0, units = "hour")
patients$minutes_to_next = difftime(patients$arrival_date_time,patients$arrival_date_time_t0, units = "min")


set.seed(1234)
patients$rexp = rexp( dim(patients)[1] , 1/mean(patients$Length_of_Stay_day))
patients$rexp_round = floor(patients$rexp)
patients$rexp_decimal = patients$rexp-patients$rexp_round
patients$los_estimate = (patients$Length_of_Stay_day*24*60)+
  round(patients$rexp_decimal*24*60,0)
patients$rexp = NULL
patients$rexp_round = NULL
patients$rexp_decimal = NULL


patients$leave_date_time = 
  patients$arrival_date_time+ #ora ingresso in surgery
  minutes(
    round(patients$Surgery_Time_min,0) +
      round(patients$POST_ANESTHESIA_CARE_UNIT_Time_min,0) +
      patients$los_estimate)


#View(head(patients[,c("arrival_date_time","leave_date_time",
#                      "Length_of_Stay_day",
#                      "Surgery_Time_min",
#                      "POST_ANESTHESIA_CARE_UNIT_Time_min",
#                      "los_estimate" )],10))

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
     ylim=c(95,140), xlab = "Hour", ylab = "Minutes to next a/l")
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

# che cazzo sono other other
table(patients$Section, patients$Surgery_Type)

###################pazienti arrivano a qualunque ora equiprobabilmente###################
barplot(prop.table(table(hour(patients$arrival_time))),main = "arrival_time")


###################pazienti arrivano a qualunque ora equiprobabilmente###################
patients$dow = factor(weekdays(patients$arrival_date), ordered=T,
                      levels=c("lunedì","martedì","mercoledì","giovedì","venerdì","sabato","domenica")) 
barplot(prop.table(table(patients$dow)),main = "dow")



################### arrivano gli stesi tutti i mesi ###################
barplot(prop.table(table(patients$arrival_month)))
barplot(prop.table(table(patients$arrival_dom)))


################### p di los dato ingresso in recovery ###################
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

#simulate POST_ANESTHESIA_CARE_UNIT_Time_min
rexp(20, rate = 1/mean(patients$Length_of_Stay_day))*24
hist(rexp(2000, rate = 1/mean(patients$Length_of_Stay_day)), breaks=20)
hist(patients$Length_of_Stay_day)






##############################################################
############      stima pazienti oraria           ############
##############################################################

hour_series = seq(ymd_hms('2015-01-02 00:00:00'), ymd_hms('2015-10-30 23:00:00'), by = '30 min')  
final = data.frame(date_time = hour_series)

#write.table(patients, "C:\\Users\\Giacomo Monti\\Desktop\\decision model\\final projects\\patients_modified.csv", 
#            sep = ";", quote = FALSE, row.names = F)

timestamp()
for (i in seq_along(final$date_time)){
  j = final$date_time[i]
  
  final[i,"n_patient_im_in"] = 
    length(patients[patients$arrival_date_time <= j & patients$leave_date_time > j &
                      patients$Surgery_Type == "Internal_Medicine" &
                      patients$Section == "8000595","Patient_ID"])

  final[i,"n_patient_im_out"] = 
    length(patients[patients$arrival_date_time <= j & patients$leave_date_time > j &
                      patients$Surgery_Type == "Internal_Medicine" &
                      patients$Section == "other","Patient_ID"])

  final[i,"n_patient_other_in"] = 
    length(patients[patients$arrival_date_time <= j & patients$leave_date_time > j &
                      patients$Surgery_Type == "Others" &
                      patients$Section == "8000595","Patient_ID"])
  
  final[i,"n_patient_other_out"] = 
    length(patients[patients$arrival_date_time <= j & patients$leave_date_time > j &
                      patients$Surgery_Type == "Others" &
                      patients$Section == "other","Patient_ID"])
  print(i)
}

###################   controllo se i dati tornano con historical   ###################
plot(final$n_patient_im_in+final$n_patient_other_in, type="l")
abline(h = 19, col=2)

check = final %>% filter(hour(final$date_time)==00)
check$date = ymd(floor_date(check$date_time, unit = c("day")))
check$recovery = check$n_patient_im_in 
#+ check$n_patient_other_in
#+check$n_patient_other_in
check_2= merge(check,  historical_info, by.x = "date", by.y = "Date") 

#errore stima con historical
sqrt(sum((check_2$patients-check_2$recovery)^2)/dim(check_2)[1])
#sum(abs(check_2$recovery-check_2$patients))/dim(check_2)[1]

#plot(historical_info$patients, type="l")
plot(check$recovery, type="l", lwd = 1, lambda=0.9)
abline(h = 19, col=2)
abline(h = mean(check$recovery))
lines(historical_info$patients,col="blue",type="l")
abline(h = mean(historical_info$patients) ,col="blue")
legend(0, 27, legend=c("Estimate", "Historical"),
       col=c("black", "blue"), lty=1, cex=0.6, bty="n")

final$tot_recovery = final$n_patient_im_in + final$n_patient_other_in
final$tot_patient = final$n_patient_im_in + final$n_patient_other_in+
  final$n_patient_im_out + final$n_patient_other_out

hist(final$tot_patient)

######################################
#Conteggio stima pazienti oraria
######################################
#quanti letti ha bisogno medicina interna

score = data.frame(
  bed = numeric(),
  saturation = numeric(),
  unoccuppied = numeric(),
  productivity=numeric(),
  security = numeric(),
  accessibility = numeric())

i=1
for (b in 10:35){
  for (u in 1:10){
    for (s in 1:2){
      score[i, "saturation"] = s
      score[i, "unoccuppied"] = u
      score[i, "bed"] = b
      
      score$productivity[i] = sum(b - final$n_patient_im_in + final$n_patient_other_in > u)
      score$productivity[i] = sum(b - final$n_patient_im_in > u)
      
      #es. u=5 max 5 bed empty >> 19-18=1 >> 1>5 FALSE
      #es. u=5 max 5 bed empty >> 19-10=9 >> 9>5 TRUE
      
      score$security[i] = sum(final$n_patient_im_out)
      score$accessibility[i] = sum(b - final$n_patient_im_in < s)
      #es. s=1 at least 1 bed available >> 19-18=1 >> 1<1 FALSE
      #es. s=1 at least 1 bed available >> 19-19=1 >> 0<1 TRUE
      i = i+1
    }
  }
}

#standardize productivity
score$productivity


score3 = melt(score, id.vars = c("saturation", "unoccuppied", "bed"))


score4 =  score3 %>% group_by (saturation, unoccuppied, bed) %>%
  summarise(score_avg = mean(value),
            score_sd = sd(value))

for (u in 1:10){
  for (s in 1:2){ 
    scenario = score4 %>% filter (saturation==s, unoccuppied==u)
    plot(scenario$bed, scenario$score_avg, type = "b",
         main = paste ("unoccuppied=",u,"& saturation=",s),
         ylim = c( min(min(score4$score_avg),min(score4$score_sd)), 
                   max(max(score4$score_avg),max(score4$score_sd))
         ))
    lines(scenario$bed, scenario$score_sd, type = "b",col="blue")
  }}


#minimo standard deviation perchè




