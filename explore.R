######################################
#import data
######################################
historical_info <- read.csv("C:/Users/Giacomo Monti/Desktop/decision model/final projects/historical_info.csv", sep=";", stringsAsFactors=FALSE)
patients <- read.delim2("C:/Users/Giacomo Monti/Desktop/decision model/final projects/patients.csv", stringsAsFactors=FALSE)

######################################
#data cleaning
######################################
#format date time variables
historical_info$Date = dmy(historical_info$Date)

patients$arrival_date = dmy(patients$Arrival_Date)
patients$arrival_date_time = dmy_hms(paste(patients$Arrival_Date, patients$Arrival.Time))
patients$arrival_time = dmy_hms(paste("01 01 1991", patients$Arrival.Time))
patients$waiting_time_round = round(patients$POST_ANESTHESIA_CARE_UNIT_Time_min)
#patients$bed_date = patients$arrival_date
#patients[hour(patients$arrival_date_time)>12, "bed_date"] = patients[hour(patients$arrival_date_time)>12, "arrival_date"]+1

######################################
#data exploration
######################################

#serie storica ingressi nel reparto 
plot(historical_info$patients, type = "l")
#distribuzione ingressi reparto al giorno
hist(historical_info$patients)
summary(historical_info$patients)

#historical_info has 1 month more
max(historical_info$Date)
min(historical_info$Date)

max(patients$Arrival_Date)
min(patients$Arrival_Date)




#considero quelli arrivati dopo le 12 come arrivati il giorno dopo 
#ma non li conto se lunghezza permanenza=0 days
check = patients %>%
  filter(Length_of_Stay_day != 0 & Section=="8000595") %>%
  group_by(bed_date) %>%
  summarise( patient_number = n()) 
check_2= merge(check,  historical_info, by.x = "bed_date", by.y = "Date")
sum((check_2$patient_number-check_2$patients)^2)/dim(check_2)[1]
sum(abs(check_2$patient_number-check_2$patients))/dim(check_2)[1]

#considero quelli arrivati dopo le 12 come arrivati il giorno dopo 
check = patients %>% 
  filter(Section=="8000595") %>%
  group_by(bed_date) %>% summarise( patient_number = n()) 
check_2= merge(check,  historical_info, by.x = "bed_date", by.y = "Date")
sum((check_2$patient_number-check_2$patients)^2)/dim(check_2)[1]
sum(abs(check_2$patient_number-check_2$patients))/dim(check_2)[1]

#considero quelli arrivati dopo le 12 normalmente 
check = patients  %>% 
  filter(Section=="8000595") %>%
  group_by(arrival_date) %>% summarise( patient_number = n()) 
check_2= merge(check,  historical_info, by.x = "arrival_date", by.y = "Date")
sqrt(sum((check_2$patient_number-check_2$patients)^2)/dim(check_2)[1])
sum(abs(check_2$patient_number-check_2$patients))/dim(check_2)[1]



# che cazzo sono other other
table(patients$Section, patients$Surgery_Type)

barplot(prop.table(table(hour(patients$arrival_time))),main = "arrival_time")
barplot(prop.table(table(round(patients$Surgery_Time_min,0))),
        main = "Surgery_Time_min")
barplot(prop.table(table(round(patients$POST_ANESTHESIA_CARE_UNIT_Time_min,0))),
        main = "POST_ANESTHESIA_CARE_UNIT_Time_min")
mean(patients$POST_ANESTHESIA_CARE_UNIT_Time_min)
barplot(prop.table(table(round(patients$Length_of_Stay_day,0))),
        main = "Length_of_Stay_day")

#simulate POST_ANESTHESIA_CARE_UNIT_Time_min
rexp(20, rate = 1/mean(patients$POST_ANESTHESIA_CARE_UNIT_Time_min))



#leave_date
rdu<-function(n, min, max){sample(min:max,n,replace=T)}


patients$leave_date_time = patients$arrival_date_time+ #ora ingresso in recovery
                           minutes((patients$Length_of_Stay_day*24+rdu(1, 1, 24))*60) #stima los in minuti

hour_series = seq(ymd_hms('2015-01-01 00:00:00'),ymd_hms('2015-10-31 23:00:00'), by = 'hour')  
final_2 = data.frame(date_time = hour_series)


for (i in seq_along(final$date_time)){
  j = final$date_time[i]
  final[i,"n_patient_im_in"] = patients %>%
                     filter(arrival_date_time < j & leave_date_time > j &
                              Surgery_Type == "Internal_Medicine" &
                              Section == "8000595") %>%
                     summarise(count = n())

  final[i,"n_patient_im_out"] = patients %>%
    filter(arrival_date_time < j & leave_date_time > j & 
             Surgery_Type == "Internal_Medicine" &
             Section == "other") %>%
    summarise(count = n())
  
  final[i,"n_patient_other_in"] = patients %>%
    filter(arrival_date_time < j & leave_date_time > j & 
             Surgery_Type == "Others" &
             Section == "8000595") %>%
    summarise(count = n())
  
  
  final[i,"n_patient_other_out"] = patients %>%
    filter(arrival_date_time < j & leave_date_time > j & 
             Surgery_Type == "Others" &
             Section == "other") %>%
    summarise(count = n())
  
  print(i)
}

plot(final$n_patient_im_in+final$n_patient_other_in, type="l")
abline(h = 19, col=2)
