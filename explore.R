library("lubridate", lib.loc="C:/Program Files/R/R-3.3.1/library")
library("dplyr", lib.loc="C:/Program Files/R/R-3.3.1/library")
library("reshape2", lib.loc="C:/Program Files/R/R-3.3.1/library")
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


######################################
#Conteggio stima pazienti oraria
######################################
rdu<-function(n, min, max){sample(min:max,n,replace=T)}

set.seed(1234)
patients$leave_date_time = patients$arrival_date_time+ #ora ingresso in recovery
                           minutes((patients$Length_of_Stay_day*24+
                                      rdu(1,1,8))*60) #stima los in minuti

hour_series = seq(ymd_hms('2015-01-02 00:00:00'),ymd_hms('2015-02-28 23:00:00'), by = 'hour')  
final = data.frame(date_time = hour_series)


for (i in seq_along(final$date_time)){
  j = final$date_time[i]
  final[i,"n_patient_im_in"] = patients %>%
    filter(arrival_date_time <= j & leave_date_time > j &
             Surgery_Type == "Internal_Medicine" &
                              Section == "8000595") %>%
    summarise(count = n())

  final[i,"n_patient_im_out"] = patients %>%
    filter(arrival_date_time <= j & leave_date_time > j & 
             Surgery_Type == "Internal_Medicine" &
             Section == "other") %>%
    summarise(count = n())
  
  final[i,"n_patient_other_in"] = patients %>%
    filter(arrival_date_time <= j & leave_date_time > j & 
             Surgery_Type == "Others" &
             Section == "8000595") %>%
    summarise(count = n())
  
  
  final[i,"n_patient_other_out"] = patients %>%
    filter(arrival_date_time <= j & leave_date_time > j & 
             Surgery_Type == "Others" &
             Section == "other") %>%
    summarise(count = n())
  
  print(i)
}

#check
plot(final$n_patient_im_in+final$n_patient_other_in, type="l")
abline(h = 19, col=2)

check = final %>% filter(hour(final$date_time)==00)
check$date = ymd(floor_date(check$date_time, unit = c("day")))
check$recovery = check$n_patient_im_in+check$n_patient_other_in
check_2= merge(check,  historical_info, by.x = "date", by.y = "Date") 

sqrt(sum((check_2$patients-check_2$recovery)^2)/dim(check_2)[1])
#sum(abs(check_2$recovery-check_2$patients))/dim(check_2)[1]

plot(check$recovery, type="l", lwd = 2, lambda=0.9)
lines(historical_info$patients,col="blue")


######################################
#calcolo score
######################################

score = data.frame(
  bed = numeric(),
  saturation = numeric(),
  unoccuppied = numeric(),
  productivity=numeric(),
  security = numeric(),
  accessibility = numeric())

i=1
for (b in 1:30){
  for (u in 1:10){
    for (s in 1:2){
      score[i, "saturation"] = s
      score[i, "unoccuppied"] = u
      score[i, "bed"] = b
      
      score$productivity[i] = sum(final$n_patient_im_in + final$n_patient_other_in < b-u)
      score$security[i] = sum(final$n_patient_im_out)
      score$accessibility[i] = sum(b - final$n_patient_im_in + final$n_patient_other_in < s)
      i = i+1
    }
  }
}


score2 =  score %>% group_by (saturation, unoccuppied, bed) %>%
  summarise(productivity = sum(productivity),
            security = sum(security),
            accessibility = sum(accessibility))

score3 = melt(score2, id.vars = c("bed"))


score4 =  score3 %>% group_by (bed) %>%
  summarise(score_avg = mean(value),
            score_sd = sd(value))



plot(score4$bed, score4$score_sd, type = "b",col="blue",
      ylim = c(400, 1000))
lines(score4$bed, score4$score_avg, type = "b")


