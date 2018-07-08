########################
##   simulate_arrival
########################

simulate_arrival=function(start = "2018-01-01 00:05:00", n_days=45,
                          lambda_interarrivi, lambda_surgery, lambda_los, seed){
  simulation=data.frame(
    patient_id = integer(),
    arrival_date_time = integer(),
    surgery_time = numeric(),
    start_waiting = integer(),
    los = numeric()
  )
  
  class(simulation$arrival_date_time) <- "POSIXct"
  class(simulation$start_waiting) <- "POSIXct"
  
  #####
  start = ymd_hms(start)
  arrival = ymd_hms(start)
  i=1
  set.seed(seed)
  while (arrival < start+days(n_days)) {
    
    #stime 
    
    rexp_interarrival = round(rexp(1,lambda_interarrivi),0)
    rexp_surgery = max(50, round(rexp(1,lambda_surgery),0))
    rexp_los = round(rexp(1,lambda_los),0)
    
    #simulate series
    simulation[i,"patient_id"] = i
    simulation[i,"arrival_date_time"] = arrival + minutes(rexp_interarrival)
    simulation[i, "surgery_time"] = rexp_surgery
    simulation[i, "start_waiting"] = arrival + minutes(rexp_interarrival + rexp_surgery)
    simulation[i, "los"] = rexp_los
    
    arrival = arrival+minutes(rexp_interarrival)
    #print(rexp_interarrival)
    i=i+1
  }
  return(simulation) }




########################
##   simulate_recovery
########################

simulate_recovery = function(
  data, 
  n_letti,
  max_attesa ,  
  each #minuti
){
  
  #ordino la coda
  queue_ordered = data[order(data$start_waiting),]
  simulation_full = data
  simulation_full$transfer = FALSE
  
  #inizializzo df
  recovery = data.frame(
    patient_id = integer(),
    start_waiting = integer(),
    admission = integer(),
    leave = integer()
  )
  
  class(recovery$start_waiting) <- "POSIXct"
  class(recovery$admission) <- "POSIXct"
  class(recovery$leave) <- "POSIXct"
  
  #inizio ciclo da quando inizia a essere pronto ad entrare in recovery
  date = min(data$start_waiting)
  
  while (date < max(data$start_waiting)+days(7)){
    
    #quanti letti si liberano dalla recovery se non si libera nessuno passo ad ora dopo
    free_bed = n_letti - (dim(recovery)[1]-sum(recovery$leave < date))
    
    #------------------ 
    if (free_bed != 0) {
      
      #segno in simulation full dati uscita recovery
      patient_out = recovery[recovery$leave < date,c("patient_id","admission","leave")]
      simulation_full[simulation_full$patient_id %in% patient_out$patient_id,"admission"] = patient_out$admission
      simulation_full[simulation_full$patient_id %in% patient_out$patient_id,"leave"] = patient_out$leave
      
      #tolgo dalla recovery quelli usciti
      recovery = recovery[!recovery$patient_id %in% patient_out$patient_id,]
      
      #prendo i primi della queue pronti (se ci sono) e li metto in recovery
      ready = queue_ordered[queue_ordered$start_waiting<=date,]
      
      #-------------
      if (dim(ready)[1]!=0){
        
        free_bed = if( dim(ready)[1] < free_bed){dim(ready)[1]}else{free_bed} 
        
        recovery= rbind(
          recovery,
          
          data.frame(
            patient_id = ready[seq(1 :  free_bed),"patient_id"],
            start_waiting = ready[seq(1 :  free_bed),"start_waiting"],
            admission = rep(date, free_bed),
            leave = date + minutes(ready[seq(1 :  free_bed),"los"])
          ))
        
        #rimuovo dalla coda quelli che possono entrare
        queue_ordered = queue_ordered[- seq(1 :  free_bed),]
      }
      #-------------  
      
    }
    
    
    #sistemata la recovery controllo se ce qualcuno che aspetta da troppo
    ready2 = queue_ordered[queue_ordered$start_waiting<=date,]
    attesa = difftime(date , ready2$start_waiting,  units = "mins")
    
    
    #-------------  
    if(any(attesa>=max_attesa)){
      #print(attesa)
      trasferimenti = ready2[attesa>=max_attesa,]
      #sistemare se ci sono più di 1 paziente per match giusto
      simulation_full[simulation_full$patient_id %in% trasferimenti$patient_id,"admission"] = date
      simulation_full[simulation_full$patient_id %in% trasferimenti$patient_id,"leave"] =
        date + minutes(trasferimenti$los)
      simulation_full[simulation_full$patient_id %in% trasferimenti$patient_id,"transfer"] = TRUE
      
      queue_ordered =  queue_ordered[!queue_ordered$patient_id %in% trasferimenti$patient_id,]
    }
    #-------------  
    
    date = date + minutes(each)
  } #....fine while
  
  return(simulation_full)}#---fine funzione