# M E J O R
outcome <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available") 

MEJOR <- function(est,res){
        
        # Revisi�n de la validez de estado y resultado 
        
        if (!((res == "ataque") | (res == "falla")
              | (res == "neumonia"))) {
                stop ("Dato inv�lido")
                break
        }
        
        newout<- if (res == "ataque") {
                outcome[c(2,7,11)] 
        } else if (res == "falla") {
                outcome[c(2,7,17)] 
        } else {
                outcome[c(2,7,23)] 
        }    
        
        
        estad <- as.character(outcome$State)
        if(!(est %in% estad)){
                stop("El nombre del estado es inv�lido")
                break
        }
        
        # Regresa el nombre del hospital con la tasa m�s baja de mortalidad de 30 d�as 
        
        
        ext <- subset(newout, newout$State==est)
        minmor <- min(ext[,3], na.rm = TRUE)
        
        ext2 <- subset(ext,ext[,3]==minmor)
        hospital <- ext2$Hospital.Name
        hospital
        
        
}


# R A N K H O S P I T A L 
# Lectura de datos

outcome <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available") 
RANKHOSPITAL <- function(est,res,num="mejor"){
        
        # Revisi�n de la validez de estado y resultado 
        
        if (!((res == "ataque") | (res == "falla")
              | (res == "neumonia"))) {
                stop ("Dato inv�lido")
                break
        }
        newout<- if (res == "ataque") {
                outcome[c(2,7,11)] 
        } else if (res == "falla") {
                outcome[c(2,7,17)] 
        } else {
                outcome[c(2,7,23)] 
        }      
        
        estad <- as.character(outcome$State)
        if(!(est %in% estad)){
                stop("El nombre del estado es inv�lido")
                break
        }
        
        #Regresa el nombre del hospital con el puesto dado de la tasa m�s  
        #baja de mortalidad de 30 d�as 
        ext <- subset(newout, newout$State==est)
        
        lista<-ext[order(as.numeric(ext[[3]]),ext[[1]]),]
        
        if (num=="peor"){
                ext <- subset(newout, newout$State==est)
                peor <- max(ext[,3], na.rm = TRUE)
                ext2 <- subset(ext,ext[,3]==peor)
                hospital <- ext2$Hospital.Name
                hospital
        }else if(num=="mejor"){
                ext <- subset(newout, newout$State==est)
                mej <- min(ext[,3], na.rm = TRUE)
                ext2 <- subset(ext,ext[,3]==mej)
                hospital <- ext2$Hospital.Name
                hospital
        }else if(num > nrow(ext)){
                stop(return(NA))
        }else {
                hospital<-lista[num,1]
                hospital  
        }
        
        
}



# R A N K I N G C O M P L E T O
# Lectura de datos 
outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character") 
RANKINGCOMPLETO <- function(res, num="mejor"){
        
        # Revisi�n de la validez de estado y resultado 
        
        if (!((res == "ataque") | (res == "falla")
              | (res == "neumonia"))) {
                stop ("Dato inv�lido")
                break
        }
        
        newout<- if (res == "ataque") {
                outcome[c(2,7,11)] 
        } else if (res == "falla") {
                outcome[c(2,7,17)] 
        } else {
                outcome[c(2,7,23)] 
        }      
        
        # Para cada estado, encuentra el hospital con la posici�n dada. 
        eds <- sort(unique(outcome$State))
        pdf <- vector("character")
        edos <- vector("character")
        
        sts <- levels(eds)
        
        
        for (estado in eds){
                ext <- subset(newout, newout$State==estado  & !newout[[3]]=="Not Available")
                
                lista<-ext[order(as.numeric(ext[[3]]),ext[[1]]),]
                
                if (num=="peor"){
                        
                        peor<-lista[which.max(lista[[3]]),]
                        hospital <- peor$Hospital.Name
                        pdf <- c(pdf,hospital)
                }else if(num=="mejor"){
                        
                        mej<-lista[which.min(lista[[3]]),]
                        hospital <- mej$Hospital.Name
                        pdf <- c(pdf,hospital)
                        
                }else {
                        hospital<-lista[num,1]
                        pdf <- c(pdf,hospital)  
                }
                edos <- c(edos, eds)
        }
        
        # Regresa un data frame con el nombre del hospital y la abreviatura 
        # del nombre del estado al que pertenece. 
        
        df <- data.frame(pdf,edos)
        colnames(df) <- c("hospital", "state")
        rownames(df)<- sts
        df
}