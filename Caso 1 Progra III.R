
#Parte1
directorio<- setwd("C:/Users/lesla/Desktop/specdata")
mediacontaminante <- function(directorio,contaminante, id=1:332){
        x <- id
        y<- (as.character(substitute(contaminante)))
        nobs<- numeric()
        datos<-numeric()
        for (j in x){
                data <- read.csv(sprintf("%03d.csv", j))
                nobs <- c(nobs,sum(complete.cases(data)))
                datos<- c(datos,sum(data[[y]], na.rm=TRUE))
        }
        dataframe<- data.frame(id, nobs)
        dataframe2<- data.frame(id,datos)
        suma<- c(sum(dataframe$nobs))
        sumadatos<- c(sum(dataframe2$datos))
        media<-(sumadatos/suma)
        print(media)
}

#Parte 2

completos <- function(directorio, id = 1:332) {
        nobs <- numeric()
        for(j in id) {
                casoscom<- read.csv (sprintf("%03d.csv", j)) 
                nobs <-c(nobs, sum(complete.cases(casoscom))) 
                
        }
        dataframe<- data.frame(id, nobs)
        print(dataframe)
}

#Parte 3
        source("completos.R")
        correlacion<-(numeric)
        
        
        framecompletos <- completos(directorio, 1:332)
        
        idcom <- framecompletos[framecompletos["nobs"] >= horizonte,]$id 
        
        
        
        for(j in idcom) {
                data <- read.csv(sprintf("%03d.csv",s))
                completos<- data[complete.cases(data), ] 
                correlacion <- c(correlacion, cor(completos$sulfate, completos$nitrate)) 
                
        }
        return(funcion)
}

