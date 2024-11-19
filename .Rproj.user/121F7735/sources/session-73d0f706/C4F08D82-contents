

################# CultivNet ###############
###########################################

### BIOINFOMÁTICA ###

# Banda Zamora Jimena
# Muñoz Navarrtete Daniela Michelle
# Vazquez Ricardez Xymena

### escribir las indicaciones y descripcion del programa

base <- read.csv("datos/base.csv")
View (base)


## Aquí comienza la función


cultivnet <- function () {
  micro <- (readline (prompt = "Muy bien, comenzaremos por preguntar qué tipo de
                          microorganismo te gustaria observar o encontrar, tenemos datos 
                          disponibles para bacterias y hongos, escribe cuál de los te interesa: "))
  if ( (micro == "bacterias"| micro == "bacteria" | micro == "Bacteria" | micro == "Bacterias" )) {
    bacteria ()
  } else if ( (micro == "hongos"| micro == "hongos" | micro == "Hongos" | micro == "Hongo" )) {
    hongo ()
  } else {
    print ("Lo sentimos, esa entrada no es válida para este programa, estamos actualizando los
           datos para tener una amplia diversidad de microorganismos dentro, gracias!!")
  }
  
}

cultivnet()



bacteria <- function () {
  bac <- readline (prompt = "ahora, ya sabes en sí qué genero de bacterias quieres observar?,
                   recuerda que te daremos la información a nivel de género.
                   Por favor introduce el género, si no tienes alguno en específico solo escribe no : " )
  if ((bac == "no" | bac=="No")) {
    diversidadb ()
  } else {
    generob ()
  }
  
}



hongo <- function () {
  hon <- readline (prompt = "ahora, ya sabes en sí qué género de hongos quieres observar?,
                   recuerda que te daremos la información a nivel de género.
                   Por favor introduce el género, si no tienes alguno en específico solo escribe no : " )
  if ((hon == "no" | hon== "no")) {
    diversidadh ()
  }
  else {
    generoh ()
  }
}

diversidadb <- function(){ 
  muestra <- readline (prompt = "entonces, lo que necesitas es conocer la diversidad dentro de una muestra,
                       pero dinos, de qué tipo es tu muestra?, tenemos datos para muestra en formato liquido, frotis
                       o de suelo: ")
  if (( muestra == "liquido" | muestra == "líquido" | muestra == "Líquido" | muestra== "Liquido")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [37, ])
    foto (26)
  } else if ((muestra == "frotis" | muestra == "Frotis")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [38, ])
    foto (26)
  } else if ((muestra == "suelo" | muestra == "Suelo")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [39, ])
    foto (26)
  }  
}


diversidadh <- function(){ 
  muestra <- readline (prompt = "entonces, lo que necesitas es conocer la diversidad dentro de una muestra,
                       pero dinos, de qué tipo es tu muestra?, tenemos datos para muestra en formato liquido, alimentos
                       o de suelo: ")
  if (( muestra == "liquido" | muestra == "líquido" | muestra == "Líquido" | muestra== "Liquido")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [90, ])
    foto (52)
  } else if ((muestra == "aliemento" | muestra == "alimentos" | muestra == "Alimento" | muestra == "Alimentos")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [92, ])
    foto (52)
  } else if ((muestra == "suelo" | muestra == "Suelo")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [91, ])
    foto (52)
  }  
}



generob <- function () {
  
}
  

