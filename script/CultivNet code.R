

################# CultivNet ###############
###########################################

### BIOINFOMÁTICA ###

# Banda Zamora Jimena
# Muñoz Navarrtete Daniela Michelle
# Vazquez Ricardez Xymena

######## BIENVENIDO A CULTIVNET #########

#Hemos desarrollado este programa para ayudarte en tu transcurso como Microbiólogo(a), 
#a continuación, te dejamos algunas indicaciones que tienes que tener en cuenta
#antes de ingresar ...

# 1. Debes ingresar el microorganismo del que te gustaría obtener información 
#    (actualmente nuestra base solo contiene hongos y bacterias).
# 2. Si ya sabes con que microrganismo trabajarás puedes ingresarlo como entrada o simplemente responder que no.
# 3. Por ultimo debes ingresar tu tipo de muestra y a partir de ahí se desplegará una lista de los posibles hongo o bacteria que puedan 
#    estar en tu muestra o te arrogará información específica del organismo con el que anteriormente mencionaste 
#    que trabajarías. 




## Leer la base de datos
base <- read.csv("datos/base.csv")
View (base)


## Correr las funciones necesarias para ejecutar el programa

# función que corresponde a la visualización de las fotos
install.packages("jpeg")
install.packages("grid")

library(jpeg)
library(grid)

foto <- function (a) {
  imagen <- readJPEG(paste0("datos/fotos microorganismos/",a,".jpeg"))
  grid.raster(imagen)
}

foto(9)

# función para trabajar exclusivamente con bacterias

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

# función para trabajar exclusivamente con hongos

hongo <- function () {
  hon <- readline (prompt = "ahora, ya sabes en sí qué género de hongos quieres observar?,
                   recuerda que te daremos la información a nivel de género.
                   Por favor introduce el género, si no tienes alguno en específico solo escribe no : " )
  if ((hon == "no" | hon== "No")) {
    diversidadh ()
  }
  else {
    generoh ()
  }
}

# función para trabajar con la diversidad en bacterias

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
    print ( as.list (base [39, ]))
    foto (26)
  }  
}

# función para trabajar con la diversidad en bacterias

diversidadh <- function(){ 
  muestra <- readline (prompt = "entonces, lo que necesitas es conocer la diversidad dentro de una muestra,
                       pero dinos, de qué tipo es tu muestra?, tenemos datos para muestra en formato liquido, alimentos
                       o de suelo: ")
  if (( muestra == "liquido" | muestra == "líquido" | muestra == "Líquido" | muestra== "Liquido")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [90, ])
    foto (52)
  } else if ((muestra == "alimento" | muestra == "alimentos" | muestra == "Alimento" | muestra == "Alimentos")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [92, ])
    foto (52)
  } else if ((muestra == "suelo" | muestra == "Suelo")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [91, ])
    foto (52)
  }  
}

#función generob para conocer las necesidades del usiario y proporcionar la infomación de las bacterias

generob <- function(){
  tipo_de_muestra <- readline (prompt = "Tienes algún tipo de muestra que te gustaría evaluar? Tenemos disponibilidad de 
                               muestras de frotis, líquido, raices y suelo. Por favor escribe el tipo de muestra (si no tienes un
                               tipo de muestra en específico escribe la palabra no): ")
  
  genero <- readline (prompt = "Para buscar especificamente por genero, escribe el nombre de aquel de tu interés, 
                       actualmente poseemos información de 25 géneros de bacterias:")
  
  observacion <- readline (prompt = "en cuanto a qué es lo que quieres observar, te gustaria ver la presencia o aislar exclusivamente
                           a este microorganismo? (si no lo tienes definifo aún, escribe no)" )
  
  if (( (genero == "escherichia" | genero == "Escherichia") & (tipo_de_muestra == "no" | tipo_de_muestra == "No") & (observacion == "no" | observacion == "No"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [1, ])
    print ( base [2, ])
    foto (1)
  } else if(( (genero == "escherichia" | genero == "Escherichia") & (tipo_de_muestra == "frotis" | tipo_de_muestra == "Frotis") & (observacion == "presencia" | observacion == "Presencia"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [1, ])
    foto (1)
  } else if(( (genero == "escherichia" | genero == "Escherichia") & (tipo_de_muestra == "líquido" | tipo_de_muestra == "Líquido") & (observacion == "presencia" |observacion == "Presencia") )){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [2, ])
    foto (1)
  } else if (((genero == "salmonella" | genero == "Salmonella") & (tipo_de_muestra == "no" | tipo_de_muestra == "No") & (observacion == "no" | observacion == "No"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [3, ])
    print ( base [4, ])
    print ( base [5, ])
    print ( base [6, ])
    foto (2)
  } else if (((genero == "salmonella" | genero == "Salmonella") & (tipo_de_muestra == "frotis" | tipo_de_muestra == "Frotis") & (observacion == "no" | observacion == "No"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [3, ])
    print ( base [4, ])
    foto (2)
  } else if (((genero == "salmonella" | genero == "Salmonella") & (observacion == "aislar" | observacion == "Aislar") & (tipo_de_muestra == "frotis" | tipo_de_muestra == "Frotis"))){
    print ( base [3, ])
    foto (2)
  } else if (((genero == "salmonella" | genero == "Salmonella") & (observacion == "presencia" | observacion == "Presencia") & (tipo_de_muestra == "frotis" | tipo_de_muestra == "Frotis"))){
    print ( base [5, ])
    foto (2)
  } else if (((genero == "salmonella" | genero == "Salmonella") & (tipo_de_muestra == "líquido" | tipo_de_muestra == "Líquido") & (observacion == "No" | observacion == "no"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [4, ])
    print ( base [6, ])
    foto (2)
  } else if (((genero == "salmonella" | genero == "Salmonella") & (observacion == "aislar" | observacion == "aislar") & (tipo_de_muestra == "líquido" | tipo_de_muestra == "Líquido"))){
    print ( base [4, ])
    foto (2)
  } else if (((genero == "salmonella" | genero == "Salmonella") & (observacion == "presencia" | observacion == "Prescencia") & (tipo_de_muestra == "líquido" | tipo_de_muestra == "Líquido"))){
    print ( base [6, ])
    foto (2)
  } else if ((genero == "clostridium" | genero == "Clostridium")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [7, ])
    foto (3)
  } else if ((genero == "pseudomonas" | genero == "Pseudomonas" & tipo_de_muestra == "no" | tipo_de_muestra == "No" & obsevacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [8, ])
    print ( base [9, ])
    foto (4)
  } else if ((genero == "pseudomonas" | genero == "Pseudomonas" & observacion == "presencia" | observacion == "Presencia" & tipo_de_muestra == "Líquido" | tipo_de_muestra == "líquido")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [8, ])
    foto (4)
  } else if ((genero == "pseudomonas" | genero == "Pseudomonas" & observacion == "aislar" | observacion == "Aislar" & tipo_de_muestra == "Líquido" | tipo_de_muestra == "líquido")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [9, ])
    foto (4)
  } else if ((genero == "staphylococcus" | genero == "Staphylococcus")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [10, ])
    foto (5)
  } else if ((genero == "proteus" | genero == "Proteus" & tipo_de_muestra == "no" | tipo_de_muestra == "No" & observacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [11, ])
    print ( base [12, ])
    foto (6)
  } else if ((genero == "proteus" | genero == "Proteus" & tipo_de_muestra == "frotis" | tipo_de_muestra == "Frotis" & observacion == "Aislar" | observacion == "aislar" )){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [11, ])
    foto (6)
  } else if ((genero == "proteus" | genero == "Proteus" & tipo_de_muestra == "líquido" | tipo_de_muestra == "Líquido" & observacion == "Aislar" | observacion == "aislar" )){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [12, ])
    foto (6)
  } else if ((genero == "bacillus" | genero == "Bacillus")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [13, ])
    foto (7)
  } else if ((genero == "shigella" | genero == "Shigella")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [14, ])
    foto (8)
  } else if ((genero == "klebsiella" | genero == "Klebsiella")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [15, ])
    foto (9)
  } else if ((genero == "streptococcus" | genero == "Streptococcus")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [16, ])
    foto (10)
  } else if ((genero == "bacteroides" | genero == "Bacteroides")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [17, ])
    foto (11)
  } else if ((genero == "listeria" | genero == "Listeria")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [18, ])
    foto (12)
  } else if ((genero == "vibrio" | genero == "Vibrio")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [19, ])
    foto (13)
  } else if ((genero == "enterococcus" | genero == "Enterococcus" & tipo_de_muestra == "no" | tipo_de_muestra == "No" & observacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [20, ])
    print ( base [21, ])
    foto (14)
  } else if ((genero == "enterococcus" | genero == "Enterococcus" & tipo_de_muestra == "líquido" | tipo_de_muestra == "Líquido" & observacion == "aislar" | observacion== "Aislar")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [20, ])
    foto (14)
  } else if ((genero == "enterococcus" | genero == "Enterococcus" & tipo_de_muestra == "frotis" | tipo_de_muestra == "Frotis" & observacion == "aislar" | observacion== "Aislar")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [21, ])
    foto (14)
  } else if ((genero == "rhizobium" | genero == "Rhizobium" & tipo_de_muestra == "no" | tipo_de_muestra == "No" & observacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [22, ])
    print ( base [23, ])
    foto (15)
  } else if ((genero == "rhizobium" | genero == "Rhizobium" & tipo_de_muestra == "raices" | tipo_de_muestra == "Raices" & observacion == "aislar" | observacion== "Aislar")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [22, ])
    foto (15)
  } else if ((genero == "rhizobium" | genero == "Rhizobium" & tipo_de_muestra == "suelo" | tipo_de_muestra == "Suelo" & observacion == "aislar" | observacion== "Aislar")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [23, ])
    foto (15)
  } else if ((genero == "acinetobacter" | genero == "Acinetobacter" & tipo_de_muestra == "no" | tipo_de_muestra == "No" & observacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [24, ])
    print ( base [25, ])
    foto (16)
  } else if ((genero == "acinetobacter" | genero == "Acinetobacter" & tipo_de_muestra == "frotis" | tipo_de_muestra == "Frotis" & observacion == "aislar" | observacion== "Aislar")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [24, ])
    foto (16)
  } else if ((genero == "acinetobacter" | genero == "Acinetobacter" & tipo_de_muestra == "líquido" | tipo_de_muestra == "Líquido" & observacion == "presencia" | observacion== "Presencia")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [25, ])
    foto (16)
  } else if ((genero == "bifidobacterium" | genero == "Bifidobacterium")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [26, ])
    foto (17)
  } else if ((genero == "xanthomonas" | genero == "Xanthomonas")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [27, ])
    foto (18)
  } else if ((genero == "serratia" | genero == "Serratia" & tipo_de_muestra == "no" | tipo_de_muestra == "No" & observacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [28, ])
    print ( base [29, ])
    foto (19)
  } else if ((genero == "serratia" | genero == "Serratia" & tipo_de_muestra == "líquido" | tipo_de_muestra == "Líquido" & observacion == "aislar" | observacion== "Aislar")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [28, ])
    foto (19)
  } else if ((genero == "serratia" | genero == "Serratia" & tipo_de_muestra == "frotis" | tipo_de_muestra == "Frotis" & observacion == "aislar" | observacion== "Aislar")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [29, ])
    foto (19)
  } else if ((genero == "lactobacillus" | genero == "Lactobacillus" & tipo_de_muestra == "no" | tipo_de_muestra == "No" & observacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [30, ])
    print ( base [31, ])
    foto (20)
  } else if ((genero == "lactobacillus" | genero == "Lactobacillus" & tipo_de_muestra == "líquido" | tipo_de_muestra == "Líquido" & observacion == "aislar" | observacion== "Aislar")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [30, ])
    foto (20)
  } else if ((genero == "lactobacillus" | genero == "Lactobacillus" & tipo_de_muestra == "frotis" | tipo_de_muestra == "Frotis" & observacion == "aislar" | observacion== "Aislar")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [31, ])
    foto (20)
  } else if ((genero == "azotobacter" | genero == "Azotobacter")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [32, ])
    foto (21)
  } else if ((genero == "leuconostoc" | genero == "Leuconostoc")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [33, ])
    foto (22)
  } else if ((genero == "planococcus" | genero == "Planococcus")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [34, ])
    foto (23)
  } else if ((genero == "zymomonas" | genero == "Zymomonas")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [35, ])
    foto (24)
  } else if ((genero == "rhodococcus" | genero == "Rhodococcus")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [36, ])
    foto (25)
}else{
  print("Lo siento, por el momento no poseemos información sobre eso, 
        espera nuestras próximas actualizaciones")
}

}

#función generoh para conocer las necesidades del usiario y proporcionar la infomación de los hongos

generoh <- function(){ 
  muestra <- readline (prompt = "Para buscar especificamente por género, escribe el nombre de aquel de tu interés, 
                       actualmente poseemos información de 25 géneros de hongos y levaduras:")
  if (( genero == "aspergillus" | genero == "Aspergillus")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [40, ])
    print ( base [41, ])
    print ( base [42, ])
    foto (27)
  } else if(( genero == "aspergillus" | genero == "Aspergillus" & tipo_de_muestra == "suelo" | tipo_de_muestra == "Suelo" & tipo_de_muestra == "no" | tipo_de_muestra == "No" & obsevacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [40, ])
    print ( base [41, ])
    foto (27)
  } else if(((genero == "aspergillus" | genero == "Aspergillus") & (tipo_de_muestra == "suelo" | tipo_de_muestra == "Suelo") & (observacion == "esporulacion" | obervacion == "Esporulacion"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [40, ])
    foto (27)
  } else if(((genero == "aspergillus" | genero == "Aspergillus") & (tipo_de_muestra == "suelo" | tipo_de_muestra == "Suelo") & (observacion == "presencia" | tipo_de_muestra == "Presencia"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [41, ])
    foto (27)
  } else if(( genero == "aspergillus" | genero == "Aspergillus" & tipo_de_muestra == "líquido" | tipo_de_muestra == "Líquido")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [42, ])
    foto (27)
  } else if ((genero == "Rhizopus" | genero == "Rhizopus"& tipo_de_muestra == "no" | tipo_de_muestra == "No" & obsevacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [43, ])
    print ( base [44, ])
    foto (28)
  } else if (((genero == "Rhizopus" | genero == "Rhizopus") & (tipo_de_muestra == "alimentos" | tipo_de_muestra == "Alimentos") & (observacion == "esporulacion" | observacion == "Esporulacion"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [43, ])
    foto (28)
  } else if (((genero == "Rhizopus" | genero == "Rhizopus") & (tipo_de_muestra == "alimentos" | tipo_de_muestra == "Alimentos") & (observacion == "presencia" | observacion == "Presencia"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [44, ])
    foto (28)
  } else if ((genero == "sacharomyces" | genero == "Sacharomyces" & tipo_de_muestra == "no" | tipo_de_muestra == "No" & obsevacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de info(rmación que te puede ser de utilidad")
    print ( base [45, ])
    print ( base [46, ])
    print ( base [47, ])
    foto (29)
  } else if (((genero == "sacharomyces" | genero == "Sacharomyces") & (observacion == "presencia" | observacion == "Presencia") & (tipo_de_muestra == "líquido" | tipo_de_muestra == "Líquido"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [45, ])
    foto (29)
  } else if ((genero == "sacharomyces" | genero == "Sacharomyces" & tipo_de_muestra == "alimentos" | tipo_de_muestra == "Alimentos" & obsevacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [46, ])
    print ( base [47, ])
    foto (29)
  } else if (((genero == "sacharomyces" | genero == "Sacharomyces") & (tipo_de_muestra == "alimentos" | tipo_de_muestra == "Alimentos") & (observacion == "esporulacion" | observacion == "Esporulacion"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [46, ])
    foto (29)
  } else if (((genero == "sacharomyces" | genero == "Sacharomyces") & (tipo_de_muestra == "alimentos" | tipo_de_muestra == "Alimentos") & (observacion == "presencia" | obervacion == "Presencia"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [47, ])
    foto (29)
  } else if ((genero == "cryptococcus" | genero == "Cryptococcus")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [48, ])
    foto (30)
  } else if ((genero == "ustilago" | genero == "Ustilago" & tipo_de_muestra == "alimentos" | tipo_de_muestra == "Alimentos" & obsevacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [49, ])
    print ( base [50, ])
    foto (31)
  } else if (((genero == "ustilago" | genero == "Ustilago") & (tipo_de_muestra == "suelos" | tipo_de_muestra == "Suelos") & (observacion == "esporulacion" | observacion == "Esporulacion"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [49, ])
    foto (31)
  } else if (((genero == "ustilago" | genero == "Ustilago") & (tipo_de_muestra == "granos" | tipo_de_muestra == "Granos") & (observacion == "presencia" | observacion == "Presencia"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [50, ])
    foto (31)
  } else if ((genero == "fusarium" | genero == "Fusarium" & tipo_de_muestra == "no" | tipo_de_muestra == "No" & obsevacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [51, ])
    print ( base [52, ])
    foto (32)
  } else if (((genero == "fusarium" | genero == "Fusarium") & (tipo_de_muestra == "suelo" | tipo_de_muestra == "Suelo") & (observacion == "presencia" | observacion == "Presencia"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [51, ])
    foto (32)
  } else if (((genero == "fusarium" | genero == "Fusarium") & (tipo_de_muestra == "líquido" | tipo_de_muestra == "Líquido") & (observacion == "presencia" | observacion == "Presencia"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [52, ])
    foto (32)
  } else if ((genero == "candida" | genero == "Candida" & tipo_de_muestra == "no" | tipo_de_muestra == "No" & obsevacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [53, ])
    print ( base [54, ])
    foto (33)
  } else if (((genero == "candida" | genero == "Candida") & (tipo_de_muestra == "líquido" | tipo_de_muestra == "Líquido") & (observacion == "esporulacion" | observacion == "Esporulacion"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [53, ])
    foto (33)
  } else if (((genero == "candida" | genero == "Candida") & (tipo_de_muestra == "líquido" | tipo_de_muestra == "Líquido") & (observacion == "presencia" | observacion == "Presencia"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [54, ])
    foto (33)
  } else if ((genero == "trichoderma" | genero == "Trichoderma" & tipo_de_muestra == "no" | tipo_de_muestra == "No" & obsevacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [55, ])
    print ( base [56, ])
    foto (34)
  } else if (((genero == "trichoderma" | genero == "Trichoderma") & (tipo_de_muestra == "suelos" | tipo_de_muestra == "Suelos") & (observacion == "esporulacion" | observacion == "Esporulacion"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [55, ])
    foto (34)
  } else if (((genero == "trichoderma" | genero == "Trichoderma") & (tipo_de_muestra == "suelos" | tipo_de_muestra == "Suelos") & (observacion == "presencia" | observacion == "Presencia"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [56, ])
    foto (34)
  } else if ((genero == "penicillium" | genero == "Penicillium" & tipo_de_muestra == "no" | tipo_de_muestra == "No" & obsevacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [57, ])
    print ( base [58, ])
    print ( base [59, ])
    foto (35)
  } else if (((genero == "penicillium" | genero == "Penicillium") & (tipo_de_muestra == "granos" | tipo_de_muestra == "Granos") & (observacion == "presencia" | observacion == "Presencia"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [57, ])
    foto (35)
  } else if ((genero == "penicillium" | genero == "Penicillium" & tipo_de_muestra == "suelo" | tipo_de_muestra == "Suelo" & obsevacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [58, ])
    print ( base [59, ])
    foto (35)
  } else if (((genero == "penicillium" | genero == "Penicillium") & (tipo_de_muestra == "suelo" | tipo_de_muestra == "Suelo") & (observacion == "esporulacion" | observacion == "Esporulacion"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [58, ])
    foto (35)
  } else if (((genero == "penicillium" | genero == "Penicillium") & (tipo_de_muestra == "suelo" | tipo_de_muestra == "Suelo") & (observacion == "presencia" | observacion == "Presencia"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [59, ])
    foto (35)
  } else if ((genero == "alternaria" | genero == "Alternaria" & tipo_de_muestra == "no" | tipo_de_muestra == "No" & obsevacion == "no" | observacion == "No")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [60, ])
    print ( base [61, ])
    print ( base [62, ])
    foto (36)
  } else if (((genero == "alternaria" | genero == "Alternaria") & (tipo_de_muestra == "frotis" | tipo_de_muestra == "Frotis") & (observacion == "presencia" | observacion == "Presencia"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [60, ])
    foto (36)
  } else if (((genero == "alternaria" | genero == "Alternaria") & (tipo_de_muestra == "suelo" | tipo_de_muestra == "Suelo") & (obsevacion == "no" | observacion == "No" ))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [61, ])
    print ( base [62, ])
    foto (36)
  } else if (((genero == "alternaria" | genero == "Alternaria") & (tipo_de_muestra == "suelo" | tipo_de_muestra == "Suelo") & (observacion == "esporulacion" | observacion == "Esporulacion"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [61, ])
    foto (36)
  } else if (((genero == "alternaria" | genero == "Alternaria") & (tipo_de_muestra == "suelo" | tipo_de_muestra == "Suelo") & (observacion == "presencia" | observacion == "Presencia"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [62, ])
    foto (36)
  } else if (((genero == "paecilomyces" | genero == "Paecilomyces") & (tipo_de_muestra == "no" | tipo_de_muestra == "No") & (obsevacion == "no" | observacion == "No"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [63, ])
    print ( base [64, ])
    print ( base [65, ])
    foto (37)
  } else if (((genero == "paecilomyces" | genero == "Paecilomyces") &  (tipo_de_muestra == "líquido" | tipo_de_muestra == "Líquido") & (obsevacion == "presencia" | observacion == "Presencia"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [63, ])
    foto (37)
  } else if (((genero == "paecilomyces" | genero == "Paecilomyces") & (tipo_de_muestra == "aliemntos" | tipo_de_muestra == "Alimentos") & (obsevacion == "presencia" | observacion == "Presencia"))){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [64, ])
    foto (37)
  } else if (((genero == "paecilomyces" | genero == "Paecilomyces") & (tipo_de_muestra == "suelo" | tipo_de_muestra == "Suelo") & (obsevacion == "presencia" | observacion == "Presencia") )){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [65, ])
    foto (37)
  } else if ((genero == "trichophyton" | genero == "Trichophyton")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [66, ])
    foto (38)
  } else if ((genero == "allomyces" | genero == "Allomyces")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [67, ])
    print ( base [68, ])
    foto (39)
  } else if ((genero == "allomyces" | genero == "Allomyces" & tipo_de_muestra == "alimentos" | tipo_de_muestras == "Alimentos")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [67, ])
    foto (39)
  } else if ((genero == "allomyces" | genero == "Allomyces" & tipo_de_muestra == "suelo" | tipo_de_muestras == "Suelo")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [68, ])
    foto (39)
  } else if ((genero == "pichia" | genero == "Pichia")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [69, ])
    print ( base [70, ])
    foto (40)
  } else if ((genero == "pichia" | genero == "Pichia" & obervacion == "esporulacion" | observacion == "Esporulacion")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [69, ])
    foto (40)
  } else if ((genero == "pichia" | genero == "Pichia" & obervacion == "presencia" | observacion == "Presencia")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [70, ])
    foto (40)
  } else if ((genero == "cladosporium" | genero == "Cladosporium")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [71, ])
    print ( base [72, ])
    print ( base [73, ])
    foto (41)
  } else if ((genero == "cladosporium" | genero == "Cladosporium" & tipo_de_muestra == "frotis" | tipo_de_muestra == "Frotis")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [71, ])
    print ( base [72, ])
    foto (41)
  } else if ((genero == "cladosporium" | genero == "Cladosporium" & observacion == "esporulacion" | observacion == "Esporulacion")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [71, ])
    foto (41)
  } else if ((genero == "cladosporium" | genero == "Cladosporium" & observacion == "presencia" | observacion == "Presentacion")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [72, ])
    foto (41)
  } else if ((genero == "cladosporium" | genero == "Cladosporium" & tipo_de_muestra == "suelo" | tipo_de_muestra == "Suelo")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [73, ])
    foto (41)
  } else if ((genero == "microsporum" | genero == "Microsporum")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [74, ])
    foto (42)
  } else if ((genero == "neurospora" | genero == "Neurospora")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [75, ])
    print ( base [76, ])
    print ( base [77, ])
    foto (43)
  } else if ((genero == "neurospora" | genero == "Neurospora" & tipo_de_muestra == "alimentos" | tipo_de_muestra == "Alimentos")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [75, ])
    foto (43)
  } else if ((genero == "neurospora" | genero == "Neurospora" & tipo_de_muestra == "suelo" | tipo_de_muestra == "Suelo")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [76, ])
    print ( base [77, ])
    foto (43)
  } else if ((genero == "neurospora" | genero == "Neurospora" & observacion == "esporulacion" | observacion == "Esporulacion")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [76, ])
    foto (43)
  } else if ((genero == "neurospora" | genero == "Neurospora" & observacion == "presencia" | observacion == "Presencia")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [77, ])
    foto (43)
  } else if ((genero == "blastomyces" | genero == "Blastomyces")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [78, ])
    foto (44)
  } else if ((genero == "paracoccidioides" | genero == "Paracoccidioides")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [79, ])
    foto (45)
  } else if ((genero == "schizosaccharomyces" | genero == "Schizosaccharomyces")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [80, ])
    print ( base [81, ])
    foto (46)
  } else if ((genero == "schizosaccharomyces" | genero == "Schizosaccharomyces" & tipo_de_muestra == "alimentos" | tipo_de_muestra == "Alimentos")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [80, ])
    foto (46)
  } else if ((genero == "schizosaccharomyces" | genero == "Schizosaccharomyces" & tipo_de_muestra == "suelo" | tipo_de_muestra == "Suelo")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [81, ])
    foto (46)
  } else if ((genero == "geotrichum" | genero == "Geotrichum")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [82, ])
    print ( base [83, ])
    foto (47)
  } else if ((genero == "geotrichum" | genero == "Geotrichum" & tipo_de_muestra == "suelo" | tipo_de_muestra == "Suelo")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [82, ])
    foto (47)
  } else if ((genero == "geotrichum" | genero == "Geotrichum" & tipo_de_muestra == "líquido" | tipo_de_muestra == "Líquido")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [83, ])
    foto (47)
  } else if ((genero == "bipolaris" | genero == "Bipolaris")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [84, ])
    foto (48)
  } else if ((genero == "mucor" | genero == "Mucor")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [85, ])
    print ( base [86, ])
    foto (49)
  } else if ((genero == "mucor" | genero == "Mucor" & obervacion == "esporulacion" | observacion == "Esporulacion")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [85, ])
    foto (49)
  } else if ((genero == "mucor" | genero == "Mucor" & obervacion == "presencia" | observacion == "Presencia")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [86, ])
    foto (49)
  } else if ((genero == "botrytis" | genero == "Botrytis")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [87, ])
    print ( base [88, ])
    foto (50)
  } else if ((genero == "botrytis" | genero == "Botrytis" & observacion == "esporulacion" | observacion == "Esporulacion")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [87, ])
    foto (50)
  } else if ((genero == "botrytis" | genero == "Botrytis" & observacion == "presencia" | observacion == "Presencia")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [88, ])
    foto (50)
  } else if ((genero == "mortierella" | genero == "Mortierella")){
    print ("aquí tienes un poco de información que te puede ser de utilidad")
    print ( base [89, ])
    foto (51)
  }else{
    print("Lo siento, por el momento no poseemos información sobre ese género, 
        espera nuestras próximas actualizaciones")
  }
}



#### FUNCIÓN PARA EJECUTAR EL PROGRAMA

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



#### EJECUTAR EL PROGRAMA

cultivnet()




