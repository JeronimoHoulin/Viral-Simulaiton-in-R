#Parcial Programación Aplicada a Finanzas
#Jerónimo Houlin

library(ggplot2)
library(tidyverse)


################################### VIRAL AUTOMATA 1 ########################################


"CONSIGO LOS PARÁMETROS"

get_params <- function(){
  G <- readline("Enter G, the zize of a G^2 world: ")
  N <- readline("Enter N, the number of individuals in the world: ")

  parametros <- list(x=G,y=N)
  return(parametros)
}

parametros <- get_params()


"ARMO LA POBLACION BASE"

start_world1 <- function(parametros){
  G <- as.numeric(parametros$x)
  N <- as.numeric(parametros$y)
  
  #creo la matrix 
  matriz1 <- matrix(0,nrow = G, ncol = G)
  colnames(matriz1) <- (1:G)
  rownames(matriz1) <- (1:G)
  
  #set.seed(12999)
  xlist1 <- c()
  ylist1 <- c()
  
  for(i in 1:N){
    #creo coordenadas ramdom para cada individuo
    posible_x <- (1:G)
    posible_y <- (1:G)
    x <- sample(posible_x, replace = TRUE, size = 1)
    y <- sample(posible_y, replace = TRUE, size = 1)
    
    #se la asigno a un autómata de N
    #pero si ya había un autómata en ese lugar, que sumen
    matriz1[x,y] <- matriz1[x,y] +1
    
    #Para el gráfico sumo las coordenadas en la lista
    xlist1 <- append(xlist1, x)
    ylist1 <- append(ylist1, y)
    
  }
  print(matriz1)
  
  pob <- list(xlist1, ylist1)
  return(pob)
  #alpha <- N/(G^2)
  #cat("This world´s density ALPHA is: ", alpha)

}

poblacion <- start_world1(parametros)


"GRAFICO MUNDO INICIAL"

plot_population1 <- function(poblacion,parametros){
  G <- as.numeric(parametros$x)
  N <- as.numeric(parametros$y)
  xlist1 <- poblacion[[1]]
  ylist1 <- poblacion[[2]]
  
  "Simple Plot:"
  plot(xlist1,ylist1, pch=19, col="blue", xlim = c(1,G), ylim=c(1,G),xlab="x",ylab="y")
  
  #Fist spot
  dataframe1 <- data.frame(xlist1, ylist1)
  
  "First density plot:"
  plot1 <- (ggplot(data = dataframe1  , aes(x = xlist1, y = ylist1)) +
              stat_density2d(aes(fill = ..density..), geom = 'tile', contour = F) +
              scale_fill_distiller(palette = 'RdYlBu')+
              ggtitle("Population density in our country") +
              xlab("x axis") + ylab("y axis")
  )
  print(plot1)
}

plot_pop1 <- plot_population1(poblacion,parametros)

################################### VIRAL AUTOMATA 2 ########################################


"TOMO PARAMETROS DE MOVIMIENTO"

get_params2 <- function(){
  G <- readline("Enter G, the zize of a G^2 world: ")
  N <- readline("Enter N, the number of individuals in the world: ")
  M <- readline("Enter M, the number of imovements an individual can take: ")
  
  parametros <- list(x=G,y=N,z=M)
  return(parametros)
}

parametros <- get_params2()


"PRIMER MOVIMIENTO DE LOS AUTÓMATAS"

move_automata <- function(poblacion,parametros){
  G <- as.numeric(parametros$x)
  N <- as.numeric(parametros$y)
  M <- as.numeric(parametros$z)
  
  #de la poblacion anterior saco los nuevos movimientos
  xlist1 <- poblacion[[1]]
  ylist1 <- poblacion[[2]]
  print(xlist1)
  print(ylist1)
  
  #Para el gráfico sumo las coordenadas en la lista
  xlist2 <- c()
  ylist2 <- c()
  
  #creo el loop con las nuevas posiciones
  for(i in 1:N){
    #creo la posibilidades de movimiento de cu
    delta_x <- sample(-M:M,  replace = TRUE, size = 1)
    delta_y <- sample(-M:M,  replace = TRUE, size = 1)
    
    new_pos_x <- xlist1[i] + delta_x
    new_pos_y <- ylist1[i] + delta_y
  
    #Le establesco los parámetros de la red donde no pueden pasar
    if (new_pos_x>0 & new_pos_x <= G) {xlist2 <- append(xlist2, new_pos_x)}
    else if(new_pos_x <= 0){xlist2 <- append(xlist2, 0)}
    else if(new_pos_x > G){xlist2 <- append(xlist2, G)}
    
    if (new_pos_y>0 & new_pos_y <= G) {ylist2 <- append(ylist2, new_pos_y)}
    else if(new_pos_y <= 0){ylist2 <- append(ylist2, 0)}
    else if(new_pos_y > G){ylist2 <- append(ylist2, G)}  
    else{ylist2<-append(ylist2, 0)}
    }

  pob_up <-list(xlist2, ylist2)
  print(pob_up)
  return(pob_up)
}

poblacion_update <- move_automata(poblacion,parametros)


"GRAFICO PROX MUNDO"

plot_population2 <- function(poblacion_update,parametros){
  G <- as.numeric(parametros$x)
  N <- as.numeric(parametros$y)
  xlist2 <- poblacion_update[[1]]
  ylist2 <- poblacion_update[[2]]
  
  "Simple Plot:"
  plot(xlist2,ylist2, pch=19, col="blue", xlim = c(1,G), ylim=c(1,G),xlab="x",ylab="y")
  
}

plot_pop2 <- plot_population2(poblacion_update,parametros)



################################### VIRAL AUTOMATA 2 ETERNO #################################


"ARMO EL LOOP PARA X CANT. DE PERIODOS"

next_move <- function(poblacion_update, parametros){
  continue <- readline("Do you want to continue one more period ?")
  while(continue != "ESC"){
    G <- as.numeric(parametros$x)
    N <- as.numeric(parametros$y)
    M <- as.numeric(parametros$z)
    
    #de la poblacion anterior saco los nuevos movimientos
    xlist1 <- poblacion_update[[1]]
    ylist1 <- poblacion_update[[2]]
    
    #Para el gráfico sumo las coordenadas en la lista
    xlistn <- c()
    ylistn <- c()
    
    #creo el loop con las nuevas posiciones
    for(i in 1:N){
      #creo la posibilidades de movimiento de cu
      delta_x <- sample(-M:M,  replace = TRUE, size = 1)
      delta_y <- sample(-M:M,  replace = TRUE, size = 1)
      
      new_pos_x <- xlist1[i] + delta_x
      new_pos_y <- ylist1[i] + delta_y
      
      #Le establesco los parámetros de la red donde no pueden pasar
      if (new_pos_x>0 & new_pos_x <= G) {xlistn <- append(xlistn, new_pos_x)}
      else if(new_pos_x <= 0){xlistn <- append(xlistn, 1)}
      else if(new_pos_x > G){xlistn <- append(xlistn, G)}
      
      if (new_pos_y>0 & new_pos_y <= G) {ylistn <- append(ylistn, new_pos_y)}
      else if(new_pos_y <= 0){ylistn <- append(ylistn, 1)}
      else if(new_pos_y > G){ylistn <- append(ylistn, G)}  
      else{ylistn<-append(ylistn, 0)}
    }
    print(xlistn)
    print(ylistn)
    poblacion_update <-list(xlistn, ylistn)
    return(poblacion_update)
  }
}

poblacion_update <- next_move(poblacion_update, parametros)



"GRAFICO N MUNDOS"

plot_populationn <- function(poblacion_update,parametros){
  G <- as.numeric(parametros$x)
  N <- as.numeric(parametros$y)
  xlistn <- poblacion_update[[1]]
  ylistn <- poblacion_update[[2]]
  
  "Simple Plot:"
  plot(xlistn, ylistn, pch=19, col="blue", xlim = c(1,G), ylim=c(1,G),xlab="x",ylab="y")
  
}

plot_popn <- plot_populationn(poblacion_update,parametros)




################################### VIRAL AUTOMATA 3 ########################################


"BUSCO LOS PARAMETROS & CARACTERÍSTICAS DE CADA AUTOMATA"
get_params3 <- function(){
  
  G <- readline("Enter G, the zize of a G^2 world: ")
  N <- readline("Enter N, the number of individuals in the world: ")
  M <- readline("Enter M, the number of imovements an individual can take: ")
  K <- readline("Enter K, the time intervals you want to run the simulation:")
  
  id <- c(1:N)
  tiempo <- rep(0:N)
  
  age_group <- c(0.4, 0.3, 0.2, 0.1)
  names(age_group) <- c(1:4)
  
  health_group <- c(1:5)
  names(health_group) <- c("S", "E", "G", "R", "F")
  
  
  parametros <- list(G=G,N=N,M=M,id=id, tiempo = tiempo, age_group = age_group, health_group = health_group, K = K)
  return(parametros)
}

parametros <- get_params3()


"CREO MI MONDO BASE CON CADA AUTÓMATA CON SUS CARACTERÍSTICAS"

start_world3 <- function(parametros){
  #llamo los params
  G <- parametros$G
  M<- parametros$M
  N<- parametros$N
  id<- parametros$id
  tiempo<- parametros$tiempo
  age_group<- parametros$age_group
  health_group<- parametros$health_group
  K <- parametros$K
  
  
  #creo la nueva lista de autómatas con sus características
  #posiciones
  xlist3 <- c()
  ylist3 <- c()
  #características
  id <- c()
  tiempo<- c(0)
  age_list <- c()
  health_list <- c()
  
  for(i in 1:N){
    #creo coordenadas ramdom para cada individuo
    posible_x <- (1:G)
    posible_y <- (1:G)
    x <- sample(posible_x, replace = TRUE, size = 1)
    y <- sample(posible_y, replace = TRUE, size = 1)
    
    #Agrego las posiciones base
    xlist3 <- append(xlist3, x)
    ylist3 <- append(ylist3, y)
    
    #Agrego sus características
    age_list <- append(age_list, sample(c(1:4), replace = TRUE, 
                                        size = 1, prob = c(0.4,0.3,0.2,0.1)))
    health_list <- append(health_list, sample(c("S", "E", "G", "R", "F"), 
                                              replace = TRUE, size = 1))
    id <- append(id, i)
  }
  #tiempo <- append(tiempo, 1)
  #cat("id:", id, "\n")
  #cat("age:",age_list, "\n")
  #cat("health:", health_list, "\n")
  #cat("tiempo:", tiempo, "\n")
  
  pobss <- list(xlist3, ylist3, id, tiempo, age_list, health_list) 
  return(pobss)
}

poblacion_caracter <- start_world3(parametros)

#check si todas las posiciones y características son igual de largas que N
for(i in 1:5){print(length(poblacion_caracter[[i]]))}



"CREO LA FUNCION QUE ARMA ESTE MUNDO K VECES"

poblacion_update3 <- function(parametros, poblacion_caracter){
  #llamo los params
  G <- as.numeric(parametros$G)
  M<- as.numeric(parametros$M)
  N<- as.numeric(parametros$N)
  K <- as.numeric(parametros$K)
  #llamo a la poblacion base
  xlist3 <- poblacion_caracter[[1]]
  ylist3 <- poblacion_caracter[[2]]
  id<- poblacion_caracter[[3]]
  tiempo<- poblacion_caracter[[4]]
  age_group<- poblacion_caracter[[5]]
  health_group<- poblacion_caracter[[6]]
  
  #creo una historia para cada período t
  historia_x <- list(poblacion_caracter[[1]])
  historia_y <- list(poblacion_caracter[[2]])
  
  tiempo <- c(0)

  
  #armo el loop para la cant de iteraciones
  for (j in 1:K){
    
    #creo las poblaciones n
    xlistn <- c()
    ylistn <- c()
    #sigo armando un mundo nuevo normalmente
    for(i in 1:N){
      #creo la posibilidades de movimiento de cu
      delta_x <- sample(-M:M,  replace = TRUE, size = 1)
      delta_y <- sample(-M:M,  replace = TRUE, size = 1)
      
      new_pos_x <- xlist3[i] + delta_x
      new_pos_y <- ylist3[i] + delta_y
      
      #Le establesco los parámetros de la red donde no pueden pasar
      if (new_pos_x>0 & new_pos_x <= G) {xlistn <- append(xlistn, new_pos_x)}
      else if(new_pos_x <= 0){xlistn <- append(xlistn, 1)}
      else if(new_pos_x > G){xlistn <- append(xlistn, G)}
      
      if (new_pos_y>0 & new_pos_y <= G) {ylistn <- append(ylistn, new_pos_y)}
      else if(new_pos_y <= 0){ylistn <- append(ylistn, 1)}
      else if(new_pos_y > G){ylistn <- append(ylistn, G)}  
      else{ylist2<-append(ylistn, 0)}
    }
    
    historia_x[[length(historia_x) + 1]] <- xlistn    # Append new list element
    historia_y[[length(historia_y) + 1]] <- ylistn    # Append new list element
    #historia_x[j] <-append(historia_x, xlistn)
    #historia_y[j] <-append(historia_y, ylistn)
    
    xlistn <<- NULL
    ylistn <<- NULL
    
    tiempo <- append(tiempo, j)

  }
  
  print(historia_x)
  print(historia_y)
  hist_up <- list(historia_x, historia_y)
  return(hist_up)
}


poblacion_update <- poblacion_update3(parametros, poblacion_caracter)

poblacion_hist_x <- poblacion_update[[1]]
poblacion_hist_y <- poblacion_update[[2]]


"AMINO EL HISTORIAL GRAFICAMENTE"


animate_history <- function(){
  #llamo los params
  G <- as.numeric(parametros$G)
  M<- as.numeric(parametros$M)
  N<- as.numeric(parametros$N)
  K <- as.numeric(parametros$K)
  #llamo las caracteristicas
  age <- as.numeric(parametros$age_group)
  health <- parametros$health_group
  #llamo el historial 
  poblacion_hist_x <- poblacion_hist_x
  poblacion_hist_y <- poblacion_hist_y
  
  
  #creo el loop del gráfico 
  auxi <- readline("Do you want to make a base population graph (y/n)?")
  count <- 1
  while(auxi == "y"){
    
    "Simple Plot:"
    plot(poblacion_hist_x[[count]], poblacion_hist_y[[count]],
         pch=c(15,17,19,23)[age],
         col=c("green","orange","red","purple","black")[health],
         xlim = c(1,G), ylim=c(1,G),xlab="x",ylab="y")
    count <- count +1
    
    #si count se pasa de K +1 lo rompo por que no hay más que graficar
    #ES K+1 PARA INCLUIR LA POB BASEE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if(count >K+1){break}
    auxi <- readline("Do you want to make a new graph ?")
  }
  
}


animate_plot <- animate_history()






################################### VIRAL AUTOMATA 4 ########################################

"INTRODUZCO LOS PARÁMETROS DE CONTAGIABILIDAD, EDITARLOS SEGÚN PREFERENCIAS"
get_params4 <- function(){
  
  G <- readline("Enter G, the zize of a G^2 world: ")
  N <- readline("Enter N, the number of individuals in the world: ")
  M <- readline("Enter M, the number of imovements an individual can take: ")
  K <- readline("Enter K, the time intervals you want to run the simulation:")
  
  id <- c(1:N)
  tiempo <- rep(0:N)
  
  age_group <- c(0.4, 0.3, 0.2, 0.1)
  names(age_group) <- c(1:4)
  
  health_group <- c(1:5)
  names(health_group) <- c("S", "E", "G", "R", "F")
  
  #parametros de contagiabilidad
  beta = 0.5  #por que suponemos una población con espacios "semi-abiertos" 
  #si quisiera considerar boliches Beta = 1, si quiero lugares exteriores Beta = 0
  l =  1     
  p = 1- exp(-beta*l) #probabilidad de contagio
  TS = 2  #Time sick (out of K)
  TG = 4  #Time Grave (out of K)
  PEG = 0.6 #Prob Enfermo Grave
  PEF = 0.3 #Prob Enfermo Fallecido
  parametros <- list(G=G,N=N,M=M,id=id, tiempo = tiempo, age_group = age_group, 
                     health_group = health_group, K = K, l = l, p = p, TS = TS, TG = TG, 
                     PWG = PEG, PEF = PEF)
  return(parametros)
}

parametros <- get_params4()


"CREO EL MUNDO BASE"

start_world4 <- function(parametros){
  #llamo los params
  G <- parametros$G
  M<- parametros$M
  N<- parametros$N
  id<- parametros$id
  tiempo<- parametros$tiempo
  age_group<- parametros$age_group
  health_group<- parametros$health_group
  K <- parametros$K
  p <- parametros$p
  TS <- parametros$TS
  TG <- parametros$TG
  PEG <- parametros$PEG
  PEF <- parametros$PEF
  
  #creo la nueva lista de autómatas con sus características
  #posiciones
  xlist4 <- c()
  ylist4 <- c()
  #características
  id <- c()
  tiempo<- c(0)
  age_list <- c()
  #Por el momento base están todos sanos
  health_list <-  rep("S", N)
  #por el momento base todos tienen TS = 0
  TS <- rep(0, N)
  
  for(i in 1:N){
    #creo coordenadas ramdom para cada individuo
    posible_x <- (1:G)
    posible_y <- (1:G)
    x <- sample(posible_x, replace = TRUE, size = 1)
    y <- sample(posible_y, replace = TRUE, size = 1)
    
    #Agrego las posiciones base
    xlist4 <- append(xlist4, x)
    ylist4 <- append(ylist4, y)
    
    #Agrego sus características
    age_list <- append(age_list, sample(c(1:4), replace = TRUE, 
                                        size = 1, prob = c(0.4,0.3,0.2,0.1)))

    id <- append(id, i)
  }
  
  pobss <- list(xlist4, ylist4, id, tiempo, age_list, health_list, TS) 
  return(pobss)
}

poblacion_caracter <- start_world4(parametros)

poblacion_caracter    #vemos que tiene todo Health = S y TS = 0

"hasta acá el 4b"


"CREO LA POBLACION INFECTADA EN t = 0"
"4c"

N0 <- 1 # si quiero arrancar con un solo infectado

start_world4 <- function(parametros){
  #llamo los params
  G <- parametros$G
  M<- parametros$M
  N<- parametros$N
  id<- parametros$id
  tiempo<- parametros$tiempo
  age_group<- parametros$age_group
  health_group<- parametros$health_group
  K <- parametros$K
  p <- parametros$p
  TS <- parametros$TS
  TG <- parametros$TG
  PEG <- parametros$PEG
  PEF <- parametros$PEF
  
  #creo la nueva lista de autómatas con sus características
  #posiciones
  xlist4 <- c()
  ylist4 <- c()
  #características
  id <- c()
  tiempo<- c(0)
  age_list <- c()
  health_list <- rep("S", N)
  TS_list <- rep(0, N)
  
  for(i in 1:N){
    #creo coordenadas ramdom para cada individuo
    posible_x <- (1:G)
    posible_y <- (1:G)
    x <- sample(posible_x, replace = TRUE, size = 1)
    y <- sample(posible_y, replace = TRUE, size = 1)
    
    #Agrego las posiciones base
    xlist4 <- append(xlist4, x)
    ylist4 <- append(ylist4, y)
    
    #Agrego sus características
    age_list <- append(age_list, sample(c(1:4), replace = TRUE, 
                                        size = 1, prob = c(0.4,0.3,0.2,0.1)))
    id <- append(id, i)
  }
  #Ahora creo los infectados iniciales
  for (j in 1:N0){
    auxy <- sample(1:N, replace = FALSE, size = 1)
    health_list[auxy] <- "E"
    TS_list[auxy] <- TS
  }
  print(health_list)
  print(TS_list)
  pobss <- list(xlist4, ylist4, id, tiempo, age_list, health_list, TS_list) 
  return(pobss)
}

poblacion_caracter <- start_world4(parametros)


"CREO MI MUNDO DE CONTAGIOS SEGÚN LOS PARAMETROS DE CONTAGIABILIDAD"

poblacion_update4 <- function(parametros, poblacion_caracter){
  #llamo los params
  G <- parametros$G
  M<- as.numeric(parametros$M)
  N<- parametros$N
  id<- parametros$id
  tiempo<- parametros$tiempo
  age_group<- parametros$age_group
  health_group<- parametros$health_group
  K <- parametros$K
  p <- parametros$p
  TS <- parametros$TS
  TG <- parametros$TG
  PEG <- parametros$PEG
  PEF <- parametros$PEF
  
  #creo una historia para cada período t
  historia_x <- list(poblacion_caracter[[1]])
  historia_y <- list(poblacion_caracter[[2]])
  
  tiempo <- c(0)
  
  h_historia <- c(poblacion_caracter[[6]])
  
  #armo el loop para la cant de iteraciones
  for (j in 1:K){
    
    #creo las poblaciones n
    xlistn <- c()
    ylistn <- c()
    #creo los contagios en cada n poblacion
    h_listn = c()
    
    #sigo armando un mundo nuevo normalmente
    for(i in 1:N){
      #creo la posibilidades de movimiento de cu
      delta_x <- sample(-M:M,  replace = TRUE, size = 1)
      delta_y <- sample(-M:M,  replace = TRUE, size = 1)
      
      new_pos_x <- as.numeric(poblacion_caracter[[1]][i]) + delta_x
      new_pos_y <- as.numeric(poblacion_caracter[[2]][i]) + delta_y
      
      #parametros de contagio
      if(new_pos_x == poblacion_caracter[[1]][i] & new_pos_y == poblacion_caracter[[2]][i]){
        append(h_listn, sample("E", replace = TRUE, prob = p))
      }
      
      
      #Le establesco los parámetros de la red donde no pueden pasar
      if (new_pos_x>0 & new_pos_x <= G) {xlistn <- append(xlistn, new_pos_x)}
      else if(new_pos_x <= 0){xlistn <- append(xlistn, 1)}
      else if(new_pos_x > G){xlistn <- append(xlistn, G)}
      
      if (new_pos_y>0 & new_pos_y <= G) {ylistn <- append(ylistn, new_pos_y)}
      else if(new_pos_y <= 0){ylistn <- append(ylistn, 1)}
      else if(new_pos_y > G){ylistn <- append(ylistn, G)}  
      else{ylist2<-append(ylistn, 0)}
    }
    
    historia_x[[length(historia_x) + 1]] <- xlistn    # Append new list element
    historia_y[[length(historia_y) + 1]] <- ylistn    # Append new list element

    
    append(h_historia, h_listn)
        
    xlistn <<- NULL
    ylistn <<- NULL
    
    h_listn <<- NULL
    
    tiempo <- append(tiempo, j)
    
  }
  

  
  
  hist_up <- list(historia_x, historia_y, h_historia)
  return(hist_up)
}

poblacion_update <- poblacion_update4(parametros, poblacion_caracter)







compute_load <- function(poblacion_caracter, parametros){
  
  l = parametros$l
  health <- poblacion_caracter[6]
  
  l_list <- c(l)
  
  health_list <- c(health)
  
  for (j in 1:K){
    
    l_listy <- c()
    
    health_listy <- c()
   for (i in 1:N){
     l_listy <- append(l_listy, )
     
     
     
   }
  
  }
  
}




