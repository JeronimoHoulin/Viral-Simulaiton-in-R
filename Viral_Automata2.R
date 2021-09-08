#Parcial Programación Aplicada a Finanzas
#Jerónimo Houlin

library(ggplot2)
library(tidyverse)

start_wrld <- function(){
  #obtengo los parametros del usuario
  G <- as.numeric(readline("Enter G, your number of posicions in a grid of G^2: "))
  N <- as.numeric(readline("Enter N, the number of individuals in the grid, or estatic world: "))
  M <- as.numeric(readline("Enter M, the number of movements a person can do in any direction: "))
  
  #creo la matrix 
  poblacion1 <- matrix(0,nrow = G, ncol = G)
  colnames(poblacion1) <- (1:G)
  rownames(poblacion1) <- (1:G)
  
  #creo la matrix de nuevas posiciones
  poblacion2 <- matrix(0,nrow = G, ncol = G)
  colnames(poblacion2) <- (1:G)
  rownames(poblacion2) <- (1:G)
  
  #set.seed(12999)
  x_list <- c()
  y_list <- c()
  xlist <- c()
  ylist <- c()
  for(i in 1:N){
    #creo coordenadas ramdom para cada individuo
    posible_x <- (1:G)
    posible_y <- (1:G)
    x <- sample(posible_x, replace = TRUE, size = 1)
    y <- sample(posible_y, replace = TRUE, size = 1)
    
    #se la asigno a un autómata de N
    #pero si ya había un autómata en ese lugar, que sumen
    poblacion1[x,y] <- poblacion1[x,y] +1
    
    #creo la posibilidades de movimiento de cu
    delta_x <- sample(-M:M,  replace = TRUE, size = 1)
    delta_y <- sample(-M:M,  replace = TRUE, size = 1)
    
    new_pos_x <- x + delta_x
    new_pos_y <- y + delta_y
    
    #Le establesco los parámetros de la red donde no pueden pasar
    if(new_pos_x >= G){new_pos_x <- G}
    else if(new_pos_y >= G){new_pos_y <- G}
    
    #Le otorgo su nuevo lugar
    poblacion2[new_pos_x,new_pos_y] <- poblacion2[new_pos_x,new_pos_y] + 1
    
    #al moverse resto su anterior puesto
    #poblacion1[x,y] <- poblacion1[x,y] -1
    
    #para el gráfico
    x_list <- append(x_list, x)
    y_list <- append(y_list, y)
    xlist <- append(xlist, new_pos_x)
    ylist <- append(ylist, new_pos_y)
  }

  print(poblacion1)
  print(poblacion2)
  alpha <- N/(G)^2
  cat("This world´s density is: ", alpha)
  

  #Next movement
  dataframe2 <- data.frame(xlist, ylist)
  
  "Second density plot:"
  plot2 <- (ggplot(data = dataframe2  , aes(x = xlist, y = ylist)) +
              stat_density2d(aes(fill = ..density..), geom = 'tile', contour = F) +
              scale_fill_distiller(palette = 'RdYlBu')+
              ggtitle("Population density in our country") +
              xlab("x axis") + ylab("y axis")
            )
  print(plot2)


  continue <- readline("Do you want to run another time lapse ?")
  if(continue != "ESC"){
    #creo la posibilidades de movimiento de cu
    delta_x <- sample(-M:M,  replace = TRUE, size = 1)
    delta_y <- sample(-M:M,  replace = TRUE, size = 1)
    
    for(j in 1:G){
      new_pos_x <- xlist[j] + delta_x
      new_pos_y <- ylist[j] + delta_y
      
      #Le establesco los parámetros de la red donde no pueden pasar
      if(new_pos_x > G){new_pos_x <- G}
      else if(new_pos_y > G){new_pos_y <- G}
      
      xlist <- xlist[j]*0 + new_pos_x
      ylist <- ylist[j]*0 + new_pos_y
    }
    #Le establesco los parámetros de la red donde no pueden pasar
    if(new_pos_x > G){new_pos_x <- G}
    else if(new_pos_y > G){new_pos_y <- G}
    
    #borro los lugares anteriores
    poblacion2 <- poblacion2 * 0      
    #Le otorgo su nuevo lugar
    poblacion2[new_pos_x,new_pos_y] <- poblacion2[new_pos_x,new_pos_y] + 1
    
  }
  print(poblacion2)
    
  #Movement N
  dataframe2 <- data.frame(xlist, ylist)
  
  "Second density plot:"
  plotn <- (ggplot(data = dataframe2  , aes(x = xlist, y = ylist)) +
              stat_density2d(aes(fill = ..density..), geom = 'tile', contour = F) +
              scale_fill_distiller(palette = 'RdYlBu')+
              ggtitle("Population density in our country") +
              xlab("x axis") + ylab("y axis")
  )
  print(plotn)
  
}







