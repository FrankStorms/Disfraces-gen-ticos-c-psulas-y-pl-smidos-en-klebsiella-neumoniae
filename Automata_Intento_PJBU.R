#Plásmidos AC

rm(list=ls())
q=0.5 # Probabilidad de quedarse sin plásmido
n=25 # Tamaño de la matriz
mat=matrix(ncol=n,nrow=n,0) # Crear matriz con ceros, que representan las células sin plásmido

mat[5,15]=1 # Colocar célula con plásmido 1 en las coordenadas (5,15)
mat[9,20]=2 # Colocar célula con plásmido 2 en las coordenadas (9,20)
image(mat,col=c("grey50","deeppink","seagreen1"),yaxt="n",xaxt="n",zlim=c(0,2)) # Plot image
grid(nx=n,ny=n,col="grey70",lty=1)

temp=mat
t=50 # Tiempo
n_Splasmido=rep(0,t) # Vector que cuenta las células sin plásmido en cada momento 
n_plasmido1=rep(0,t) # Vector que cuenta las células con el plásmido 1 en cada momento
n_plasmido2=rep(0,t) # Vector que cuenta las células con el plásmido 2 en cada momento

for (k in 1:t){ # Repetir t veces
  kSp=0 # Inicia el conteo de sin plásmido
  kp1=0 # Inicia el conteo de plásmido 1
  kp2=0 # Inicia el conteo de plásmido 2
  # Recorrer cada elemento de la matriz
  for (i in 1:n){
    for (j in 1:n){
      if (mat[i,j]==0) { kSp=kSp+1} # Cuenta los sin plásmido
      if (mat[i,j]==1) { kp1=kp1+1} # Cuenta los plásmido 1 
      if (mat[i,j]==2) { kp2=kp2+1} # Cuenta los plásmido 2
      R1=0 # Inicia el conteo de números de los vecinos infectados con el plásmido 1
      R2=0 # Inicia el conteo de números de los vecinos infectados con el plásmido 2
      
      if (mat[i,j]==0){ # Sin los individuos sin plásmido
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        # Comprobar fuera de la matriz
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
        # Cuenta los vecinos infectados
        if(mat[E,j]==1){R1=R1+1} # East
        if(mat[W,j]==1){R1=R1+1} # West
        if(mat[i,N]==1){R1=R1+1} # North
        if(mat[i,S]==1){R1=R1+1} # South
        if(mat[E,N]==1){R1=R1+1} # North East
        if(mat[E,S]==1){R1=R1+1} # South East
        if(mat[W,N]==1){R1=R1+1} # North West
        if(mat[W,S]==1){R1=R1+1} # South West
        
        if(mat[E,j]==1){R2=R2+1} # East
        if(mat[W,j]==1){R2=R2+1} # West
        if(mat[i,N]==1){R2=R2+1} # North
        if(mat[i,S]==1){R2=R2+1} # South
        if(mat[E,N]==1){R2=R2+1} # North East
        if(mat[E,S]==1){R2=R2+1} # South East
        if(mat[W,N]==1){R2=R2+1} # North West
        if(mat[W,S]==1){R2=R2+1} # South West
      }
      gama=0.05
      p=3
      p1=2
      delta=0.7
      Pinfect= ((1 - delta)*p*p1 + gama*p*p1)# Calcular la probabilidad de que los sin plásmido obtengan plásmido
      g=runif(1) # Que arroje un número al azar entre 0 y 1
      if (g<Pinfect & mat[i,j]==0 & R1>0){
        temp[i,j]=1 # Los sin plásmido se infecten con el plásmido 1
      }
      if (mat[i,j]==1){ # Si el infectado esta solo
        g=runif(1) # Que arroje un número al azar entre 0 y 1
        if (g<q){
          temp[i,j]=2 # Infectado plasmido 1 se queda sin plásmido
        }
        gama=0.05
        p=3
        delta=0.7
        p2=3
        Pinfect2= ((1 - delta)*p*p2 + gama*p*p2)# Calcular la probabilidad de que los sin plásmido obtengan plásmido
        g=runif(1) # Que arroje un número al azar entre 0 y 1
        if (g<Pinfect & mat[i,j]==0 & R2>0){
          temp[i,j]=1 # Los sin plásmido se infecten con el plásmido 2
        }
        if (mat[i,j]==1){ # Si el infectado esta solo
          g=runif(1) # Que arroje un número al azar entre 0 y 1
          if (g<q){
            temp[i,j]=2 # Infectado plasmido 2 se queda sin plásmido
          }
        }
      }
    }
    image(mat,col=c("grey50","deeppink","seagreen1"),add=F, zlim=c(0,2))
    grid(nx=n, ny=n, col = "grey70", lty=1) # Plot image
    Sys.sleep(0.1) # Para que se mueva hay que parar el loop 
    mat=temp # Sobre escribir la matriz 
    # Que guarde el número de sin plásmido, con plásmido 1 y 2 
    n_Splasmido[k]=kSp
    n_plasmido1[k]=kp1
    n_plasmido2[k]=kp2
  } 
}

  graphics.off()
  plot(1:k,n_Splasmido,type="l",ylab="Number",xlab="Time steps (weeks)",col=1,ylim=c(0,2600))
  lines(1:k,n_plasmido1,col=2)
  lines(1:k,n_plasmido2,col=3)
  legend(x=52,y=1599,c("Sin plásmido","Plásmido 1","Plásmido 2"),lty=1,col=1:3)
  
  stop()
  
  