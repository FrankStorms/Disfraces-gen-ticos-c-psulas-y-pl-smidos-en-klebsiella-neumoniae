# plasmidos infectivos entre ellos pero con muerte de bacterias
# mas completo
rm(list=ls())
q=0.005 # Probabilidad de muerte
n=50 # matriz de 50 x 50
mat=matrix(ncol=n,nrow=n,0) # Crea una matriz de bacterias sin capsula

mat[2,39]=1 # coloca la bacteria con capsula 1 en (25,15)
mat[29,2]=2 # Colocar bacteria con capsula 2 en (47,41)
image(mat,col=c("grey50","deeppink","seagreen1", "black"),yaxt="n",xaxt="n",zlim=c(0,3)) # Plot image
grid(nx=n,ny=n,col="grey70",lty=1)

temp=mat
t=80 # Tiempo del automata
n_SinCapsula=rep(0,t) # Vector sin capsula (conteos)
n_Capsula1=rep(0,t) # Vector con capsula1
n_Capsula2=rep(0,t) # Vector con capsula
n_death = rep(0,t)

for (k in 1:t){ # Repetir t veces 
  kH=0 # inicio de contadores 
  kI=0 
  kR=0 
  kD=0
  # 
  for (i in 1:n){
    for (j in 1:n){
      if (mat[i,j]==0) { kH=kH+1} # Conteo sincapsula
      if (mat[i,j]==1) { kI=kI+1} # Conteo capsula1
      if (mat[i,j]==2) { kR=kR+1} # Conteo capsula2
      if (mat[i,j]==3) { kD=kD+1} # conteo de muertes
      R=0 # inicializar los conteos desde cero
      R1= 0
      R0 = 0
      
      # inicia conteos
      if (mat[i,j]==0){ # van para capsula, bacteria sin capsula
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        # 
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
        # 
        if(mat[E,j]==1){R=R+1} # East
        if(mat[W,j]==1){R=R+1} # West
        if(mat[i,N]==1){R=R+1} # North
        if(mat[i,S]==1){R=R+1} # South
        if(mat[E,N]==1){R=R+1} # North East
        if(mat[E,S]==1){R=R+1} # South East
        if(mat[W,N]==1){R=R+1} # North West
        if(mat[W,S]==1){R=R+1} # South West
      }
      a=-1.5
      b=0.6
      Pinfect=(1/(1+exp(-(a+b*R)))) # Calcula la probabilidad de cambiar
      g=runif(1) # Arroja un numero aleatorio
      if (g<Pinfect & mat[i,j]==0 & R>0){
        temp[i,j]=1 # Healthy becomes infected
      } ### fin de los suceptibles a infectados
      
      # plasmido 2 con sin plasmido
      if (mat[i,j]==0){ # If healthy individual
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        # 
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
        # 
        if(mat[E,j]==2){R1=R1+1} # East
        if(mat[W,j]==2){R1=R1+1} # West
        if(mat[i,N]==2){R1=R1+1} # North
        if(mat[i,S]==2){R1=R1+1} # South
        if(mat[E,N]==2){R1=R1+1} # North East
        if(mat[E,S]==2){R1=R1+1} # South East
        if(mat[W,N]==2){R1=R1+1} # North West
        if(mat[W,S]==2){R1=R1+1} # South West
      }
      a1<- -1.5
      b1<- 0.9
      Pinfect1<- (1/(1+exp(-(a+b*R1)))) # Calcula la probabilidad de cambiar
      g1<- runif(1) # Arroja un numero aleatorio
      if (g1<Pinfect1 & mat[i,j]==0 & R1>0){
        temp[i,j]=2 # Bacteria sin capsula pasa a plasmido 2
      }
      
      ### plasmido 1 infecta al 2 
      if (mat[i,j]==2){ #  el plasmido 2 se infecta con el plasmido 1
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        # 
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
        #
        if(mat[E,j]==1){R=R+1} # East
        if(mat[W,j]==1){R=R+1} # West
        if(mat[i,N]==1){R=R+1} # North
        if(mat[i,S]==1){R=R+1} # South
        if(mat[E,N]==1){R=R+1} # North East
        if(mat[E,S]==1){R=R+1} # South East
        if(mat[W,N]==1){R=R+1} # North West
        if(mat[W,S]==1){R=R+1} # South West
      }
      a=-1.5
      b=0.6
      Pinfect=(1/(1+exp(-(a+b*R)))) # Calcula la probabilidad de cambiar
      g2<-runif(1) # Arroja un numero aleatorio
      if (g2<Pinfect & mat[i,j]==2 & R>0){
        temp[i,j]=1 # pasan a plasmido 1 
      }
      
      #### plasmido 2 infecta al 1
      if (mat[i,j]==1){ # 
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        # 
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
        # 
        if(mat[E,j]==2){R1=R1+1} # East
        if(mat[W,j]==2){R1=R1+1} # West
        if(mat[i,N]==2){R1=R1+1} # North
        if(mat[i,S]==2){R1=R1+1} # South
        if(mat[E,N]==2){R1=R1+1} # North East
        if(mat[E,S]==2){R1=R1+1} # South East
        if(mat[W,N]==2){R1=R1+1} # North West
        if(mat[W,S]==2){R1=R1+1} # South West
      }
      a1<- -1.5
      b1<- 0.9
      Pinfect1<- (1/(1+exp(-(a+b*R1)))) # Calcula la probabilidad de cambiar
      g3<- runif(1) # Arroja un numero aleatorio
      if (g3<Pinfect1 & mat[i,j]==1 & R1>0){
        temp[i,j]=2 # plasmido 1 pasa a plasmido 2
      }
      
      ## vuelven a ser sin plasmido
      ### plasmido 2 pierde su capcide
      if (mat[i,j]==2){ # SI el plasmido 2 se infecta con el plasmido 1
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        # 
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
        # 
        if(mat[E,j]==0){R0=R0+1} # East
        if(mat[W,j]==0){R0=R0+1} # West
        if(mat[i,N]==0){R0=R0+1} # North
        if(mat[i,S]==0){R0=R0+1} # South
        if(mat[E,N]==0){R0=R0+1} # North East
        if(mat[E,S]==0){R0=R0+1} # South East
        if(mat[W,N]==0){R0=R0+1} # North West
        if(mat[W,S]==0){R0=R0+1} # South West
      }
      a=-1.4
      b=0.8
      Pinfect=(1/(1+exp(-(a+b*R0)))) # Calcula la probabilidad de cambiar
      g4<-runif(1) # Arroja un numero aleatorio
      if (g4<Pinfect & mat[i,j]==2 & R0>0){
        temp[i,j]=0 # pasan a plasmido 1 
      }
      
      #### plasmido 1 pierde su capsula
      if (mat[i,j]==1){ # 
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        # 
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
        # 
        if(mat[E,j]==0){R0=R0+1} # East
        if(mat[W,j]==0){R0=R0+1} # West
        if(mat[i,N]==0){R0=R0+1} # North
        if(mat[i,S]==0){R0=R0+1} # South
        if(mat[E,N]==0){R0=R0+1} # North East
        if(mat[E,S]==0){R0=R0+1} # South East
        if(mat[W,N]==0){R0=R0+1} # North West
        if(mat[W,S]==0){R0=R0+1} # South West
      }
      a1<- -1.4
      b1<- 0.8
      Pinfect1<- (1/(1+exp(-(a+b*R0)))) # Calcula la probabilidad de cambiar
      g5<- runif(1) # Arroja un numero aleatorio
      if (g5<Pinfect1 & mat[i,j]==1 & R0>0){
        temp[i,j]=0 # plasmido 1 pasa a plasmido 2
      }
      
      #muertos
      if (mat[i,j]==1){ 
        g6<-runif(1) ##### Conteo por probabilidad de los muertos de la capsula 1
        if (g6<q){ ########
          temp[i,j]=3
        }
        }
        if (mat[i,j]==2){ 
          g7<-runif(1) #####Conteo por probabilidad de los muertos de la capsula 2
          if (g7<q){ ########
            temp[i,j]=3
          }
        }  
      if (mat[i,j]==0){ 
        g8<-runif(1) #####Conteo por probabilidad de los muertos de la capsula 2
        if (g8<q){ ########
          temp[i,j]=3
        }
      }  
      
      
      
      
    }
  }
  image(mat,col=c("grey50","deeppink","seagreen1", "black"),add=F,,zlim=c(0,3)) # Plot image
  grid(nx=n,ny=n,col="grey70",lty=1)
  Sys.sleep(0.1) # To see movement on screen we need to pause the loop
  mat=temp # Overwrite matrix
  # Guarda el conteo de sin capsula, cap1, cap2 y muertos
  n_SinCapsula[k]=kH
  n_Capsula1[k]=kI
  n_Capsula2[k]=kR
  n_death[k]=kD
}
