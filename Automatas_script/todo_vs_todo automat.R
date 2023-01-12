# Se infectan plasmidos que incluso tienen bacterias con un plasmido
# mas completo
rm(list=ls())
q=0.5 # Probability for recovery
n=50 # Size of matrix 50 x 50
mat=matrix(ncol=n,nrow=n,0) # Create matrix with zeros (Healthy individuals)

mat[2,39]=1 # Place an infected individual at pos (25,15)
mat[29,2]=2 # Colocar célula con plásmido 2 en (47,41)
image(mat,col=c("grey50","deeppink","seagreen1"),yaxt="n",xaxt="n",zlim=c(0,2)) # Plot image
grid(nx=n,ny=n,col="grey70",lty=1)

temp=mat
t=80 # Number of time steps
n_helalthy=rep(0,t) # Vector to count healthy at each time step
n_infected=rep(0,t) # Vector to count infected at each time step
n_resistant=rep(0,t) # Vector to count resistant at each time step

for (k in 1:t){ # Repeat t times
  kH=0 # Initialize counter for healthy
  kI=0 # Initialize counter for infected
  kR=0 # Initialize counter for resistant
  # Step through each element in the matrix
  for (i in 1:n){
    for (j in 1:n){
      if (mat[i,j]==0) { kH=kH+1} # Count healthy
      if (mat[i,j]==1) { kI=kI+1} # Count infected
      if (mat[i,j]==2) { kR=kR+1} # Count resistant
      R=0 # Initialize counter for number of infected neighbors
      R1= 0
      R0 = 0
      
      # inicia conteos
      if (mat[i,j]==0){ # If healthy individual
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        # Check if outside the matrix
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
        # Count number of infected neighbors
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
      Pinfect=(1/(1+exp(-(a+b*R)))) # Calc probability for healthy to become infected
      g<-runif(1) # Draw a random number between 0 and 1
      if (g<Pinfect & mat[i,j]==0 & R>0){
        temp[i,j]=1 # Healthy becomes infected
      } ### fin de los suceptibles a infectados
      
      # plasmido 2 con sin plasmido
      if (mat[i,j]==0){ # If healthy individual
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        # Check if outside the matrix
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
        # Count number of infected plasmido 2 neighbors
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
      Pinfect1<- (1/(1+exp(-(a+b*R1)))) # Calc probability for healthy to become infected
      g1<- runif(1) # Draw a random number between 0 and 1
      if (g1<Pinfect1 & mat[i,j]==0 & R1>0){
        temp[i,j]=2 # Healthy becomes infected plasmido 2
      }
      
      ### plasmido 1 infecta al 2 
      if (mat[i,j]==2){ # SI el plasmido 2 se infecta con el plasmido 1
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        # Check if outside the matrix
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
        # Count number of infected neighbors
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
      Pinfect=(1/(1+exp(-(a+b*R)))) # Calc probability for healthy to become infected
      g2<-runif(1) # Draw a random number between 0 and 1
      if (g2<Pinfect & mat[i,j]==2 & R>0){
        temp[i,j]=1 # pasan a plasmido 1 
      }
      
      #### plasmido 2 infecta al 1
      if (mat[i,j]==1){ # If healthy individual
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        # Check if outside the matrix
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
        # Count number of infected plasmido 2 neighbors
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
      Pinfect1<- (1/(1+exp(-(a+b*R1)))) # Calc probability for healthy to become infected
      g3<- runif(1) # Draw a random number between 0 and 1
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
        # Check if outside the matrix
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
        # Count number of infected neighbors
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
      Pinfect=(1/(1+exp(-(a+b*R0)))) # Calc probability for healthy to become infected
      g4<-runif(1) # Draw a random number between 0 and 1
      if (g4<Pinfect & mat[i,j]==2 & R0>0){
        temp[i,j]=0 # pasan a plasmido 1 
      }
      
      #### plasmido 1 pierde su capsula
      if (mat[i,j]==1){ # If healthy individual
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        # Check if outside the matrix
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
        # Count number of infected plasmido 2 neighbors
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
      Pinfect1<- (1/(1+exp(-(a+b*R0)))) # Calc probability for healthy to become infected
      g5<- runif(1) # Draw a random number between 0 and 1
      if (g5<Pinfect1 & mat[i,j]==1 & R0>0){
        temp[i,j]=0 # plasmido 1 pasa a plasmido 2
      }
      
      
    }
  }
  image(mat,col=c("grey50","deeppink","seagreen1"),add=F,,zlim=c(0,2)) # Plot image
  grid(nx=n,ny=n,col="grey70",lty=1)
  Sys.sleep(0.1) # To see movement on screen we need to pause the loop
  mat=temp # Overwrite matrix
  # Save number of healthy, infected and resistant at each time step
  n_helalthy[k]=kH
  n_infected[k]=kI
  n_resistant[k]=kR
}
