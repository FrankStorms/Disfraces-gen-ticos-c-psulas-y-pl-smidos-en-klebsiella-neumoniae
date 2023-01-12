# Se infectan plasmidos que incluso tienen bacterias con un plasmido
# mas completo
rm(list=ls())
n=50 
mat=matrix(ncol=n,nrow=n,0) 

mat[2,39]=1 
mat[29,2]=2 
image(mat,col=c("grey50","deeppink","seagreen1"),yaxt="n",xaxt="n",zlim=c(0,2)) 
grid(nx=n,ny=n,col="grey70",lty=1)

temp=mat
t=80 
n_helalthy=rep(0,t) 
n_infected=rep(0,t) 
n_resistant=rep(0,t) 

for (k in 1:t){ 
  kH=0 
  kI=0 
  kR=0 
  for (i in 1:n){
    for (j in 1:n){
      if (mat[i,j]==0) { kH=kH+1} 
      if (mat[i,j]==1) { kI=kI+1} 
      if (mat[i,j]==2) { kR=kR+1} 
      R = 0 
      R1= 0
      R0= 0
      
      # inicia conteos
      if (mat[i,j]==0){ 
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
       
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
      Pinfect=(1/(1+exp(-(a+b*R)))) 
      g<-runif(1) 
      if (g<Pinfect & mat[i,j]==0 & R>0){
        temp[i,j]=1 
      } 
      
      
      if (mat[i,j]==0){ 
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
       
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
      Pinfect1<- (1/(1+exp(-(a+b*R1)))) 
      g1<- runif(1) 
      if (g1<Pinfect1 & mat[i,j]==0 & R1>0){
        temp[i,j]=2 
      }
      
      ### plasmido 1 infecta al 2 
      if (mat[i,j]==2){ 
        E=i+1
        W=i-1
        N=j-1
        S=j+1
       
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
        
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
      Pinfect=(1/(1+exp(-(a+b*R)))) 
      g2<-runif(1) 
      if (g2<Pinfect & mat[i,j]==2 & R>0){
        temp[i,j]=1 # pasan a plasmido 1 
      }
      
      #### plasmido 2 infecta al 1
      if (mat[i,j]==1){ 
        E=i+1
        W=i-1
        N=j-1
        S=j+1
       
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
       
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
      Pinfect1<- (1/(1+exp(-(a+b*R1)))) 
      g3<- runif(1) 
      if (g3<Pinfect1 & mat[i,j]==1 & R1>0){
        temp[i,j]=2 
      }
      
      ## vuelven a ser sin plasmido
      ### plasmido 2 pierde su capcide
      if (mat[i,j]==2){ 
        E=i+1
        W=i-1
        N=j-1
        S=j+1
       
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
       
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
      Pinfect=(1/(1+exp(-(a+b*R0)))) 
      g4<-runif(1) 
      if (g4<Pinfect & mat[i,j]==2 & R0>0){
        temp[i,j]=0  
      }
      
      #### plasmido 1 pierde su capsula
      if (mat[i,j]==1){ 
        E=i+1
        W=i-1
        N=j-1
        S=j+1
      
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
       
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
      Pinfect1<- (1/(1+exp(-(a+b*R0)))) 
      g5<- runif(1)
      if (g5<Pinfect1 & mat[i,j]==1 & R0>0){
        temp[i,j]=0 
      }
      
      
    }
  }
  image(mat,col=c("grey50","deeppink","seagreen1"),add=F,,zlim=c(0,2)) 
  grid(nx=n,ny=n,col="grey70",lty=1)
  Sys.sleep(0.1) 
  mat=temp 
  n_helalthy[k]=kH
  n_infected[k]=kI
  n_resistant[k]=kR
}
