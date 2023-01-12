# se comparten plasmidos entre todos sin afectar a bacterias con un plasmido
## Bueno

rm(list=ls())
q=0.5 
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
      R=0 
      R1= 0
      R0 = 0
      
      
      if (mat[i,j]==0){ 
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
        
        if(mat[E,j]==1){R=R+1} 
        if(mat[W,j]==1){R=R+1} 
        if(mat[i,N]==1){R=R+1} 
        if(mat[i,S]==1){R=R+1} 
        if(mat[E,N]==1){R=R+1} 
        if(mat[E,S]==1){R=R+1} 
        if(mat[W,N]==1){R=R+1} 
        if(mat[W,S]==1){R=R+1} 
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
        
        if(mat[E,j]==2){R1=R1+1} 
        if(mat[W,j]==2){R1=R1+1} 
        if(mat[i,N]==2){R1=R1+1} 
        if(mat[i,S]==2){R1=R1+1} 
        if(mat[E,N]==2){R1=R1+1} 
        if(mat[E,S]==2){R1=R1+1} 
        if(mat[W,N]==2){R1=R1+1} 
        if(mat[W,S]==2){R1=R1+1} 
      }
      a1<- -1.5
      b1<- 0.9
      Pinfect1<- (1/(1+exp(-(a+b*R1)))) 
      g1<- runif(1) 
      if (g1<Pinfect1 & mat[i,j]==0 & R1>0){
        temp[i,j]=2 
      }
      
      
      if (mat[i,j]==2){ 
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
       
        if(mat[E,j]==0){R0=R0+1} 
        if(mat[W,j]==0){R0=R0+1} 
        if(mat[i,N]==0){R0=R0+1} 
        if(mat[i,S]==0){R0=R0+1} 
        if(mat[E,N]==0){R0=R0+1} 
        if(mat[E,S]==0){R0=R0+1} 
        if(mat[W,N]==0){R0=R0+1} 
        if(mat[W,S]==0){R0=R0+1} 
      }
      a=-1.4
      b=0.8
      Pinfect=(1/(1+exp(-(a+b*R0)))) 
      g2<-runif(1) 
      if (g2<Pinfect & mat[i,j]==2 & R0>0){
        temp[i,j]=0 
      }
      
      
      if (mat[i,j]==1){ 
        E=i+1
        W=i-1
        N=j-1
        S=j+1
        
        if (E==n+1) { E=1}
        if (W==0) { W=n}
        if (N==0) { N=n}
        if (S==n+1) { S=1}
        
        if(mat[E,j]==0){R0=R0+1} 
        if(mat[W,j]==0){R0=R0+1} 
        if(mat[i,N]==0){R0=R0+1} 
        if(mat[i,S]==0){R0=R0+1} 
        if(mat[E,N]==0){R0=R0+1} 
        if(mat[E,S]==0){R0=R0+1} 
        if(mat[W,N]==0){R0=R0+1} 
        if(mat[W,S]==0){R0=R0+1} 
      }
      a1<- -1.4
      b1<- 0.8
      Pinfect1<- (1/(1+exp(-(a+b*R0)))) 
      g3<- runif(1) 
      if (g3<Pinfect1 & mat[i,j]==1 & R0>0){
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
