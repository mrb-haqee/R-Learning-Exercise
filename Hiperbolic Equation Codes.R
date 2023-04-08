# ANALISIS NUMERIK II
# Nama : Muhammad Rafli Baihaqi
# NIM  : 20085410066
# TGL  : 28 November 2022
# TOPIK: Solusi Numerik Persamaan Hiperbola

h <- 0.1                     #panjang selisih x
k <- 0.05                    #panjang selisih t
c <- sqrt(4)                 #nilai konstanta
r <-c*k/h                    # rumus r
x<-seq(from=0, to=1, by=h)   #titik x
t<-seq(from=0, to=0.5, by=k) #titik t
n<-length(x)                 #panjang interval x
m<-length(t)                 #oanjang interval y
f<-function(x){
  sin(pi*x)+sin(2*pi*x)
} #fungsi f(x) saat u(x,t=o)
g<-function(x){
  0
} #fungsi g(x) saat du(x,t=0)/dt

C <- matrix(0, nrow = m, ncol = n) #membuat matriks 0 dengan nxn

#iterasi baris 1
for(j in 1:n){
  if(j<n-1){
    C[1,j]<-round(f(x[j]),6)
  }else{
    C[1,j]<-f(x[j])
  }
}

#iterasi baris 2
for(j in 2:n-1){
  if(j<n-1){
    C[2,j+1]<-((1-r^2)*C[1,j+1])+k*g(x[j])+(r^2)*(C[1,j+2]+C[1,j])/2
  }else{0}
}

#iterasi baris selanjutnya
for(i in 1:m){
  if(i<m-1){
    for(j in 1:n){
      if(j<n-1){
        C[i+2,j+1]<-(2-2*r^2)*C[i+1,j+1]+(r^2)*(C[i+1,j+2]+C[i+1,j])-C[i,j+1]
      }else{0}
    }
  }else{0}
}

C<-round(C[,1:n-1],6)