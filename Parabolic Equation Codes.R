# ANALISIS NUMERIK II
# Nama : Muhammad Rafli Baihaqi
# NIM  : 20085410066
# TGL  : 28 November 2022
# TOPIK: Solusi Numerik Persamaan Parabola

h <- 0.2                     #panjang selisih x
k <- 0.02                    #panjang selisih t
c <- 1                       #nilai konstanta
r <-c^2*k/h^2                #rumus r
x<-seq(from=0, to=1, by=h)   #titik x
t<-seq(from=0, to=0.20, by=k)#titik t
n<-length(x)                 #panjang interval x
m<-length(t)                 #panjang interval y

f<-function(x){
  4*x-4*x^2
} #fungsi f(x) saat u(x,t=o)

C <- matrix(0, nrow = m, ncol = n) #membuat matriks 0 dengan nxn

#iterasi baris 1
for(j in 1:n){
    C[1,j]<-round(f(x[j]),6)
}

#iterasi selanjutnya
for(i in 1:m){
  if(i<m){
    for(j in 1:n){
      if(j<n-1){
        C[i+1,j+1]<-(1-2*r)*C[i,j+1]+r*(C[i,j]+C[i,j+2])
      }
    }
  }
}

C<-round(C,6)