# ANALISIS NUMERIK II
# Nama : Muhammad Rafli Baihaqi
# NIM  : 20085410066
# TGL  : 28 November 2022
# TOPIK: Solusi Numerik Persamaan Elips

x<-c(0:4)
y<-c(0:4)
C <- matrix(rep(0,5), nrow = 5, ncol = 5) #membuat matriks 0 dengan nxn
u_ki<-function(y){
  80
}
u_ka<-function(y){
  0
}
u_a<-function(x){
  180
}
u_b<-function(x){
  20
}

for(i in 2:4){
  C[1,i]<-u_a(x[1])
  C[5,i]<-u_b(x[5])
  C[i,1]<-u_ki(y[1])
  C[i,5]<-u_ka(y[5])
} #input nilai x dan t

for(i in 1:3){
  if(i==1){
    for(j in 1:9){
      if(j<4){
        C[i+3,j+1]<-paste0("p_",j)
      }
    }
  }else if(i==2){
    for(j in 1:9){
      if(j<4){
        C[i+1,j+1]<-paste0("p_",j+3)
      }
    }
  } else if(i==3){
    for(j in 1:9){
      if(j<4){
        C[i-1,j+1]<-paste0("p_",j+6)
      }
    }
  }
} #membuat matriks p 3x3

#membuat matriks solusi
S<-matrix(0,9,10)
for(i in 1:9){
  if(i==1){
    S[i,10]<--as.numeric(C[5,2])-as.numeric(C[4,1])
  }else if(i==2){
    S[i,10]<--as.numeric(C[5,3])
  }else if(i==3){
    S[i,10]<--as.numeric(C[5,4])-as.numeric(C[4,5])
  }else if(i==4){
    S[i,10]<--as.numeric(C[3,1])
  }else if(i==5){
    S[i,10]<-0
  }else if(i==6){
    S[i,10]<--as.numeric(C[3,5])
  }else if(i==7){
    S[i,10]<--as.numeric(C[2,1])-as.numeric(C[1,2])
  }else if(i==8){
    S[i,10]<--as.numeric(C[1,3])
  }else if(i==9){
    S[i,10]<--as.numeric(C[1,4])-as.numeric(C[2,5])
  }
}
diag(S)<-(-4)
for(i in 1:9){
  if(i<4){
    for(j in 1:6){
     if(i==j){
       S[i,j+3]<-1
     }else if(i==1 & j==2){
       S[i,j]<-1
     }else if(i==2 & j==1){
       S[i,j]<-1
     }else if(i==2 & j==3){
       S[i,j]<-1
     }else if(i==3 & j==2){
       S[i,j]<-1
     }
      
    }
  }else if(i %in% 4:8){
    for(j in 1:9){
      if((i-3)==j){
        S[i,j]<-1
        if(j<4){
          S[i,j+6]<-1
        }
      }else if(i==4 & j==5){
        S[i,j]<-1
      }else if(i==5 & j==4){
        S[i,j]<-1
      }else if(i==5 & j==6){
        S[i,j]<-1
      }else if(i==6 & j==5){
        S[i,j]<-1
      }else if(i==7 & j==8){
        S[i,j]<-1
      }else if(i==8 & j==7){
        S[i,j]<-1
      }else if(i==8 & j==9){
        S[i,j]<-1
      }
    }
  }else if(i==9){
    S[i,6]<-1
    S[i,8]<-1
  }
}

#Solusi dengan gauss jordan
gauss_jordan<- function(matrix) {
  
  m <- nrow(matrix)
  n <- ncol(matrix)
  kolom <- 1
  baris <- 0
  
  while ( (kolom < n+1) & (baris+1 <= m) )  {
    
    #mengecek jika jumlah dalam kolom bernilai 0
    #jika 0 maka akan di ganti ke kolom berikutnya
    if (sum(matrix[(baris+1):m, kolom]) == 0) {
      
      kolom <- kolom + 1
      
    } 
    
    else {
      
      kolom_index <- 0
      
      for (i in baris+1:m) {
        #pengecekan cell utama identitas
        if (matrix[i,kolom] != 0) {
          
          kolom_index <- i
          break
          
        }
        
      }
      
      baris <- baris + 1
      
      # menukar baris
      
      row1 <- matrix[kolom_index,]
      row2 <- matrix[baris,]
      matrix[kolom_index,] <- row2
      matrix[baris,] <- row1
      
      #membagi semua baris baris dengan baris
      matrix[baris,] <- (1/matrix[baris,kolom]) * matrix[baris,]
      
      
      for (k in 1:m) {
        
        if ( (matrix[k, kolom] != 0) & (k != baris) ) {
          
          #membuat 0 untuk dibawah cell utama
          
          scalar <- matrix[k, kolom] / matrix[baris, kolom] 
          
          matrix[k, ] <- -1 * scalar * matrix[baris, ] + matrix[k, ]
          
        }
        
      }
      
      # increment
      
      kolom <- kolom + 1
    }
  }
  
  return(matrix)
  
}

#Hasil Solusi
H<-gauss_jordan(S)
