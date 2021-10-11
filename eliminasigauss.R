#===OPERASI BARIS ELEMENTER==========================================
#1. Row Scalling. Mengalikan baris matriks dengan konstanta bukan nol.
#2. Row Swaping. Menukar urutan baris pada sebuah matriks 
#(contoh:menukar baris 1 dengan baris 2 dan sebaliknya).
#3. Row Replacement. Baris matriks diganti dengan hasil penjumlahan
#atau pengurangan baris matriks tersebut dengan baris matriks lainnya,
#dimana baris matriks lainnya yang akan dijumlahkan/dikurangkan dengan matriks tersebut
#telah dilakukan proses row scalling. Luaran yang
#diperoleh pada umumnya adalah nilai nol pada baris matriks awal atau
#akhir.
fullmodel <- lm(Eeff ~ NDF + ADF + CP + NEL + DMI + FCM, data = na.omit(phuong))
step(fullmodel, direction = "backward", trace=FALSE ) 

scale_row <- function(m, row, k){
  m[row, ] <- m[row, ]*k
  return(m)
}
A <- matrix(1:15, nrow=5)
scale_row(m=A, row=2, 10)

swap_row <- function(m, row1, row2){
  row_tmp <- m[row1, ]
  m[row1, ] <- m[row2, ]
  m[row2, ] <- row_tmp
  return(m)
}
swap_row(m=A, row1=2, row2=5)

replace_row <- function(m, row1, row2, k){
  m[row2, ] <- m[row2, ] + m[row1, ]*k
  return(m)
}
replace_row(m=A, row1=1, row2=3, k=-3)

#====REF===============================================
ref_matrix <- function(a)
{
  m <- nrow(a)
  n <- ncol(a)
  piv <- 1
  # cek elemen diagonal apakah bernilai nol
  for(row_curr in 1:m)
    {
      if(piv <= n)
        {
          i <- row_curr
          while(a[i, piv] == 0 && i < m)
            {
              i <- i+1
              if(i > m)
                {
                  i <- row_curr
                  piv <- piv+1
                  if(piv > n)
                  return(a)
                }
            }
      # jika diagonal bernilai nol, lakukan row swapping
        if(i != row_curr)
          a <- swap_row(a, i, row_curr)
      # proses triangulasi untuk membentuk matriks segitiga atas
        for(j in row_curr:m)
          if(j != row_curr)
            {
              c <- a[j, piv]/a[row_curr, piv]
              a <- replace_row(a, row_curr, j, -c)
            }
        piv <- piv+1
        }
    }
  return(a)
}
am <- c(1,1,2,
        1,2,1,
        1,-1,2,
        6,2,10)
m <- matrix(am, nrow=3)
ref_matrix(m)

#=============GAUSS JORDAN======================
gauss_jordan <- function (a)
  {
    m <- nrow (a)
    n <- ncol (a)
    piv <- 1
  # cek elemen diagonal utama apakah bernilai nol
    for(row_curr in 1:m)
      {
        if(piv <= n)
          {
            i <- row_curr
            while(a[i, piv] == 0 && i < m)
              {
                i <- i + 1
                if(i > m)
                  {
                    i <- row_curr
                    piv <- piv + 1
                    if(piv > n)
                      return (a)
                  }
              }
      # jika diagonal utama bernilai nol,lakukan row swapping
            if(i != row_curr)
              a <- swap_row(a, i, row_curr)
      # proses pembentukan matriks reduced row echelon form
            piv_val <- a[row_curr , piv]
            a <- scale_row (a, row_curr , 1 / piv_val)
            for(j in 1: m)
              {
                if(j != row_curr)
                  {
                    k <- a[j, piv]/a[row_curr, piv]
                    a <- replace_row (a, row_curr, j, -k)
                  }
              }
            piv <- piv + 1
          }
      }
    return (a)
  }
gauss_jordan(m)
#=============TRIDIAGONAL==================================
#install.packages("limSolve")
library(limSolve)
l <- u <- c(4, 2, 3); d <- c(3, 5, 5, 5)
b <- c(20, 28, 18, 18)
Solve.tridiag(diam1=l, dia=d, diap1=u, B=b)
#diam1: vektor bukan nol di bawah diagonal matriks
#dia: vektor bukan nol pada diagonal matriks
#diap1: vektor bukan nol di atas diagonal matriks
#B: vektor konstanta
