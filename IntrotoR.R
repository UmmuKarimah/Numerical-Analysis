#Pengenalan R

#Pengulangan
rep(c("A","B","C"),5)
rep(c(1,2,3,4),2)
rep(1:2,each=5)
rep(1:3,5)
rep(1:3,each=2,5)


#1. bilangan bulat terdekat 
round(x)
#2. pembulatan ke desimal tertentu
signif(5.43212678, digits=3)
#3. bilangan bulat terkecil yang tidak kurang dari bilangan tertentu
ceiling(x)
#4. bilangan bulat terbesar yang tidak lebih dari
floor(x)

x<-seq(0,10,length=8)
y<-seq(1,5,3)
round(x)
floor(x)
ceiling(x)
signif(x,3)

#matriks
#matrix(data, nbaris, nkolom);
mx=matrix(x,2,4,byrow=TRUE)
my=matrix(c(1,2,3,4),2,2)
#transpose matriks
t(mx)
#determinan
det(my)
#diagonal
m1=diag(y)
#perkalian matriks
my%*%m1
#inverse 
solve(my)

min(x)
max(x)
rataan=mean(x)
median(x)
var(x)
summary(x)

rataan*2
