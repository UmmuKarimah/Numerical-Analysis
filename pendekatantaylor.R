library(pracma)
p <- taylor(f = cos, x0 = 0, n = 3) 
polyval(p, pi/3)  

#advance
mytaylor<-function(f,x0,n){
  taylor(f, x0, n )
}
hasil=matrix(0,6,1)
funct=exp
x0=0
x=0.5
for(i in 1:6){
  p<-mytaylor(funct,x0,i)
  hasil[i]<-polyval(p,x)
  print(hasil[i])
}
cos(pi/4)
nilaiasli<-exp(0.5)
#menghitung kesalahan
#kesalahan relatif
for (i in 1:6){
  et<-((nilaiasli-hasil[i])/nilaiasli)*100
  print(et)
}


#kesalahan pendekatan
for (i in 1:5){
  ea<-((hasil[i+1]-hasil[i])/hasil[i+1])*100
  print(ea)
}


#cara membuat grafik
t<-seq(0,50,1)
y<-sin(t)
x<-cos(t)
plot(t,y,type="l")
plot(x,y,type='l',col='blue',main='(cos(t),sin(t))')
