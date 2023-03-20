#INDAH SULISTIYA (B2A020064)


#MULTIPLICATIVE
multiplicative_RNG<-function(a,z0,m,n) {
  xj<-matrix(NA,n,3)
  colnames(xj)<-c("aZ","Xj","Uj")
  for (j in 1:n)
  {
    xj[j,1]<-(a*z0)
    xj[j,2]<-xj[j,1]%%m
    xj[j,3]<-xj[j,2]/m
    z0<-xj[j,2]
  }
  hist(xj[,3])
  View(xj)
}

multiplicative_RNG(45,21139,417,150)


multiplicative_RNG<-function(a,z0,m,n)
{
  z<-rep(0,100)
  for (i in 1:100)
  {
    z[i]<-(a*z0)%%m
    z0<-z[i]
  }
  print(z) 
}
multiplicative_RNG(45,21139,417,150)


#BERNOULI 1
Bernouli_1<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-NULL
  for (k in 1:i) ifelse (X[k]<=p, Y[k]<-1, Y[k]<-0)
  (tabel<-table(Y)/length(Y))
}
Bernouli_1(78, 0.83)
Bernouli_1(174, 0.83)
Bernouli_1(324, 0.83)
Bernouli_1(402, 0.83)
Bernouli_1(159, 0.83)
Bernouli_1(66, 0.83)
Bernouli_1(51, 0.83)
Bernouli_1(210, 0.83)
Bernouli_1(276, 0.83)
Bernouli_1(327, 0.83)
Bernouli_1(120, 0.83)
Bernouli_1(396, 0.83)
Bernouli_1(306, 0.83)
Bernouli_1(9, 0.83)
Bernouli_1(405, 0.83)
Bernouli_1(294, 0.83)
Bernouli_1(303, 0.83)
Bernouli_1(291, 0.83)
Bernouli_1(168, 0.83)
Bernouli_1(54, 0.83)
Bernouli_1(345, 0.83)
Bernouli_1(96, 0.83)
Bernouli_1(150, 0.83)