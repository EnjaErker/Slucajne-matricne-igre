# Programska koda uporabljena pri pripravi projekta Sluèajne matriène igre pri predmetu Matematika z raèunalnikom

# Algoritmi ######################################################################################################################

# Algoritem 1: Funkcija, ki generira matriko dimenzije 2x2 z elementi, ki predstavljajo realizacije izbrane sluèajne spremenljivke
Generator <- function(porazdelitev,parameter_1,parameter_2) {
  if (porazdelitev == "U"){
    a <- parameter_1
    b <- parameter_2
    M <- matrix(c(runif(1,a,b), runif(1,a,b), runif(1,a,b), runif(1,a,b)),nrow=2,ncol=2,byrow = TRUE)}
  if (porazdelitev == "Student"){
    df <- parameter_1
    M <- matrix(c(rt(1,df), rt(1,df), rt(1,df), rt(1,df)),nrow=2,ncol=2,byrow = TRUE)}
  if (porazdelitev == "N"){
    mi <- parameter_1
    sigma <- parameter_2 
    M <- matrix(c(rnorm(1,mi,sigma), rnorm(1,mi,sigma), rnorm(1,mi,sigma), rnorm(1,mi,sigma)),nrow=2,ncol=2,byrow = TRUE) }
  if (porazdelitev == "Gamma"){
    a <- parameter_1
    b <- parameter_2 
    M <- matrix(c(rgamma(1,a,b), rgamma(1,a,b), rgamma(1,a,b), rgamma(1,a,b)),nrow=2,ncol=2,byrow = TRUE) }
  if (porazdelitev == "Chi") {
    df <- parameter_1
    M <- matrix(c(rchisq(1,df), rchisq(1,df), rchisq(1,df), rchisq(1,df)),nrow=2,ncol=2,byrow = TRUE)}
  if (porazdelitev == "Ber"){
    p <- parameter_1 
    M <- matrix(c(rbinom(1,1,p), rbinom(1,1,p), rbinom(1,1,p), rbinom(1,1,p)),nrow=2,ncol=2,byrow = TRUE)}
  if (porazdelitev == "Geo"){
    p <- parameter_1 
    M <- matrix(c(rgeom(1,p), rgeom(1,p), rgeom(1,p), rgeom(1,p)),nrow=2,ncol=2,byrow = TRUE)}
  return(M)
}

# Algoritem 2: Funkcija za izraèun vrednosti matriène igre 2x2
Vrednost <- function(B) {
  if (max(min(B[1,1],B[1,2]),min(B[2,1],B[2,2]))==min(max(B[1,1],B[2,1]),max(B[1,2],B[2,2]))) 
    {return(min(max(B[1,1],B[2,1]),max(B[1,2],B[2,2])))}
  else 
    {return((B[1,1]*B[2,2]-B[1,2]*B[2,1])/(B[1,1]-B[1,2]-B[2,1]+B[2,2]))}
}

# Algoritem 3: Funkcija, ki izraèuna povpreèno vrednost igre s simuliranjem 
Povprecna_vrednost <- function(stevilo_simulacij,porazdelitev,parameter_1,parameter_2 ){
  v <- numeric(stevilo_simulacij)
  for (i in c(1:stevilo_simulacij)){
    M <- Generator(porazdelitev,parameter_1,parameter_2)
    v[i] <- Vrednost(M)}
return((1/stevilo_simulacij)*sum(v))
}

# Algoritem 4: Funkcija za izraèun vrednosti povpreène igre 
Vrednost_povprecne_igre <- function(porazdelitev,parameter_1,parameter_2){
  if (porazdelitev == "U"){E <- (parameter_1+parameter_2)/2}
  if (porazdelitev == "Student"){E <- 0}
  if (porazdelitev == "N"){E <- parameter_1}
  if (porazdelitev == "Gamma"){E <- parameter_1/parameter_2}
  if (porazdelitev == "Chi") {E <- parameter_1}
  if (porazdelitev == "Ber"){E <- parameter_1}
  if (porazdelitev == "Geo"){E <-  1/parameter_1}
  return(E)
}

# Analiza odvisnosti razlike med povpreèno vrednostjo in vrednostjo povpreène igre od števila simulacij s spreminjanjem parametrov #######

razlike <- function(porazdelitev,parameter_1,parameter_2){
  r <- numeric(4)
  r[1] <- abs(Povprecna_vrednost(100,porazdelitev,parameter_1,parameter_2)-Vrednost_povprecne_igre(porazdelitev,parameter_1,parameter_2))
  r[2] <- abs(Povprecna_vrednost(1000,porazdelitev,parameter_1,parameter_2)-Vrednost_povprecne_igre(porazdelitev,parameter_1,parameter_2))
  r[3] <- abs(Povprecna_vrednost(5000,porazdelitev,parameter_1,parameter_2)-Vrednost_povprecne_igre(porazdelitev,parameter_1,parameter_2))
  r[4] <- abs(Povprecna_vrednost(10000,porazdelitev,parameter_1,parameter_2)-Vrednost_povprecne_igre(porazdelitev,parameter_1,parameter_2))
 return (r)
}

# Studentova porazdelitev
x <- c(100,1000,5000,10000)
y1 <- razlike('Student',1)
y2 <- razlike('Student',5)
y3 <- razlike('Student',10)
podatki <- data.frame(c(x,x,x),c(1,1,1,1,5,5,5,5,10,10,10,10),c(y1,y2,y3))
colnames(podatki) <- c("s","p","r")
ggplot(podatki,aes(s,r,group=p))+geom_point()+ geom_point(aes(colour = factor(p)),size=1)+ theme(legend.position = "top")+
  labs(color  ="Število prostostnih stopenj",x='Število simulacij',y='Razlika')+geom_line(data=subset(podatki,p==1),color="cadetblue",lwd=1)+
  geom_line(data=subset(podatki,p==5),color="cadetblue1",lwd=1)+geom_line(data=subset(podatki,p==10),color="cadetblue3",lwd=1)+
  scale_color_manual(name = "Število prostostnih stopenj", labels = c("1", "5","10"), values = c("cadetblue", "cadetblue1","cadetblue3"))

# Enakomerno zvezna porazdelitev
x <- c(100,1000,5000,10000)
y1 <- razlike('U',0,1)
y2 <- razlike('U',0,5)
y3 <- razlike('U',0,10)
podatki <- data.frame(c(x,x,x),c(1,1,1,1,5,5,5,5,10,10,10,10),c(y1,y2,y3))
colnames(podatki) <- c("s","p","r")
ggplot(podatki,aes(s,r,group=p))+geom_point()+ geom_point(aes(colour = factor(p)),size=1)+ theme(legend.position = "top")+
  labs(color  ="Dolžina intervala",x='Število simulacij',y='Razlika')+geom_line(data=subset(podatki,p==1),color="cadetblue",lwd=1)+
  geom_line(data=subset(podatki,p==5),color="cadetblue1",lwd=1)+geom_line(data=subset(podatki,p==10),color="cadetblue3",lwd=1)+
  scale_color_manual(name = "Velikost intervala", labels = c("1", "5","10"), values = c("cadetblue", "cadetblue1","cadetblue3"))

# Normalna porazdelitev
# Spreminjanje aritmeriène sredine
x <- c(100,1000,5000,10000)
y1 <- razlike('N',0,4)
y2 <- razlike('N',-100,4)
y3 <- razlike('N',100,4)
podatki <- data.frame(c(x,x,x),c(1,1,1,1,5,5,5,5,10,10,10,10),c(y1,y2,y3))
colnames(podatki) <- c("s","p","r")
ggplot(podatki,aes(s,r,group=p))+geom_point()+ geom_point(aes(colour = factor(p)),size=1)+ theme(legend.position = "top")+
  labs(color  ="Dolžina intervala",x='Število simulacij',y='Razlika')+geom_line(data=subset(podatki,p==1),color="cadetblue",lwd=1)+
  geom_line(data=subset(podatki,p==5),color="cadetblue1",lwd=1)+geom_line(data=subset(podatki,p==10),color="cadetblue3",lwd=1)+
  scale_color_manual(name = "Parametra", labels = c("(0,4)", "(-100,4)","(100,4)"), values = c("cadetblue", "cadetblue1","cadetblue3"))
# Spreminjanje standardnega odklona
x <- c(100,1000,5000,10000)
y1 <- razlike('N',0,1)
y2 <- razlike('N',0,2)
y3 <- razlike('N',0,9)
podatki <- data.frame(c(x,x,x),c(1,1,1,1,5,5,5,5,10,10,10,10),c(y1,y2,y3))
colnames(podatki) <- c("s","p","r")
ggplot(podatki,aes(s,r,group=p))+geom_point()+ geom_point(aes(colour = factor(p)),size=1)+ theme(legend.position = "top")+
  labs(color  ="Dolžina intervala",x='Število simulacij',y='Razlika')+geom_line(data=subset(podatki,p==1),color="cadetblue",lwd=1)+
  geom_line(data=subset(podatki,p==5),color="cadetblue1",lwd=1)+geom_line(data=subset(podatki,p==10),color="cadetblue3",lwd=1)+
  scale_color_manual(name = "Parametra", labels = c("(0,1)", "(0,2)","(0,9)"), values = c("cadetblue", "cadetblue1","cadetblue3"))

# Gamma porazdelitev
# Spreminjanje parametra oblike
x <- c(100,1000,5000,10000)
y1 <- razlike('Gamma',1,1)
y2 <- razlike('Gamma',1,2)
y3 <- razlike('Gamma',1,5)
podatki <- data.frame(c(x,x,x),c(1,1,1,1,5,5,5,5,10,10,10,10),c(y1,y2,y3))
colnames(podatki) <- c("s","p","r")
ggplot(podatki,aes(s,r,group=p))+geom_point()+ geom_point(aes(colour = factor(p)),size=1)+ theme(legend.position = "top")+
  labs(color  ="Dolžina intervala",x='Število simulacij',y='Razlika')+geom_line(data=subset(podatki,p==1),color="cadetblue",lwd=1)+
  geom_line(data=subset(podatki,p==5),color="cadetblue1",lwd=1)+geom_line(data=subset(podatki,p==10),color="cadetblue3",lwd=1)+
  scale_color_manual(name = "Parametra", labels = c("(1,1)", "(1,2)","(1,5)"), values = c("cadetblue", "cadetblue1","cadetblue3"))
# Spreminjanje parametra merila
x <- c(100,1000,5000,10000)
y1 <- razlike('Gamma',0.5,1)
y2 <- razlike('Gamma',5,1)
y3 <- razlike('Gamma',10,1)
podatki <- data.frame(c(x,x,x),c(1,1,1,1,5,5,5,5,10,10,10,10),c(y1,y2,y3))
colnames(podatki) <- c("s","p","r")
ggplot(podatki,aes(s,r,group=p))+geom_point()+ geom_point(aes(colour = factor(p)),size=1)+ theme(legend.position = "top")+
  labs(color  ="Dolžina intervala",x='Število simulacij',y='Razlika')+geom_line(data=subset(podatki,p==1),color="cadetblue",lwd=1)+
  geom_line(data=subset(podatki,p==5),color="cadetblue1",lwd=1)+geom_line(data=subset(podatki,p==10),color="cadetblue3",lwd=1)+
  scale_color_manual(name = "Parametra", labels = c("(0.5,1)", "(5,1)","(10,1)"), values = c("cadetblue", "cadetblue1","cadetblue3"))

# Bernoullijeva porazdelitev
x <- c(100,1000,5000,10000)
y1 <- razlike('Ber',0.3)
y2 <- razlike('Ber',0.6)
y3 <- razlike('Ber',0.9)
podatki <- data.frame(c(x,x,x),c(1,1,1,1,5,5,5,5,10,10,10,10),c(y1,y2,y3))
colnames(podatki) <- c("s","p","r")
ggplot(podatki,aes(s,r,group=p))+geom_point()+ geom_point(aes(colour = factor(p)),size=1)+ theme(legend.position = "top")+
  labs(color  ="Dolžina intervala",x='Število simulacij',y='Razlika')+geom_line(data=subset(podatki,p==1),color="cadetblue",lwd=1)+
  geom_line(data=subset(podatki,p==5),color="cadetblue1",lwd=1)+geom_line(data=subset(podatki,p==10),color="cadetblue3",lwd=1)+
  scale_color_manual(name = "Parameter", labels = c("0.3", "0.6","0.9"), values = c("cadetblue", "cadetblue1","cadetblue3"))

# Geometrijska porazdelitev
x <- c(100,1000,5000,10000)
y1 <- razlike('Geo',0.2)
y2 <- razlike('Geo',0.5)
y3 <- razlike('Geo',0.8)
podatki <- data.frame(c(x,x,x),c(1,1,1,1,5,5,5,5,10,10,10,10),c(y1,y2,y3))
colnames(podatki) <- c("s","p","r")
ggplot(podatki,aes(s,r,group=p))+geom_point()+ geom_point(aes(colour = factor(p)),size=1)+ theme(legend.position = "top")+
  labs(color  ="Dolžina intervala",x='Število simulacij',y='Razlika')+geom_line(data=subset(podatki,p==1),color="cadetblue",lwd=1)+
  geom_line(data=subset(podatki,p==5),color="cadetblue1",lwd=1)+geom_line(data=subset(podatki,p==10),color="cadetblue3",lwd=1)+
  scale_color_manual(name = "Parameter", labels = c("0.2", "0.5","0.8"), values = c("cadetblue", "cadetblue1","cadetblue3"))

# Hi^2 porazdelitev
x <- c(100,1000,5000,10000)
y1 <- razlike('Chi',1)
y2 <- razlike('Chi',2)
y3 <- razlike('Chi',3)
podatki <- data.frame(c(x,x,x),c(1,1,1,1,5,5,5,5,10,10,10,10),c(y1,y2,y3))
colnames(podatki) <- c("s","p","r")
ggplot(podatki,aes(s,r,group=p))+geom_point()+ geom_point(aes(colour = factor(p)),size=1)+ theme(legend.position = "top")+
  labs(color  ="Dolžina intervala",x='Število simulacij',y='Razlika')+geom_line(data=subset(podatki,p==1),color="cadetblue",lwd=1)+
  geom_line(data=subset(podatki,p==5),color="cadetblue1",lwd=1)+geom_line(data=subset(podatki,p==10),color="cadetblue3",lwd=1)+
  scale_color_manual(name = "Število prostostnih stopenj", labels = c("1", "2","3"), values = c("cadetblue", "cadetblue1","cadetblue3"))
