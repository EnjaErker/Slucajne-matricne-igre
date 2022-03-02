# Programska koda uporabljena pri pripravi projekta Sluèajne matriène igre pri predmetu Matematika z raèunalnikom

# Funkcija za izraèun vrednosti matriène igre 2x2
v <- function(B) {
  if (max(min(B[1,1],B[1,2]),min(B[2,1],B[2,2]))==min(max(B[1,1],B[2,1]),max(B[1,2],B[2,2]))) 
    {return(min(max(B[1,1],B[2,1]),max(B[1,2],B[2,2])))}
  else 
    {return((B[1,1]*B[2,2]-B[1,2]*B[2,1])/(B[1,1]-B[1,2]-B[2,1]+B[2,2]))}
  }

# Primera dveh matrik
A = matrix(c(2, 4, 1, 5),nrow=2,ncol=2,byrow = TRUE) 
C = matrix(c(2, 1, 1, 3),nrow=2,ncol=2,byrow = TRUE) 

# Funkcija, ki generira matriko dimenzije 2x2 z elementi, ki predstavljajo realizacije izbrane sluèajne spremenljivke
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
