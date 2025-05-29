#B1
B1=function(n,m,sigma)
  {N=rnorm(n,mean=m,sd=sigma);
  means=cumsum(N)/(1:n);
  return(tail(means,1));
}
print("Exercitiul B1")
print(B1(50000,0,1))
print(B1(100000,0,5))
print(B1(50000,2,1))
print(B1(100000,2,5))

#B2
B2=function(n,N,lambda,z) {
  nr=0;
  for(i in 1:N) {
    sample=rpois(n,lambda);
    m=mean(sample);
    s=sd(sample);
    if(abs((m-lambda)/(s/sqrt(n)))<=z)
      nr=nr+1;
  }
  p=pnorm(z)-pnorm(-z);
  relative_error=abs((nr/N-p)/p);
  return(relative_error);
}
cat("\n")
print("Exercitiul B2")
print(B2(50,50000,1,1))
print(B2(50,100000,2,2))
print(B2(50,50000,3,3))
print(B2(50,100000,4,4))
print(B2(50,50000,5,5))
cat("\n")
#B3
B3=function(r,h,N)
{
 i1=0;
  for(i in 1:N)
  {
    x=runif(1,-r,r);
    y=runif(1,-r,r);
    z=runif(1,0,h);
    if(x^2+y^2<= (r^2/h^2)*z^2 )
    {
    i1=i1+1;
    }
  }
volum_cutie=(2*r)^2*h;
estimare=volum_cutie*i1/N;
exact=pi*r^2*h/3;
eroare_relativa=abs(estimare-exact)/exact;
print(estimare);
return(eroare_relativa);
  }
  print("Exercitiul B3")
  print (B3(4,6,20000))
  print (B3(4,6,50000))
  cat("\n")
  
#B4
B4=function(N)
{i1=0;
for(i in 1:N)
{
  x=runif(1,0,6);
  y=runif(1,0,2);
  if(y<=2*x && x+y<=6)
    i1=i1+1;
}
arie_dreptunghi=6*2;
estimare=arie_dreptunghi*i1/N;
return(estimare);
}
print("Exercitiul 4")
print(B4(10000))
cat("\n")

#B5
#a)
B5.a=function(N){
  suma=0;
  for(i in 1:N) {
    x=runif(1,0,1);
    suma=suma+exp(sqrt(x))*sin(sqrt(x))/sqrt(x);
  }
  estimare=suma/N;
  exact =exp(1)*(sin(1)-cos(1))+1;
  eroare_relativa=abs(estimare-exact)/exact;
  print(estimare);
  return(eroare_relativa);
}

print("Exercitiul B5.a")
print(B5.a(40000))
cat("\n")
#b)
B5.b=function(N) {
  suma=0;
  for(i in 1:N) {
    x=rexp(1,2); 
    valoare_integrand=(3*x+2);
    suma=suma+valoare_integrand;
  }
  estimare=suma/N;
  valoare_exacta=7/4;
  eroare_relativa=abs(estimare-valoare_exacta)/valoare_exacta;
  print(estimare);
  return(eroare_relativa);
}
print("Exercitiul B5.b")
print(B5.b(40000))
cat("\n")

#B6
B6=function() {
  nr=2;
  ultimele=c(32,25);
  while(ultimele[1]>=10) {
    lambda=min(ultimele);
     valoare_noua=rpois(1,lambda);
    ultimele=c(valoare_noua,ultimele[1]);
    nr=nr+1;
  }
  return(nr);
}
medie=function(N) {
  total=0;
  for(i in 1:N)
    total=total+B6();
  return(total/N);
}
print("Exercise 6")
print(medie(100000))