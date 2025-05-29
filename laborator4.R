matrix product=function(A,B,C)
{
  n=nrow(A);
  r=matrix(,nrown=n, ncol=1);
  x= matrix(,nrown=n,ncol=1);
  y=matrix( ,nrown=n,ncol=1);
  r=samp(0.1,n,replace=TRUE);
}
for(i in 1:n)
{
  y[i]=0;
  for(j in 1:n)
    y[i]=(y[i]+B[i,j]*x[j])%2;
}
for(i in 1:n)
{
  y[i]=0;
  for(j in 1:n)
    y[i]=(y[i]+A[i,j]*x[j])%2;
}
for(i in 1:n)
{
  x[i]=0;
  for(j in 1:n)
    x[i]=(x[i]+C[i,j]*r[j])%2;
}
for(i in 1:n)
{
  if(x[i]!=y[n])
    return("NO.");
}
return("YES.")