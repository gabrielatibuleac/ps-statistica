#3.1
zconfidence_interval=function(n,sample_mean,alfa,sigma)
{
  z_star=-qnorm(alfa/2);
  left=sample_mean-z_star*sigma/sqrt(n);
  right=sample_mean+z_star*sigma/sqrt(n);
  print(left);
  print(right);
  
}
#3.2
print(" ex 3.2");
zconfidence_interval(25,65.53,0.1,10)
cat( "\n")
#3.3
print(" ex 3.3");
zconfidence_interval(50,5,0.05,0.5)
cat("\n")
#3.4
print(" ex 3.4");
zconfidence_interval(100,1280,0.01,140)
cat("\n")
#3.5
print(" ex 3.5");
zconfidence_interval(35,60,0.1,5)
cat("\n")
zconfidence_interval(35,60,0.05,5)
cat("\n")
zconfidence_interval(35,60,0.01,5)
cat("\n")

#3.6
print(" ex 3.6");

zconfidence_interval_from_file=function(file_name, alfa, sigma) {
 values=scan(file_name)
  n=length(values)
  sample_mean=mean(values)
zconfidence_interval(n,sample_mean,alfa,sigma)
}
zconfidence_interval_from_file("history.txt",0.05,5)
cat("\n")
#4.1
tconfidence_interval=function(n,sample_mean,alfa,s)
{
  t_star=-qt(alfa/2,n-1);
  left=sample_mean-t_star*s/sqrt(n);
  right=sample_mean+t_star*s/sqrt(n);
  print(left);
  print(right);
}
cat("\n")
print(" ex 4.2");
#4.2
tconfidence_interval(196,44.65,0.01,sqrt(2.25))
cat("\n")
print(" ex 4.3")
#4.3
tconfidence_interval(49,12,0.05,sqrt(1.75))
cat("\n")
tconfidence_interval(49,12,0.01,sqrt(1.75))
cat("\n")
tconfidence_interval(49,13.5,0.05,sqrt(1.75))
cat("\n")
print(" ex 4.4")
#4.4
tconfidence_interval_from_file=function(file_name,alfa) {
  values=scan(file_name)
  n=length(values)
  sample_mean=mean(values)
  s=sd(values)
  t_star=-qt(alfa/2, n-1)
  left=sample_mean-t_star*s/sqrt(n)
  right=sample_mean+t_star*s/sqrt(n)
  print(left)
  print(right)
}
tconfidence_interval_from_file("history.txt", 0.05)
cat("\n");
