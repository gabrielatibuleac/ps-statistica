#1
z_test=function(sample_mean, miu0,sigma,n,alfa,type){
  #type = "l" , "r" or "s"
  z_score=(sample_mean-miu0)/(sigma/sqrt(n));
  print(z_score);
  if(type=="l"){
    z_star=qnorm(alfa);
    print(z_star);
    if(z_score<z_star){
      print("Reject the null hypothesis and accept the alternative hypothesis.")
    }
    else{
      print("We cannot reject the null hypothesis.")
    }
  }
  
  if(type=="r"){
    z_star=qnorm(1-alfa);
    print(z_star);
    if(z_score>z_star){
      print("Reject the null hypothesis and accept the alternative hypothesis.")
    }
    else{
      print("We cannot reject the null hypothesis.")
    }
  }
  
  if(type=="s"){
    z_star=qnorm(1-alfa/2);
    print(z_star);
    if(abs(z_score)>abs(z_star)){
      print("Reject the null hypothesis and accept the alternative hypothesis.")
    }
    else{
      print("We cannot reject the null hypothesis.")
    }
  }
}
cat("\n")
#1.2
cat("I.2")
cat("\n")
z_test(88,90,12,49,0.05,"l")
z_test(88,90,12,49,0.01,"l")  
cat("\n")
#1.3
cat("I.3")
cat("\n")
z_test(85,75,sqrt(17),36,0.01,"s")
#1.4
cat("I.4")
cat("\n")
z_test(20.5,21,2.5,100,0.05,"l")
#1.5
cat("I.5")
cat("\n")
z_test(970,1000,85,100,0.05,"l")
z_test(970,1000,85,100,0.01,"l")
#1.6
cat("I.6")
cat("\n")
z_test(20, 22, 3, 16, 0.05, "s")
#2
t_test=function(sample_mean, miu0, s ,n,alfa,type){
  #type = "l", "r" or "s"
  t_score=(sample_mean-miu0)/(s/sqrt(n));
  print(t_score);
  if(type=="l"){
    t_star=qt(alfa, n-1);
    print(t_star);
    if(t_score<t_star){
      print("Reject the null hypothesis and accept the alternative hypothesis.")
    }
    else{
      print("We cannot reject the null hypothesis.")
    }
  }
  
  if(type=="r"){
    t_star=qt(1-alfa, n-1);
    print(t_star);
    if(t_score>t_star){
      print("Reject the null hypothesis and accept the alternative hypothesis.")
    }
    else{
      print("We cannot reject the null hypothesis.")
    }
  }
  
  if(type=="s"){
    t_star= - qt(alfa/2, n-1);
    print(t_star);
    if(abs(t_score)>abs(t_star)){
      print("Reject the null hypothesis and accept the alternative hypothesis.")
    }
    else{
      print("We cannot reject the null hypothesis.")
    }
  }
}
#2.2
cat("2.2")
cat("\n")
x=c(36,32,28,33,41,28,31,26,29,34)
sample_mean=mean(x);
s=sd(x)
n=length(x)
t_test(sample_mean,34,s,n,0.01,"s")
#2.3
cat("2.3")
cat("\n")
t_test(11.9,11.4,0.25,100,0.05,"r")
#2.5
t_test(52,49,sqrt(89.5),49,0.05,"s")
t_test(52,49,sqrt(89.5),49,0.01,"s")
#2.4
t_test4=function(filename, mul0, alpha, type){
  x=scan(filename);
  sample_mean = mean(x);
  s=sd(x);
  n=length(x);
  t_score=(sample_mean-mul0)/(s/sqrt(n));
  print(t_score);
  if(type == "l"){
    t_star=qt(alpha,n-1);
    print(t_star);
    if(t_score<t_star){
      print("Reject the null hypothesis and accept the alternative hypothesis.")
    } else {
      print("We cannot reject the null hypothesis.")
    }
  }
  if(type=="r"){
    t_star=qt(1-alpha,n-1);
    print(t_star);
    if(t_score>t_star){
      print("Reject the null hypothesis and accept the alternative hypothesis.")
    } else {
      print("We cannot reject the null hypothesis.")
    }
  }
  if(type=="s"){
    t_star=-qt(alpha/2,n-1);
    print(t_star);
    if(abs(t_score)>abs(t_star)){
      print("Reject the null hypothesis and accept the alternative hypothesis.")
    } else {
      print("We cannot reject the null hypothesis.")
    }
  }
}
t_test4("history.txt", 80, 0.05, "s")
cat("\n")
#3
z_test_means=function(sample_mean1, sample_mean2,m0,sigma1, sigma2, n1, n2, alfa,type){
  #type = "l" , "r" or "s"
  z_score=((sample_mean1-sample_mean2)-m0)/(sigma1^2/n1 + sigma2^2/n2);
  print(z_score);
  if(type=="l"){
    z_star=qnorm(alfa);
    print(z_star);
    if(z_score<z_star){
      print("Reject the null hypothesis and accept the alternative hypothesis.")
    }
    else{
      print("We cannot reject the null hypothesis.")
    }
  }
  
  if(type=="r"){
    z_star=qnorm(1-alfa);
    print(z_star);
    if(z_score>z_star){
      print("Reject the null hypothesis and accept the alternative hypothesis.")
    }
    else{
      print("We cannot reject the null hypothesis.")
    }
  }
  
  if(type=="s"){
    z_star=qnorm(1-alfa/2);
    print(z_star);
    if(abs(z_score)>abs(z_star)){
      print("Reject the null hypothesis and accept the alternative hypothesis.")
    }
    else{
      print("We cannot reject the null hypothesis.")
    }
  }
}
cat("3.2")
cat("\n")
#3.2
z_test_means(160,155,0,3.24,2.25,80,70,0.01,"s")
#3.3
cat("3.3")
cat("\n")
z_test_means(22.8,23.3,0,1.3,1.9,100,100,0.01,"l")
