#E1
cat("Exercitiul E1")
cat("\n")
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
z_test(3.75, 3.5, 0.5, 100, 0.05, "r")
cat("\n")
z_test(3.75, 3.5, 0.5, 100, 0.01, "r")
cat("\n")
#E2
cat("Exercitiul E2")
cat("\n")
t_test=function(sample_mean, miu0, s ,n,alfa,type){
  #type = "l" , "r" or "s"
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
t_test(38670, 35520, 3540, 200, 0.05, "r")
cat("\n")
t_test(38670, 35520, 3540, 200, 0.01, "r")
cat("\n")
#E3
cat("Exercitiul E3")
cat("\n")
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
z_test_means(37954, 33145, 0, 2231, 2039, 200, 230, 0.05, "r")
cat("\n")
z_test_means(37954, 33145, 0, 2231, 2039, 200, 230, 0.01, "r")
