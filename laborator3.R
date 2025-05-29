density_exponential = function(lambda, n, a) {
  x = seq(0, a, n);
  y = dexp(x, lambda);
  plot(x, y, type = 'l');
}
density_gauss = function(mean, variance,n, a) {
  x = seq(mean-a, mean+a,length= n);
  y = dnorm(x,mean,sqrt(variance));
  plot(x, y, type = 'l');
}
density_student = function(r,n,a) {
  x = seq(-a, a,length= n);
  y = dt(x,r);
  plot(x, y, type = 'l');
}
LLN_Poisson = function(lambda, N) {
return(mean(rpois(N,lambda)));
}
LLN_Gama = function(alfa,lambda, N) {
  print(alfa/lambda);
  return(mean(rgamma(N,lambda)));
}
LLN_Binomial = function(n,p, N) {
  print(n*p);
  return(mean(rbinom(N,n,p)));
}
CLT_Poisson = function(lambda, n, N, z) {
  expectation = lambda;
  st_dev = sqrt(lambda);
  upper_bound = z * st_dev/sqrt(n) + expectation;
  sum = 0;
  for(i in 1:N) {
    x_n = mean(rpois(n, lambda));
    if(x_n <= upper_bound) {
      sum = sum + 1;
    }
  }
  print(pnorm(z));
  return(sum/N);
}
#exercitii lab
#2.2
LLN_Student <- function(r, n) {
  
  return(mean(rt(n, r)))
}
n_values <- c(1000, 10000, 100000, 1000000)
r_values <- c(2, 3, 4, 5)
for(r in r_values)
  for(n in n_values)
    print(LLN_Student(r,n))
cat ("\n")
#3.2
CLT_Gamma <- function(alpha, lambda, n, N, z) {
  mu <- alpha / lambda
  sigma <- sqrt(alpha) / lambda
  upper_bound <- z * sigma / sqrt(n) + mu
  
  count <- 0
  for (i in 1:N) {
    sample_mean <- mean(rgamma(n, shape = alpha, rate = lambda))
    if (sample_mean <= upper_bound) {
      count <- count + 1
    }
  }
  
  return(count / N)
}

alpha <- 3
lambda <- 2
n <- 50
N_vals <- c(5000, 10000, 20000)
z_vals <- c(-1.5, 0, 1.5)

for (N in N_vals) {
  for (z in z_vals) {
    print(CLT_Gamma(alpha,lambda,n,N,z))
  }
}
cat ("\n")
#5.1b
MC_integration_exp <- function(N) {
  sum = 0
  for (i in 1:N) {
    u = runif(1, 1, 4) 
    sum = sum + exp(u)  
  }
  return(sum / N)  
}


MC_integration_with_error <- function(N) {
  exact_value = 51.87987  
  estimate = MC_integration_exp(N) 
  absolute_error = abs(estimate - exact_value)  
  relative_error = absolute_error / exact_value  
  cat("Estimarea integralei: ", estimate, "\n")
  cat("Eroare absolută: ", absolute_error, "\n")
  cat("Eroare relativă: ", relative_error, "\n")
}
MC_integration_with_error(20000)
MC_integration_with_error(50000)

cat("\n")
#5.1.d
MC_integration_fraction <- function(N, M = 1000) {
  sum = 0
  for (i in 1:N) {
    u = runif(1, 1, M)  
    sum = sum + 1 / (4 * u^2 - 1) 
  }
  return(sum / N)  
}
MC_integration_with_error_fraction <- function(N) {
  exact_value = log(3/4) 
  estimate = MC_integration_fraction(N)  
  absolute_error = abs(estimate - exact_value)  
  relative_error = absolute_error / exact_value 
  
  cat("Estimarea integralei: ", estimate, "\n")
  cat("Eroare absolută: ", absolute_error, "\n")
  cat("Eroare relativă: ", relative_error, "\n")
}

MC_integration_with_error_fraction(20000)
MC_integration_with_error_fraction(50000)
cat("\n")
#5.2
MC_improved_integration <- function(N, lambda = 2) {
  sum = 0
  for (i in 1:N) {
    u = rexp(1, lambda)  
    sum = sum + exp(-2 * u^2) / exp(-lambda * u) 
  }
  return(sum / N) 
}
MC_improved_integration_with_error <- function(N, k = 30) {
  exact_value = sqrt(pi / 8)  
  estimates = numeric(k) 
  for (i in 1:k) {
    estimates[i] = MC_improved_integration(N) 
  }
  mean_estimate = mean(estimates)  
  std_dev = sd(estimates) 
  absolute_error = abs(mean_estimate - exact_value) 
  relative_error = absolute_error / exact_value  
  
  cat("Estimarea medie: ", mean_estimate, "\n")
  cat("Deviatia standard: ", std_dev, "\n")
  cat("Eroare absolută: ", absolute_error, "\n")
  cat("Eroare relativă: ", relative_error, "\n")
}
MC_improved_integration_with_error(50000, 30)

cat("\n")
#6.1
Nr_days <- function() {
  nr_days <- 1
  last_errors <- c(9, 15, 13) 
  nr_errors <- 9  
  while(nr_errors > 0) {
    lambda <- mean(last_errors)  
    nr_errors <- rpois(1, lambda)  
    last_errors <- c(nr_errors, last_errors[1:2])  
    nr_days <- nr_days + 1
  }
  return(nr_days) 
}
MC_nr_days <- function(N) {
  s <- 0
  for (i in 1:N) {
    s <- s + Nr_days()  
  }
  return(s / N)  
}
result <- MC_nr_days(10000)
cat("Numărul mediu de zile până când toate erorile sunt găsite este:", result, "\n")
#6.1
Nr_days <- function() {
  nr_days <- 1
  last_errors <- c(9, 15, 13) 
  nr_errors <- 9  
  while(nr_errors > 0) {
    lambda <- mean(last_errors)  
    nr_errors <- rpois(1, lambda)  
    last_errors <- c(nr_errors, last_errors[1:2])  
    nr_days <- nr_days + 1
  }
  return(nr_days) 
}
MC_nr_days <- function(N) {
  s <- 0
  for (i in 1:N) {
    s <- s + Nr_days()  
  }
  return(s / N)  
}
result <- MC_nr_days(10000)
cat("Numărul mediu de zile până când toate erorile sunt găsite este:", result, "\n")

