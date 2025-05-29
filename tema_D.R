#D1
cat("Exercitiul D1")
cat("\n")
n1=80;
media1=40.5;
s1=2.5;
nivel_incredere = 0.95;
alpha = 1-nivel_incredere;
z=qnorm(1-alpha/2);
margine_eroare1=z*s1/sqrt(n1);
limita_inferioara1=media1-margine_eroare1;
limita_superioara1=media1+margine_eroare1;
cat("Interval de încredere 95% pentru media slujbelor:\n");
cat("[",round(limita_inferioara1,2),",",round(limita_superioara1, 2),"]\n\n");
#D2
cat("Exercitiul D2")
cat("\n")
inaltimi=read.csv("christmas_tree.csv",header=TRUE)[[1]];
n2=length(inaltimi)
sigma2=0.12;
media2=mean(inaltimi);
margine_eroare2=z*sigma2/sqrt(n2);
limita_inferioara2=media2-margine_eroare2;
limita_superioara2=media2+margine_eroare2;
cat("Interval de încredere 95% pentru înălțimea medie:\n");
cat("[",round(limita_inferioara2,3),",",round(limita_superioara2,3),"]\n");
pret_mediu = 40 * c(limita_inferioara2, limita_superioara2)
cat("Interval de încredere 95% pentru prețul mediu al unui brad:\n");
cat("[",round(pret_mediu[1],2),",",round(pret_mediu[2],2),"] RON\n\n");
#D3
cat("Exercitiul D3")
cat("\n")
n3=200;
succes=160;
p_0=0.90;
p_observat=succes/n3;
z_test=(p_observat-p_0)/ sqrt(p_0*(1-p_0)/n3);
p_value=2*pnorm(-abs(z_test));
cat("Valoarea z:",round(z_test,3),"\n")
cat("p-value:",round(p_value,4), "\n")
if (p_value<0.01){
  cat("La nivel de semnificație 1%:respingem ipoteza,afirmația NU este legitimă.\n")
}else{
  cat("La nivel de semnificație 1%:nu respingem ipoteza,afirmația ESTE legitimă.\n")}
if(p_value<0.05){
  cat("La nivel de semnificație 5%:respingem ipoteza,afirmația NU este legitimă.\n")
}else{
  cat("La nivel de semnificație 5%:nu respingem ipoteza,afirmația ESTE legitimă.\n")
}
