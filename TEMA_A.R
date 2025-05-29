#A1
#a)
A1.a= function(m, n, k){
  cat("Rezultat exercitiul A1.a:\n");
  x=max(0,k-n):min(k,m);
  probabilitatea=dhyper(x,m,n,k);
  print(max(probabilitatea));
  barplot(probabilitatea,names.arg=x,xlab="Numar de bile albe (X)",ylab="Probabilitatea")
  
}
A1.a(10,5,6)
#b)
A1.b = function(lambda1, lambda2){
  cat("\n");
  cat("Rezultat exercitiul A1.b:\n");
  if(lambda1<lambda2){
    print("Nu exista k");
    return(-1);
  }
  else {
    print("k=0");
    return(0);
  }
  k=0;
  while(TRUE){
    p1=ppois(k,lambda1);
    p2=ppois(k,lambda2);
    barplot(p1);
    if(p1<p2){
      print(paste("Cel mai mic k:", k));
      return(k);
    }
    k=k+1;
  }
}
A1.b(10,5)
#A2
#a)
A2.a=function(nume_fisier)
{   cat("\n");
  cat("Rezultat exercitiul A2.b:\n");
  date=read.csv(nume_fisier);
  note_finale=round((date[["P"]] + date[["S"]]) / 2);  
  frecvente_absolute=table(note_finale);
  frecvente_relative=as.vector(frecvente_absolute)/length(note_finale);
  media=mean(note_finale);
  dispersia=var(note_finale);
  cat("Frecvente absolute:\n");
  print(frecvente_absolute);
  cat("Frecvente relative:\n");
  print(frecvente_relative);
  cat(paste("\nMedia:", media));
  cat(paste("\nDispersia:", dispersia, "\n"));
}
A2.a("punctaje_PS.csv")
#b)
A2.b=function(file_name) {
  cat("\n");
  cat("Rezultat exercitiul A2.b:\n");
  date=read.csv(file_name);
  note_finale=round((date[["P"]]+date[["S"]])/2);
  a=quantile(note_finale,0.25);
  b=quantile(note_finale,0.75);
  IQR=b-a;
  limita_inferioara=a-1.5*IQR;
  limita_superioara=b+1.5*IQR;
  NOTE=note_finale[note_finale>=limita_inferioara & note_finale<=limita_superioara];
  nota_minima=floor(min(NOTE));
  nota_maxima=ceiling(max(NOTE));
  hist(NOTE, breaks = seq(nota_minima,nota_maxima),right=TRUE,xlab="Note Finale",ylab="Frecventa",main="Distribuția notelor finale");
  return(NOTE);
}
A2.b("punctaje_PS.csv")