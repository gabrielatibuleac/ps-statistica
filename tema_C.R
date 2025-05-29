#C1
cat("Exercitiul C1")
cat(" \n")
C1=function(nod,pondere,muchie)
  {
  noduri_selectate=c();
  for(i in muchie){
    nod_u=i[1];
    nod_v=i[2];
    if(!(nod_u %in% noduri_selectate)&&!(nod_v %in% noduri_selectate))
      {
      pondere_u=pondere[[nod_u]];
      pondere_v=pondere[[nod_v]];
      probabilitate=pondere_u/(pondere_u + pondere_v);
      nod_ales=if(runif(1)<probabilitate)nod_u else nod_v;
      noduri_selectate=c(noduri_selectate,nod_ales);
    }
  }
  pondere_totala=sum(pondere[noduri_selectate]);
  return(list(acoperire=noduri_selectate,pondere=pondere_totala))
}
pondere=c(s=1.0,x=2.5,w=1.0,v=1.5,u=1.5,z=2.5,y=1.5,r=2.0);
nod=names(pondere);
muchie=list(c("s","x"), c("s","w"),c("s","v"),c("x","w"),c("x","u"),c("w","u"),c("v","u"),c("v","z"),c("u","y"),c("z","y"),c("z","r"),c("y","r"));
rezultat=C1(noduri,pondere,muchie);
cat("Noduri selectate în acoperire:",rezultat$acoperire,"\n")
cat("Ponderea totală w(U):",rezultat$pondere,"\n")
cat(" \n");
#C2
cat("Exercitiul C2")
cat("\n")
C2=function(i,frunze) {
  lungime=length(frunze);
  total_noduri=(lungime-1)/2;
  if (i>total_noduri) {
    return(frunze[i-total_noduri]);
  }
  copii=sample(c(3*i-1,3*i,3*i + 1))
  valori=sapply(copii,function(j)C2(j,frunze))
  nivel=floor(log(i,base=3))
  if (nivel%%2==0){
    return(min(valori))
  } else {
    return(max(valori))
  }
}
eval_game_tree=function(frunze) {
  n=length(frunze);
  h=log(n,base=3)/2;
  if(h!=floor(h)) {
    stop("Numărul de frunze trebuie să fie 3^(2h) pentru un arbore triar complet de adâncime 2h.");
  }
  return(C2(1, frunze));
}
frunze=c(1, 0, 1, 1, 1, 0, 0, 1, 0);
rezultat_C2=eval_game_tree(frunze);
cat("Valoarea booleeană în rădăcină este:", rezultat_C2, "\n")
cat(" \n")
cat("Exercitiul C3")
cat("\n")
C3=function(matrice_adiacenta,iteratii=1000) {
  n=nrow(matrice_adiacenta)
  cea_mai_buna_taietura=0;
  cea_mai_buna_A=NULL;
  cea_mai_buna_B=NULL;
  for(i in 1:iteratii){
    A=which(runif(n) < 0.5);
    B=setdiff(1:n, A);
    marime_taietura=sum(matrice_adiacenta[A,B]);
    if(marime_taietura>cea_mai_buna_taietura){
      cea_mai_buna_taietura=marime_taietura;
      cea_mai_buna_A=A;
      cea_mai_buna_B=B;
    }
  }
  list(marime_taietura=cea_mai_buna_taietura,A=cea_mai_buna_A,B=cea_mai_buna_B);
}
creaza_graf=function(){
  matrice=matrix(0,6,6)
  muchii=rbind(c(1,2),c(1,3),c(1,4),c(1,5),c(1,6),c(2,3),c(2,4),c(2,5),c(2,6),c(3,4),c(3,5),c(3,6),c(4,5),c(4,6),c(5,6));
  for (i in 1:nrow(muchii)){
    matrice[muchii[i,1],muchii[i,2]]=1;
    matrice[muchii[i,2],muchii[i,1]]=1;
  }
  matrice
}
graful = creaza_graf()
rezultat = C3(graful)
cat("Cea mai buna taietura:",rezultat$marime_taietura,"\n")
cat("Multimea A:",paste(rezultat$A,collapse=","),"\n")
cat("Multimea B:",paste(rezultat$B,collapse=","),"\n")
cat("Total muchii:",sum(graful)/2,"\n")