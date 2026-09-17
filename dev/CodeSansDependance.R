
library(utils)

#setInternet2(TRUE)

cs<-function(x){
  
  return(   (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) )}
#########################		ROUND DES P.VALEURS		######### UTILE dans les codes de prog
rdpv<-function(x){
  if(!is.na(x)){
    if(x<0.01){res<-"<0.01"}
    if(x>=0.01){res<-round(x,3)}
  }
  return(res)}


IC.diff.prop<-function(x1,n1,x2,n2,alpha01=0.5,alpha02=0.5,beta01=0.5,beta02=0.5,val=0.95){

if( n1>1 & n2>1 & x1<=n1 & x2<=n2 ) {situation=1}else{situation=0}

if(situation==1){delta<-x2/n2-x1/n1
a1<-x1+alpha01
a2<-x2+alpha02
b1<-n1-x1+beta01
b2<-n2-x2+beta02
f<-function(t){
integrate(function(p1) { 
sapply(p1, function(p1) {p1^(a1-1)*(1-p1)^(b1-1)/beta(a1,b1)*integrate(function(p2) p2^(a2-1)*(1-p2)^(b2-1)/beta(a2,b2),0,t+p1)$value
})
},-t,1)$value
}
g<-function(t){
1-integrate(function(p1) { 
sapply(p1, function(p1) {p1^(a1-1)*(1-p1)^(b1-1)/beta(a1,b1)*(1-integrate(function(p2) p2^(a2-1)*(1-p2)^(b2-1)/beta(a2,b2),0,t+p1)$value)
})
},0,1-t)$value
}
ifelse(f(0)<(1-val)/2,Binf<-optimize(function(x) abs((1-val)/2-g(x)),lower=0,upper=abs(delta)+0.001,maximum=FALSE,tol=1e-16)$minimum
,Binf<-optimize(function(x) abs((1-val)/2-f(x)),lower=-1,upper=min(delta+0.001,0),maximum=FALSE,tol=1e-16)$minimum)

f<-function(t){
integrate(function(p1) { 
sapply(p1, function(p1) {p1^(a1-1)*(1-p1)^(b1-1)/beta(a1,b1)*integrate(function(p2) p2^(a2-1)*(1-p2)^(b2-1)/beta(a2,b2),0,t+p1)$value
})
},-t,1)$value
}
g<-function(t){
1-integrate(function(p1) { 
sapply(p1, function(p1) {p1^(a1-1)*(1-p1)^(b1-1)/beta(a1,b1)*(1-integrate(function(p2) p2^(a2-1)*(1-p2)^(b2-1)/beta(a2,b2),0,t+p1)$value)
})
},0,1-t)$value
}
ifelse(f(0)<(1+val)/2,Bsup<-optimize(function(x) abs((1+val)/2-g(x)),lower=0,upper=1,maximum=FALSE,tol=1e-16)$minimum,Bsup<-optimize(function(x) abs((1+val)/2-f(x)),lower=-1,upper=0,maximum=FALSE,tol=1e-16)$minimum)

BinfW<-x2/n2-x1/n1-qnorm((val+1)/2,0,1)*sqrt((x1/n1*(n1-x1)/n1)/n1+(x2/n2*(n2-x2)/n2)/n2)
BsupW<-x2/n2-x1/n1+qnorm((val+1)/2,0,1)*sqrt((x1/n1*(n1-x1)/n1)/n1+(x2/n2*(n2-x2)/n2)/n2)

BinfWCC<-x2/n2-x1/n1-qnorm((val+1)/2,0,1)*sqrt((x1/n1*(n1-x1)/n1)/n1+(x2/n2*(n2-x2)/n2)/n2)-(1/n1+1/n2)/2
BsupWCC<-x2/n2-x1/n1+qnorm((val+1)/2,0,1)*sqrt((x1/n1*(n1-x1)/n1)/n1+(x2/n2*(n2-x2)/n2)/n2)+(1/n1+1/n2)/2

BinfBapp<-a2/(a2+b2)-a1/(a1+b1)-qnorm((val+1)/2,0,1)*sqrt((a2/(a2+b2)*b2/(a2+b2))/(a2+b2)+(a1/(a1+b1)*b1/(a1+b1))/(a1+b1))
BsupBapp<-a2/(a2+b2)-a1/(a1+b1)+qnorm((val+1)/2,0,1)*sqrt((a2/(a2+b2)*b2/(a2+b2))/(a2+b2)+(a1/(a1+b1)*b1/(a1+b1))/(a1+b1))

M<-matrix(NA,nrow=4,ncol=2)
M[1,]<-c(Binf,Bsup)
M[2,]<-c(BinfW,BsupW)
M[3,]<-c(BinfWCC,BsupWCC)
M[4,]<-c(BinfBapp,BsupBapp)
colnames(M)<-c("B.inf","B.sup")
rownames(M)<-c("Bayésien","Wald","Wald.corr.conti","Bay.approche")

R<-list(Estimation=delta,IC=M)

} # fin if situation


if(situation!=1)
{
print("Conditions non vérifiées	 n1>1 	 n2>1 	 x1<=n1  x2<=n2")
R<-""
}
return(R)
}

###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#                       LOGIT							                                          #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################

# logit<-function(y,x,ic){
# print("Fonction temporairement desactivee pour modifications")
# }

logistique.variable.cont<-function(y,x,ic=FALSE,titre=""){
if(!require(pROC))install.packages('pROC'); library(pROC)
situation<-99
if(missing(x) | missing(y)) {situation=0}
if( nlevels(as.factor(x))==2    ) {situation=0;print("Variable binaire et non quantitative")}
if( nlevels(as.factor(y))!=2    ) {situation=0;print("Variable réponse non binaire")}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

if (situation==99){
m                       <-glm(y~x,family="binomial")
predictions				<-rep(NA,length(y))
predictions[!is.na(x)]	<-predict(m,type="response")
coefs                   <-cbind(summary(m)$coefficients[,1],confint(m),summary(m)$coefficients[,-1])
AIC                             <-summary(m)$aic
OR                              <-round(exp(coefs[,1:3]),3)
p.values                <-coefs[,6]
p.values                <-noquote(ifelse(p.values>0.01,round(p.values,2),"<0.01"))
coefs                   <-noquote(cbind(round(coefs[,1:5],3),p.values))
rocobj                  <- plot.roc(y,predictions,main=titre, percent=TRUE,ci=TRUE,print.auc=TRUE)                                         
roc1                    <- roc(y~x)
roc2                    <- roc(y, rep(1,length(y)))
p.AUC                   <- roc.test(roc1, roc2)$p.value
AUC                             <-c(    round(rocobj$ci[1],2),
                                round(rocobj$ci[2],2),
                                round(rocobj$ci[3],2),
                                ifelse(p.AUC>=0.01,round(p.AUC,2),"<0.01"),
                                ifelse(p.AUC<0.05,"***",""))

##      INT CONF        ##
if(ic==TRUE){
ciobj           <- ci.se(rocobj, specificities=seq(0, 100, 5))
plot(ciobj, type="shape", col="#1c61b6AA") # plot as a blue shape
}
##      PERFORMANCE     ##
ci.thresholds(rocobj)
optimums                <-ci(rocobj, of="thresholds", thresholds="best")
plot(optimums) 
best.cut                <-as.numeric(rownames(round(optimums$sensitivity,2)))
best.sen                <-c(round(optimums$sensitivity,2),"","")
best.spe                <-c(round(optimums$specificity,2),"","")

y2                              <-y
x2                              <-ifelse(predictions>best.cut,1,0)
T                               <-table(x2,y2)
VP                              <-T[2,2]
VN                              <-T[1,1]
FP                              <-T[2,1]
FN                              <-T[1,2]
V1                              <-cbind(VP,VN,FP,FN);colnames(V1)=c("VP","VN","FP","FN")

VPP                             <-round(VP/(VP+FP),2)
VPN                             <-round(VN/(VN+FN),2)
Exact                   <-round((VP+VN)/(VP+VN+FP+FN),2)
Erreur                  <-round((FP+FN)/(VP+VN+FP+FN),2)
V2                              <-cbind(VPP,VPN,Exact,Erreur);colnames(V2)=c("VPP","VPN","Exactitude","Taux d'erreur")

        
####    RESULTATS       ####
SeSp                    <-noquote(rbind(best.sen,best.spe,AUC))
rownames(SeSp)  =c("Sensibilité","Spécificité","AUC")
colnames(SeSp)  =c("2.5%","Val","97.5%","p.val","")
R                               <-list(coefs=coefs,OR=OR,seuil=paste(round(best.cut,2),"(Sur predictions)"),Performance=SeSp,Valeurs=V1,Valeurs2=V2)
return(R)}
}
logistique.variable.bin<-function(y,x){
situation=99
if(nlevels(as.factor(x))!=2){situation=0;print("Variable non binaire")}
if(nlevels(as.factor(y))!=2){situation=0;print("Variable non binaire")}

if(situation==99){
m				<-glm(y~as.factor(x),family="binomial")
coefs			<-cbind(summary(m)$coefficients[,1],confint(m),summary(m)$coefficients[,-1])
AIC				<-summary(m)$aic
OR				<-round(exp(coefs[,1:3]),3)
p.values		<-round(coefs[,6],2)
p.values		<-noquote(ifelse(p.values>0.01,round(p.values,2),"<0.01"))
coefs			<-cbind(round(coefs[,1:5],3),p.values)

Response		<-y
T				<-table(x,Response)
VP				<-T[2,2]
VN				<-T[1,1]
FP				<-T[2,1]
FN				<-T[1,2]
V1				<-cbind(VP,VN,FP,FN);colnames(V1)=c("VP","VN","FP","FN")

Sens			<-VP/(VP+FN)
Spec			<-VN/(VN+FP)
VPP				<-VP/(VP+FP)
VPN				<-VN/(VN+FN)
Exact			<-(VP+VN)/(VP+VN+FP+FN)
Erreur			<-(FP+FN)/(VP+VN+FP+FN)
V2				<-round(cbind(Sens,Spec,VPP,VPN,Exact,Erreur),2)
colnames(V2)	=c("Sens","Spec","VPP","VPN","Exactitude","Taux d'erreur")

R<-list(Table=T,Valeurs=V1,Performance=V2,Regression.Logistique=coefs,OR=OR)
return(R)}
}
logit.ss.latex<-function(y,x,ic=FALSE,titreSL=""){
if(     nlevels(as.factor(y))!=2        )       {cas=0}
if(             all(levels(as.factor(as.numeric(y)))==c("0","1")) ) {y2<-y}
if(             all(levels(as.factor(as.numeric(y)))==c("1","2")) ) {y2<-as.numeric(y)-1}


if(     nlevels(as.factor(x))<2 )       {cas=0}
if(     nlevels(as.factor(x))==2        )       {cas=1}
if(     nlevels(as.factor(x))>2 )       {cas=2}

if(cas==1){return(logistique.variable.bin(y2,x))}
if(cas==2){return(logistique.variable.cont(y2,x,ic,titre=titreSL))}
}

logist<-function(y,x,ic=FALSE,latex=FALSE,titre=0){
 
 nomX<-deparse(substitute(x))
 nomY<-deparse(substitute(y))
 cat("--------------------------------------------------------------\n")
 cat("\nCriteres diagnostiques de la variable ",nomX, " sur la variable ",nomY ," \n") 
 cat("--------------------------------------------------------------\n\n")
 
if(titre==0){MainT<-paste("Courbe ROC :",deparse(substitute(y)),"~",deparse(substitute(x)))} else{MainT=titre}
	
if(latex==FALSE	){return(			logit.ss.latex(y,x,ic,titreSL=MainT))}
if(latex==TRUE	){              L<-	logit.ss.latex(y,x,ic,titreSL=MainT)
                                print(L)
                                Mat.performances<-matrix(as.numeric(as.character(L$Performance[,1:3])),ncol=3)
								#colnames(Mat.performances)<-c("2.5%","Valeur","97.5%")
								#rownames(Mat.performances)<-c("Sensibilite","Specificite",paste("AUC ","(p=",rdpv(as.numeric(L$Performance[3,4])),")",sep=""))
								Mat.Valeurs<-cbind(L$Valeurs,L$Valeurs2)
								rownames(Mat.Valeurs)<-c("")
								library(xtable)
								cat("---------------CODE LATEX-------------------------\n")
								cat("\\uline{\\textbf{Performances}}\\\\ \n~\\\\\n")
								print(xtable(Mat.performances))
								print(xtable(Mat.Valeurs))      
								cat("\n~\\\\\n \\uline{\\textbf{Courbe ROC}}\\\\ \n~\\\\\n")
								cat("\\begin{figure}[H]\n \\begin{center}\n  \\includegraphics[width=\\textwidth]{ROC.pdf} \n  \\end{center}\n  \\caption{Courbe ROC associee au modele logistique}\n  \\end{figure} \n")
					}
}


###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#             CS01 : transformer des variables pour les distrib beta	                                 #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################

cs01<-function(X,mini,maxi){
  if(missing(mini)|missing(maxi)){
    print("Erreur dans cs01")
    X_starstar<-"Precisez les minimums et maximums de votre score (cs01(Y,min,max))"
  }else{
    X_star	<-(X-mini)/(maxi-mini)
    X_starstar	<-(X_star*(length(X_star)-1)+0.5)/(length(X_star))
  }
  return(X_starstar)
}


###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#             TABLEAUX :   CROISEMENTS de deux variables qualitatives                                     #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################

tableaux<-function(X,Y,latex=0,p.val=T,entete=0,caption=0){
  
  options(warn=-pi)
  if(!is.data.frame(X)){
    nomX<-deparse(substitute(X))
    nomY<-deparse(substitute(Y))
    print(paste("Tableau Croise entre",deparse(substitute(X)),"(X) et",deparse(substitute(Y)),"(Y)"))
    print(addmargins(table(X,Y)))
    paste("\n","##---------------------##","\n")
    print("Proportions en ligne (en %)")
    x2                               <-round(addmargins(100*prop.table(addmargins(table(X,Y),1),1),2),2)
    rownames(x2)<-c(rownames(x2)[-length(rownames(x2))],"")
    print(x2)
    print("##---------------------##")
    print("Proportions en colonne (en %)")
    x3              <-round(addmargins(100*prop.table(addmargins(table(X,Y),2),2),1),2)
    colnames(x3)<-c(colnames(x3)[-length(colnames(x3))],"")
    print(x3)
    print("##---------------------##")
    print("Proportions du total (en %)")
    print(round(addmargins(prop.table(table(X,Y)))*100,2))
    print("##---------------------##")
    print("Valeurs theoriques")
    print(round(chisq.test(X,Y,correct = FALSE)$expected,2))
    print("##---------------------##")
    print(chisq.test(X,Y,correct = FALSE))
    print("##---------------------##")
    print(fisher.test(X,Y))
  }
  if(is.data.frame(X))
  {
    N.variables             <-dim(X)[2]
    for(i in 1:N.variables){if(nlevels(as.factor(X[,i]))>1){
      entete                  <-paste("Croisement de la variable",colnames(X)[i],"(ligne) et ",deparse(substitute(Y)),"(colonne)")
      tableaux(X[[i]],Y,latex=1,entete=0,caption=entete)}else{
        titre.unemod    <-paste(colnames(X)[i], " : Variable n'ayant qu'une seule modalite")
        cat("\n");cat(titre.unemod);cat("\n")}
    }
  }
}



###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#             DESCR1 :   Descriptif d'une variable quantitative                                           #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################

descr1<-function(Y,Tap=FALSE){
  if(Tap){res<-list(Descriptif=NULL,TestNormalite=NULL,Tap=NULL)}else{res<-list(Descriptif=NULL,TestNormalite=NULL)}
  nomY<-deparse(substitute(Y))
  library(moments)
  aze<-matrix(NA,ncol=1,nrow=24)
  rownames(aze)<-c("Effectifs presents","Proportions de presents %","Effectifs manquants","Proportions de manquants %",
                   "Moyenne","Ecart-type","Variance","Erreur standard (s.e.m)","Minimum","Maximum","Percentile 2,5","Percentile 5","Q1 ",
                   "Mediane","Q3","Percentile 95","Percentile 97,5","Ecart inter-quartiles","IC valeurs borne inf","IC valeurs borne sup",
                   "IC moyenne borne inf","IC moyenne borne sup","coefficient d'asymetrie","Kurtosis")
  
  
  colnames(aze)<-nomY ###,levels(X))
  qtl<-function(x){quantile(x,probs = c(0.025,0.05,0.25,0.5,0.75,0.95,0.975), na.rm = T)}
  nbm<-function(x){sum(is.na(x))}
  nbp<-function(x){sum(!is.na(x))}
  nbmpct<-function(x){sum(is.na(x))*100/length(x)}
  nbppct<-function(x){sum(!is.na(x))*100/length(x)}
  nbmqt<-sum(is.na(Y))
  ddl<-nbp(Y)-1
  errstm<-sd(Y,na.rm=TRUE)/sqrt(nbp(Y))
  liminfy<-mean(Y,na.rm=TRUE)-qt(0.975,ddl)*sd(Y,na.rm=TRUE)
  limsupy<-mean(Y,na.rm=TRUE)+qt(0.975,ddl)*sd(Y,na.rm=TRUE)
  liminfy2<-mean(Y,na.rm=TRUE)-qt(0.975,ddl)*sd(Y,na.rm=TRUE)/sqrt(ddl+1)
  limsupy2<-mean(Y,na.rm=TRUE)+qt(0.975,ddl)*sd(Y,na.rm=TRUE)/sqrt(ddl+1)
  asymetrie<-skewness(Y,na.rm=TRUE)
  kurt<-kurtosis(Y,na.rm=TRUE)
  aze[1,]<-nbp(Y)
  aze[2,]<-nbppct(Y)
  aze[3,]<-nbm(Y)
  aze[4,]<-nbmpct(Y)
  aze[5,]<-mean(Y,na.rm=TRUE)
  aze[6,]<-sd(Y,na.rm=TRUE)
  aze[7,]<-var(Y,na.rm=TRUE)
  aze[8,]<-errstm
  aze[9,]<-min(Y,na.rm=TRUE)
  aze[10,]<-max(Y,na.rm=TRUE)
  aze[11:17,1]<-qtl(Y)
  aze[18,]<-IQR(Y,na.rm=TRUE)
  aze[19,]<-liminfy
  aze[20,]<-limsupy
  aze[21,]<-liminfy2
  aze[22,]<-limsupy2
  aze[23,]<-asymetrie
  aze[24,]<-kurt
  aze<-round(aze,digits=4)
  
  pvalnorm<-matrix(c(NA,NA),ncol=1)
  rownames(pvalnorm)<-c("Test de normalite de Shapiro-Wilk       : p =","Test de normalite de Kolmogorov-Smirnov : p =")
  colnames(pvalnorm)<-c("")
  if(length(Y)<5000){pvalnorm[1]<-shapiro.test(Y)$p.value}else{pvalnorm[1]<-NA}
  pvalnorm[2]<-ks.test(Y,"pnorm",mean(Y,na.rm=T),sd(Y,na.rm=T))$p.value
  pvalnorm<-round(pvalnorm,digits=4)
  
  if(Tap){long<-length(table(Y))
  nbval<-sum(!is.na(Y))#length(Y)
  triap<-matrix(NA,ncol=4,nrow=long)
  rownames(triap)<-unique(sort(Y))
  colnames(triap)<-c("Eff.","Eff. cum.","Prop.","Prop. cum")
  triap[,1]<-table(Y)
  triap[,2]<-cumsum(table(Y))
  triap[,3]<-round(table(Y)*100/nbval,digits=2)
  triap[,4]<-round(cumsum(table(Y))*100/nbval,digits=2)
  res <- list(Descriptif = aze, TestNormalite = pvalnorm, Triaplat=triap )
  }
  if(!Tap){res <- list(Descriptif = aze, TestNormalite = pvalnorm)}
  return(res)
}

###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#             DESCR3 :   Croiser une variable qualitative avec une variable quantitative                  #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################

descr3<-function(Y,X,Tap=FALSE,nom=NULL, nomY =NULL, latex=0){
  
  if(Tap){    res<-list(Descriptif=NULL,TestNormalite=NULL,Testpv=NULL,TestsNPv=NULL,Tests_de_Student=NULL,TestsNP=NULL,Triaplat=NULL,CommqT=NULL)}else{
    res<-list(Descriptif=NULL,TestNormalite=NULL,Testpv=NULL,TestsNPv=NULL,Tests_de_Student=NULL,TestsNP=NULL)}
  if(is.null(nom)){nom<-deparse(substitute(X))}
  if(!is.factor(X)){X<-as.factor(X)}
  if(is.null(nomY)){nomY<-deparse(substitute(Y))}
  nbnv<-nlevels(X)
  library(moments)
  
  aze<-matrix(NA,ncol=1+nbnv,nrow=27)
  rownames(aze)<-c("Effectifs presents","Proportions de presents","Effectifs manquants","Proportions de manquants",
                   "Moyenne","Ecart-type","Variance","Erreur standard (s.e.m)","Err. Std (basee sur l'ANOVA)","Minimum","Maximum","Percentile 2,5","Percentile 5","Q1 ","Mediane","Q3",
                   "Percentile 95","Percentile 97,5","Ecart inter-quartiles","IC valeurs borne inf","IC valeurs borne sup","IC moyenne borne inf",
                   "IC moyenne borne sup","IC moyenne borne inf (ANOVA)","IC moyenne borne sup (ANOVA)","coefficient d'asymetrie","Kurtosis")
  
  
  colnames(aze)<-c(nomY,paste(" ",nom,"=",levels(X)))
  
  qtl<-function(x){quantile(x,probs = c(0.025,0.05,0.25,0.5,0.75,0.95,0.975), na.rm = T)}
  nbm<-function(x){sum(is.na(x))}
  nbp<-function(x){sum(!is.na(x))}
  nbmpct<-function(x){sum(is.na(x))*100/length(x)}
  nbppct<-function(x){sum(!is.na(x))*100/length(x)}
  nbmqt<-sum(is.na(Y))
  ddl<-length(Y)-1
  ddlg<-tapply(Y,X,nbp)-1
  aa<-summary(aov(Y~X))
  carmoy<-aa[[1]][2,3]
  ddlaov<-aa[[1]][2,1]
  nbvalg<-tapply(Y,X,nbp)
  aze[1,]<-c(nbp(Y),nbvalg)
  aze[2,]<-c(nbppct(Y),tapply(Y,X,nbppct))
  aze[3,]<-c(nbm(Y),tapply(Y,X,nbm))
  aze[4,]<-c(nbmpct(Y),tapply(Y,X,nbmpct))
  aze[5,]<-c(mean(Y,na.rm=TRUE),tapply(Y,X,mean,na.rm=TRUE))
  aze[6,]<-c(sd(Y,na.rm=TRUE),tapply(Y,X,sd,na.rm=T))
  aze[7,]<-c(var(Y,na.rm=TRUE),tapply(Y,X,var,na.rm=T))
  
  errstm<-sd(Y,na.rm=TRUE)/sqrt(ddl)
  errstmg<-tapply(Y,X,sd,na.rm=T)/sqrt(ddlg+1)
  aze[8,]<-c(errstm,errstmg)
  errstgr<-(carmoy/nbvalg)^0.5
  aze[9,]<-c(NA,errstgr)
  aze[10,]<-c(min(Y,na.rm=TRUE),tapply(Y,X,min,na.rm=T))
  aze[11,]<-c(max(Y,na.rm=TRUE),tapply(Y,X,max,na.rm=T))
  aze[12:18,1]<-qtl(Y)
  stock<-tapply(Y,X,qtl)
  for(i in 1:nbnv){aze[12:18,1+i]<-stock[[i]]}
  
  aze[19,]<-c(IQR(Y,na.rm=TRUE),tapply(Y,X,IQR,na.rm=T))
  
  liminfy<-mean(Y,na.rm=TRUE)-qt(0.975,ddl)*sd(Y,na.rm=TRUE)
  liminfyx<-tapply(Y,X,mean,na.rm=T)-tapply(Y,X,sd,na.rm=T)*qt(0.975,ddlg)
  limsupy<-mean(Y,na.rm=TRUE)+qt(0.975,ddl)*sd(Y,na.rm=TRUE)
  limsupyx<-tapply(Y,X,mean,na.rm=T)+tapply(Y,X,sd,na.rm=T)*qt(0.975,ddlg)
  aze[20,]<-c(liminfy,liminfyx)
  aze[21,]<-c(limsupy,limsupyx)
  
  liminfy2<-mean(Y,na.rm=TRUE)-qt(0.975,ddl)*sd(Y,na.rm=TRUE)/sqrt(ddl+1)
  liminfyx2<-tapply(Y,X,mean,na.rm=T)-tapply(Y,X,sd,na.rm=T)*qt(0.975,ddlg)/sqrt(ddlg+1)
  limsupy2<-mean(Y,na.rm=TRUE)+qt(0.975,ddl)*sd(Y,na.rm=TRUE)/sqrt(ddl+1)
  limsupyx2<-tapply(Y,X,mean,na.rm=T)+tapply(Y,X,sd,na.rm=T)*qt(0.975,ddlg)/sqrt(ddlg+1)
  aze[22,]<-c(liminfy2,liminfyx2)
  aze[23,]<-c(limsupy2,limsupyx2)
  
  liminfg<-tapply(Y,X,mean,na.rm=TRUE)-qt(0.975,df=ddlaov)*errstgr
  limsupg<-tapply(Y,X,mean,na.rm=TRUE)+qt(0.975,df=ddlaov)*errstgr
  
  aze[24,]<-c(NA,liminfg)
  aze[25,]<-c(NA,limsupg)
  
  aze[26,]<-c(skewness(Y,na.rm=TRUE),tapply(Y,X,skewness,na.rm=T))
  aze[27,]<-c(kurtosis(Y,na.rm=TRUE),tapply(Y,X,kurtosis,na.rm=T))
  
  
  if(nlevels(X)==2){pvaleur<-format.pval(wilcox.test(Y~X)$p.value,digits=4)}else{if(nlevels(X)>2){pvaleur<-format.pval(kruskal.test(Y~X)$p.value,digits=4)}}
  
  if(nlevels(X)==2){testnp<-paste("Test de Mann & Whitney : p =",pvaleur)}else{if(nlevels(X)>2){testnp<-paste("Test de Kruskal & Wallis : p =",pvaleur)}}
  
  pvalstud<-matrix(c(NA,NA),ncol=1)
  rownames(pvalstud)<-c("Test de Student, variances egales   : p =","Test de Student, variances inegales : p =")
  colnames(pvalstud)<-c("")
  
  if(nlevels(X)==2)
  {pvalstud[1]<-t.test(Y~X,var.equal = TRUE)$p.value}
  else
  {if(nlevels(X)>2){pval<-format.pval(summary(aov(Y~X))[[1]][1,5],digits=4)}}
  
  if(nlevels(X)==2)
  {pvalstud[2]<-t.test(Y~X,var.equal = FALSE)$p.value}
  else
  {if(nlevels(X)>2){pval<-format.pval(summary(aov(Y~X))[[1]][1,5],digits=4)}}
  
  
  if(nlevels(X)==2){testp<-round(pvalstud,digits=4)}else{if(nlevels(X)>2){testp<-paste("Analyse de la Variance : p =",pval)}}
  
  if(nlevels(X)==2){pvartest<-format.pval(var.test(Y~X)$p.value,digits=4)}else{if(nlevels(X)>2){pvartestpg<-format.pval(bartlett.test(Y~X)$p.value,digits=4)}}
  
  if(nlevels(X)==2){testpv<-paste(list(paste("Test parametrique d'egalite de deux variances (Fisher): p =",pvartest)
  ))
  }else{if(nlevels(X)>2){testpv<-paste("Test parametrique d'egalite de plus de deux variances (Bartlett) : p =",pvartestpg)}}
  
  pvalnorm<-matrix(c(NA,NA),ncol=1)
  rownames(pvalnorm)<-c("Test de normalite de Shapiro-Wilk       : p =","Test de normalite de Kolmogorov-Smirnov : p =")
  colnames(pvalnorm)<-c("")
  if(length(Y)<5000){pvalnorm[1]<-shapiro.test(Y)$p.value}else{pvalnorm[1]<-NA}
  pvalnorm[2]<-ks.test(Y,"pnorm",mean(Y,na.rm=T),sd(Y,na.rm=T))$p.value
  pvalnorm<-round(pvalnorm,digits=4)
  
  if(nlevels(X)==2){pvalfl2g<-format.pval(ansari.test(Y~X)$p.value,digits=4)}else{if(nlevels(X)>2){pvalfl3g<-format.pval(fligner.test(Y~X)$p.value,digits=4)}}
  
  if(nlevels(X)==2){testnpv<-paste(list(paste("Test non param. d'egalite de deux variances (Ansari) : p =",pvalfl2g)
  ))
  }else{if(nlevels(X)>2){testnpv<-paste("Test non param. d'egalite de plus de deux variances (Fligner) : p =",pvalfl3g)}}
  
  if(Tap){long<-length(table(Y))
  nbval<-sum(!is.na(Y))#length(Y)
  triap<-matrix(NA,ncol=5+nbnv,nrow=long)
  rownames(triap)<-unique(sort(Y))
  colnames(triap)<-c("Eff.","Eff. cum.","Prop.","Prop. cum",paste(" ",nom,"=",levels(X)),"Tot.Compl")
  triap[,1]<-table(Y)
  triap[,2]<-cumsum(table(Y))
  triap[,3]<-round(table(Y)*100/nbval,digits=2)
  triap[,4]<-round(cumsum(table(Y))*100/nbval,digits=2)
  triap[,5:(5+nbnv-1)]<-matrix(table(Y,X))
  triap[,(5+nbnv)]<-matrix(apply(table(Y,X),1,sum))
  }
  
  
  
  if(Tap){
    res$Descriptif<-round(aze,digits=3)
    res$TestNormalite<-pvalnorm
    res$Tests_de_Student<-testp
    res$TestsNP<-testnp
    res$Testpv<-testpv
    res$TestsNPv<-testnpv
    res$Triaplat<-triap
    if(sum(res$Triaplat[,1]!=res$Triaplat[,(5+nbnv)])){commqt<-paste("ATTENTION : Il y a des valeurs manquantes dans le tableau croise.");res$CommqT<-commqt}
    #res$CommqT<-commqt
  }
  
  if(!Tap){
    res$Descriptif<-round(aze,digits=3)
    res$TestNormalite<-pvalnorm
    res$Tests_de_Student<-testp
    res$TestsNP<-testnp
    res$Testpv<-testpv
    res$TestsNPv<-testnpv
  }
  
  if(latex==1){
    library(xtable)
    if(pvalnorm[1]>=0.05){pvalTEX<-testp[1]}else{pvalTEX<-as.numeric(gsub(".* ([0-9.]+).*", "\\1",testnp[1]))}
    cat(paste("Croisement de la variable", nomY, "en fonction de" ,nom))
    cat("\n")
    print(xtable(res$Descriptif[-c(8,9,19,20,21,24,25,26,27),],align="|r|rrr|"),table.placement="H",size="small")
    cat(paste("La p.valeur associee aux croisement de ces variables est de: "));cat(pvalTEX);cat("\n")
  }
  if(latex==0){return(res)}
}
###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#                       CROISEMENTS DE VARIABLES DANS UN JEU DE DONNEES                                   #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################
croisements<-function(numero.variable.dinteret,D,qualiF,quantiF,affichage=40){
  
  tableaux.croises<-1
  if(missing(quantiF)){nul<-0}else{if(length(quantiF)==1) {quantiF=c(quantiF,quantiF)}}
  if(missing(qualiF)){nul<-0}else{if(length(qualiF)==1) {qualiF=c(qualiF,qualiF)}}
  
  choix.de.sortie.R.1.LaTeX.2		            <-1		# Choisir le format d'affichage des resultats
  choix.entre.manuelle1.et.auto.2	            <-2		# Choisir la reconnaissance automatique ou specification manuelle de la nature des variables
  modalites.maximum				            <-7		# Modalites maximum des variables qualitatives (en automatique)
  
  ##     Variable d'INTERET                ###
  
  ny<-numero.variable.dinteret
  Y<-D[,ny]
  
  
  library(xtable)
  library(epitools)
  
  choix<-choix.entre.manuelle1.et.auto.2
  
  #*******\________Reconnaissance automatique_______/*******#
  
  mod.max<-modalites.maximum
  Factor<-VarNumeric<-0
  nbvar<-length(D)
  
  for(i in 1:nbvar)# Reconnaissance des facteurs et des numeriques
  { if(is.factor (D[,i])==TRUE){Factor    <-c(Factor,i        )}
    if(is.numeric(D[,i])==TRUE){VarNumeric<-c(VarNumeric,i    )}}
  Factor        <-Factor[2:(length(Factor))]
  VarNumeric    <-VarNumeric[2:(length(VarNumeric))]
  Factinfseuil  <-Factsupseuil<-0
  
  if(sum(Factor,na.rm=TRUE)!=0){
    for(i in 1:(length(Factor))){
      if(nlevels(D[,Factor][,i])<=mod.max){Factinfseuil<-c(Factinfseuil ,Factor[i])}
      if(nlevels(D[,Factor][,i])> mod.max){Factsupseuil<-c(Factsupseuil ,Factor[i])}}
    Factinfseuil<-Factinfseuil[2:length(Factinfseuil)]
    Factsupseuil<-Factsupseuil[2:length(Factsupseuil)]
  }
  
  NbNumeric<-length(D[,VarNumeric])
  quanti<-quali<-0
  for(i in 1:NbNumeric){
    if(nlevels(as.factor(D[,VarNumeric][,i]))>mod.max){quanti<-c(quanti,VarNumeric[i])}
    if(nlevels(as.factor(D[,VarNumeric][,i]))<=mod.max){quali<-c(quali ,VarNumeric[i])}}
  quanti<-quanti[2:(length(quanti))]
  quali<-quali[2:(length(quali))]
  
  situation<-0
  if(missing(quantiF))			# Distinguons volontairement le cas ou il n'y qu'une seule variable
  {nul=0}else{
    if(length(quantiF)==1){choix<-1;situation=5;quantitatives<-quantiF}
  }
  
  if(missing(qualiF))			# Distinguons volontairement le cas ou il n'y qu'une seule variable
  {nul=0}else{
    if(length(qualiF)==1){choix<-1;situation=6;qualitatives<-c(qualiF,qualiF)}
  }
  
  # Definition de 4 situations possibles:
  # 1 : Rien n'est specifie, tout est auto
  # 2 : juste quali  est specifie
  # 3 : juste quanti est specifie
  # 4 : quali et quanti sont specifies
  # 5 : une seule variable quantitative
  # 6 : une seule variable qualitative
  
  if(situation<5){
    if(missing(quantiF)& missing(qualiF )  ){choix<-2; situation<-1}
    else{
      if(missing(quantiF)){choix<-1; qualitativesM <-qualiF ; quantitativesM <--1;situation<-2 }
      else{
        if(missing(qualiF )){choix<-1; quantitativesM<-quantiF; qualitativesM  <--1; situation<-3 }
        else{choix<-1;quantitativesM<-quantiF;qualitativesM <-qualiF;situation<-4}
      }
    }
    
    if(choix==1){qualitatives<-qualitativesM;quantitatives<-quantitativesM}else{
      if(choix==2){qualitatives<-quali;quantitatives<-quanti}else{stop("Erreur de choix")}  }
    
    autorisation<-rep(0,(length(Factinfseuil)))
    for (i in 1:(length(Factinfseuil)))
      if((is.na(Factinfseuil[i])==0) &(Factinfseuil[i]!=0)){autorisation[i]<-1}
    if(situation==1){
      if((sum(autorisation))==(length(autorisation))){qualitatives<-c(qualitatives,Factinfseuil)}
    }
  }
  
  Affichage.Nombre.variables.quanti<-Affichage.Nombre.variables.quali<-0
  if(situation==1){Affichage.Nombre.variables.quali<-(length(qualitatives)-1)	   ;Affichage.Nombre.variables.quanti<-length(quantitatives)}
  if(situation==2){Affichage.Nombre.variables.quali<-(length(qualiF))		       ;Affichage.Nombre.variables.quanti<-0}
  if(situation==3){Affichage.Nombre.variables.quali<-0					       ;Affichage.Nombre.variables.quanti<-length(quantiF)}
  if(situation==4){Affichage.Nombre.variables.quali<-(length(qualiF))		       ;Affichage.Nombre.variables.quanti<-length(quantiF)}
  if(situation==5){Affichage.Nombre.variables.quali<-0					       ;Affichage.Nombre.variables.quanti<-1}
  if(situation==6){Affichage.Nombre.variables.quali<-1					       ;Affichage.Nombre.variables.quanti<-0}
  
  ##   CROISEMENTS ET TESTS                 ######
  
  options(width=400)
  if(tableaux.croises==1){
    if((situation==1)|(situation==2)|(situation==4)|(situation==6)){
      
      
      ################################        VARIABLES QUALITATIVES             ########################################
      
      
      cat(noquote(" _____________________________________________________________\n"))
      cat(noquote("/____________________________________________________________/|\n"))
      cat(noquote("|                                                            ||\n"))
      cat(noquote("|                                                            ||\n"))
      cat(noquote("|                                                            ||\n"))
      if(Affichage.Nombre.variables.quali>9){Affichage.espace.quali<-"               ||"}else{Affichage.espace.quali<-"              ||"}
      titre<-paste("|","              VARIABLES QUALITATIVES","(",Affichage.Nombre.variables.quali,")",Affichage.espace.quali)
      cat(noquote(titre));cat("\n")
      cat(noquote("|                                                            ||\n"))
      cat(noquote("|                                                            ||\n"))
      cat(noquote("|____________________________________________________________|/\n"))
      cat(noquote("\n"));
      
      
      for(i in 1:(length(qualitatives))) {if(colnames(D[,qualitatives])[i]==colnames(D)[ny]) {numero<-i} else{numero<-0}}
      
      for(i in 1:(length(qualitatives))) {  
        if(i!=numero){
          LTABLE      <-length(table(is.na(D[,qualitatives][,i])))
          if((LTABLE==1) & (is.na(D[,qualitatives][,i][1])==1)) {VIDE<-1}else{VIDE<-0}
          if(VIDE==0){
            A                    <-table(Y,D[,qualitatives][,i])                                                                                 ### Matrice des effectifs croises
            colnames(A)          <-c(paste(names(D[,qualitatives])[i],attr(table(D[,qualitatives][i]),"dimnames")[[1]]))
            rownames(A)          <-c(paste(names(D)[ny],attr(table(Y),"dimnames")$Y))
            longueur.A           <-sum(nchar(colnames(A)))                                                                   
            mat.separation.A     <-cbind(matrix("",ncol=(affichage-longueur.A),nrow=2),c("|","|"),c("",""))                                             ### De quoi faire de l'affichage propre
            Nbcolonnes	         <-length(colnames(A))
            Nblignes	         <-length(rownames(A))
            Alig                 <-A;for(k in 1:Nblignes){Alig[k,]<-round(A[k,]/sum(A[k,]),3)}                                                  ### Matrice des effectifs en ligne
            colnames(Alig)       <-c(paste(attr(table(D[,qualitatives][i]),"dimnames")[[1]],"%lign"))
            Acol                 <-A;for(m in 1:Nbcolonnes){Acol[,m]<-round(A[,m]/sum(A[,m]),3)}                                                ### Matrice des pourcentages en colonne
            colnames(Acol)       <-c(paste(attr(table(D[,qualitatives][i]),"dimnames")[[1]],"%col"))
            Atot                 <-round(A/sum(A),3);colnames(Atot)<-c(paste(attr(table(D[,qualitatives][i]),"dimnames")[[1]],"%tot"))          ### Matrice des pourcentages totaux
            colnames(Atot)       <-c(paste(attr(table(D[,qualitatives][i]),"dimnames")[[1]],"%tot"))
            Vecteur.separation   <-noquote(matrix(c("|","|"),nrow=2))
            Acomp                <-noquote(cbind(A,mat.separation.A,Alig,c(" "),Vecteur.separation,c(" "),Acol,c(" "),Vecteur.separation,c(" "),Atot))
            
            if(length(A)>4){Acomp<-list(Tableau=A,Pourcentages.lignes=Alig,Pourcentages.colonnes=Acol,Pourcentages.total=Atot)}                 ### Condition pour voir si c'est pas trop long
            ### Sinon reprendre dans une autre ligne
            p                    <-chisq.test(A)$p.value;p2<-try(fisher.test(A)$p.value,silent = TRUE)
            OR                   <-try(fisher.test(A)$estimate,silent=TRUE)
            if(length(A)==4){
              if((OR>=try(fisher.test(A)$conf.int[1],silent=TRUE))&(OR<=try(fisher.test(A)$conf.int[2],silent=TRUE)))
              {ORCI<-c(try(fisher.test(A)$conf.int[1],silent=TRUE),try(fisher.test(A)$conf.int[2],silent=TRUE))}
              if((OR<try(fisher.test(A)$conf.int[1],silent=TRUE))|(OR>try(fisher.test(A)$conf.int[2],silent=TRUE)))
              {ORCI<-unname(oddsratio(A)$measure[2,][2:3])}
              OR                   <-round(OR,digits<-3);ORCI<-round(ORCI,digits=3)
              conjonction<-"dans ["
            }
            if(length(A)!=4) {OR<-"Non_calculable";ORCI<-c("","");conjonction<-"Problème_Taille"}                                               ### Condition, si la matrice n'est pas carree
            ### Alors l'OR n'est pas defini.
            if(p<0.05 ){ p      <-paste(round(p ,2),"           ***")} else{p <-round(p,2 )}                                                    ### Affichage des etoiles si le test est signif
            if(is.numeric(p2)){if(p2<0.05 ){p2     <-paste(round(p2,2),"           ***")} else{p2<-round(p2,2)}}else{p2<-"NA"}
            
            if(choix.de.sortie.R.1.LaTeX.2==1){print(Acomp);                                                                                    ### Affichage des resultats des tests
              R                   <-noquote(paste("Test d'homogeneite du Chi-2       ",p))
              R3                  <-noquote(paste("Rapport de Cotes: ",OR,conjonction,ORCI[1],",",ORCI[2],"]"))
              R2                  <-noquote(paste("Test d'homogeneite Fisher exact   ",p2))
              R                   <-rbind(R,R2,R3)
              colnames(R)         <-c("")
              rownames(R)         <-c("","","")
              print(noquote(R))
              cat(noquote(" \n"));cat(noquote("____________________________________________________________\n"));cat(noquote(" \n"))}
            
            if(choix.de.sortie.R.1.LaTeX.2==2){print(xtable(A,caption=paste("Chi2",round(p,3),"/","Fisher",try(round(p2,3),silent=TRUE))))}
          }else{
            Affich<-c(names(D[,qualitatives])[i],"--> Vecteur de MANQUANTS")
            print("");print("");print("");print("");print("")
            print(Affich)
            print("__________________________________________________")
            print("");print("");print("");print("");print("")
          }}}}
    
    ################################        VARIABLES QUANTITATIVES            ########################################
    
    
    if(situation==5){quantitatives<-c(quantitatives,quantitatives)}
    nam<-names(D[,quantitatives])
    Nquant<-length(quantitatives)
    nbr.mod.rep<-length(attr(table(Y),"dimnames")$Y)
    modalites.rep<-as.numeric(attr(table(Y),"dimnames")$Y)
    moyenne <-variance<-mini<-NN<-NAA<-maxi<-mediane<-ecarttype<-quant025<-quant975<-rep(NA,nbr.mod.rep)
    if((situation==1)|(situation==3)|(situation==4)|(situation==5)){
      # Question d'affichage
      cat(noquote(" _____________________________________________________________\n"))
      cat(noquote("/____________________________________________________________/|\n"))
      cat(noquote("|                                                            ||\n"))
      cat(noquote("|                                                            ||\n"))
      cat(noquote("|                                                            ||\n"))
      if(Affichage.Nombre.variables.quanti>9){Affichage.espace<-"              ||"}else{Affichage.espace<-"             ||"}
      titre<-paste("|","              VARIABLES QUANTITATIVES","(",Affichage.Nombre.variables.quanti,")",Affichage.espace)
      cat(noquote(titre));cat(noquote("\n"))
      cat(noquote("|                                                            ||\n"))
      cat(noquote("|                                                            ||\n"))
      cat(noquote("|____________________________________________________________|/\n"))
      cat(noquote("\n"));
      borne<-Nquant
      if(situation==5){borne=Nquant-1}
      for(i in 1:borne)
      {
        for(j in 1:2)
        {
          NAA[j]      <-sum(is.na(        D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]]))
          NN[j]       <-length(           D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]])-NAA[j]
          moyenne[j]  <-round(mean(       D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]],na.rm=TRUE),3)                    # Calcul des statistiques desriptives appropriees
          mediane[j]  <-round(median(     D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]],na.rm=TRUE),3)
          variance[j] <-round(var (       D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]],na.rm=TRUE),3)
          ecarttype[j]<-round(sd (        D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]],na.rm=TRUE),3)
          mini[j]     <-round(min (       D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]],na.rm=TRUE),3)
          maxi[j]     <-round(max (       D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]],na.rm=TRUE),3)
          quant025[j] <-round(quantile (  D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]],na.rm=TRUE,prob=0.025),3)
          quant975[j] <-round(quantile (  D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]],na.rm=TRUE,prob=0.975),3)}
        A<-cbind(NN,round((NN/(sum(NN))),2),NAA,moyenne,variance,ecarttype,mini,maxi,mediane,quant025,quant975)
        colnames(A) <-c("N","% ","NA","moyenne","variance","sd","min","max","med","q_2.5%","q_97.5%")
        rownames(A) <-c(paste(names(D)[ny],attr(table(Y),"dimnames")$Y))
        LT1         <-length(table(is.na(D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[1]])))                                # Verifier la condition, l'un des deux vecteurs est nul
        LT2         <-length(table(is.na(D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[2]])))
        if(((LT2==1)&(is.na(D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[2]][1]==TRUE)))|((LT1==1)&(is.na(D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[1]][1]==TRUE))))
        {p<-"MANQUANTS";p2<-"MANQUANTS"}
        else{ 
          p           <-      try(t.test      (D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[1]],D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[2]])$p.value,silent=TRUE)
          p2          <-      try(wilcox.test (D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[1]],D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[2]])$p.value,silent=TRUE)
          p.norm.1    <-      try(shapiro.test(D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[1]])$p.value,silent=TRUE)           
          p.norm.2    <-      try(shapiro.test(D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[2]])$p.value,silent=TRUE)
          p3          <-round(try(var.test    (D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[1]],D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[2]])$p.value,silent=TRUE),2)
          p4          <-round(try(ansari.test (D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[1]],D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[2]])$p.value,silent=TRUE),2)
          
          
          
          if((p.norm.1>0.05) & (p.norm.2>0.05)){normalite<-"Acceptee 2/2"} 
          if((p.norm.1>0.05) & (p.norm.2<0.05)){normalite<-"Rejetee 1/2"} 
          if((p.norm.1<0.05) & (p.norm.2>0.05)){normalite<-"Rejetee 1/2"}
          if((p.norm.1<0.05) & (p.norm.2<0.05)){normalite<-"Rejetee 0/2"}}
        if(p<0.05 ){ p <-paste(round(p,2),"      ***")} else{p<-round(p,2)}
        if(p2<0.05 ){p2<-paste(round(p2,2),"      ***")}else{p2<-round(p2,2)}
        if(p3<0.05 ){p3<-paste(round(p3,2),"      '''")}else{p3<-round(p3,2)}
        if(p4<0.05 ){p4<-paste(round(p4,2),"      '''")}else{p4<-round(p4,2)}
        if((p!="MANQUANTS")&(p2!="MANQUANTS"))			{V0 =noquote(cbind(nam[i],"         ",normalite))}     # Affichage des resultats
        if((p!="MANQUANTS")&(p2!="MANQUANTS"))			{V  =noquote(rbind(paste("Egalite des moyennes : Test de Student    ",p),paste("Comparaison distributions: Test de MW.Wilcoxon",p2)))}else{V=cbind("Prob de manquants",p,"Prob de manquants",p2)}     # Affichage des resultats
        if((p!="MANQUANTS")&(p2!="MANQUANTS"))     		{V2 =noquote(rbind(paste("Egalite des variances: Test de Fisher     ",p3),        paste("Egalite des variances: Test de Ansari     ",p4)))}
        colnames(V0)=c("Variable","","Normalite echantillons")
        rownames(V0)=c("")
        V<-noquote(rbind(V,V2))
        colnames(V)<-c("")
        rownames(V)<-c("","","","")
        if(choix.de.sortie.R.1.LaTeX.2==1){print(A);print(V0); print(V);cat(noquote(" \n"));cat(noquote("____________________________________________________________\n"));cat(noquote(" \n"));cat(noquote(" \n"))}
        if(choix.de.sortie.R.1.LaTeX.2==2){print(xtable(A,caption=paste(nam[i],"/","t.test",round(p,2),"/","Wilcoxon",round(p2,2))))}
      }}
  }
  options(width=80)
  cat("**************************** FINI ************************************************\n")
}


###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#                       TEST DE HOSMER LEMESHOW                                                           #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################

HosLem<-function(modele){
  Y<-modele$y
  p0<-0.5
  p<-predict(modele,type="response")
  Y.hat<-ifelse(p>p0,1,0)
  bornes<-quantile(p,probs=seq(0,1,0.1))        # Decoupage en une dizaine de classes a partir des quantiles empiriques
  if(length(table(bornes))<10) print("Erreur, ex-aequo dans les quantiles, prob de decoupage en classes") 
  L<-length(table(bornes))-1                    # Preferez 0:10 a 1:11
  classes<-rep(0,length(p))                     # Creation d'un vecteur de classes 
  for(k in 1:L)                                 # Vecteur de classes 
  {
    bo<-bornes[k+1];
    for(j in 1:(length(p)))
      if((p[j]<=bornes[k+1]) & (p[j]> bornes[k])) {classes[j]<-k+1}
  }
  classes<-classes-1                            
  classes[classes==-1]<-1                       # Le premier Y est attribue a une classe seule -> Jonction
  donnees<-cbind(p,Y,Y.hat,classes)             # Lecture de nos donnees
  donnees<-donnees[order(donnees[,1]),]
  Obs<-matrix(99,L,2)                           # Creation des observes par classe
  for(k in 1:L)
  {
    Obs[k,1]<-   sum(Y[classes==k])
    Obs[k,2]<-length(Y[classes==k])-sum(Y[classes==k])
  }
  Exp<-matrix(99,L,2)                           # Creation des attendus par classe
  for(k in 1:L)
  {
    Exp[k,1]<-   sum(p[classes==k])
    Exp[k,2]<-length(p[classes==k])-sum(p[classes==k])
  }                                             # Statistique du Khi-2
  HR<-sum((Obs[,1]-Exp[,1])^2/(Exp[,1])+(Obs[,2]-Exp[,2])^2/(Exp[,2]))
  p.val<-1-pchisq(HR,(L-2))                     # P-valeur du test
  if(HR>qchisq(1-0.05,df=(L-2)))
  {Hosmer_LS="H1->Rejet"}else
  {Hosmer_LS="H0->Non-Rejet"}
  res<-list(Test=Hosmer_LS,p.valeur=p.val)
  return(res)
}

###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#                       COURBE ROC D'UNE REGRESSION LOGISTIQUE                                            #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################

Roc<-function(mod){
  library(Epi)
  ROC(fitted(mod,type="response"),mod$y,AUC=TRUE,main="Courbe ROC", MI=F)
}




###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#                       FONCTION DE DESCRIPTION d'UNE SEULE VARIABLE                                      #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################

desc1<-function(x,modmax=7){
  #modmax<-7  # passe en argument
  if(length(table(as.factor(x)))==1){res<-paste("Variable n'ayant qu'une seule modalite",levels(as.factor(x)),sep="  :  ")}else{
    if(is.character(x)){nature.variable<-0;res<-"Variable textuelle"}
    if(is.factor(x)){if(nlevels(x)<modmax){nature.variable<-2}else{nature.variable<-3;res<-"Facteur nombreuses modalites, a verifier manuellement"}}else{
      if(is.numeric(x))
      { if (nlevels(as.factor(x))<modmax) {nature.variable<-2} else{nature.variable<-1}}
      else{nature.variable<-0;print("Variable non numerique")     } }
    
    nature<-ifelse(nature.variable==1,"quantitative","qualitative")
    
    
    if(nature.variable==1)                                                              # Dans le cas d'une variable quantitative
    {
      effectif              <-noquote(c(length(x),length(x[!is.na(x)]),100*round(length(x[!is.na(x)])/length(x),2),round(sum(is.na(x)),2),100*round(sum(is.na(x))/length(x),2)))
      names(effectif)       <-c("N","N.presents","%presents","NA","%NA")
      x1                    <-x[!is.na(x)]
      stats                 <-round(c(mean(x1,na.rm=TRUE),var(x1,na.rm=TRUE),sd(x1,na.rm=TRUE),median(x1,na.rm=TRUE)),3)
      names(stats)          <-c("Moyenne","Variance","Ecart-type","Mediane")
      quantiles             <-round(quantile(x,probs=c(0.01,0.025,0.05,0.1,0.25,0.5),na.rm=TRUE),3)
      quantiles2            <-round(quantile(x,probs=c(0.75,0.9,0.95,0.975,0.99),na.rm=TRUE),3)
      min.max               <-round(c(min(x1,na.rm=TRUE),max(x1,na.rm=TRUE),max(x1,na.rm=TRUE)-min(x1,na.rm=TRUE)),3)
      names(min.max)        <-c("min","max","etendue")
      p.val.shapiro         <-round(shapiro.test(x)$p.value,4);norm<-ifelse((p.val.shapiro>0.05),"Non Rejet Normalite","Rejet Normalite")
      Shap                  <-c(p.val.shapiro,norm)
      names(Shap)           <-c("p.valeur","Hypothèse")
      res                   <-list(Effectifs=effectif,Type=nature,Stats=stats,Quantiles=quantiles,Quantiles=quantiles2,Extrêmes=min.max,Test_de_Shapiro_Wilk=Shap)
      #cat("\n")                                                                                                       # Affichage
      #cat("Variable",nature,"\n")
      #cat("\n")
      #cat("Effectifs","\n")
      #cat("\n")
      #cat("\t","N","\t","N.presents","\t","%presents","\t","NA","\t","%NA");cat("\n")
      #cat("\t",effectif[1],"\t",effectif[2],"\t","\t",effectif[3],"\t","\t",effectif[4],"\t",effectif[5],"\n")
      #cat("\n")
      #cat("Statistiques","\n")
      #cat("\n")
      #cat("\t","Moyenne","\t","Ecart-type","\t","Variance","\t","Mediane","\t");cat("\n")
      #cat("\t",stats[1],"\t",stats[2],"\t",stats[3],"\t","\t",stats[4],"\t","\n")
      #cat("\n")
      #cat("\t","2.5%","5.0%","10 %","25 %","50 %","75 %","90 %","95 %","97.5%");cat("\n")
      #cat("\t",quantiles[1],quantiles[2],quantiles[3],quantiles[4],quantiles[5],quantiles[6],quantiles[7],quantiles[8],quantiles[9])
      #cat("\n")
    }
    
    if(nature.variable==2)                                                                # Dans le cas d'une variable qualitative
    {
      effectif              <-noquote(c(length(x),length(x[!is.na(x)]),100*round(length(x[!is.na(x)])/length(x),2),round(sum(is.na(x)),2),100*round(sum(is.na(x))/length(x),2)))
      names(effectif)       <-c("N","Npres","%pres","NA","%NA")
      modalites             <-nlevels(as.factor(x))
      type                  <-paste(nature,"a",modalites,"modalites")
      tabl                  <-table(x)
      pour                  <-round(table(x)/length(x[!is.na(x)]),3)
      pourcumul             <-round(table(x)/length(x[!is.na(x)]),3);for(i in 2:length(pourcumul)){pourcumul[i]<-pourcumul[i-1]+pour[i]}
      IC.2.5<-IC.97.5       <-rep(0,modalites)
      for(i in 1:modalites){
        IC.2.5[i]             <- round(binom.test(tabl[i],sum(tabl))$conf.int[1],3)
        IC.97.5[i]            <- round(binom.test(tabl[i],sum(tabl))$conf.int[2],3)}
      T                     <-cbind(tabl,pour,pourcumul,IC.2.5,IC.97.5)
      
      colnames(T)           <-c("N","%","%cumul","IC_2.5","IC_97.5")
      res                   <-list(Effectifs=effectif,Type=type,Tableaux=T)
      #cat("\n")                                                                                                       # Affichage
      #cat("Variable",type,"\n")
      #cat("\n")
      #cat("Effectifs","\n")
      #cat("\n")
      #cat("\t","N","\t","N.presents","\t","%presents","\t","NA","\t","%NA");cat("\n")
      #cat("\t",effectif[1],"\t",effectif[2],"\t","\t",effectif[3],"\t","\t",effectif[4],"\t",effectif[5],"\n")
      #cat("\n")
      #cat("Tableaux","\n")
      #cat("\n")
      #cat("\t","N","\t","%","\t","%cumules","\t","ICinf","\t","ICsup","\n")
      #for(i in 1:length(T[,1])){
      #AF<-T[i,]
      #cat("\t",AF[1]);cat("\t");cat(AF[2]);cat("\t");cat(AF[3]);cat("\t","\t");cat(AF[4]);cat("\t");cat(AF[5]);cat("\n")
      #}
    }}
  return(res)
  #return(noquote(""))
  
} 


###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#                       DESCRIPTION DE CHACUNE DES VARIABLES D' UN JEU DE DONNEES                         #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################
desc.table<-function(D,modmax=7){
  Nbre.variables<-length(D)
  for (i in 1:(Nbre.variables))
  {
    nom<-colnames(D)[i]
    print("_____________________________________")
    print(noquote(c("","","","","","","","","","")))
    print(noquote(paste("","","","","","","","","","","","","","","","","","",nom,"","","","","","","","","")))
    print("_____________________________________")
    d<-desc1(D[,i],modmax=modmax)
    print(d)
  }
}


###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#                       FONCTION DE DESCRIPTION D'UNE VARIBABLE ou d'UN jeu de donnees                    #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################
desc<-function(D,modmax=7){
  
  if(is.data.frame(D))	{cas=1;desc.table(D,modmax=modmax)}	                             	# DATA.FRAME
  if(is.numeric(D))	  	{cas=2;r<-desc1(D,modmax=modmax);print(r)}	                        # VARIABLE NUMERIQUE ( Quali ou Quanti )
  if(is.factor(D))		  {cas=3;r<-desc1(D,modmax=modmax);print(r)}                        	# VARIABLE FACTEUR
  if(all(cas!=c(1,2,3))){print("Variable inconnue_ Verifiez la source")}
}



###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#                       COMPARER DEUX VARIABLES                                                           #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################

compare<-function(x,y,paired,modmax){	
  
  if(missing(modmax))  {mod.max<-7}    else{mod.max<-modmax}
  if(missing(paired))   {appa<-"FALSE"} else{appa<-paired}
  cas=2								                                                            # Reconnaitre la nature des variables 
  if(is.factor(x)  & is.factor (y)){cas=1}
  if(is.factor(x)  & is.numeric(y)){if(nlevels(as.factor(y))<=mod.max)    {cas=1}else{cas=2}}
  if(is.numeric(x) & is.factor (y)) {if(nlevels(as.factor(x))<=mod.max)    {cas=1}else{cas=2}}
  if(is.numeric(x) & is.numeric(y)){if(nlevels(as.factor(x))<=mod.max & nlevels(as.factor(y))<=mod.max) {cas=1}}
  if(is.numeric(x) & is.numeric(y)){if(nlevels(as.factor(x))<=mod.max & nlevels(as.factor(y))> mod.max) {cas=2}}
  if(is.numeric(x) & is.numeric(y)){if(nlevels(as.factor(x))> mod.max & nlevels(as.factor(y))<=mod.max) {cas=2}}
  if(is.numeric(x) & is.numeric(y)){if(nlevels(as.factor(x))> mod.max & nlevels(as.factor(y))> mod.max) {cas=3}}
  if(is.character(x)&is.character(y)){cas=1}
  if(is.character(x)&is.factor(y)){cas=1}
  if(is.factor(x)&is.character(y)){cas=1}
  
  
  if(cas==2){result<-"Erreur: Les variables cont de nature differentes, impossibilite de comparer"}
  
  
  ########################################################## PREMIER CAS : COMPARAISON DE DEUX VARIABLES CONTINUES ####################################
  if(cas==3){
    S	<-Q<-matrix(99,ncol=9,nrow=3)
    N   <-c(99,99,99)
    C	<-matrix(99,1,2)
    T   <-matrix(99,ncol=2,nrow=2)
    S1<-round(c(length(x),sum(is.na(x)),mean(x,na.rm=TRUE),var(x,na.rm=TRUE),sd(x,na.rm=TRUE),median(x,na.rm=TRUE),min(x,na.rm=TRUE),max(x,na.rm=TRUE),max(x,na.rm=TRUE)-min(x,na.rm=TRUE)),3)
    S2<-round(c(length(y),sum(is.na(y)),mean(y,na.rm=TRUE),var(y,na.rm=TRUE),sd(y,na.rm=TRUE),median(y,na.rm=TRUE),min(y,na.rm=TRUE),max(y,na.rm=TRUE),max(y,na.rm=TRUE)-min(y,na.rm=TRUE)),3)
    S[1,]<-S1;S[2,]<-S2;S[3,]<-S[1,]-S[2,]
    colnames(S)=c("N","NA","Moyenne","Var","Sd","Med","Min","Max","Etendue")
    ifelse(is.null(names(x)),temp<-c("Variable_1","Variable_2","Delta"),temp<-c(names(x),names(y),"Delta"))
    rownames(S)=temp
    
    Q[1,]<-quantile(x,probs=c(0.01,0.025,0.05,0.1,0.5,0.9,0.95,0.975,0.99),na.rm=TRUE)
    Q[2,]<-quantile(y,probs=c(0.01,0.025,0.05,0.1,0.5,0.9,0.95,0.975,0.99),na.rm=TRUE)
    Q[3,]<-Q[1,]-Q[2,]
    colnames(Q)<-c("1%","2.5%","5%","10%","50%","90%","95%","97.5%","99%")
    rownames(Q)=temp
    
    C[1,1]<-ifelse(round(cor.test(x,y,type="p")$p.value,2)>0.01,round(cor.test(x,y,type="p")$p.value,2),"<0.01")
    C[1,2]<-ifelse(round(cor.test(x,y,type="s")$p.value,2)>0.01,round(cor.test(x,y,type="s")$p.value,2),"<0.01")
    colnames(C)<-c("Pearson","Spearman")
    rownames(C)<-c("")
    C<-noquote(C)
    
    N[1]<-round(shapiro.test(x)$p.value,2)
    N[2]<-round(shapiro.test(y)$p.value,2)
    N[3]<-paste(ifelse(N[1]>0.05,1,0)+ifelse(N[2]>0.05,1,0),"/ 2")
    names(N)<-c(paste("Shapiro",temp[1]),paste("Shapiro",temp[2]),"Total")
    N<-noquote(N)
    
    if(appa=="FALSE"){
      T[1,1]<-round(try(t.test(x,y)$p.value),2);T[1,2]<-round(try(wilcox.test(x,y)$p.value),2)
      ifelse(try(t.test(x,y)$p.value)>0.05,res.t.test<-"Ho",res.t.test<-"H1")
      ifelse(try(wilcox.test(x,y)$p.value)>0.05,res.wilcox<-"Ho",res.wilcox<-"H1")
      T[2,1]<-res.t.test;T[2,2]<-res.wilcox
      colnames(T)<-c("t.test","wilcoxon")
      rownames(T)<-c("p.valeur","Hyp")
    }
    T<-noquote(T)
    
    if(appa=="TRUE"){
      T[1,1]<-round(try(t.test(x,y,paired=TRUE)$p.value),2);T[1,2]<-round(try(wilcox.test(x,y,paired=TRUE)$p.value),2)
      ifelse(try(t.test(x,y,paired=TRUE)$p.value)>0.05,res.t.test<-"Ho",res.t.test<-"H1")
      ifelse(try(wilcox.test(x,y,paired=TRUE)$p.value)>0.05,res.wilcox<-"Ho",res.wilcox<-"H1")
      T[2,1]<-res.t.test;T[2,2]<-res.wilcox
      colnames(T)<-c("t.test","wilcoxon")
      rownames(T)<-c("p.valeur","Hyp")
    }
    result<-list(Stats=S,Quantile=Q,Correlation=C,Normalite=N,Comparaison=T)
  }
  
  
  ########################################################## DEUXIEME CAS : COMPARAISON DE DEUX FACTEURS       ####################################
  if(cas==1)	  		
  {
    x<-as.factor(x)
    y<-as.factor(y)
    if(length(intersect(levels(x),levels(y)))==0){result="Erreur : Les variables demandees n'ont aucune modalite en commun."}
    else{
      cas=3
      Nombre.mod.x<-nlevels(x)
      Nombre.mod.y<-nlevels(y)
      
      D<-matrix(99,nrow=2,ncol=2)
      D[1,1]<-length(x);D[1,2]<-length(y)
      D[2,1]<-sum(is.na(x));D[2,2]<-sum(is.na(y))
      colnames(D)<-c("N1","N2")
      rownames(D)<-c("N","NA")
      
      max.lev<-levels(as.factor(c(as.vector(x),as.vector(y))))
      Tx<-Ty<-matrix(0,ncol=2,nrow=length(max.lev))
      tablx<-table(x);pourx<-round(table(x)/length(x[is.na(x)==F]),3)
      for(i in 1:length(max.lev)) {try(Tx[i,1]<-tablx[names(tablx)==max.lev[i]],silent=TRUE)}
      for(i in 1:length(max.lev)) {try(Tx[i,2]<-pourx[names(tablx)==max.lev[i]],silent=TRUE)}
      tably<-table(y);poury<-round(table(y)/length(y[is.na(y)==F]),3)
      for(i in 1:length(max.lev)) {try(Ty[i,1]<-tably[names(tably)==max.lev[i]],silent=TRUE)}
      for(i in 1:length(max.lev)) {try(Ty[i,2]<-poury[names(tably)==max.lev[i]],silent=TRUE)}
      Tdelta<-round(Tx-Ty,3);
      Tx.Ty<-
        noquote(
          cbind(
            Tx,
            rep("  |  ",length(max.lev)),
            Ty,
            rep("  |  ",length(max.lev)),
            Tdelta
          )
        )
      
      colnames(Tx.Ty)<-c("N_1"," %","","N_2"," %","","DELTA","%")
      rownames(Tx.Ty)<-max.lev
      
      Txy<-table(x,y)
      p.val.khi2<-try(chisq.test(Txy)$p.value,silent=TRUE)
      p.val.Mcnem<-try(mcnemar.test(as.matrix(table(x,y)))$p.value,silent=TRUE)
      p.val.fish<-try(fisher.test(Txy)$p.value,silent=TRUE)
      if(appa=="FALSE"){
        T<-matrix(99,ncol=2,nrow=2)
        T[1,]<-round(cbind(p.val.khi2,p.val.fish),2)
        colnames(T)<-c("Chi2","F.exact")
        rownames(T)<-c("p.val","Hyp")
        ifelse(p.val.khi2>0.05,T[2,1]<-"Ho",T[2,1]<-"H1")
        ifelse(p.val.fish>0.05,T[2,2]<-"Ho",T[2,1]<-"H1")
        T<-noquote(T)
      }
      else{T<-matrix(99,ncol=1,nrow=2)
      T[1,]<-round(p.val.Mcnem,2)
      colnames(T)<-c("McNemar")
      rownames(T)<-c("p.val","Hyp")
      ifelse(p.val.Mcnem>0.05,T[2,1]<-"Ho",T[2,1]<-"H1")
      T<-noquote(T)
      }
      result<-list(Effectifs=D,Descriptif=Tx.Ty,Tableau_croise=Txy,Tests=T)
    }
  }# fin si 
  if((cas!=1) & (cas!=2) & (cas!=3) ){result<-"Erreur: Le type de variable n'est pas reconnu, merci de votre comprehension"}
  print(result)
  
}# fin fonction


###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#                       DECRIRE UN JEU DE DONNEES ( GENERALITES )                                           #
#                                                                                                            #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################

 descd<-function(D){
  
  
  if(is.data.frame(D)){situation=2}else{
    if(is.matrix(D))    {situation=1}else{situation=0}}
  if(situation==2 | situation==1) {
    n.lignes		<-dim(D)[1]
    n.col			<-dim(D)[2]
    N			    <-n.lignes*n.col
    #---------------------------------------------
    # Calcul du nombre de variables quanti et quali
    n.quanti<-n.quali<-n.txt<-n.autres<-0
    for(i in 1:n.col){
      x                   <-D[,i]
      modmax              <-7
      if(length(table(as.factor(x)))==1){nature.variable=-1}else{
        if(is.character(x)){nature.variable<-0;res<-"Variable textuelle"}
        if(is.factor(x)){
        if(nlevels(x)<modmax){nature.variable<-2}else{nature.variable<-3;res<-"Facteur nombreuses modalites, a verifier manuellement"}}else{
          if(is.numeric(x))
                { if (nlevels(as.factor(x))<modmax) {nature.variable<-2} else{nature.variable<-1}}
          else{nature.variable<-0     } }
      }
      if(nature.variable==1)                          {n.quanti   =n.quanti+1}
      if(nature.variable==2)                          {n.quali    =n.quali+1}}
    if(nature.variable==0)                          {n.txt      =n.txt+1}
    if (nature.variable==-1 | nature.variable==3)   {n.autres   =n.autres+1}}
  
  #----------------------------------------------------
  C.vide <- apply(D,2,function(x)all(is.na(x)))                                                                                     #colonnes vides
  lig.vides <- sum(apply(D,1,function(x)all(is.na(x))))                                                                     #nombre de lignes vides
  col.vides <- sum(C.vide)                                                                                                                           #nombre de colonnes vides
  dat.vides <- paste(sum(is.na(D)),"(",round(sum(is.na(D))/N*100,0),"%",")")
  sub.comp.lig <- dim(D[complete.cases(D[,-which(C.vide)]),])[1]                                            #nombre de lignes completes (sans les colonnes vides)
  sub.comp <- paste(sub.comp.lig," lignes, dans ", sum(!C.vide)," colonnes", sep="")
  
  #----------------------------------------------------
  
  RES1                    <-matrix(NA,nrow=3,ncol=1)
  RES1[1,1]               <-n.col
  RES1[2,1]               <-n.lignes
  RES1[3,1]               <-N
  colnames(RES1)          <-""
  rownames(RES1)          <-c("Nombre de colonnes","Nombre de lignes","Nombre de donnees")
  
  RES2                    <-matrix(NA,nrow=4,ncol=1)
  RES2[1,1]               <-col.vides
  RES2[2,1]               <-lig.vides
  RES2[3,1]               <-dat.vides
  RES2[4,1]               <-sub.comp
  RES2                    <-noquote(RES2)
  colnames(RES2)          <-""
  rownames(RES2)          <-c("Nombre de colonnes vides","Nombre de lignes vides","Nombre de donnees manquantes","Dimensions ss-groupe complet")
  
  RES3                    <-matrix(NA,nrow=4,ncol=1)
  RES3[1,1]               <-n.quanti
  RES3[2,1]               <-n.quali
  RES3[3,1]               <-n.txt
  RES3[4,1]               <-n.autres
  colnames(RES3)          <-""
  rownames(RES3)          <-c("Nombre de Var. Quantitatives","Nombre de Var. Qualitatives","Nombre de Var. Textuelles","Nombre de Var. Nature Autre")
  
  RES<-list(Dim=RES1,N.A.=RES2,Nature=RES3)
  
  if (situation==0) {RES<-"1 seule variable a decrire, utilisez desc()"}
  return(RES)
}


###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#                      TESTER LA NORMALITE D'UNE VARIABLE                                                 #
#                                                                                                         #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################

dessin.temporaire	<-function(x1){
  library(ggplot2)
  ggplot(data.frame(cbind(x1)), aes(x=x1,fill=as.factor(rep(1,length(x1))),colour=1)) + 
    geom_histogram(aes(y = ..density..),breaks=pretty(range(x1,na.rm=TRUE), n = nclass.Sturges(x1), min.n = 1))+
    scale_fill_manual( values = c("dodgerblue4"))+
    theme(legend.position = "none")+
    stat_function(fun = function(xyz) dnorm(xyz,mean=mean(x1,na.rm=TRUE),sd=sd(x1,na.rm=TRUE)), colour = "red")}
norm1				<-function(X,alpha=0.05,precision=5,aff=1,graph=1){
  if(!require(ggplot2))install.packages('ggplot2'); library(ggplot2)
  options(warn=-pi)
  A<-X[!is.na(X)]
  
  no  <-round(shapiro.test(A)$p.value,                precision)     
  noks.p<-ks.test(A,"pnorm",mean(A),sd(A))$p.value                                                                  # Normalite
  noks  <-round(noks.p,                precision)                                                           # Normalite Kolmogorov Smironov
  if(all(A>=0)){sq  <-round(shapiro.test(sqrt(A))$p.value,          precision)}else{sq<-"Valeurs negatives ou nulles"}                           # Transfo RACINE
  if(all(A>0)){lg  <-round(shapiro.test(log(A))$p.value,           precision)}else{lg<-"Valeurs negatives ou nulles"}                            # Transfor LOG
  if(all(A>=0)){ar  <-round(shapiro.test(asin(sqrt(A)/100))$p.value,precision)}else{ar<-"Valeurs negatives ou nulles"}                   # ARCSIN RACINE
  
  library(car)
  if(all(A>0)){Lambda  <-powerTransform(A)$lambda
  if(Lambda!=0){A.p     <-(A^Lambda-1)/Lambda}else{A.p=log(A)}
  bc      <-round(shapiro.test(A.p)$p.value,precision)}else{bc<-"Valeurs negatives ou nulles";Lambda=NA}
  RES     <-matrix(NA,ncol=2,nrow=6)
  RES[,1] <-c(no,noks,sq,lg,ar,bc)
  RES[,2][RES[,1]!="Valeurs negatives ou nulles"] <-ifelse(as.numeric(RES[,1][RES[,1]!="Valeurs negatives ou nulles"])>alpha,"OK","NON")
  RES[,2][RES[,1]=="Valeurs negatives ou nulles"] <-"NA"
  for(i in 1:length(RES[,1])){if(RES[i,1]=="Valeurs negatives" |RES[i,1]=="Valeurs negatives ou nulles" ){RES[i,2]<-"NA"}}
  
  
  colnames(RES)<-c("p.val","Hyp")
  rownames(RES)<-c("Normalite Shapiro-Wilk","Normalite Kolmogorov-Smirnov","Normalite_transformation.Racine","Normalite_transformation.Logarithme","Normalite_transformation.Arcsin.Racine",paste("Normalite_transformation.BoxCox_(lambda:",round(Lambda,2),")"))
  if(aff==1){return(noquote(RES))}
  if(aff==51)(print(as.numeric(RES[,1])))
  
}
norm				<-function(	x1,	x2=NULL,	x3=NULL,	x4=NULL,	x5=NULL,	x6=NULL,	x7=NULL,	x8=NULL,	x9=NULL,	x10=NULL,	x11=NULL,	x12=NULL,	x13=NULL,	x14=NULL,	x15=NULL){
  
  if(!is.null(x1) &  is.null(x2)){taille=1}
  if(!is.null(x1) & !is.null(x2) &  is.null(x3)){taille=2}
  if(!is.null(x1) & !is.null(x2) & !is.null(x3) &  is.null(x4)){taille=3}
  if(!is.null(x1) & !is.null(x2) & !is.null(x3) & !is.null(x4) &  is.null(x5)){taille=4}
  if(!is.null(x1) & !is.null(x2) & !is.null(x3) & !is.null(x4) & !is.null(x5) &  is.null(x6)){taille=5}
  if(!is.null(x1) & !is.null(x2) & !is.null(x3) & !is.null(x4) & !is.null(x5) & !is.null(x6) &  is.null(x7)){taille=6}
  if(!is.null(x1) & !is.null(x2) & !is.null(x3) & !is.null(x4) & !is.null(x5) & !is.null(x6) & !is.null(x7) &  is.null(x8)){taille=7}
  if(!is.null(x1) & !is.null(x2) & !is.null(x3) & !is.null(x4) & !is.null(x5) & !is.null(x6) & !is.null(x7) & !is.null(x8) &  is.null(x9)){taille=8}
  if(!is.null(x1) & !is.null(x2) & !is.null(x3) & !is.null(x4) & !is.null(x5) & !is.null(x6) & !is.null(x7) & !is.null(x8) & !is.null(x9) &  is.null(x10)){taille=9}
  if(!is.null(x1) & !is.null(x2) & !is.null(x3) & !is.null(x4) & !is.null(x5) & !is.null(x6) & !is.null(x7) & !is.null(x8) & !is.null(x9) & !is.null(x10) &  is.null(x11)){taille=10}
  if(!is.null(x1) & !is.null(x2) & !is.null(x3) & !is.null(x4) & !is.null(x5) & !is.null(x6) & !is.null(x7) & !is.null(x8) & !is.null(x9) & !is.null(x10) & !is.null(x11) &  is.null(x12)){taille=11}
  if(!is.null(x1) & !is.null(x2) & !is.null(x3) & !is.null(x4) & !is.null(x5) & !is.null(x6) & !is.null(x7) & !is.null(x8) & !is.null(x9) & !is.null(x10) & !is.null(x11) & !is.null(x12) & is.null(x13)){taille=12}
  if(!is.null(x1) & !is.null(x2) & !is.null(x3) & !is.null(x4) & !is.null(x5) & !is.null(x6) & !is.null(x7) & !is.null(x8) & !is.null(x9) & !is.null(x10) & !is.null(x11) & !is.null(x12) & !is.null(x13) &  is.null(x14)){taille=13}
  if(!is.null(x1) & !is.null(x2) & !is.null(x3) & !is.null(x4) & !is.null(x5) & !is.null(x6) & !is.null(x7) & !is.null(x8) & !is.null(x9) & !is.null(x10) & !is.null(x11) & !is.null(x12) & !is.null(x13) & !is.null(x14) &  is.null(x15)){taille=14}
  if(!is.null(x1) & !is.null(x2) & !is.null(x3) & !is.null(x4) & !is.null(x5) & !is.null(x6) & !is.null(x7) & !is.null(x8) & !is.null(x9) & !is.null(x10) & !is.null(x11) & !is.null(x12) & !is.null(x13) & !is.null(x14) & !is.null(x15)){taille=15}
  
  
  MV<-matrix(NA,nrow=100000,ncol=taille)
  
  if(taille==1){ MV[1:length(x1),1]<-x1}
  if(taille==2){ MV[1:length(x1),1]<-x1;MV[1:length(x2),2]<-x2}
  if(taille==3){ MV[1:length(x1),1]<-x1;MV[1:length(x2),2]<-x2; MV[1:length(x3),3]<-x3}
  if(taille==4){ MV[1:length(x1),1]<-x1;MV[1:length(x2),2]<-x2; MV[1:length(x3),3]<-x3; MV[1:length(x4),4]<-x4;}
  if(taille==5){ MV[1:length(x1),1]<-x1;MV[1:length(x2),2]<-x2; MV[1:length(x3),3]<-x3; MV[1:length(x4),4]<-x4; MV[1:length(x5),5]<-x5;}
  if(taille==6){ MV[1:length(x1),1]<-x1;MV[1:length(x2),2]<-x2; MV[1:length(x3),3]<-x3; MV[1:length(x4),4]<-x4; MV[1:length(x5),5]<-x5; MV[1:length(x6),6]<-x6;}
  if(taille==7){ MV[1:length(x1),1]<-x1;MV[1:length(x2),2]<-x2; MV[1:length(x3),3]<-x3; MV[1:length(x4),4]<-x4; MV[1:length(x5),5]<-x5; MV[1:length(x6),6]<-x6; MV[1:length(x7),7]<-x7;}
  if(taille==8){ MV[1:length(x1),1]<-x1;MV[1:length(x2),2]<-x2; MV[1:length(x3),3]<-x3; MV[1:length(x4),4]<-x4; MV[1:length(x5),5]<-x5; MV[1:length(x6),6]<-x6; MV[1:length(x7),7]<-x7; MV[1:length(x8),8]<-x8;}
  if(taille==9){ MV[1:length(x1),1]<-x1;MV[1:length(x2),2]<-x2; MV[1:length(x3),3]<-x3; MV[1:length(x4),4]<-x4; MV[1:length(x5),5]<-x5; MV[1:length(x6),6]<-x6; MV[1:length(x7),7]<-x7; MV[1:length(x8),8]<-x8; MV[1:length(x9),9]<-x9;}
  if(taille==10){MV[1:length(x1),1]<-x1;MV[1:length(x2),2]<-x2; MV[1:length(x3),3]<-x3; MV[1:length(x4),4]<-x4; MV[1:length(x5),5]<-x5; MV[1:length(x6),6]<-x6; MV[1:length(x7),7]<-x7; MV[1:length(x8),8]<-x8; MV[1:length(x9),9]<-x9; MV[1:length(x10),10]<-x10;}
  if(taille==11){MV[1:length(x1),1]<-x1;MV[1:length(x2),2]<-x2; MV[1:length(x3),3]<-x3; MV[1:length(x4),4]<-x4; MV[1:length(x5),5]<-x5; MV[1:length(x6),6]<-x6; MV[1:length(x7),7]<-x7; MV[1:length(x8),8]<-x8; MV[1:length(x9),9]<-x9; MV[1:length(x10),10]<-x10; MV[1:length(x11),11]<-x11;}
  if(taille==12){MV[1:length(x1),1]<-x1;MV[1:length(x2),2]<-x2; MV[1:length(x3),3]<-x3; MV[1:length(x4),4]<-x4; MV[1:length(x5),5]<-x5; MV[1:length(x6),6]<-x6; MV[1:length(x7),7]<-x7; MV[1:length(x8),8]<-x8; MV[1:length(x9),9]<-x9; MV[1:length(x10),10]<-x10; MV[1:length(x11),11]<-x11; MV[1:length(x12),12]<-x12;}
  if(taille==13){MV[1:length(x1),1]<-x1;MV[1:length(x2),2]<-x2; MV[1:length(x3),3]<-x3; MV[1:length(x4),4]<-x4; MV[1:length(x5),5]<-x5; MV[1:length(x6),6]<-x6; MV[1:length(x7),7]<-x7; MV[1:length(x8),8]<-x8; MV[1:length(x9),9]<-x9; MV[1:length(x10),10]<-x10; MV[1:length(x11),11]<-x11; MV[1:length(x12),12]<-x12; MV[1:length(x13),13]<-x13;}
  if(taille==14){MV[1:length(x1),1]<-x1;MV[1:length(x2),2]<-x2; MV[1:length(x3),3]<-x3; MV[1:length(x4),4]<-x4; MV[1:length(x5),5]<-x5; MV[1:length(x6),6]<-x6; MV[1:length(x7),7]<-x7; MV[1:length(x8),8]<-x8; MV[1:length(x9),9]<-x9; MV[1:length(x10),10]<-x10; MV[1:length(x11),11]<-x11; MV[1:length(x12),12]<-x12; MV[1:length(x13),13]<-x13; MV[1:length(x14),14]<-x14;}
  if(taille==15){MV[1:length(x1),1]<-x1;MV[1:length(x2),2]<-x2; MV[1:length(x3),3]<-x3; MV[1:length(x4),4]<-x4; MV[1:length(x5),5]<-x5; MV[1:length(x6),6]<-x6; MV[1:length(x7),7]<-x7; MV[1:length(x8),8]<-x8; MV[1:length(x9),9]<-x9; MV[1:length(x10),10]<-x10; MV[1:length(x11),11]<-x11; MV[1:length(x12),12]<-x12; MV[1:length(x13),13]<-x13; MV[1:length(x14),14]<-x14; MV[1:length(x15),15]<-x15}
  
  if(taille==1){
    print(norm1(x1))
    print(dessin.temporaire(x1))}
  
  
  
  
  
  
  if(taille>1)
  {
    x<-MV
    complet<-as.vector(x[!is.na(x)])
    library(car)
    if(all(complet>0)){Lambda  <-round(powerTransform(complet)$lambda,2)}
    Ncol=dim(x)[2]
    M<-MM<-matrix(NA,nrow=6,ncol=Ncol)
    C<-array(data = rep(NA,(6* 3* Ncol)), dim = c(6, 3, Ncol))
    M0<-norm1(complet,aff=51,graph=0)
    M00<-ifelse(M0<0.01,"<0.01",round(M0,2))
    C0<-cbind(M00,ifelse(M0<0.05,"NON","OK"),rep("|",length(M0)))
    r<-0
    repo<-1:6
    for(i in 1:Ncol){
      var<-x[,i]
      M[,i]<-norm1(var[!is.na(var)],aff=51,graph=0)
      MM[,i]<-ifelse(M[,i]<0.01,"<0.01",round(M[,i],2))
      C[,,i]<-cbind(MM[,i],ifelse(M[,i]<0.05,"NON","OK"),rep("|",length(M[,i])))
      repo<-cbind(repo,C[,,i])
      r<-c(r,paste("vecteur",i),"","")};
    r<-r[-1]
    repo<-repo[,-1]
    titre.temps<-c("Globale","","",r)
    
    RES<-noquote(cbind(C0,repo))
    rownames(RES)<-c("Normalite Shapiro-Wilk","Normalite Kolmogorov-Smirnov","Normalite transformation.Racine","Normalite transformation.Logarithme","Normalite transformation.Arcsin.Racine",paste("Normalite transformation.BoxCox (lambda:",round(Lambda,2),")"))
    colnames(RES)<-titre.temps
    
    if(Ncol<=4){
      par(mfrow=c(Ncol+1,2))
      hist(complet);qqnorm(complet);qqline(complet,col="red")
      for(i in 1:(Ncol)){hist(MV[,i][!is.na(MV[,i])],main=paste("Hist de vecteur",i));
        qqnorm(MV[,i][!is.na(MV[,i])])
        ;qqline(MV[,i][!is.na(MV[,i])],col="red")}
      par(mfrow=c(1,1))}else{par(mfrow=c(1,2));hist(complet);qqnorm(complet);qqline(complet,col="red")
        par(mfrow=c(1,1))
      }
    RES
  }
  
}



###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#         	cm		                          										  #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################

cm<-function(max=20,mode="multiplication",temps=30){
  
  
  compteur<-0
  
  LimiteTemps<-Sys.time()+temps
  if(LimiteTemps-Sys.time()<0){print("fin")}else{
    
    resultat<-0
    
    while(LimiteTemps-Sys.time()>0){
      compteur<-compteur+1
      print(LimiteTemps-Sys.time())
      set.seed(sample(1:1000))
      x<-sample(1:max,1)
      y<-sample(1:max,1)
      if(mode=="addition"			){res<-x+y}
      if(mode=="multiplication"	){res<-x*y}
      while(res!=resultat){
        if(mode=="addition"			){cat("\n",x,"+",y,"\n")}
        if(mode=="multiplication"	){cat("\n",x,"x",y,"\n")}
        resultat<-as.integer(readLines(n = 1))
      }
    }
  }
  
  cat("\n Resultats \n")
  cat("Plafond:",max,"\n")
  cat("Mode:",mode,"\n")
  cat("Temps:",temps,"\n\n")
  cat("\n Total",compteur, "\n")
}

###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                    
#                                                                                                         #
#                    desql : 												                              #
#                                                                                                         #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################


desql<-function(X){
  
  nomX<-deparse(substitute(X))
  cat("\nDescriptif de la variable : ",nomX, "  \n\n") 
  if(!is.factor(X)){X<-as.factor(X)}
  nbl<-nlevels(X)
  aze<-matrix(NA,ncol=2,nrow=nbl+3)
  rownames(aze)<-c(levels(X),"Total","Non Manquants","MANQUANTS")
  colnames(aze)<-c("Effectifs","Proportions")
  tcr<-table(X)
  aze[1:(nbl),1]<-table(X,exclude=NULL)[1:(nbl)]
  aze[1:nbl,2]<-round(tcr*100/sum(tcr),digits=3)
  aze[nbl+1,1]<-sum(tcr)
  aze[nbl+1,2]<-100
  nbmq<-table(X,exclude=NULL)[nbl+1]
  nbmq<- ifelse(is.na(nbmq), 0, nbmq)
  aze[nbl+3,1]<-nbmq
  aze[nbl+3,2]<-nbmq*100/length(X)
  aze[nbl+2,1]<-length(X)-aze[nbl+3,1]
  aze[nbl+2,2]<-100-aze[nbl+3,2]
  
  return(aze)
}

###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                    
#                                                                                                         #
#                   DIAGNOSTIQUER DES VALEURS EXTREMES                                                    #
#                                                                                                         #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################

extreme<-function (x, nlab = 2, labs = as.character(1:length(x)), ylab = "Sorted Data", ...) {
  
  if(!is.numeric(x)){print("Erreur: le vecteur n'est pas numerique")}else{
    
    
    
    
    #########	Creation du graphe	######################
    par(mfrow=c(1,2))
    hist(x,main="Histogramme")
    x2<-x
    x <- abs(x)
    labord <- order(x)
    x <- sort(x)
    i <- order(x)
    n <- length(x)
    ui <- qnorm((n + 1:n)/(2 * n + 1))
    plot(ui, x[i],main="Half-normal plot", xlab = "Half-normal quantiles", ylab = ylab, 
         ylim = c(0, max(x)), type = "n")
    if (nlab < n) 
      points(ui[1:(n - nlab)], x[i][1:(n - nlab)])
    text(ui[(n - nlab + 1):n], x[i][(n - nlab + 1):n], labs[labord][(n-nlab + 1):n])
    
    
    #########	Recup des valeurs extrêmes	###################
    
    val.extr<-as.numeric(labs[labord][(n-nlab + 1):n])
    VE<-cbind(val.extr,x[i][(n - nlab + 1):n])
    colnames(VE)=c("N°","Val")
    Moyenne.avec<-paste(" : ",round(mean(x,na.rm=TRUE),2),"( +-",round(sd(x,na.rm=TRUE)),")")
    Moyenne.sans<-paste(" : ",round(mean(x2[-val.extr],na.rm=TRUE),2),"( +-",round(sd(x2[-val.extr],na.rm=TRUE)),")")
    AUTRE<-rbind(Moyenne.avec,Moyenne.sans)
    
    Liste1<-list(v.ex=VE,desc=noquote(AUTRE))
    par(mfrow=c(1,1))
    return(Liste1)}
}

###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#          DIAGNOSTIC D'ADEQUATION A UNE DISTRI GAMMA								                      #
#                                                                                                         #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################

gamm<-function(x){
  
  library(MASS)
  library(car)
  fd	<-fitdistr(x[!is.na(x)],"gamma")
  shape.X	<-fd$estimate[1]
  rate.X 	<-fd$estimate[2]
  
  par(mfrow=c(2,2))
  
  hist(x,freq=FALSE,breaks=20)
  
  hist(x,freq=FALSE,breaks=20,border="white")
  lines(density(x[!is.na(x)],adjust=2),cex=2,col="red")
  curve(dgamma(x,shape.X,rate.X),0,50,xlab="x",ylab="Pdf",add=TRUE,col="blue")
  legend("topright", legend = c("Histogram", "Theorical Gamma"), col = c("red", "blue"), pch = 15, bty = "n", pt.cex = 2, cex = 0.8, text.col = "forestgreen",  inset = c(0.1, 0.1))
  
  qqPlot(x,distr="gamma",shape=shape.X)
  
  plot(c(0,0),col="white",bty="n",xaxt="n",yaxt="n",col.lab="white",main="Theorical Gamma : Max Lik")
  text(1.2,0.6,"Shape:",cex=1.5,col="blue")
  text(1.2,0.2  ,"Rate:",cex=1.5,col="blue")
  text(1.7,0.6,round(shape.X,2),cex=1.5,col="blue")
  text(1.7,0.2,round(rate.X,2),cex=1.5,col="blue")
  
  text(1.2,-0.5,"a:",cex=1.5,col="purple")
  text(1.2,-0.9  ,"b:",cex=1.5,col="purple")
  text(1.7,-0.5,round(shape.X,2),cex=1.5,col="purple")
  text(1.7,-0.9,round(1/rate.X,2),cex=1.5,col="purple")
  
  par(mfrow=c(1,1))
}


###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#         										GGSURV								                      #
#                                                                                                         #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################


ggsurv <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',                   cens.col = 'red', lty.est = 1, lty.ci = 2,                   cens.shape = 3, back.white = F, xlab = 'Time',                   ylab = 'Survival', main = ''){
  
  library(ggplot2)  
  strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)
  
  ggsurv.s <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'red', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival', main = ''){
    
    dat <- data.frame(time = c(0, s$time),
                      surv = c(1, s$surv),
                      up = c(1, s$upper),
                      low = c(1, s$lower),
                      cens = c(0, s$n.censor))
    dat.cens <- subset(dat, cens != 0)
    
    col <- ifelse(surv.col == 'gg.def', 'black', surv.col)
    
    pl <- ggplot(dat, aes(x = time, y = surv)) + 
      xlab(xlab) + ylab(ylab) + ggtitle(main) + 
      geom_step(col = col, lty = lty.est)
    
    pl <- if(CI == T | CI == 'def') {
      pl + geom_step(aes(y = up), color = col, lty = lty.ci) +
        geom_step(aes(y = low), color = col, lty = lty.ci)
    } else (pl)
    
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                      col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations') 
    } else(pl)
    
    pl <- if(back.white == T) {pl + theme_bw()
    } else (pl)
    pl
  }
  
  ggsurv.m <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'red', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival', main = '') {
    n <- s$strata
    
    groups <- factor(unlist(strsplit(names
                                     (s$strata), '='))[seq(2, 2*strata, by = 2)])
    gr.name <-  unlist(strsplit(names(s$strata), '='))[1]
    gr.df <- vector('list', strata)
    ind <- vector('list', strata)
    n.ind <- c(0,n); n.ind <- cumsum(n.ind)
    for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]
    
    for(i in 1:strata){
      gr.df[[i]] <- data.frame(
        time = c(0, s$time[ ind[[i]] ]),
        surv = c(1, s$surv[ ind[[i]] ]),
        up = c(1, s$upper[ ind[[i]] ]), 
        low = c(1, s$lower[ ind[[i]] ]),
        cens = c(0, s$n.censor[ ind[[i]] ]),
        group = rep(groups[i], n[i] + 1)) 
    }
    
    dat <- do.call(rbind, gr.df)
    dat.cens <- subset(dat, cens != 0)
    
    pl <- ggplot(dat, aes(x = time, y = surv, group = group)) + ylim(0,1)+
      xlab(xlab) + ylab(ylab) + ggtitle(main) + 
      geom_step(aes(col = group, lty = group))
    
    col <- if(length(surv.col == 1)){
      scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
    } else{
      scale_colour_manual(name = gr.name, values = surv.col)
    }
    
    pl <- if(surv.col[1] != 'gg.def'){
      pl + col
    } else {pl + scale_colour_discrete(name = gr.name)}
    
    line <- if(length(lty.est) == 1){
      scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
    } else {scale_linetype_manual(name = gr.name, values = lty.est)}
    
    pl <- pl + line
    
    pl <- if(CI == T) {
      if(length(surv.col) > 1 && length(lty.est) > 1){
        stop('Either surv.col or lty.est should be of length 1 in order
             to plot 95% CI with multiple strata')
      }else if((length(surv.col) > 1 | surv.col == 'gg.def')[1]){
        pl + geom_step(aes(y = up, color = group), lty = lty.ci) +
          geom_step(aes(y = low, color = group), lty = lty.ci)
      } else{pl +  geom_step(aes(y = up, lty = group), col = surv.col) +
          geom_step(aes(y = low,lty = group), col = surv.col)}   
    } else {pl}
    
    
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                      col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations') 
    } else(pl)
    
    pl <- if(back.white == T) {pl + theme_bw()
    } else (pl) 
    pl
  } 
  pl <- if(strata == 1) {ggsurv.s(s, CI , plot.cens, surv.col ,
                                  cens.col, lty.est, lty.ci,
                                  cens.shape, back.white, xlab,
                                  ylab, main) 
  } else {ggsurv.m(s, CI, plot.cens, surv.col ,
                   cens.col, lty.est, lty.ci,
                   cens.shape, back.white, xlab,
                   ylab, main)}
  pl
}


###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#         										PLOT.na								                      #
#                                                                                                         #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################


plot.na2<-function(D,cumul=0,latex=0){
  
  if(cumul==2){D<-t(D)}
  setInternet2(TRUE)
  if(!require(TraMineR))install.packages('TraMineR'); library(TraMineR)
  D2<-ifelse(is.na(D),"NA","Present")
  D2.alphab<-c("NA","Present")
  D2.seq <- seqdef(D2,  xtstep = 2, alphabet = D2.alphab)
  
  par(mfrow=c(2,1))
  seqplot(D2.seq, border = NA,type="I", withlegend = "right",space=0,cpal=c("red","blue"),title="Valeurs manquantes par variable",ylab="Sujets")                  # TAPIS
  if(cumul==1){
    seqdplot(D2.seq, border = NA, withlegend = "right",cpal=c("red","blue"),title="Valeurs manquantes par variable (cumulees)",ylab=" % Sujets" )                                                       # CMULÃ©
  }
  if(cumul==2){
    seqdplot(D2.seq, border = NA, withlegend = "right",cpal=c("red","blue"),title="Valeurs manquantes par sujet",ylab=" % Variable" )                                                       # CMULÃ©
  }
  cat("\n ")
  cat("\n ")
  cat("\n ")
  NbVariables<-dim(D)[2]
  matriceNA<-matrix(NA,nrow=NbVariables,ncol=3)
  for(i in 1:NbVariables){
    matriceNA[i,1]<-round(sum               (is.na(D[,i])))
    matriceNA[i,2]<-round(length            (is.na(D[,i])))
    matriceNA[i,3]<-round(sum               (100*is.na(D[,i]))/length       (is.na(D[,i])),2)
  }
  colnames(matriceNA)<-c("Nb.manquants","Nb.données","%")
  rownames(matriceNA)<-colnames(D)
  if(latex==1){return(xtable(matriceNA,digits=0))}else{return(matriceNA)}
}


plot.na <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 1, 0))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(       x = unlist(data_temp$x), 
                                 y = unlist(data_temp$y), 
                                 m = unlist(data_temp$m))
  
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + 
    scale_fill_manual(values=c("lightblue", "red"), 
                      name="Missing\n(1=Yes, 0=No)") + theme_light() + ylab("Variable") + xlab("Numero du sujet") + ggtitle(title)
}


###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#         										Correl							                      #
#                                                                                                         #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################



correl<-function(x,y,droite=1, nomx=NULL , nomy = NULL){
  
  
  options(warn=-1)
  if(!require(ggplot2))install.packages('ggplot2'); library(ggplot2)
  if(length(x)==length(y)){
    r<-rbind(       c(round(cor.test(x,y,method = "kendall") $estimate,3),cor.test(x,y,method = "kendall") $p.value),
                    c(round(cor.test(x,y,method = "pearson" )$estimate,3),cor.test(x,y,method = "pearson") $p.value),
                    c(round(cor.test(x,y,method = "spearman")$estimate,3),cor.test(x,y,method = "spearman")$p.value))
    
    r2                      <-cbind(r,ifelse(r[,2]<0.05,"H1","H0"))
    r2[1,2]         <-rdpv(r[1,2])
    r2[2,2]         <-rdpv(r[2,2])
    r2[3,2]         <-rdpv(r[3,2])
    rownames(r2)<-c("kendall","pearson","spearman")
    colnames(r2)<-c("Tau","P.val exacte","Hyp")
    DDD                     <-data.frame(cbind(y,x))
    c                       <- ggplot(DDD, aes(x, y))
    #x11()
    print(noquote(r2))
    if(is.null(nomx)){nomx<-deparse(substitute(x))}
    if(is.null(nomy)){nomy<-deparse(substitute(y))}
    if(droite==1){        return(c + stat_smooth(method = "lm") + geom_point()+ xlab(nomx)+ylab(nomy)) }
    else{                                   
      if(droite==0){        return(c +                              geom_point()+ xlab(nomx)+ylab(nomy)) }}
    if(droite!=0 & droite!=1){return("Mauvaise valeur pour l'option droite")}} else{   return("Les longueurs des vecteurs ne sont pas egales")}
  options(warn=0)                 
}


###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#         										Plot.EVOL						                      #
#                                                                                                         #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################

plot.evol2<-function(DT,groups,main,etyp=0) { 
  
  par.groupes<-0
  if(missing(main)){main=""}
  if(!missing(groups)){par.groupes=1}
  N  	<-dim(DT)[1]
  Nt	<-dim(DT)[2]
  noms<-colnames(DT)
  xmin<-0.5
  ymin<-0.9*min(DT,na.rm=TRUE)
  ymax<-1.1*max(DT,na.rm=TRUE)
  xmax<-Nt+0.5
  if(par.groupes!=1){
    plot(c(0,0),col="white",ylab="",xlab="",xaxt="n", xlim=c(xmin,xmax),ylim=c(ymin,ymax),main=main)
    
    axis(1,at=1:Nt,labels=noms)
    moyenne<-rep(NA,Nt)
    for(m in 1:Nt){moyenne[m]<-mean(DT[,m],na.rm=TRUE)}
    for(i in 1:N){
      v<-DT[i,]
      points(v,col=i);lines(v,col=i)
      points(moyenne,col="red",lwd=3)
      lines(moyenne,col="red",lwd=4)
    }
    legend("topleft", c("mean"), col = 2,
           text.col = "green4", lty = c(1),lwd=4,
           merge = TRUE, bg = "gray90")
  } 
  if(par.groupes==1){
    Ngroupes<-nlevels(as.factor(groups))
    plot(c(0,0),col="white",ylab="",xlab="",xaxt="n", xlim=c(xmin,xmax),ylim=c(ymin,ymax),main=main)
    axis(1,at=1:Nt,labels=noms)   
    M<-matrix(NA,nrow=nlevels(as.factor(groups)),ncol=Nt)
    for(c in 1:Nt){  M[,c]<-tapply(DT[,c],groups,mean,na.rm=TRUE) }   
    for(i in 1:Ngroupes){ v<-M[i,]; points(v,col=i+1);lines(v,col=i+1,lwd=4)}
    legend("topleft", as.character(levels(as.factor(groups))), col = 1:Ngroupes+1,
           text.col = "green4",lwd=4,
           merge = TRUE, bg = "gray90")
  }
  
  if(par.groupes==1 & etyp==1){ 
    
    Niveaux<-levels(as.factor(groups))
    if(length(Niveaux)==2){
      for(i in 1:Nt){
        s1<-c(mean(DT[,i][groups==Niveaux[1]],na.rm=TRUE)-1.96*sd(DT[,i][groups==Niveaux[1]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])),mean(DT[,i][groups==Niveaux[1]],na.rm=TRUE)+1.96*sd(DT[,i][groups==Niveaux[1]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])))
        s2<-c(mean(DT[,i][groups==Niveaux[2]],na.rm=TRUE)-1.96*sd(DT[,i][groups==Niveaux[2]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])),mean(DT[,i][groups==Niveaux[2]],na.rm=TRUE)+1.96*sd(DT[,i][groups==Niveaux[2]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])))
        segments(i-0.05,s1[1],i-0.05,s1[2],col="red",lwd=4)
        segments(i+0.05,s2[1],i+0.05,s2[2],col="green",lwd=4)
      }}
    if(length(Niveaux)==3){
      for(i in 1:Nt){
        s1<-c(mean(DT[,i][groups==Niveaux[1]],na.rm=TRUE)-1.96*sd(DT[,i][groups==Niveaux[1]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])),mean(DT[,i][groups==Niveaux[1]],na.rm=TRUE)+1.96*sd(DT[,i][groups==Niveaux[1]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])))
        s2<-c(mean(DT[,i][groups==Niveaux[2]],na.rm=TRUE)-1.96*sd(DT[,i][groups==Niveaux[2]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])),mean(DT[,i][groups==Niveaux[2]],na.rm=TRUE)+1.96*sd(DT[,i][groups==Niveaux[2]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])))
        s3<-c(mean(DT[,i][groups==Niveaux[3]],na.rm=TRUE)-1.96*sd(DT[,i][groups==Niveaux[3]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])),mean(DT[,i][groups==Niveaux[3]],na.rm=TRUE)+1.96*sd(DT[,i][groups==Niveaux[3]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])))
        
        segments(i-0.05,s1[1],i-0.05,s1[2],col="red",lwd=4)
        segments(i,s2[1],i,s2[2],col="green",lwd=4)
        segments(i+0.05,s3[1],i+0.05,s3[2],col="blue",lwd=4)
        
      }}
    
    if(length(Niveaux)>3){print("L'option etyp n'est pas utilisable pour plus de 3 groupes, au risque d'avoir un graphique trop charge")}
    
  }
  
  
  
}


########### Pr test	######################
#groupes<-c(1,1,1,1,2,2,2,2,3,3)
#x1	<-rnorm(10,8,1)
#x2	<-2*x1+rnorm(10,0,2)
#x3	<-x2-4+rnorm(10,0,3)
#DT	<-cbind(x1,x2,x3)
#plot.evol(cbind(x1,x2,x3))

plot.evol<-function(  Trajectoires,  Groupe=NULL,  IC = FALSE,  Moyenne = TRUE,  Temps = NULL,  label_x = "Temps",  label_y= "Valeur",  labels_ticks_x=NULL){
  
  
  
  
  # Verifications des arguments.
  # Trajectoires doit est une matrice de type int ou numeric
  if( !(
    is.matrix(Trajectoires) & 
    ( is.numeric(Trajectoires) | is.integer(Trajectoires)
    ))) stop("Trajectoire doit être une matrice numerique")
  
  nbvisites <- ncol(Trajectoires)
  nbpatients <- nrow(Trajectoires)
  
  # Temps doit avoir la même taille que le nombre de visites
  if(is.null(Temps)){
    Temps <- 1:nbvisites
  }else{
    if( !(length(Temps) == nbvisites & ( is.numeric(Temps) | is.integer(Temps)))) stop("Temps doit être un vecteur numerique de la même taille que le nombre de colonnes de la matrice")
  }
  
  
  
  # Transformer la matrice de large a long. TODO a ameliorer car cas particulier ici
  # En particulier, permettre que le jour de visite ne soit pas equidistant
  
  
  # Si pas de groupe specifie, attribuer le même groupe a toutes les lignes
  if(is.null(Groupe)){
    Groupe <- rep(nbpatients,1)
  }
  
  
  # Creer la dataframe en version "long" adaptee pour ggplot (une colonne par dimension)
  dfTraj<-data.frame(
    valeurs = as.numeric(as.matrix(Trajectoires)), 
    patient = rep(1:nbpatients, times=nbvisites), 
    visites = rep(Temps , each=nbpatients),
    groupe = as.factor(rep(Groupe, times=nbvisites))
  )
  
  # Verifier que ggplot present
  if(!require(ggplot2)){stop("ggplot est necessaire")}
  
  ggtrajectoire<-ggplot() +
    geom_line(data=dfTraj, aes(x=visites, y=valeurs, group=patient)) 
  
  if(Moyenne){
    # Si la moyenne, creer la df moyenne
    moyennes<-aggregate( formula= valeurs~visites+groupe, data=dfTraj, FUN=mean)
    
    # TODO : nettoyer le alpha
    
    # Si un seul groupe, laisser les trajectoires en noir
    if(length(unique(Groupe))>1){
      ggtrajectoire <- ggtrajectoire + aes(color=groupe) 
    }else{
      ggtrajectoire <- ggtrajectoire  + scale_color_discrete(guide="none")
    }
    # Plotter la moyenne en plus
    ggtrajectoire <- ggtrajectoire + 
      aes(alpha= 0.8) +
      geom_line(data=moyennes, aes(color=groupe, size=3, x=visites,y=valeurs,group=groupe, alpha=1 )) +
      scale_alpha(range=c(0.5,1),guide="none")
  }
  
  if(IC & Moyenne){
    bornes <- function(x){
      t.test(x)$conf.int
    }
    bornesIC<-aggregate( formula= valeurs~visites+groupe, data=dfTraj, FUN=bornes)
    
    # Attention! Matrice dans la dataframe 
    library(dfexplore)
    bornesIC<-expand_dfmatrix(bornesIC)
    names(bornesIC) <- c("visites", "groupe", "ICmin", "ICmax")
    ggtrajectoire <- ggtrajectoire + geom_errorbar(data=bornesIC, size=2, width=0.3,position="dodge" ,aes(x=visites, ymin=ICmin, ymax=ICmax, group=groupe,color=groupe, alpha=1))
  }
  
  if(is.null(labels_ticks_x)){labels_ticks_x<-as.character(Temps)}
  
  # Ajouter des elements pour faire joli
  ggtrajectoire <- ggtrajectoire +
    scale_x_continuous(breaks = Temps, labels=labels_ticks_x) +
    scale_size(guide="none") +
    xlab(label_x) +
    ylab(label_y)
  
  return(ggtrajectoire)
}



###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#         										lister les variables d'un jeu de donnees                  #
#                                 avec les numeros a côte		                                          #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################


names2<-function(D){
  n	<-length(D[1,])
  vec	<-1:n
  r	<-rep(NA,n)
  for(i in 1:n){
    r[i]	<-paste(names(D)[i],"(",vec[i],")")}
  return(noquote(r))
}

###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#         				BURTER UNE VARIABLE		en matrice disjonctive                                    #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################


burt<-function(x){
  nbre.colonnes	<-nlevels(as.factor(x))
  nbre.lignes		<-length(x)
  MatRES<-matrix(NA,nrow=nbre.lignes,ncol=nbre.colonnes)
  for(j in 1:nbre.colonnes){MatRES[,j]<-ifelse(as.factor(x)==levels(as.factor(x))[j],1,0  ) } 
  colnames(MatRES)<-levels(as.factor(x))
  return(MatRES)
}



###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#         				DESQLB			                          										  #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################


# avec inference bayesienne (en choisissant un alpha et un beta par niveaux)
desqlb<-function(X,alpha=rep(1,nlevels(X)),beta=rep(1,nlevels(X)),ic=0.95   ){
  if(!is.factor(X)){X<-as.factor(X)}
  nomX<-deparse(substitute(X))
  nbl<-nlevels(X)
  aze<-matrix(NA,ncol=5,nrow=nbl+3)
  rownames(aze)<-c(levels(X),"Total","Non Manquants","Manquants")
  colnames(aze)<-c("Effectifs","Proportions obs","Proportions est","Borne inf IC","Borne sup IC")
  tcr<-table(X)
  aze[1:nbl,1]<-table(X,exclude=NULL)[1:(nbl)]
  aze[1:nbl,2]<-round(tcr[1:(nbl)]*100/sum(tcr),digits=3)
  for(i in 1:nbl){aze[i,3]<-100*(aze[i,1]+alpha[i])/(sum(tcr)+alpha[i]+beta[i])}
  aze[nbl+1,1]<-sum(tcr)
  aze[nbl+1,2]<-100
  nbmq<-table(X,exclude=NULL)[nbl+1]
  aze[nbl+3,1]<-table(X,exclude=NULL)[nbl+1]
  aze[nbl+3,2]<-nbmq*100/length(X)
  aze[nbl+2,1]<-length(X)-aze[nbl+3,1]
  aze[nbl+2,2]<-100-aze[nbl+3,2]
  for(i in 1:nbl){aze[i,4]<-100*qbeta((1-ic)/2,aze[i,1]+alpha[i],sum(tcr)-aze[i,1]+beta[i])}
  for(i in 1:nbl){aze[i,5]<-100*qbeta(1-(1-ic)/2,aze[i,1]+alpha[i],sum(tcr)-aze[i,1]+beta[i])}
  return(aze)
}








###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#         			DputB				                          										  #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################




dputB<-function(x){
  
  
  nomX<-deparse(substitute(x))
  
  x<-as.numeric(x)
  
  if(length(x)==1){
    cat("\n\n Cette fonction ne traite pas les elements uniques \n\n")
  }
  
  if(is.vector(x) & length(x)>1){
    cat( "\nlist(\n\n",nomX,"=c(", paste(x,collapse=",")     ,")\n\n)\n\n" )
  }
  
  if(is.data.frame(x)|is.matrix(x)){
    
    
    sortie=NULL
    for(i in 1:dim(x)[2]){
      sortie[i]<-paste(colnames(x)[i],"=c(",paste(x[,i],collapse=","),")",sep="")
    }
    ttes_sorties<-paste(sortie,collapse=",")
    
    
    
    cat(	"\n\n\nlist(\n\n",nomX,"= structure(\n\t.Data=c(",		
         paste(	as.vector(t(x)) , collapse=","),
         "),\n\t.Dim=c(",
         paste(dim(x),collapse=","),
         ")\n\t)     \n)\n\n\n"
    )
  }
}




###########################################################################################################
###########################################################################################################
#*********************************************************************************************************#
#                                                                                                         #
#         		Fonctions Graphiques			                          										  #
#                                                                                                         #
#*********************************************************************************************************#
###########################################################################################################
###########################################################################################################



ggboxplot<-function(x,y,Groupe=NULL)
{
  
  nomx<-deparse(substitute(x))
  nomy<-deparse(substitute(y))
  if(is.null(Groupe)){DDD<-data.frame(cbind(x,y))}else{DDD<-data.frame(cbind(x,y,Groupe))}
  DDD<-DDD[complete.cases(DDD),]
  library(ggplot2)
  if(is.null(Groupe)){
    p <- ggplot(DDD,aes(factor(y),x)				)+xlab(nomy)+ylab(nomx)}else{
      p <- ggplot(DDD,aes(factor(y),x,fill=factor(Groupe))	)+xlab(nomy)+ylab(nomx)
    }
  titre<-paste(nomx,"en fonction de",nomy)
  return(p + geom_boxplot()  + ggtitle(titre)+ scale_x_discrete(labels=levels(y)))
}





ggcompar<-function(x,y, DDD= NULL)
{
if( is.null(DDD)) {  DDD<-as.data.frame(cbind(x,y)) 
}else{ DDD<- DDD[,c(x,y)]}
  titre<-paste("Distributions de",x,"en fonction de",y)
  DDD[,2]<- as.factor(DDD[,2])
  return(
    ggplot(DDD,aes(x=DDD[,1], fill=DDD[,2])) + geom_density(alpha=.3)+ ggtitle(titre)+xlab(x)+   guides(fill = guide_legend(title = y))
  )
}


ggpoints<-function(x,y,droite=0,nomx= NULL,nomy=NULL)
{
  
  options(warn=-1)
  if(is.null(nomx)){nomx<-deparse(substitute(x))}
  if( is.null(nomy) ){nomy<-deparse(substitute(y))}
  if(!require(ggplot2))install.packages('ggplot2'); library(ggplot2)
  if(length(x)==length(y)){
    DDD                     <- data.frame(cbind(y,x))
    c                       <- ggplot(DDD, aes(x, y))
    #x11()
    if(droite==1){        return(c + stat_smooth(method = "lm") + geom_point() +xlab(nomx)+ylab(nomy)) }
    else{  	if(droite==0){        return(c +                              geom_point() +xlab(nomx)+ylab(nomy)) }}
    if(droite!=0 & droite!=1){return("Mauvaise valeur pour l'option droite")}} else{   return("Les longueurs des vecteurs ne sont pas egales")}
  options(warn=0)                 
}






gghist<-function(x1,Groupe=NULL){
  
  library(ggplot2)
  
  if(is.null(Groupe)){
    nomx<-deparse(substitute(x1))
    ggplot(data.frame(cbind(x1)), aes(x=x1,fill=as.factor(rep(1,length(x1))),colour=1)) + 
      geom_histogram(aes(y = ..density..),breaks=pretty(range(x1,na.rm=TRUE), n = nclass.Sturges(x1), min.n = 1))+
      scale_fill_manual( values = c("dodgerblue4"))+
      theme(legend.position = "none")+xlab(nomx)
  }else{
    
    d2<-data.frame(x1,Groupe)
    ggplot(data=d2) +
      aes(x = x1, fill = Groupe) +
      geom_histogram(alpha = 1, position = "dodge",breaks=pretty(range(x1,na.rm=TRUE), n = nclass.Sturges(x1), min.n = 1) )
  }
}






ggsurvie<-function(x,y,groups=0,latex=0,titre=0)
{
  
  options(warn=-pi)
  if(length(groups)==1){
    
    if(groups==0 & latex==0){situation=1}			# Pas de GRP		# Pas de LTX
    if(groups==0 & latex!=0){situation=3}			# Pas de GRP		#        LTX
  }
  else{
    if(latex==0){situation=2}			# 	   GRP		# Pas de LTX
    if(latex!=0){situation=4}			# 	   GRP		#        LTX
  }
  if(!require(xtable))      install.packages('xtable')  ;    library(xtable)
  if(!require(survival))    install.packages('survival');    library(survival)
  
  
  nom.y<-deparse(substitute(y))
  if(titre==1){titre.ss.groupe<-paste("Courbe de survie: Variable",nom.y)}
  if(groups!=0){nom.groupes<-deparse(substitute(groups))
  if(titre==1){titre.ac.groupe<-paste("Courbe de survie: Variable",nom.y,"en fonction de:",nom.groupes)}}
  
  if(titre==0){titre.ss.groupe<-titre.ac.groupe<-""}
  if(titre!=0 & titre!=1){titre.ss.groupe<-titre.ac.groupe<-titre}
  
  if(situation==1){		surv2 		<- survfit(Surv(x,y) ~ 1)
  plot2 		<- ggsurv(surv2,main=titre.ss.groupe)+ylim(0,1)
  print(plot2+ylim(0,1))
  print(surv2)
  print(summary(surv2))
  }
  
  if(situation==2){		Groupe		<- groups
  surv2 		<- survfit(Surv(x,y) ~ Groupe)
  plot2 		<- ggsurv(surv2,main=titre.ac.groupe)+ylim(0,1)
  
  print(plot2+ylim(0,1))
  print(surv2)
  print(summary(surv2))
  print(survdiff(Surv(x,y) ~ groups))
  }
  
  if(situation==3){		surv2 		<- survfit(Surv(x,y) ~ 1)
  plot2 		<- ggsurv(surv2,main=titre.ss.groupe)+ylim(0,1)
  print(plot2+ylim(0,1))
  DETAIL		<-cbind(surv2 $ time , surv2 $ n.risk,  surv2 $ n.event,  surv2 $ n.censor, surv2$surv ,surv2 $ std.err  ,surv2 $ upper,surv2 $ lower   )
  colnames(DETAIL)	<-c("time","n.risk","n.event","n.censor","surv","std.err","upper95%","lower95%")
  print(xtable(DETAIL))
  }
  
  if(situation==4){		Groupe		<- groups
  surv2 		<- survfit(Surv(x,y) ~ Groupe)
  plot2 		<- ggsurv(surv2,main=titre.ac.groupe)+ylim(0,1)
  print(plot2+ylim(0,1))
  DETAIL		<-cbind(surv2 $ time , surv2 $ n.risk,  surv2 $ n.event,  surv2 $ n.censor, surv2$surv ,surv2 $ std.err  ,surv2 $ upper,surv2 $ lower   )
  colnames(DETAIL)	<-c("time","n.risk","n.event","n.censor","surv","std.err","upper95%","lower95%")
  ST			<-survdiff(Surv(x,y) ~ groups)
  df			<-nlevels(as.factor(groups))-1
  p.val			<-1-pchisq(ST$chisq,df)
  cat(paste("La p.valeur associee au test de comparaison des courbes de survie (Test du log-Rank) est de:",round(p.val,2)))
  cat(" \n")				
  print(xtable(DETAIL))			
  }
}


ggpie<-function(Valeur,Groupe="Graphique"){
  library(reshape)
  library(plyr)
  library(ggplot2)
  y  = data.frame(category=Groupe,
                  value=Valeur)
  # get counts and melt it
  data.m = melt(table(y))
  names(data.m)[3] = "count"
  # calculate percentage:
  m1 = ddply(data.m, .(category), summarize, ratio=count/sum(count))
  #order data frame (needed to comply with percentage column):
  m2 = data.m[order(data.m$category),]
  # combine them:
  mydf = data.frame(m2,ratio=m1$ratio)
  # get positions of percentage labels:
  mydf = ddply(mydf, .(category), transform, position = cumsum(ratio) - 0.5*ratio)
  # create bar plot
  colnames(mydf)[2]<-"Legende"
  mydf$Legende<-as.factor(mydf$Legende)
  pie = ggplot(mydf, aes(x = factor(1), y = ratio, fill = Legende)) +
    theme_void()+
    xlab("")+ylab("")+
    geom_bar(stat = "identity",width = 1) +
    
    facet_wrap(~category)
  # make a pie
  pie = pie + coord_polar(theta = "y")
  # add labels
  return(
    pie +
      geom_text(aes(label = sprintf("%1.2f%%", ratio*100), y =1- position))
  )
}

pie<-function(x,nomx,na.rm=FALSE){
  
  if(missing(nomx)) {nomx<-deparse(substitute(x))}
  if(na.rm==TRUE){x<-x[!is.na(x)]}
  
  
  if(sum(is.na(x))!=0){	levels(x)<-c(levels(x),"NA")
  x[is.na(x)]<-"NA"}
  
  effec           <-as.numeric(table(factor(as.character(x))))
  labls           <-names(table(factor(as.character(x))))
  zz                      <-data.frame(labls,effec)
  
  myTitle         <-paste("Proportions : Variable",nomx)
  percent <-paste(as.character(round(effec/sum(effec)*100,1)),"%")
  coord   <-effec/2 + c(0, cumsum(effec)[-length(effec)])
  
  g               <-ggplot(data.frame(labls,effec,coord,percent), aes(x="", y=effec, fill=labls)) + 
    geom_bar(width=1) + 
    coord_polar("y")+ 
    labs(x="")+labs(y="")+
    geom_text(aes(y =coord , label = percent), size=7) +
    guides( fill = guide_legend(label.position = "bottom",keywidth = 3, keyheight = 3,  title="",title.theme = element_text(size=rel(2),angle=0)))+
    theme(  legend.position="bottom",
            legend.text = element_text(colour = 'black', angle = 0, size = 13, hjust = 2, vjust = 2, face = 'bold'),
            plot.title = element_text(size = 20))+
    labs(title = myTitle,size=15)
  
  
  return(g)
}


ggbars<-function(Y,X="Variable"){
  
  nomX<-deparse(substitute(X))
  nomY<-deparse(substitute(Y))
  
  if(length(X)==1 & X[1]=="Variable"){X<-rep("",length(Y))}
  
  
  pc<-as.numeric(as.character(as.vector(prop.table(table(X,Y),1))))
  Groupe<-as.factor(rep(levels(as.factor(Y)),each=dim(prop.table(table(X,Y),1))[1]))
  G<-as.factor(rep(levels(as.factor(X)),dim(prop.table(table(X,Y),1))[2]))
  D<-data.frame(pc,Groupe,G)
  
  graph <- ggplot(data=D, aes(x=G, y=pc,fill=Groupe)) +                  
    geom_bar(stat="identity",position = "dodge",ymax=100) + xlab(nomX)+ ylab("%")+
    geom_text(aes(label =paste(round(pc*100,0),"%",sep=""),ymax=0),position=position_dodge(width=0.9), vjust=-0.25)
  
  return(graph) 
  
}
