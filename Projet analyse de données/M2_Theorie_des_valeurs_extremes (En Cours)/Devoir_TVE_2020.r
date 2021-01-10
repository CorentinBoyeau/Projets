
library(ggplot2)
library(lubridate)
library(fExtremes)
library(ismev)
library(ReIns)
library(VineCopula)
library(ggplot2)
library(minpack.lm)  #Méthode Levenberg Marquardt pour implémentation de l'algo GPD_G_GPD
library(copula)


# Importation des données
Data=read.table("https://query1.finance.yahoo.com/v7/finance/download/%5EGSPC?period1=536457600&period2=1604188800&interval=1d&events=history&includeAdjustedClose=true",dec=".",sep=",",header=TRUE)

#Conversion en format date

Data$Date=as.Date.character(Data$Date, format ="%Y-%m-%d")
#Data <- Data[(Data$Date >= "1987-01-02") & (Data$Date <= "2016-02-29"),]

# Définition des variables

Prices=Data$Open    # On s'intéresse à la variable Open pour l'étude
n=length(Prices)
x_date=Data$Date
LogReturns=log(Prices[2:n]/Prices[1:(n-1)])   # Variable LogReturns sur laquelle portera l'étude

########## ANALYSE DES LOG RETURNS

#Time Series des log returns

plot(x_date[-1], LogReturns, type="l", xlab= "Year", ylab="LogReturns", main="Daily log returns with Open value from 1987 to 2020")

#Summary des log returns

summary(LogReturns)


#Statistiques sur les log returns
mean_SP=mean(LogReturns)
var_SP=var(LogReturns)
min_SP=min(LogReturns)
Q1_SP=quantile(LogReturns, 0.25)
median_SP = median(LogReturns)
Q3_SP=quantile(LogReturns, 0.75)
max_SP = max(LogReturns)
skew_SP=skewness(LogReturns)
kurt_SP=kurtosis(LogReturns)

Conversion_pourcentage = c(mean_SP, var_SP, min_SP,Q1_SP, median_SP, Q3_SP, max_SP)
Conversion_pourcentage = paste0(as.character(round(Conversion_pourcentage*100,3)), "%")


Stat_SP=matrix(c(Conversion_pourcentage, round(skew_SP,3), round(kurt_SP,3)) ,9,1)
rownames(Stat_SP)=c("Moyenne", "Variance", "Minimum", "1er quartile", "médiane", "3eme quartile", "maximum","Skewness", "Kurtosis")
colnames(Stat_SP)=c("S&P LogReturns")
as.table(Stat_SP)


#Density log returns



ggplot(data=as.data.frame(LogReturns), aes(x=-LogReturns))+
        geom_density(aes(color="LogReturns"))+
        stat_function(fun=dnorm, args= list(mean=mean(LogReturns), sd=sd(LogReturns)), aes(color="Normal"))+
        xlab("x")+
        labs(title="Log Returns density vs Normal with same mean and sd")

########## BLOCK MAXIMA

num_an <- seq(1987, year(x_date[length(x_date)]),1) #package lubridate
tailleblock <- as.matrix(as.numeric(table(factor(year(x_date), levels = num_an))))
nbblock <- round(mean(tailleblock[-length(tailleblock),]))

        
loss <- -LogReturns
tailleblock[1] <- tailleblock[1]-1    #la première ligne du dataset n'a pas de logreturns


        
        
### A LA MAIN

BMax_hand=c()
taille = 0
for (i in 1:length(tailleblock)){
        BMax_hand <- append(BMax_hand, max( loss[ (taille+1):(taille+tailleblock[i]) ]))
        taille= taille + tailleblock[i]
}

# On fait le maximum sur chaque séquence

plot(num_an, BMax_hand, 
     type="h",
     col="light blue",
     ylab="maxValue", 
     xlab="Year",
     main="Block maxima on daily negative logreturns")

### AVEC PACKAGES

BMax <- blockMaxima(x=loss, block=nbblock)   #packages fExtremes
plot(num_an, BMax, 
     type="h",
     col="light blue",
     ylab="maxValue", 
     xlab="Year",
     main="Block maxima on daily negative logreturns")

gevLoss <- gev.fit(BMax)   #package ismev


#Graphes 

gev.diag(gevLoss)
par(mfrow=c(2,2))
plot(fgev(BMax))
dev.off()

# Parameters
ShapeParam = gevLoss$mle[3]
SE_Shape = gevLoss$se[3]

paste0("Shape parameter = ",ShapeParam)
paste0("Standard error = ",SE_Shape)

# Intervalle de confiance

inf = ShapeParam-1.96*SE_Shape
sup = ShapeParam+1.96*SE_Shape

paste0("IC shape param = " ,"[ ",inf,",",sup,"]")


######### MEP

# On trace le Mean Excess Plot

LR=sort(loss)
mePlot(LR,xlim=c(0,0.1))
abline(v=0.023)

# On choisit 0,023 puisque la courbe semble se stabiliser sur une fonction lin?aire juste apr?s

threshold=0.023
quantMEP=quantile(loss,seq(0,1,0.0001))
Nu=loss>0.023
sum(Nu==TRUE)

# Il s'agit du quantile d'ordre 97,50% de notre distribution.
# Nous avons 214 valeurs extr?mes au-dessus de ce seuil.

#On fit ensuite notre loi de GPD ? l'aide des estimateurs des moments :

Ybarre=mean((loss-threshold)[which(loss>=threshold)])
Sbarre=sd((loss-threshold)[which(loss>=threshold)])

ksi=0.5*(1-Ybarre^2/Sbarre^2)
s=Ybarre*(0.5+Ybarre^2/Sbarre^2)
ksi
s

# M?me si ksi est plus petit qu'avant, cela confirme que nous sommes probablement dans le domaine d'attraction de Fr?chet, avec une queue lourde

# On peut ?galement obtenir les estimateurs du MLE :

MLEgpdMEP = gpdFit(loss,0.023,type="mle")


# Le ksi obtenu par la m?thode du max de vraisemblance est plus ?lev? que celui obtenu par la m?thode des moments, ce qui nous conforte dans l'id?e que nous somme dans le domaine d'attraction de Frechet

ksiMEP = as.numeric(MLEgpdMEP@fit$par.ests[1])
sigmaMEP = as.numeric(MLEgpdMEP@fit$par.ests[2])

tail = LR[LR>0.023]
cdfemp = (1:length(tail))/length(tail)
gpdCDFMEP = pgpd(tail, ksiMEP, 0.023, beta = sigmaMEP)
mse = mean((gpdCDFMEP - cdfemp)^2)
View(gpdCDFMEP)


######## Hill estimator

# On trace le Hill plot

HillGraph=hillPlot(loss,ci=0.95,doplot = TRUE,plottype = c("alpha", "xi"))

# Nous devons choisir le point ? partir duquel la courbe semble se stabiliser. Nous choisissons d'utiliser les 840 plus grandes valeurs.

abline(v=840,col="blue")

# Nous devons donc obtenir la valeur de alpha correspondante sur le graphique, en sachant que nous utilisons les 840 plus grandes valeurs.
# Nous travaillons donc avec les valeurs sup�rieures au quantile d'ordre 90,15%.

100 - 840/length(loss)*100
alpha=HillGraph$y[length(HillGraph$y)-840]
abline(h=alpha,col="red")

# Il ne reste plus qu'? inverser alpha pour obtenir notre param?tre de forme

ksi=1/alpha

# Nous obtenons une valeur de ksi de 0,504806. Nous pouvons ?galement obtenir un intervalle de confiance ? 95% pour xi :

borneinf=ksi-1.96*alpha/sqrt(840)
bornesup=ksi+1.96*alpha/sqrt(840)



######## QQ Estimator


# Nous affichons le Pareto QQ-plot (quantiles empiriques d'une fonction log appliqu?e ? nos log-returns en fonction des quantiles th?oriques d'une distribution exponentielle standard).
# Notons que nous devons seulement utiliser les valeurs positives de nos logreturns puisque la fonction log n'est d?fini que pour des valeurs sup?rieure ? 0.

PQQ=ParetoQQ(loss[which(loss>0)])
abline(v=1.8,col="blue")

# Le Pareto QQ-plot semble ?tre lin?aire ? partir du quantile 1.8. Concentrons nous sur la partie droite de ce quantile pour calculer la pente.

Theo = PQQ$pqq.the
Emp = PQQ$pqq.emp
n=length(Emp)

Theo2 = Theo[Theo>1.8]
Emp2 = Emp[(n-length(Theo2)+1):n]
plot(Theo2,Emp2,main='Partie lin�aire du Pareto QQ-plot',xlab='Quantiles of standard exponential',ylab='log(X)')
100-length(Emp2)/length(loss)*100

# Nous travaillons donc avec les valeurs sup�rieures au quantile d'ordre 92,4827%

# Afin d'estimer la pente, effectuons une r?gression lin?aire simple :

rl = lm(Emp2~Theo2)
constante = rl$coefficients[1]
alpha_inv = rl$coefficients[2]
abline(as.numeric(constante),as.numeric(alpha_inv),col="blue")
alpha=1/as.numeric(alpha_inv)
ksiQQ=as.numeric(alpha_inv)

ksiQQ

# La pente est estim?e ? 0,3746871. Cette valeur est celle de notre param?tre de forme. Nous sommes donc une nouvelle fois en pr�sence d'une queue lourde en utilisant cette m?thode.
# Calculons ? pr?sent un intervalle de confiance pour ksi :

borneinf = ksiQQ - 1.96*summary(rl)$coefficients[4]
bornesup = ksiQQ + 1.96*summary(rl)$coefficients[4]
borneinf
bornesup

######### GPD - G - GPD

#Fonction GPD shape param>0

GPD_shape_pos <- function(x, shape, B){
        (1/B)*(1+shape*(x)/B)^(-1-1/shape)
}

GPD_cdf_shape_pos <- function(x, shape, B){
        1-(1+shape*x/B)^(-1/shape)
}

#Définition de la densité de la fonction hybride et de sa fonction de répartition 

h_density<- function(x, mu, sig, u1, u2, shape1,shape2){   #Densité de la fonction hybride
        B1 = - (1+shape1)*sig^2/(u1-mu)
        B2 = (1+shape2)*sig^2/(u2-mu)
        gamma2 = (B1 * dnorm(u1, mean = mu, sd = sig)+B2*dnorm(u2, mean = mu, sd = sig)+(pnorm(u2, mean = mu, sd = sig) - pnorm(u1, mean = mu, sd = sig)))^(-1)
        gamma1 =B1*gamma2*dnorm(u1, mean = mu, sd = sig)
        gamma3=B2*gamma2*dnorm(u1, mean = mu, sd = sig)
        
        
        y = ifelse(x<=u1,
                   gamma1 * GPD_shape_pos(u1-x, shape1,B1),
                   ifelse((x> u1) & (x<=u2), gamma2 * dnorm(x, mean = mu, sd = sig),
                          gamma3 * GPD_shape_pos(x-u2,shape2,B2)))
        
        
        return(y)
}

h<- function(x, mu, sig, u1, u2, shape1,shape2){  #FONCTION DE REPARTITION
        B1 = - (1+shape1)*sig^2/(u1-mu)
        B2 = (1+shape2)*sig^2/(u2-mu)
        gamma2 = (B1 * dnorm(u1, mean = mu, sd = sig)+B2*dnorm(u2, mean = mu, sd = sig)+(pnorm(u2, mean = mu, sd = sig) - pnorm(u1, mean = mu, sd = sig)))^(-1)
        gamma1 =B1*gamma2*dnorm(u1, mean = mu, sd = sig)
        gamma3=B2*gamma2*dnorm(u1, mean = mu, sd = sig)
        
        
        y = ifelse(x<=u1,
                   gamma1 * (1-GPD_cdf_shape_pos(u1-x, shape1,B1)),
                   ifelse((x> u1) & (x<=u2), gamma1+ gamma2 * (pnorm(x, mean = mu, sd = sig)-pnorm(u1, mean = mu, sd = sig)),
                          (1-gamma3 *(1- GPD_cdf_shape_pos(x-u2,shape2,B2)))))
        
        
        return(y)
}


#Fonction GPD_G_GPD - Implementation de l'algorithme 

GPD_G_GPD <- function(loss, eps, k_max){
        
        #Fonction de répartion empirique
        
        Hn = ecdf(loss)
        
        #Initialisation
        
        
        u1 = quantile(loss,0.10)
        u2 = quantile(loss,0.90)
        
        
        
        #Initialisation (non obligatoire, surtout pour donner un point de départ à la méthode LM)
        mu = mean(loss)
        p = pnorm(mean(loss)-sd(loss),mean(loss),sd(loss))  #% of observations bellow u-sigma
        sig = sd(loss) + quantile(loss,p)
        shape1 =0.1
        shape2 =0.1
        
        #Itération
        
        k=0
        while( (norm2(h(loss, mu, sig, u1, u2, shape1,shape2) - Hn(loss))^2/n>= eps) &  k<k_max) {
                
                #STEP 1 
                
                minimiseurs <- list(mu = mu, sig=sig, shape1=shape1, shape2=shape2)
                Find <- function(minimiseurs, x, u1, u2){
                        return(h(x, minimiseurs$mu, minimiseurs$sig, u1, u2, minimiseurs$shape1, minimiseurs$shape2) - Hn(x))
                }
                
                Min = nls.lm(par = minimiseurs,lower = c(-Inf, 0.0000001, 0.01, 0.01), fn=Find,x=loss, u1=u1, u2=u2)
                mu = Min$par$mu
                sig = Min$par$sig
                shape1 = Min$par$shape1
                shape2 =Min$par$shape2
                
                #STEP 2
                
                minimiseurs <- list(u1 = u1, u2=u2)
                SeuilFind <- function(minimiseurs, x,mu, sig, shape1,shape2){
                        return(h(x, mu, sig,minimiseurs$u1, minimiseurs$u2, shape1, shape2) - Hn(x))
                }
                
                u1 = nls.lm(par = minimiseurs, fn=SeuilFind,x=loss, mu = mu, sig = sig, shape1=shape1,shape2=shape2 )$par$u1
                u2 = nls.lm(par = minimiseurs, fn=SeuilFind,x=loss, mu = mu, sig = sig, shape1=shape1,shape2=shape2 )$par$u2
                
                k=k+1
                
        }
        paste0("MSE =", norm2(h(loss, mu, sig, u1, u2, shape1,shape2) - Hn(loss))^2/n)
        paste0("Nombre d'itérations", k)
        paste0("mu = ", mu)
        paste0("sig = ", sig)
        paste0("u1 = ", u1)
        paste0("u2 = ", u2)
        paste0("shape1 = ", shape1)
        paste0("shape2 = ", shape2)
        
        return(list(mu = mu, sig = sig, u1 = u1, u2=u2, shape1=shape1, shape2=shape2))
        
}    ##### FIN DE LA FONCTION G_GPD_G


#Résultats sur le S&P

p_SP = GPD_G_GPD(loss, eps = 10^-6, k_max =200)

paste0("mu = ", p_SP$mu *100)
paste0("sig = ", p_SP$sig *100)
paste0("u1 = ", p_SP$u1 *100)
paste0("u2 = ", p_SP$u2 *100)
paste0("shape1 = ", p_SP$shape1)
paste0("shape2 = ", p_SP$shape2)

x= seq(-0.06,0.06,0.00001)
y = h_density(x,mu=p_SP$mu, sig=p_SP$sig, u1=p_SP$u1, u2=p_SP$u2, shape1=p_SP$shape1, shape2=p_SP$shape2)
df = data.frame(cbind(x,y))
df$cat <- ""
df$cat[df$x<p_SP$u1] <- paste0("GPD with shape param = ", round(p_SP$shape1,2))
df$cat[df$x<=p_SP$u2 & df$x>=p_SP$u1]<-paste0("Gaussian with mu = ", round(p_SP$mu,3), " sigma =", round(p_SP$sig,3))
df$cat[df$x>p_SP$u2] <- paste0("GPD with shape param = ", round(p_SP$shape2,2))


ggplot(data=df,aes(x=x, y=y, fill =cat))+
        geom_line()+
        geom_area()+
        xlab("Pertes")+
        ylab("Densité")+
        scale_fill_discrete(name="lois")+
        ggtitle("Décomposition de la GPD_G_GPD déterminée par l'algorithme itératif")

ggplot(data=as.data.frame(loss), aes(x=loss))+
        geom_density(aes(color="Loss S&P500 observed"))+
        stat_function(fun=h_density, args= list(mu=p_SP$mu, sig=p_SP$sig, u1=p_SP$u1, u2=p_SP$u2, shape1=p_SP$shape1, shape2=p_SP$shape2), aes(color="GPD_G_GPD"))+
        xlab("Pertes")+
        ylab("Densité")+
        labs(title="Loss S&P500 observed (1987 - 2020) vs GPD_G_GPD density")


######### PARTIE COPULES

CAC=read.table("https://query1.finance.yahoo.com/v7/finance/download/%5EFCHI?period1=636336000&period2=1604016000&interval=1d&events=history&includeAdjustedClose=true",dec=".",sep=",",header=TRUE)

#On retire les valeurs manquantes de la base

CAC <- CAC[CAC$Open!="null",]

#A cause des valeurs "null" les données ont été importées en tant que type factor, on convertit en numérique :

CAC$Open <- as.numeric(as.character(CAC$Open))
CAC$Close <- as.numeric(as.character(CAC$Close))

#Conversion en format date

CAC$Date=as.Date.character(CAC$Date, format ="%Y-%m-%d")

SP_CAC = merge(Data, CAC, by="Date", all.x= FALSE, all.y=FALSE)
colnames(SP_CAC) <- c("Date", "OpenSP", "HighSP", "LowSP", "CloseSP", "Adj.CloseSP", "VolumeSP","OpenCAC", "HighCAC", "LowCAC", "CloseCAC", "Adj.CloseCAC", "VolumeCAC")

#Evolution of index prices (1990 - 2020)

plot(SP_CAC$Date, SP_CAC$OpenSP, type = "l", ylim = c(0,8000), xlab="Year",ylab ="Open index price", ,col="blue",main = "Evolution of index prices (1990 - 2020)")
par(new=TRUE)
plot(SP_CAC$Date, SP_CAC$OpenCAC, type = "l", ylim = c(0,8000),xlab="",ylab ="", col="red")
legend("topleft", legend=c("S&P500 ($)", "CAC40 (€)"), col=c("blue", "red"), , lty=1, cex=1)

# Evolution of standardized index prices (1990 - 2020)

SP_CAC$OpenCACStandardized <- ( SP_CAC$OpenCAC-min(SP_CAC$OpenCAC) )/ (max(SP_CAC$OpenCAC)- min(SP_CAC$OpenCAC))
SP_CAC$OpenSPStandardized <- ( SP_CAC$OpenSP-min(SP_CAC$OpenSP) )/(max(SP_CAC$OpenSP) - min(SP_CAC$OpenSP))


plot(SP_CAC$Date, SP_CAC$OpenSPStandardized, type = "l", ylim = c(0,1), xlab="Year",ylab ="Open index price", ,col="blue",main = "Evolution of standardized index prices (1990 - 2020)")
par(new=TRUE)
plot(SP_CAC$Date, SP_CAC$OpenCACStandardized, type = "l", ylim = c(0,1),xlab="",ylab ="", col="red")
legend("topleft", legend=c("S&P500", "CAC40"), col=c("blue", "red"), , lty=1, cex=1)

#Rendements depuis 1990

n = length(SP_CAC$OpenCAC)
ReturnsCAC=( SP_CAC$CloseCAC[2:n] - SP_CAC$OpenCAC[1]) /SP_CAC$OpenCAC[1]
ReturnsSP =( SP_CAC$CloseSP[2:n] - SP_CAC$OpenSP[1]) /SP_CAC$OpenSP[1]

plot(SP_CAC$Date[2:n], ReturnsSP*100, type = "l", ylim = c(0,1000), xlab="Year",ylab ="Total returns (%)", ,col="blue",main = "Evolution of total returns since 1990")
par(new=TRUE)
plot(SP_CAC$Date[2:n], ReturnsCAC*100, type = "l", ylim = c(0,1000),xlab="",ylab ="", col="red")
legend("topleft", legend=c("S&P500", "CAC40"), col=c("blue", "red"), , lty=1, cex=1)

#CAC40 enfonction du S&P500

lm_eqn <- function(df){
        m <- lm(OpenCAC ~ OpenSP, df);
        a = format(unname(coef(m)[1]), digits = 2)
        b = format(unname(coef(m)[2]), digits = 2)
        r2 = format(summary(m)$r.squared, digits = 3)
        eq <- paste0("y = ",a," + ",b,".x , r2 = ",r2)
}


ggplot(SP_CAC, aes(x=OpenSP, y=OpenCAC, color=year(Date))) + 
        geom_smooth(method='lm', formula = y ~ x) + 
        geom_point()+
        scale_color_continuous(name="Année")+
        ggtitle("Prix d'ouverture du CAC40 en fonction du S&P500 depuis 1990")+
        geom_text(x=800, y=7000, label = lm_eqn(SP_CAC))


ggplot(subset(SP_CAC, Date < "2011-08-02"), aes(x=OpenSP, y=OpenCAC, color=year(Date))) + geom_point() #Première tendance 1990 - 2011

ggplot(subset(SP_CAC, Date>= "2011-08-02"), aes(x=OpenSP, y=OpenCAC, color=year(Date))) + geom_point() #Nouvelle tendance le 2 août 2011

ggplot(subset(SP_CAC, Date>= "2020-03-09"), aes(x=OpenSP, y=OpenCAC, color=year(Date))) + geom_point()  #Nouvelle tendance 9 mars 2020

SP_CAC$Periode <- ""
SP_CAC$Periode[SP_CAC$Date < "2011-08-02"] <- "1990-03-02 to 2011-08-01"
SP_CAC$Periode[SP_CAC$Date >= "2011-08-02" & SP_CAC$Date < "2020-03-09"] <- "2011-08-02 to 2020-03-08"
SP_CAC$Periode[SP_CAC$Date >= "2020-03-09"] <- "2020-03-09 to 2020-10-29"

ggplot(SP_CAC, aes(x=OpenSP, y=OpenCAC, color=Periode)) +
        geom_smooth(method='lm',size=2, formula = y ~ x)+
        geom_point()+
        ggtitle("Prix d'ouverture du CAC40 en fonction du S&P500 depuis 1990 par période")+
        geom_text(x=800, y=7000, label = lm_eqn(SP_CAC[SP_CAC$Periode == "1990-03-02 to 2011-08-01",]), color="red")+
        geom_text(x=800, y=6500, label = lm_eqn(SP_CAC[SP_CAC$Periode == "2011-08-02 to 2020-03-08",]), color="green")+
        geom_text(x=800, y=6000, label = lm_eqn(SP_CAC[SP_CAC$Periode == "2020-03-09 to 2020-10-29",]),color="blue")

#loss

X_SP = -log(SP_CAC$OpenSP[2:n]/SP_CAC$OpenSP[1:(n-1)])
Y_CAC = -log(SP_CAC$OpenCAC[2:n]/SP_CAC$OpenCAC[1:(n-1)])



#Plot des log returns du CAC

plot(SP_CAC$Date[2:n], -Y_CAC, type="l", xlab= "Year", ylab="LogReturns", main="Daily CAC log returns with Open value from 1990 to 2020")


#Statistiques sur les log returns

mean_SP=mean(-X_SP)
var_SP=var(-X_SP)
min_SP=min(-X_SP)
Q1_SP=quantile(-X_SP, 0.25)
median_SP = median(-X_SP)
Q3_SP=quantile(-X_SP, 0.75)
max_SP = max(-X_SP)
skew_SP=skewness(-X_SP)
kurt_SP=kurtosis(-X_SP)

Conversion_pourcentage = c(mean_SP, var_SP, min_SP,Q1_SP, median_SP, Q3_SP, max_SP)
Conversion_pourcentage = paste0(as.character(round(Conversion_pourcentage*100,3)), "%")

mean_CAC=mean(-Y_CAC)
var_CAC=var(-Y_CAC)
min_CAC=min(-Y_CAC)
Q1_CAC=quantile(-Y_CAC, 0.25)
median_CAC = median(-Y_CAC)
Q3_CAC=quantile(-Y_CAC, 0.75)
max_CAC = max(-Y_CAC)
skew_CAC=skewness(-Y_CAC)
kurt_CAC=kurtosis(-Y_CAC)

Conversion_pourcentage_CAC = c(mean_CAC, var_CAC, min_CAC,Q1_CAC, median_CAC, Q3_CAC, max_CAC)
Conversion_pourcentage_CAC = paste0(as.character(round(Conversion_pourcentage_CAC*100,3)), "%")

all_stats = cbind( c(Conversion_pourcentage, round(skew_SP,3), round(kurt_SP,3)), c(Conversion_pourcentage_CAC, round(skew_CAC,3), round(kurt_CAC,3)))

Stat_SP=matrix(all_stats ,9,2)
rownames(Stat_SP)=c("Moyenne", "Variance", "Minimum", "1er quartile", "médiane", "3eme quartile", "maximum","Skewness", "Kurtosis")
colnames(Stat_SP)=c("S&P LogReturns", "CAC LogReturns")
as.table(Stat_SP)


#Plot des pertes

lm_eqn_loss <- function(df){
        m <- lm(Y_CAC ~ X_SP, df);
        a = format(unname(coef(m)[1]), digits = 2)
        b = format(unname(coef(m)[2]), digits = 2)
        r2 = format(summary(m)$r.squared, digits = 3)
        eq <- paste0("y = ",a," + ",b,".x , r2 = ",r2)
}

ggplot(as.data.frame(cbind(X_SP, Y_CAC)), aes(x = X_SP, y=Y_CAC))+
        geom_smooth(method='lm',size=2, formula = y ~ x)+
        geom_point()+
        ggtitle("Pertes journalières du CAC40 en fonction du S&P500")+
        geom_text(x=-0.07,y=0.11,label = lm_eqn_loss(as.data.frame(cbind(X_SP, Y_CAC))), color="blue")



# Corrélations

cor(X_SP,Y_CAC,method = "pearson")
cor(X_SP,Y_CAC,method = "kendall")
cor(X_SP,Y_CAC,method = "spearman")

#Pseudo-obervations et scatter plot

SP_pobs <- rank(X_SP)/(length(X_SP)+1)       #On peut également utiliser la fonction pobs du package VineCopula
CAC_pobs <- rank(Y_CAC)/(length(Y_CAC)+1)

plot(SP_pobs,CAC_pobs, pch = 20,cex=0.5, main="Rank scatter plot")


# Sélection de la copule

selectedCopula <- BiCopSelect(SP_pobs,CAC_pobs,familyset=NA)
selectedCopula

#verification
t.cop <- tCopula(dim=2)
set.seed(500)
m <- pobs(as.matrix(cbind(X_SP, Y_CAC)))
fit <- fitCopula(t.cop,m,method='ml')
coef(fit)
#the parameters are indeed the same as the ones selected by the function BiCopSelect

#plot the density of our selected copula
rho <- coef(fit)[1]
df_copula <- coef(fit)[2]
persp(tCopula(dim=2,rho,df=df_copula),dCopula)


Nsamples = length(X_SP)
cop <- rCopula(Nsamples,tCopula(dim=2,rho,df=df_copula))
plot(cop[,1],cop[,2],pch = 20,cex=0.5,col='blue', main="Scatter plot d'un échantillon aléatoire d'une copule de Student avec alpha=0.65 et df=3.4")

cor(cop,method='spearman')

#######Distribution bivariée


### BIVARIATE DISTRIB

#Première simulation en considérant que les lois marginales sont des gaussiennes

copula_dist <- mvdc(copula=tCopula(rho,dim=2,df=df_copula), margins=c("norm","norm"),
                    paramMargins=list(list(mean=mean(X_SP), sd=sd(X_SP)),
                                      list(mean=mean(Y_CAC), sd=sd(Y_CAC))))

sim <- rMvdc(length(Y_CAC), copula_dist)


plot(X_SP,Y_CAC,main='Simulated bivariate distribution with gaussian marginals vs real observations')
points(sim[,1],sim[,2],col='red')
legend('topleft',c('Observed','Simulated'),col=c('black','red'),pch=21)


#Deuxième simulation grâce au GPD_G_GPD

#Pour construire la distribution bivariée il nous reste à calculer les lois marginales
#Nous avons déjà trouvé une distibution convaincainte du S&P500 dans la partie précédente grâce au GPD_G_GPD

#GPD_G_GPD sur le CAC40

p_CAC = GPD_G_GPD(Y_CAC, eps = 10^-7, k_max =300)

paste0("mu = ", p_CAC$mu)
paste0("sig = ", p_CAC$sig)
paste0("u1 = ", p_CAC$u1)
paste0("u2 = ", p_CAC$u2)
paste0("shape1 = ", p_CAC$shape1)
paste0("shape2 = ", p_CAC$shape2)

x= seq(-0.06,0.06,0.00001)
y = h_density(x,mu=p_CAC$mu, sig=p_CAC$sig, u1=p_CAC$u1, u2=p_CAC$u2, shape1=p_CAC$shape1, shape2=p_CAC$shape2)
df = data.frame(x=x,y=y)
df$cat <- ""
df$cat[df$x<p_CAC$u1] <- paste0("GPD with shape param = ", round(p_CAC$shape1,2))
df$cat[df$x<=p_CAC$u2 & df$x>=p_CAC$u1]<-paste0("Gaussian with mu = ", round(p_CAC$mu,3), " sigma =", round(p_CAC$sig,3))
df$cat[df$x>p_CAC$u2] <- paste0("GPD with shape param = ", round(p_CAC$shape2,2))


ggplot(data=df,aes(x=x, y=y, fill =cat))+
        geom_line()+
        geom_area()+
        ylab("Densité")+
        scale_fill_discrete(name="lois")+
        ggtitle("Décomposition de la GPD_G_GPD déterminée par l'algorithme itératif pour le CAC40")

ggplot(data=as.data.frame(Y_CAC), aes(x=Y_CAC))+
        geom_density(aes(color="Loss CAC40 observed"))+
        stat_function(fun=h_density, args= list(mu=p_CAC$mu, sig=p_CAC$sig, u1=p_CAC$u1, u2=p_CAC$u2, shape1=p_CAC$shape1, shape2=p_CAC$shape2), aes(color="GPD_G_GPD"))+
        xlab("x")+
        labs(title="Loss CAC40 observed (1990 - 2020) vs GPD_G_GPD density")

dmargins_SP <- function(x,mu, sig, u1, u2, shape1, shape2){
        h_density(x,mu, sig, u1, u2, shape1, shape2)
}

dmargins_CAC <- function(x,mu, sig, u1, u2, shape1, shape2){
        h_density(x,mu, sig, u1, u2, shape1, shape2)
}

pmargins_SP <- function(x,mu, sig, u1, u2, shape1, shape2){
        h(x,mu, sig, u1, u2, shape1, shape2)
}

pmargins_CAC <- function(x,mu, sig, u1, u2, shape1, shape2){
        h(x,mu, sig, u1, u2, shape1, shape2)
}


inverse = function(fn, interval = NULL, lower = min(interval), upper = max(interval), ...){
        Vectorize(function(y){
                uniroot(f=function(x){fn(x)-y}, lower=lower, upper=upper, ...)$root
        })
}


qmargins_SP <- function(x,mu, sig, u1, u2, shape1, shape2){       #Pour être tout à fait rigoureux il aurait été préfable de calculer manuellement la fonction quantile
        pmargins_SPX<-function(x){pmargins_SP(x=x,mu=mu, sig=sig, u1=u1, u2=u2, shape1=shape1, shape2=shape2)}
        ifelse(x<=0,return(-Inf),
               ifelse((x>=1),return(Inf),
                      return(inverse(pmargins_SPX , lower=-10^(45), upper=10^45)(x))))
}

qmargins_CAC <- function(x,mu, sig, u1, u2, shape1, shape2){
        pmargins_CACX<-function(x){pmargins_CAC(x=x,mu=mu, sig=sig, u1=u1, u2=u2, shape1=shape1, shape2=shape2)}
        ifelse(x<=0,return(-Inf),
               ifelse((x>=1),return(Inf),
                      return(inverse(pmargins_CACX , lower=-10^(45), upper=10^45)(x))))
}




copula_dist <- mvdc(copula=tCopula(rho,dim=2,df=df_copula), margins=c("margins_SP","margins_CAC"),
                    paramMargins=list(list(mu=p_SP$mu, sig=p_SP$sig, u1=p_SP$u1, u2=p_SP$u2, shape1=p_SP$shape1, shape2=p_SP$shape2),
                                      list(mu=p_CAC$mu, sig=p_CAC$sig, u1=p_CAC$u1, u2=p_CAC$u2, shape1=p_CAC$shape1, shape2=p_CAC$shape2)))

sim <- rMvdc(length(Y_CAC), copula_dist)


plot(X_SP,Y_CAC,main='Simulated bivariate distribution with GPD_G_GPD marginals vs real observations')
points(sim[,1],sim[,2],col='red')
legend('topleft',c('Observed','Simulated'),col=c('black','red'),pch=21)

