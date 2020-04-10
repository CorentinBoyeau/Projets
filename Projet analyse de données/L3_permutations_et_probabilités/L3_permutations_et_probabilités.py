# -*- coding: utf-8 -*-
"""
Created on Tue Apr 16 20:40:06 2019

@author: coren
"""

import numpy as np
import matplotlib.pyplot as plt 

print ("*** Temps d'éxécution du programme : 1 minute ***")

#%%
""" Première Partie - Décomposition en cycles """

print ("")
print (" ****** Première partie - Décomposition en cycles ******\n")

def cycle(P,p0):
    c=np.array([p0])
    pos=p0-1    #On va devoir faire -1 à chaque fois, car Python commence à 0 et non pas 1
    while P[pos]!=p0:
        c=np.append(c, P[pos])
        pos=P[pos]-1
    return c

print ("Idée du programme :\nOn va s'inspirer d'une fonction cycle(P,p0) qui étant donnée une permutation ,renvoie le cycle qui débute en p0")
print ("\nExemple : cycle(P,1) renvoie donc le premier cycle (qui commence en 1)")
print ("On va donc faire le premier cycle, puis une fois terminée, commencer un deuxième cycle en p0, avec p0 étant le plus petit nombre qui n'est pas présent dans le premier cycle, et ainsi de suite...")
print ("Pour savoir si un nombre est déja dans un cycle on va introduire un vecteur de booléens, si en position p0-1 le vecteur vaut True p0 est déjà dans un cycle, sinon il n'est pas encore dans un cycle \n")

def decomposition (P):
    D=[]  
    check = [False for j in range(len(P))] #Il n'y pas de cycles pour l'instant--> False partout
    p0=1
    N=len(P)
    while True :    #La boucle s'arrêtera avec un break 
        while p0<N+1 and check[p0-1]:  #Si check[p0-1] vraie --> p0 est déjà dans un cycle
                                       #On ne va donc pas commencer le nouveau cycle par p0
            p0 = p0 + 1
        if p0==N+1:       #On a donc fini la décompositon en cycles on peut stoper la boucle 
            break
        
        c=np.array([p0])  #Nouveau cycle qui commence  par p0
        pos=p0-1
        check[pos]=True   
        while P[pos]!=p0:
            c=np.append(c, P[pos])
            pos=P[pos]-1
            check[pos]=True   #p0 est désormais dans un cycle, on met sa position à True
        D.append(c)
    return (D)

print ("Description du programme : decomposition(P)")
print ("  Entrée : permutation P de {1,..,N} \n  Sortie : liste de np.array qui représentent les cycles de la décomposition \n")

N=15
P=np.random.permutation(N)+1
D=decomposition (P)

print("Exemple :")
print ("   Omega_N      =", np.arange(1,N+1))
print ("      P         =", P)
print ("decomposition(P)=", D)
print ("                = ", end="")
for j in range (len(D)):
    print (D[j], end= "")
print ("")

#%%
"""Question 1 : longueur du cycle le plus long """

print ("\n \n")
print ("****** Question 1 : longueur du cycle le plus long ******\n")
print ("Idée du programme :\n\n1ère méthode : De manière naïve, générer la décomposition en entier et regarder quel cycle est le plus grand ")
print ("\n2ème méthode : Il est inutile de faire la décomposition en entier pour pouvoir connaître la longueur du cycle le plus long. La plupart du temps, le cycle le plus long sera dans les premières positions et sa longueur sera supérieur à N/2 ")
print ("On va donc suivre à peu près cette idée. On s'intéresse au premier cycle si sa longueur est supérieur à N/2 c'est fini on retourne sa longueur")
print ("Sinon on regarde le cycle suivant si sa longueur est supérieur à (N-le nombre d'éléments dans les cycles précédents)/2")
print ("Alors dans ce cas, il est impossible d'avoir le cycle le plus long dans les cycles qui suivent. On retourne donc la plus grande longueur rencontrée jusque là. \n" )

print ("Description du programme : longest(P)")
print ("  Entrée : permutation P de {1,..,N} \n  Sortie : longueur du plus grand cycle de la décomposition en cycles de P \n\n")


def longest(P):
    check = [False for j in range(len(P))]
    p0=1
    l=0      #Longueur du cycle actuel 
    long=l   #Longueur du plus grand cycle rencontré jusque là
    In_Previous_Cycle=0   #Compte le nombre d'éléments dans les cycles précédents 
    N=len(P)
    while l<(N-In_Previous_Cycle)/2 :
        
        In_Previous_Cycle+=l   
        while p0<N+1 and check[p0-1]: 
            p0 = p0 + 1
        if p0==N+1:
            break
        
        c=np.array([p0])
        pos=p0-1
        check[pos]=True
        while P[pos]!=p0:
            c=np.append(c, P[pos])
            pos=P[pos]-1
            check[pos]=True
        l=len(c)
        long=max(long,l)
    return long


print ("Pour N petit on va représenter la variable par une courbe")
print ("Pour N grand, on va faire un histogramme car il faudrait beaucoup trop de répétitions pour avoir un résultat convaincant avec une courbe")
print ("Dans les 2 cas l'allure des représentations semble concorder")

mem0=np.array([])
N=500
rep = 20000

for i in range (rep):
    P=np.random.permutation(N)+1
    mem0=np.append(mem0, longest(P))

plt.hist(mem0/N,bins="auto",normed=True)   
   

mem=np.array([])
N=20
rep = 60000

for i in range (rep):
    P=np.random.permutation(N)+1
    mem=np.append(mem, longest(P))
    
x=np.arange(1,N+1)
freq=np.array([np.mean(mem==j) for j in np.arange(1,N+1)])
plt.plot(x/N,freq*N,".-")


plt.legend(["N=20", "N=500"])
plt.title("Représentation graphique de XN/N")
plt.show()

print ("Appelons Xn la variable aléatoire qui donne la longueur du cycle le plus long pour une permutation de taille n")
print ("Par lecture graphique directe on observe par exemple que la probabilité que XN=0.5*N (N pair) est environ égale à 1.9/N") 
print ("\nesp[X500/500]= ",np.mean(mem0)/500)
print ("Avec N=10000 et rep=10^6 on trouve une espérance = 0.6243112649 ")
print ("P(X"+str(500)+">=0.5*"+str(500)+")=", np.mean(mem0>=(500/2)), "Avec N=10000 et rep=10^6 on trouve 0.692948") 

plt.plot(np.arange(1,N+1), freq,"g.-")
plt.title("Représentation graphique de la loi de X"+str(N))
plt.show()

x=np.arange(1,N+1)
F=np.array([np.mean(mem<=j) for j in range (1,N+1)])
plt.plot(x,F,".")
plt.title("Fonction de répartition de X"+str(N))
plt.show()


print ("Les probabilités en chaque point deviennent de plus en plus grande jusqu'à N/2 (ou (N-1)/2 si N impair), point à laquelle la probabilité est maximale, avant de décroitre jusqu'à N")


#%%

"""Question 2 : Probabilité que k points soient dans le même cycle """

print ("\n\n****** Question 2 : Probabilité que k points soient dans le même cycle ******\n")
print ("Idée du programme : On prend un sous ensemble E quelconque de taille k (Remarque : Pour simplifier on va prendre {1,...k} mais cela ne change rien)" )
print ("Pour savoir si les éléments de E sont dans le même cycle, on parcourt la décomposition en cycles. Dès qu'on rencontre un élément de E dans un cycle il y a 2 possibilités :")
print ("Soit les éléments de E sont tous dans ce cycle on peut donc renvoyer True, soit il y a des éléments qui n'y sont pas, on renvoie False\n" )


print ("Description du programme : InSameCycle(D,E)")
print ("  Entrées : D une décomposition en cycle et E un vecteur \n  Sortie : renvoie 1 si les éléments de E sont dans le même cycle, 0 sinon \n")
def InSameCycle(D, E):  
    for i in range (len(D)):
        Nb=np.sum([D[i]==j for j in E]) #Nombre d'éléments de E dans le i-eme cycle
        if Nb==len(E):    
            return 1
        if Nb!=0:  #Il y a donc un élément de e dans ce cycle mais il n'y sont pas tous
            return 0

N=40
rep=10000
compt=np.array([0]*N)

for i in range (rep):
    D=decomposition(np.random.permutation(N)+1)
    for k in range (1,N+1):
#        E=np.random.choice(np.arange(1,N+1),k,replace=False)  
        E=np.arange(1,k+1)
        compt[k-1]+=InSameCycle(D,E)
        
x=np.arange(1,N+1)
plt.plot(x,compt/rep,"b.",x,1/x,"r")
plt.legend(["Fréquence théorique","Fonction inverse : k--> 1/k"])
plt.title("Probabilité que k points soient dans le même cycle (N="+str(N)+")")
plt.show()   

print ("Il semble donc que la probabilité que k points choisis de manière quelconque dans {1,..,N} soient dans le même cycle est 1/k.")

#%%

""" Question 3 : Nombre de cycles de longueur j """ 

print ("\n \n")
print ("****** Question 3 : Nombre de cycles de longueur j ******\n")

print ("On va étudier la loi de Xnj, ainsi que des lois conjointes pour différentes valeurs de j et pour n=100")


N=100
rep=10000
Lj=np.array([1,2,8]) #En prendre au moins 2

print ("On pose un vecteur Lj=", Lj, " (Qu'on peut modifier pour tester d'autres valeurs)")
print ("On va étudier la loi de Xnj pour j dans " + str(Lj))
print ("Et on va étudier les lois conjointes de (Xnp,Xnk) pour p="+str(Lj[0])+ " et k dans " + str(Lj[1:]))

mem=[np.array([]) for i in range (len(Lj))]  #mem[k] est l'échantillon de Xnj (j=Lj[k])
memJoint = [np.array([[0,0]]) for i in range (len(Lj)-1)]  #liste de vecteurs de dim 2 ! 
#memJoint[k] va contenir l'echantillon de (Xnj,Xnp) pour différentes valeurs de p 
#On intialise chaque vecteur à [[0,0]] pour ne pas avoir de problèmes lors du append 
#On supprimera le [0,0] après  

for i in range (rep):
    D=decomposition(np.random.permutation(N)+1)    
    for k in range (len(Lj)):
        j=Lj[k]
        mem[k]=np.append(mem[k],np.sum([len(D[i])==j for i in range (len(D))])) 
        
    for k in range (len(memJoint)):
        memJoint[k]=np.append(memJoint[k],[[mem[0][i],mem[k+1][i]]], axis=0)         
        #axis=0 permet de rajouter une ligne et ainsi avoir un vecteur de dimension 2
        #Si on le met pas, np.append([[0,0]],[[1,2]])=np.array([0,0,1,2])
        #Alors qu'on veut avoir np.array([[0,0],[1,2]])
        
for k in range (len(memJoint)):
    memJoint[k]=np.delete(memJoint[k],0,axis=0)  #On supprime le [0,0] qu'on avait initialisé 
                                    
#Etude de Xnj pour tout j dans Lj                                                                          
                                        
for k in range (len(Lj)):
    j=Lj[k]
    maxi=int(np.max(mem[k]))   #Pour éviter de représenter Xnj de 0 jusqu'à n, la proba devient nulle assez vite
    XNj=np.array([np.mean(mem[k]==i) for i in range (maxi+1)])
    plt.plot(XNj,"*--")
    
    l=1/j
    P=np.random.poisson(l,rep)
    X=np.array([np.mean(P==j) for j in range (maxi+1)])
    plt.plot(X)
    
    plt.legend(["Fréquence théorique", "Loi de Poisson de paramètre 1/j"])
    plt.title("Représentation de la loi de $X_{n,j}$ pour j=" + str(j))
    plt.show()
    print ("esp Xn,j= ", np.mean(mem[k]))
    print ("1/j=",1/j)
    
print ("\nConclusion : quelque soit la valeur de j Xn,j converge en loi vers une loi de Poisson de paramètre 1/j \n")
    
#Etude des lois conjointes 

print ("Etude des lois conjointes")

for h in range (1,len(Lj)):
    print ("\n Etude de " + "(Xn" + str(Lj[0]) +",Xn"+str(Lj[h])+") :")
    I=[[0,0],[1,0],[2,2]]   #On peut modifier le vecteur si on veut étudier d'autres couples
    for i in range (len(I)):
        p=I[i][0]
        k=I[i][1]
        print ("P((Xn" + str(Lj[0]) +",Xn"+str(Lj[h])+")=(" + str(p) +"," +str(k) + "))=" ,np.mean((memJoint[h-1]==[p,k]).all(axis=1)))
        print ("P(Xn" + str(Lj[0]) + "=" +str(p)+ ")*" + "P(Xn" + str(Lj[h]) + "=" +str(k) +") =", np.mean(mem[0]==p)*np.mean(mem[h]==k))
        print ("")
        
print ("Les variables Xnp et Xnk semblent être indépendantes quelque soit p différent de k")
print ("Ainsi la loi conjointe de (Xnp,Xnk) est le produit des lois marginales de Xnp et Xnk qui convergent vers des exponentielles de paramètres respectifs 1/p et 1/k")


#%%
""" Question 4 : Nombre de cycles """ 
print ("\n \n")
print ("****** Question 4 : Nombre de cycles ******\n")


N=100
rep=10000
mem=np.array([])

for j in range (rep):
    D=decomposition(np.random.permutation(N)+1)
    mem=np.append(mem,len(D))
    
Nb=np.array([np.mean(mem==i) for i in range (1,N+1)])

maxi=0   
for j in range (N):
    if Nb[j]!=0:
        maxi=j
        
x=np.arange(1,N+1)
plt.plot(x[:min(maxi+3,len(x))],Nb[:min((maxi+3),len(x))],".--")   

esp=np.mean(mem)
var=np.var(mem)

def fG(x, esp, var):
    return (1/np.sqrt(2*np.pi*var))*np.exp(-0.5*((x-esp)**2)/var)

x=np.arange(0,15,0.01)
plt.plot(x,fG(x,esp,var))
plt.title ("Représentation de la variable aléatoire X qui compte le nombre de cycles pour N=" +str(N))
plt.legend (["Fréquence théorique de X", "Normale(esp(X), var(X))"])
plt.show()

print ("\nCe n'est pas tout à fait convaincant, mais la variable aléatoire qui compte le nombre de cycles semble converger vers une loi normale avec comme paramètres : son éspérance et sa variance.")
    
    
    