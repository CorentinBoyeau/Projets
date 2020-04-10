# -*- coding: utf-8 -*-
"""
Created on Sat Jul 13 20:29:30 2019

@author: Corentin
"""

import numpy as np
from tkinter import * 


root= Tk()
root.geometry("370x500")
root.title("Mass Hysteria Generator")
frame1=Frame(root)

text=Label(frame1, text="Nombre de serviteurs sur le board :")
text.grid(row=0, column=0,sticky=SE)
              

nb_minions=IntVar()
scale_w=Scale(frame1, from_=1, to=14, orient='horizontal', variable=nb_minions)
scale_w.grid(row=0, column=1, padx=10, sticky=W)


#text=Label(root, textvariable=var_entry)
#text.pack()

frame=Frame(root)
#%%

def hysterie(life,attack):
    global divine_fixe
    divine=np.array(divine_fixe)  #Pour éviter de changer divine_fixe quand on change divine
    global toxicite
    n=nb_minions.get()
    life_minions=np.array(life, dtype=int)  #Pour éviter de modifier le vecteur mis en entrée 
    attack_minions=np.array(attack)
    Still_Alive=np.arange(1,n+1)
    Didnt_Attack=np.arange(1,n+1)  #Numero des serviteurs qui n'ont pas attaqués (et encore en vie ...)
    while len(Didnt_Attack)!=0 and (len(Didnt_Attack)!=1 or len(Still_Alive)!=1) :
        Attaquant=np.random.choice(Didnt_Attack)  #Numero de celui qui attaque 
        Attaqué = np.random.choice(np.setdiff1d(Still_Alive, [Attaquant]))  #On choisit quelqu'un a attaquer sauf celui qui attaque 
        
        if divine[Attaqué-1]==0:  #On retranche les PDV seulement si il n'a pas de bouclier divin
            life_minions[Attaqué-1] -= attack_minions[Attaquant-1] #On retranche les PDV de Attaqué à l'attaque de Attaquant
        if divine[Attaquant-1]==0:
            life_minions[Attaquant-1] -= attack_minions[Attaqué-1]        
                    
        if (life_minions[Attaqué-1]<=0 or toxicite[Attaquant-1]==1) and divine[Attaqué-1]==0:   #Toxicite ne marche pas sur les boucliers divins
            life_minions[Attaqué-1]=0
            Still_Alive=np.setdiff1d(Still_Alive,[Attaqué]) 
            Didnt_Attack=np.setdiff1d(Didnt_Attack,[Attaqué])  #Il est mort il ne peut plus attaquer !
            
        Didnt_Attack=np.setdiff1d(Didnt_Attack, [Attaquant])  #On enleve le numero du serviteur a la liste de ceux qui n'ont pas attaqués (pas la position !)
        if (life_minions[Attaquant-1]<=0 or toxicite[Attaqué-1]==1) and divine[Attaquant-1]==0:
            life_minions[Attaquant-1]=0
            Still_Alive=np.setdiff1d(Still_Alive,[Attaquant])
        if divine[Attaqué-1]==1:
            divine[Attaqué-1]=0
        if divine[Attaquant-1]==1:
            divine[Attaquant-1]=0
    return life_minions

#%%
  

            
            
#%%
Life=[]
Attack=[]
FrameListBox=[] 

def afficher_questions():
    for c in frame.winfo_children():  
            c.destroy()                  #Pour réinitialiser les entrées                
    
    life_minions = np.array([])
    attack_minions = np.array([])
    
    text=Label(frame, text="Attaque")
    text.grid(row=1, column=1)

    text=Label(frame, text="PDV")
    text.grid(row=1, column=2, pady=5)
    

    global Life
    global Attack
    Life=[]
    Attack=[]
    for i in range (nb_minions.get()):
        text=Label(frame, text="Serviteur n°"+str(i+1))
        text.grid(row=i+2, column=0, pady=5, padx=5)
        

        entryA=Entry(frame, width=5)
        entryA.grid(row=i+2, column=1, padx=5)
        Attack.append(entryA)

        entryL=Entry(frame, width=5)
        entryL.grid(row=i+2, column=2, padx=5)
        Life.append(entryL)                 #Pour pouvoir récupérer la valeur par la suite
        

        
    textD=Label(frame, text="Numero serviteurs avec bouclier divins (ex: 1,3)")
    textD.grid(row=i+3,column=0,columnspan=2,pady=10,padx=5)
    
    entryD=Entry(frame, width=10)
    entryD.grid(row=i+3,column=2,padx=5)

    textT=Label(frame, text="Numero serviteurs avec toxicité")
    textT.grid(row=i+4,column=0,columnspan=2,pady=10,padx=5)
    
    entryT=Entry(frame, width=10)
    entryT.grid(row=i+4,column=2,padx=5)

    textA=Label(frame, text="Nombre de serviteurs adverses (si différent)")
    textA.grid(row=i+5,column=0,columnspan=2,pady=10,padx=5)
    
    entryA=Entry(frame, width=10)
    entryA.grid(row=i+5,column=2,padx=5)
    
    global entree
    entree=[entryD, entryT, entryA]  #Pour récuperer les entrée par la suite
    button=Button(frame, text = "Calculer les probas", command=calcul_probas)
    button.grid(row=i+6, column=1,columnspan=2, pady=5)
    button_reset=Button(frame, text = "Reset", command=reset)
    button_reset.grid(row=i+6, column=0, pady=5)



#%%
    
def triBulle(a,b) :                            #Permet de trier a (et de faire les memes transformations pour b)
    for i in range(len(a)-1) :
        for j in range(len(a)-1,i,-1) :
            if a[j] >= a[j-1]:
                a[j],a[j-1] = a[j-1],a[j]
                c=np.array(b)             #Technique du dessus ne marche pas avec des array
                m=c[j]                 
                b[j]= b[j-1]
                b[j-1]=m 
    return a,b



def calcul_probas():   #Inspirer du fichier Hysterie(sans tkinter)
    row=nb_minions.get()+6
    n=nb_minions.get()
    life_minions = np.array([])
    attack_minions = np.array([])
    global Life
    global Attack
    for i in range (n):
        life_minions=np.append(life_minions, int(Life[i].get()))
        attack_minions=np.append(attack_minions, int(Attack[i].get()))
    global divine_fixe
    divine_fixe=np.array([0]*n)
    global entree
    d=entree[0].get()
    t=entree[1].get()       
    if d!="" and d!="0":
        for i in range (0,len(d),2):
            divine_fixe[int(d[i])-1]=1  
    global toxicite
    toxicite=np.array([0]*n)
    if t!="" and t!="0" :
        for i in range (0,len(t),2):
            toxicite[int(t[i])-1]=1
    N=5000
    mem=np.array([hysterie(life_minions, attack_minions)])
    issues=np.array([hysterie(life_minions, attack_minions)]) #Vecteur contenant les issues possibles 
    if entree[2].get()!="" : a=int(entree[2].get()) 
    else : a=nb_minions.get()
    Att=np.array([])
    Health=np.array([])
    for i in range (N):
        exp=hysterie(life_minions,attack_minions)
        if np.sum(np.equal(issues,exp).all(axis=1))==0:   #C'est a dire si exp n'est pas dans issues 
            issues=np.append(issues,[exp],axis=0) #Permet de garder une liste de liste
        mem=np.append(mem,[exp],axis=0)
        at=0
        hlt=0
        for j in range (len(exp[0:a])):
            if exp[j]!=0:
                at+=attack_minions[j]
                hlt+=exp[j]
        Att=np.append(Att, at)
        Health=np.append(Health, hlt)
    frame2=Frame(root)
    global FrameListBox  
    FrameListBox.append(frame2)
    scrollbar = Scrollbar(frame2)
    scrollbar.pack(side=RIGHT, fill=Y)

    listbox = Listbox(frame2, width=50, height=7)
    listbox.pack()
    probas=[]
    for i in range (len(issues)):
        probas.append(np.mean(np.equal(mem,issues[i]).all(axis=1)))
    triBulle(probas, issues)
    for i in range (len(issues)):
        issue=""
        for j in range (len(issues[i])):
            if j!=len(issues[i])-1 : 
                issue+=str(round(issues[i][j]))+" - "
            else:
                issue+=str(round(issues[i][j]))
        issue+="        "+  str(round(probas[i]*100,2)) + " %"
        listbox.insert(END, issue)

    # attach listbox to scrollbar
    listbox.config(yscrollcommand=scrollbar.set) 
    scrollbar.config(command=listbox.yview)
    frame2.grid(column=0)

    text=Label(frame2, text="moy_att="+str(np.mean(Att))+"                      moy_health="+str(np.mean(Health)))
    text.pack()
# def reset():
#     global FrameListBox
#     for i in range (len(FrameListBox)):
#         for c in FrameListBox[i].winfo_children():   
#             c.destroy()
#     FrameListBox=[]

def reset():
    global FrameListBox
    for i in range (len(FrameListBox)):
        FrameListBox[i].grid_forget()
        FrameListBox[i].destroy()
    FrameListBox=[]
       
button=Button(frame1, text = "Valider", command=afficher_questions)
button.grid(row=0, column=2, padx=5, sticky=S)





frame1.grid(row=0, column=0)
frame.grid(row=1,column=0)
        
root.mainloop()