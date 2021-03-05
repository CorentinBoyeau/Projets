 # -*- coding: utf-8 -*-
"""
Created on Thu May 21 15:01:35 2020

@author: coren
"""

import mcpi.minecraft as minecraft
import mcpi.block as block
import time
from nbtschematic import SchematicFile
import numpy as np



mc=minecraft.Minecraft.create()
Nombre_pixel_art = 22       #Nombre de pixel art qu'on veut faire à la suite

for t in range (1, Nombre_pixel_art+1):
    
    sf = SchematicFile.load(str(t) +'.schematic')    #Fichier schematic. Pour la conversion d'une image en schematic utiliser le logiciel Spritecraft
                                                     #Nommer les fichiers schematic de "1" à Nombre_pixel_art
    largeur = int(sf['Schematic']['Width'])
    longueur=int(sf['Schematic']['Length'])
    
    #mc.player.setPos(largeur/2,longueur/2,-125)
    L=[]
    for i in range(longueur):
        for j in range (largeur):
            L.append([i,j])
     
    np.random.seed(30)       
    L=np.random.permutation(L)
    
    
    time.sleep(20)
    for k in L:
        i=k[0]
        j=k[1]
        time.sleep(0.0001)
        if (sf.blocks[0,i,j]<0):
            mc.setBlock(j, i+1, 0,256+sf.blocks[0,i,j],sf.data[0,i,j])
        else :        
            mc.setBlock(j, i+1, 0,sf.blocks[0,i,j],sf.data[0,i,j])
    
    time.sleep(15)
    
    for k in L:
        i=k[0]
        j=k[1]
        mc.setBlock(j, i+1, 0, 0)
            
    
