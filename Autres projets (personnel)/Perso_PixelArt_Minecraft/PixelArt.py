# -*- coding: utf-8 -*-
"""
Created on Thu Dec 26 21:51:21 2019

@author: coren
"""

import os.path, sys
sys.path.append(os.path.join(os.path.dirname(os.path.realpath(__file__)), os.pardir))

import mcpi.minecraft as minecraft
from math import sqrt
from PIL import Image
import time



mc = minecraft.Minecraft.create()


# Possible blocks  in (Name, ID, (RGB1,RGB2,..),Data)
	#RGBs are used to color match.
possibleBlocks = (
    ("Smooth Stone", 1, ( (125,125, 125) ,),0),
    ("Dirt", 3, ( (133,96,66),),0),
    ("Cobblestone", 4, ( (117,117,117),),0),
    ("Wooden Plank", 5, ( (156,127,78),),0),
    ("Bedrock", 7, ( (83,83,83),),0),
    ("Sand", 12, ( (217,210,158),),0),
    ("Gravel", 13, ( (136, 126, 125),),0),
    ("Gold Ore", 14, ( (143,139,124),),0),
    ("Iron Ore", 15, ( (135,130,126),),0),
    ("Coal Ore", 16, ( (115,115,115),),0),
    ("Wood", 17, ( (154,125,77),),0),
    ("Sponge", 19, ( (182,182,57),),0),
    ("White Wool", 35, ( (221,221,221),),0),
    ("Orange Wool", 35, ( (233,126,55),),1),
    ("Magenta Wool", 35, ( (179,75,200),),2),
    ("Light Blue Wool", 35, ( (103,137,211),),3),
    ("Yellow Wool", 35, ( (192,179,28),),4),
    ("Light Green Wool", 35, ( (59,187,47),),5),
    ("Pink Wool", 35, ( (217,132,153),),6),
    ("Dark Gray Wool", 35, ( (66,67,67),),7),
    ("Gray Wool", 35, ( (157,164,165),),8),
    ("Cyan Wool", 35, ( (39,116,148),),9),
    ("Purple Wool", 35, ( (128,53,195),),10),
    ("Blue Wool", 35, ( (39,51,153),),11),
    ("Brown Wool", 35, ( (85,51,27),),12),
    ("Dark Green Wool", 35, ( (55,76,24),),13),
    ("Red Wool", 35, ( (162,44,42),),14),
    ("Black Wool", 35, ( (26,23,23),),15),
    ("Gold", 41, ( (249,236,77),),0),
    ("Iron", 42, ( (230,230,230),),0),
    ("TwoHalves", 43, ( (159,159,159),),0),
    ("Brick", 45, ( (155,110,97),),0),
    ("Mossy Cobblestone", 48, ( (90,108,90),),0),
    ("Obsidian", 49, ( (20,18,29),),0),
    ("Diamond Ore", 56, ( (129,140,143),),0),
    ("Diamond Block", 57, ( (99,219,213),),0),
    ("Workbench", 58, ( (107,71,42),),0),
    ("Redstone Ore", 73, ( (132,107,107),),0),
    ("Snow Block", 80, ( (239,251,251),),0),
    ("Clay", 82, ( (158,164,176),),0),
    ("Jukebox", 84, ( (107,73,55),),0),
    ("Pumpkin", 86, ( (192,118,21),),0),
    ("Netherrack", 87, ( (110,53,51),),0),
    ("Soul Sand", 88, ( (84,64,51),),0),
    ("Glowstone", 89, ( (137,112,64),),0)
)

def getBlockFromColor(RGB):
	smallestDistIndex = -1
	smallestDist = 300000
	curIndex = 0
	for block in possibleBlocks:
		for blockRGB in block[2]:
			curDist = getColorDist(RGB, blockRGB)

			if (curDist < smallestDist):
				smallestDist = curDist
				smallestDistIndex = curIndex

		curIndex = curIndex + 1

	if (smallestDistIndex == -1):
		return -1

	return possibleBlocks[smallestDistIndex]

def getColorDist(colorRGB, blockRGB):
	return sqrt( pow(colorRGB[0]-blockRGB[0],2) + pow(colorRGB[1]-blockRGB[1],2) + pow(colorRGB[2]-blockRGB[2],2))

pos = mc.player.getPos()

maxsize = (160, 228)

L=[]
for i in range (0,972):
    im = Image.open('resources/1309/'+str(i)+'.png')
    im.thumbnail(maxsize, Image.ANTIALIAS)
    rgb_im = im.convert('RGB')
    columns, rows = rgb_im.size
    for r in range(rows):
        for c in range(columns):
            rgb = rgb_im.getpixel((columns-c-1, rows-r-1))
            mc_block = getBlockFromColor(rgb)
            mc.setBlock(-50+c, 1+r, 50, mc_block[1],mc_block[3])
        
time.sleep(10)
mc.setBlocks(-150,1,0,600,256,0, 0)
