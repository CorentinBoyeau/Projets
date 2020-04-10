# -*- coding: utf-8 -*-
"""
Created on Sat Mar 14 00:23:01 2020

@author: coren
"""

import requests
from bs4 import BeautifulSoup

url = "https://www.pronosoft.com/fr/lotofoot/historiques/loto-foot-7/"
Annee = [2011,2012,2013,2014,2015,2016,2017,2018,2019]
NbreGrille = [136,231,291,311,329,325,345,345,354]


""" cf partie type de match"""
numero_page="137"    
indice = 21
#page de la première grille complète 
            #La vrai 1ere page est 138 mais le 02/01/2011 est seul, pour simplifier il faut 20 dates à remplir par page
            #On rempli le 02/01/2011 à la main

with open('Webscrapping_pronosoft.csv', 'a', newline="") as filerap :
    filerap.write("Annee;Num;Date;Type;Gains_distribues;7sur7_nombre;7sur7_valeur;6sur7_nombre;6sur7_valeur;prono_nombre;M1_Team1;M1_Team2;M1_resultat;M1_rate_1;M1_rate_N;M1_rate_2;M2_Team1;M2_Team2;M2_resultat;M2_rate_1;M2_rate_N;M2_rate_2;M3_Team1;M3_Team2;M3_resultat;M3_rate_1;M3_rate_N;M3_rate_2;M4_Team1;M4_Team2;M4_resultat;M4_rate_1;M4_rate_N;M4_rate_2;M5_Team1;M5_Team2;M5_resultat;M5_rate_1;M5_rate_N;M5_rate_2;M6_Team1;M6_Team2;M6_resultat;M6_rate_1;M6_rate_N;M6_rate_2;M7_Team1;M7_Team2;M7_resultat;M7_rate_1;M7_rate_N;M7_rate_2"+"\n")
    for year in Annee : 
        for i in range (1,NbreGrille[Annee.index(year)]+1):
            link = url +str(year) +"-grille-"+ str(i) +"/"
            response = requests.get(link)
            if response.ok:  #Si code != 200 --> pas ok
                
                """ RAPPORT """
                
                soup=BeautifulSoup(response.text, 'lxml')
                rapport=soup.find('table', {'class':'rapports'}).find_all('tr')
                row1=rapport[1].find_all('td')            
                rapport1Nombre=row1[1].text
                rapport1Valeur=(row1[2].text).replace("\xa0€","").replace(" ","")
                try : 
                    row2=rapport[2].find_all('td')
                    rapport2Nombre=row2[1].text
                    rapport2Valeur=(row2[2].text).replace("\xa0€","").replace(" ","")
                    GainsDistrib = rapport[3].text[19:].replace('\xa0€* source: fdj.com', "").replace(" ","")
                except IndexError: 
                    rapport2Nombre="0"
                    rapport2Valeur="0"
                    GainsDistrib="0"
            
                """ INFOS GRILLE """ 
                
                matchDate = soup.find('table', {'class':'hist_rapports'}).find('tr', {'class':'head'}).span.attrs["data-date-utc"][:10]
            
                """ INFOS MATCHS """
                
                match = soup.find('table', {'class':'hist_rapports'}).find_all('tr')
                matchDom = []
                matchExte = []
                resultats = []
                for j in range (7):
                    matchDom.append(match[j+2].find_all('td')[1].text)
                    matchExte.append(match[j+2].find_all('td')[2].text)
                    for k in range (3):
                        img = match[j+2].find_all('td')[3].find_all('img')[k].attrs["src"]
                        if img[len(img)-15:len(img)-10]=="check":
                            if k==2 :resultats.append("2")
                            if k==1 : resultats.append("N")
                            if k==0 : resultats.append("1")
                            break
                        if k==2 :     #Ce passe si aucune équipe à gagner (match annulé)
                            resultats.append("0")
                            
                """ INFOS PRONOSTIQUEURS """
                
            link2 = "https://www.pronosoft.com/fr/lotofoot/repartition/lf7/"+str(year)+"-grille-"+str(i)+"/"
            response2 = requests.get(link2)
            if response2.ok:
                
                soup2=BeautifulSoup(response2.text, 'html.parser')   #'lxml' ne marchait pas (yavait des espaces ...)
                
                """ Nombre de pronos sur le site """
                
                TitreTab=soup2.find('div', {'id':'repart_lf7'}).find('h2').text
                pos1 = TitreTab.index("(")
                pos2 = TitreTab.index(")")
                NombreProno = TitreTab[pos1+1:pos2-11]
                
                """ Proportions """
                
                Tab = soup2.find('div', {'class':'repart_inside'}).find_all('div', {'class':'perc'})
                DomRate = []
                NRate=[]
                ExteRate=[]
                for t in range (7):
                    DomRate.append(Tab[t].find_all('div')[0].text.replace("\xa0",""))
                    NRate.append(Tab[t].find_all('div')[1].text.replace("\xa0",""))
                    ExteRate.append(Tab[t].find_all('div')[2].text.replace("\xa0",""))
                    
            """ Type de match (Ligue 1,championnat d'Europe ...) """
                
            
           
            lien = "https://www.pronosoft.com/fr/lotofoot/rapports/loto-foot-7/date/page-"+numero_page+"/"
            r=requests.get(lien)
            
            if r.ok and (year!=2011 or i!=1):
                soup3=BeautifulSoup(r.text, 'html.parser')
                Type=soup3.find('table', {'class':'stat_hist r-lf7'}).find_all('tr')[indice].find('td',{'class':'type'}).text
                indice = indice - 1
                if indice == 1 :   #On change de page
                    indice = 21
                    numero_page=str(int(numero_page)-1)
            
                                
            """ Transcription en CSV """
            if (year!=2011 or i!=1): filerap.write(str(year)+";"+str(i)+';'+matchDate+';'+Type+';'+GainsDistrib+";"+rapport1Nombre+";"+rapport1Valeur+";"+rapport2Nombre+";"+rapport2Valeur+";"+NombreProno)
            else: filerap.write(str(year)+";"+str(i)+';'+matchDate+';'+"Autre"+';'+GainsDistrib+";"+rapport1Nombre+";"+rapport1Valeur+";"+rapport2Nombre+";"+rapport2Valeur+";"+NombreProno)
            
            for z in range (7):
                filerap.write(";"+matchDom[z]+";"+matchExte[z]+";"+resultats[z]+";"+DomRate[z]+";"+NRate[z]+";"+ExteRate[z])

            filerap.write("\n")
                    
            
    