import sqlite3, datetime

connexion = sqlite3.connect('data.db') #On crée data.db ou on l'ouvre juste si elle existe
c = connexion.cursor()
c.execute('''SELECT * FROM {tab};'''.format(tab="Rinavia6")) #On selectionne toute la table
data=c.fetchall()
for i in data:
    date = datetime.datetime.strptime(i[2], "%Y-%m-%d %H:%M:%S.%f")
    if date.month == 11 :
        newmonth = 1
        newyear = date.year + 1
        print ("ok")
    elif date.month == 12 :
        newmonth = 2
        newyear = date.year + 1
    else :
        newmonth = date.month+2
    date = date.replace(month=newmonth)
    date = date.replace(year = newyear)
    print (date)
                                  
