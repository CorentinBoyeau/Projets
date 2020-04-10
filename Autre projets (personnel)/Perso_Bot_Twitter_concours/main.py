import tweepy,RetweetConcours,BypassAntiBot,time,random,sys,GestionFollow,csv
tabname = []

###Constante Paramètre du bot ###
version = 2.8 #Version du bot
compte = {"1":["oNz00FgSvopT14398fPqOjEEN","msrU36dR5sLRh2wpJ6MCoYSYWBCFZ2L4xS68Om7u94B1XopUyV","1027818387899117570-mz7pLHmALqzLhVEOJyimwPbcxPAlz7","lrk6jRsKui2Gtf1RR5B5PFK9M21pp2KUY7Sr4MiDJcGUM"],"2":["J9gzPB25FF3Z4w6pX00S6aN5k","TVoq4JCx5rAeosSPqKNSPEd30nZ6JWlW4dAm6ZFh2IiTT7JmoO","1027534916538585089-y4Ibwc9WtqkyK3Rj8mLQUkzPht8WXt","0H37RL3H0Df0yWVfcSLA36VfMHfJBwH3pbVIM5moCj4RG"],"4":["WCfhsPWvLN2D7XCbUmPRmNGaT","8rQUnbzkn5jCAIj1x87T3IvG43Hji53jKoaT78tctIp5iqZ6Qk","1027542688894214145-AABbZhD8X6OkbvqK945tQw4t73VzFL","RbO68cP4SGJj9hBZR9kqa0OQPz1c8hFBHlM6SzGFwdPMb"]} #Liste des comptes avec les identifiants de connexion à l'api
NombreDeRetweet = 10 #Nombre de tweet que l'on recupère par recherche
listerecherchefr = ["#concours","#JeuConcours","RT & Follow","tenter de gagner","Gagnez rt + follow","concours pour gagner", "on vous fait gagner"]#Mot à retweeté pour un concours
FILE = "Blacklist.txt"
with open(FILE,'r') as f:
    lines=[line.strip('\n') for line in f.readlines()]

BlackListCompte = [line for line in lines]
CompteTag = ["@hicarion4", "@LodBet"]#Les comptes à utiliser pour tag. Si vous utilisez plusieurs comptes bot vous n'avez pas besoins d'ajouter de comptes dans ce tableau. Vous devez rentrer le compte avec son @ (@toto)
###


for cle,tabauth in compte.items():
    try :
        auth = tweepy.OAuthHandler(tabauth[0], tabauth[1]) #Authentification avec les valeurs du tableau trouvées dans le dictionnaire
        auth.set_access_token(tabauth[2], tabauth[3]) #Authentification avec les valeurs du tableau trouvées dans le dictionnaire
        api = tweepy.API(auth,wait_on_rate_limit=True,wait_on_rate_limit_notify=True) #Authentification
        user = api.me()
        tabname.append("@" + user.screen_name)
        GestionFollow.CreateTables(user)
    except tweepy.TweepError as e:
        if e.api_code == 326 :
            print("Le compte " + cle + " est bloqué !")
        else :
            print(e.reason)
tabname = tabname + CompteTag


while True :
    for tabauth in compte.values(): #Pour chaque compte on passe dans cette boucle
        try :
            auth = tweepy.OAuthHandler(tabauth[0], tabauth[1]) #Authentification avec les valeurs du tableau trouvées dans le dictionnaire
            auth.set_access_token(tabauth[2], tabauth[3]) #Authentification avec les valeurs du tableau trouvées dans le dictionnaire
            api = tweepy.API(auth,wait_on_rate_limit=True,wait_on_rate_limit_notify=True) #Authentification
            user = api.me()
            #GestionFollow.Unfollow(user,api)
            RetweetConcours.retweet(api,NombreDeRetweet,listerecherchefr,tabname,BlackListCompte)#on retweet les concours
            BypassAntiBot.bypass(api)#On bypass l'anti bot
        except tweepy.TweepError as e :
            if e.api_code == 326 :
                pass
    nbrandom = random.randrange(2800,3500)
    try :
        print("Programme en attente de : " + str(nbrandom) + " s") #Temps d'attente en seconde avant une nouvelle boucle
        time.sleep(nbrandom)
    except tweepy.TweepError as e:
        if e.api_code == 326 :
            pass
    except KeyboardInterrupt : #On termine le programme proprement en cas de ctrl-c
        print("Programme terminé !")
        sys.exit()
