import tweepy,random,BypassAntiBot,time,re,GestionFollow

def retweet(api,NombreDeRetweet,listerecherchefr,tabname,BlackListCompte) :#Fonction de retweet de concours
    try :
        user = api.me()
    except tweepy.TweepError as e :
        if e.api_code == 326 :
            pass
    for mot in listerecherchefr : #Pour chaque mot dans la liste un lance une recherche
        for tweet in tweepy.Cursor(api.search,q=mot + " since:" + time.strftime('%Y-%m-%d',time.localtime()),lang="fr",tweet_mode="extended").items(NombreDeRetweet): #On cherche avec #concours parmis les plus populaires en france
            commenter=1
            try:
                if tweet.retweet_count > 5 :
                    if hasattr(tweet, 'retweeted_status') : #Si le tweet est un retweet ou pas
                        if(tweet.retweeted_status.author.screen_name in BlackListCompte) : #Si l'utilisateur est dans la blacklist on fait rien.
                            print("Compte blacklist : " + tweet.retweeted_status.author.screen_name)
                            commenter=0
                            pass
                        elif re.search("PIOCHE|PICKAXE|SKIN(\s|S)|V-BUCKS|CADEAU(\s|X\s|S\s)AL[E|É]ATOIRE(\s|S)|LOT(\s|S\s)AL[E|É]ATOIRE(\s|S)|SPOTIFY|J'AI SAUT[É|ER|EE|ÉE]|JAI SAUT[É|ER|EE|ÉE]|RETROUV[E|É|ER] MA TL|AGRANDIR MA TL|NUDE(\s|S)|MINTY|\sDZ\s|ALGERIEN|MAROCAIN|FOLLOW ENTRE NOUS|FOLLOW BACK|FOLLOWBACK|BAISER|FOLLOW TOUT CEUX QUI RT|TOUT CEUX QUI ME RT ET FOLLOW|SCREEN RECORD|SOMME(\s|S\s)AL[E|É]ATOIRE(\s|S)", tweet.retweeted_status.full_text.upper()):
                            print ("Concours arnaque ou pas un concours(mot bannis : V-BUCKS, lot aléatoire, agrandir ma TL ...")
                            commenter=0
                            pass
                        else :
                            tweet.retweet() #On retweet
                            tweet.favorite()  #On like
                            api.create_friendship(tweet.retweeted_status.author.id)
                            
                            sentence=tweet.retweeted_status.full_text.upper()
                            words = sentence.split(' ')
                            pos=[]
                            i=0
                            for word in words:
                                i=i+1
                                if re.search("FOLLOW", word):
                                    pos.append(i)
                            for j in pos:
                                if j<len(words):
                                    m=words[j]
                                    if len(m)>0:
                                        if m[0]=="@":
                                            api.create_friendship(m[1:len(m)])
                                if j<(len(words)-1):
                                    m=words[j+1]
                                    if len(m)>0:
                                        if m[0]=="@":
                                            api.create_friendship(m[1:len(m)])
                                if j<(len(words)-2):
                                    m=words[j+2]
                                    if len(m)>0:
                                        if m[0]=="@":
                                            api.create_friendship(m[1:len(m)])          
                                if j<(len(words)-3):
                                    m=words[j+3]
                                    if len(m)>0:
                                        if m[0]=="@":
                                            api.create_friendship(m[1:len(m)])           

                            print('Vous avez retweet le tweet de  @' + tweet.retweeted_status.author.screen_name)
                            GestionFollow.UpdateTable(tweet.retweeted_status.author.id,user)
                    else :
                        if(tweet.user.screen_name in BlackListCompte) :
                            print("Compte blacklist : " + tweet.user.screen_name)
                            commenter=0
                            pass
                        elif re.search("PIOCHE|PICKAXE|SKIN(\s|S)|V-BUCKS|CADEAU(\s|X|S) AL[E|É]ATOIRE(\s|S)|LOT(\s|S) AL[E|É]ATOIRE(\s|S)|SPOTIFY|J'AI SAUT[É|ER|EE|ÉE]|J’AI SAUT[É|ER|EE|ÉE]|RETROUV[E|É|ER] MA TL|AGRANDIR MA TL|\sDZ\s|ALG(E|É)RIEN|MAROCAIN", tweet.full_text.upper()):
                            print ("Concours arnaque ou pas un concours(mot bannis : V-BUCKS, lot aléatoire, agrandir ma TL ...")
                            commenter=0
                            pass
                        else :
                            tweet.retweet() #On retweet
                            tweet.favorite() #On like
                            api.create_friendship(tweet.user.id) #On follow

                            sentence=tweet.retweeted_status.full_text.upper()
                            words = sentence.split(' ')
                            pos=[]
                            i=0
                            for word in words:
                                i=i+1
                                if re.search("FOLLOW", word):
                                    pos.append(i)
                            for j in pos:
                                if j<len(words):
                                    m=words[j]
                                    if len(m)>0:
                                        if m[0]=="@":
                                            api.create_friendship(m[1:len(m)])
                                if j<(len(words)-1):
                                    m=words[j+1]
                                    if len(m)>0:
                                        if m[0]=="@":
                                            api.create_friendship(m[1:len(m)])
                                if j<(len(words)-2):
                                    m=words[j+2]
                                    if len(m)>0:
                                        if m[0]=="@":
                                            api.create_friendship(m[1:len(m)])          
                                if j<(len(words)-3):
                                    m=words[j+3]
                                    if len(m)>0:
                                        if m[0]=="@":
                                            api.create_friendship(m[1:len(m)]) 

                            print('Vous avez retweet le tweet de  @' + tweet.user.screen_name)
                            GestionFollow.UpdateTable(tweet.user.id,user)
                    if "retweeted_status" in dir(tweet):
                        tweet=tweet.retweeted_status
                    else:
                        tweet=tweet
                    friend = 0
                    hashtag='' 
                    if commenter==1:
                        if re.search("(^|\s|#|-|.)AMI(E|ES|S|\s)|(^|\s|#|-|.)POTE(S|\s)|(^|\s|#|-|.)PERSONNE(S|\s)|(\s)TAG(\s)", tweet.full_text.upper()) : #On vérifie avec une expression régulière si il faut inviter des amies.
                            friend = 1
                        elif ("AMI(E)" or "AMI(E)S" or "#AMI(E)" or "#AMI(E)S") in tweet.full_text.upper():
                            friend = 1
                        m=re.search("MET(S|\s) UN #|MET(S|\s) #|TWEET(EZ\s|\s)AVEC(\sLE\s|\s)#|TWEET(EZ\s|\s)(LE\s|)#|HASHTAG #|COMMENTE(R\s|Z\s|\s)AVEC(\sLE\s|\s)#|COMMENTE(R\s|Z\s|)(LE\s|)#|MENTIONNE(R\s|Z\s|\s)AVEC(\sLE\s|\s)#|R[E|É]POND(S\s|\s)AVEC(\sLE\s|\s)#",  tweet.full_text.upper())
                        if m:
                            pos_hash=m.span()[1]-1
                            T=tweet.full_text[pos_hash:]
                            hashtag=T.split(' ')[0]
                        commentaire(api,tweet,tabname, friend , hashtag)                                         
                BypassAntiBot.randomtweet(api)
            except tweepy.TweepError as e:
                if e.api_code == 185 :
                    print("Message en attente, on a envoyé trop de message :(")
                    time.sleep(1250)
                elif (e.api_code == 327) or (e.api_code == 139) or (e.api_code == 326):
                    pass
                else :
                    print(e.reason)
            except StopIteration:
                break

def commentaire(api,tweet,tabname, friend = 1, hashtag= "") : #Fonction pour faire un commentaire
    try:
        if friend == 1 :
            if hasattr(tweet, 'retweeted_status') :
                comment = "@" + tweet.retweeted_status.author.screen_name + " "
            else :
                comment = "@" + tweet.user.screen_name + " " #On prepare le message de commentaire
            user = api.me()
            nbusernotif = 0 #Variale compteur de compte tag
            random.shuffle(tabname) #On mélange le tableau aléatoirement.
            for username in tabname :
                if nbusernotif < 2 : #On veut pas tag plus de 2 comptes
                    if username == "@" + user.screen_name : #On veut pas mentionner le compte actif.
                        pass
                    else :
                        comment = comment + username + " " #On fait le message de commentaire
                        nbusernotif +=1 # On augmente le compteur de compte tag
            comment = comment + " " + hashtag
        else :
            if hasattr(tweet, 'retweeted_status') :
                comment = "@" + tweet.retweeted_status.author.screen_name + " " +hashtag
            else :
                comment = "@" + tweet.user.screen_name + " " + hashtag
        if hasattr(tweet, 'retweeted_status') :
            api.update_status(comment,tweet.retweeted_status.id)
        else :
            api.update_status(comment,tweet.id) #On envoit le commentaire
    except tweepy.TweepError as e:
        if e.api_code == 185 :
            print("Message en attente, on a envoyé trop de message")
            time.sleep(1250)
        elif e.api_code == 326 :
            pass
        else :
            print(e.reason)
