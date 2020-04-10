import tweepy,random,time, re

def bypass(api) :#Fonction principal de bypass anti bot (basé sur le site twren.ch)
    try :
        user = api.me()
        print("Bypass pour le compte : " + user.name)
        nb = 1
        pourcentageRTFollow = CalculPourcentageRtFollow(api)
        while pourcentageRTFollow >= 25 : #On veut moins de 25% de RT avec le mot Follow
            randomretweet(api)
            pourcentageRTFollow = CalculPourcentageRtFollow(api)
            print("Pourcentage de Rt avec le mot Follow (en cours de diminution) : " + str(round(pourcentageRTFollow, 2)))
        print("Pourcentage de Rt avec le mot Follow (final) : " + str(round(pourcentageRTFollow, 2)))
        pourcentageRT = CalculPourcentageRT(api)
        while pourcentageRT >= 50 : #On veut moins de 50% de RT
            randomtweet(api)
            pourcentageRT = CalculPourcentageRT(api)
            nb += 1
        print("Pourcentage RT final : " + str(round(pourcentageRT, 2)))
    except tweepy.TweepError as e :
        if e.api_code == 326 :
            pass

def CalculPourcentageRT(api) : #Fonction calcul pourcentage RT  de l'utilisateur
    nb = 0
    for tweet in api.user_timeline(count=200, tweet_mode="extended") :
        if tweet.retweeted == True :#Si le tweet est un RT
            nb += 1
    pourcentage = (nb * 100)/200 #Calcul pourcentage
    return pourcentage

def CalculPourcentageRtFollow(api) : #Fonction calcul pourcentage de RT avec le mot Follow
    nb = 0
    nbrt = 0
    for tweet in api.user_timeline(count=200, tweet_mode="extended") :
        if tweet.retweeted == True :
            nbrt += 1
            if "FOLLOW" in tweet.full_text.upper() : #On met tout le texte en majuscule et n cherche le mot follow dans le tweet
                nb += 1
    pourcentage = (nb * 100)/nbrt
    return pourcentage

def randomretweet(api) : #On retweet un tweet random
    trends1 = api.trends_place(610264)
    trends = list([trend['name'] for trend in trends1[0]['trends']])
    nbrandom =  random.randrange(0,len(trends))
    for tweet in tweepy.Cursor(api.search,q=trends[nbrandom],result_type="recent",lang="fr").items(10):
        try:
            tweet.retweet()
        except tweepy.TweepError as e:
            if e.api_code == 185 :
                print("Message en attente, on a envoyé trop de message")
                time.sleep(1500)
            elif (e.api_code == 327) or (e.api_code == 326) :
                pass
            else :
                print(e.reason)
        except StopIteration:
            break


def randomtweet(api) : #On récupère un message tweeter et on le tweet
    try:
        trends1 = api.trends_place(610264)#Code France (marseille) FR
        trends = list([trend['name'] for trend in trends1[0]['trends']])
        
        quote = True      #On veut éviter de citer le tweeter de quelqu'un d'autre (=RT avec commentaires)
        while quote==True :
            nbrandom =  random.randrange(0,len(trends))
            for tweet in tweepy.Cursor(api.search,q=trends[nbrandom] + " -filter:replies -filter:images",lang="fr",tweet_mode="extended",result_type='recent').items(1):
                if hasattr(tweet, 'retweeted_status') :
                    tweettext = tweet.retweeted_status.full_text
                    word = tweettext.split(' ')
                    if (len(word[len(word)-1])>=5):          #On va tester si le dernier "mot" est un lien web (ce qui correspond souvent à un tweet cité)
                        if (word[len(word)-1][0:5]=="https"):
                            pass
                        else :
                            quote = False
                    else : quote = False
                else : 
                    tweettext = tweet.full_text
                    word = tweettext.split(' ')
                    if (len(word[len(word)-1])>=5):          
                        if (word[len(word)-1][0:5]=="https"):
                            pass
                        else :
                            quote = False
                    else : quote = False


        for i in range (0,1) :  #Inutile, juste la flemme de réindenter toutes les lignes
            if hasattr(tweet, 'retweeted_status') :
                tweettext = tweet.retweeted_status.full_text
                if "@" in tweettext : #On évite de notifier les gens quand on récupère un tweet d'un autre
                    tweettext = tweettext.replace("@",".")
                if "#" in tweettext : #On évite les # pour etre discret
                    tweettext = tweettext.replace("#",".")
                if "," in tweettext : #On modifie quelques trucs du tweet pour éviter de remarquer trop facilement le plagiat
                    tweettext = tweettext.replace("#"," ")
                if re.search("ait\s", tweettext):        #On fais exprès de faire des fautes
                    tweettext = re.sub("ait\s","ais ", tweettext)
                if re.search("er\s", tweettext): 
                    tweettext = re.sub("er\s","é ", tweettext)

                api.update_status(tweettext)
                time.sleep(10)
            else :
                tweettext = tweet.full_text
                if "@" in tweettext :
                    tweettext = tweettext.replace("@",".")
                if "#" in tweettext : #On évite de notifier les gens quand on récupère un tweet d'un autre
                    tweettext = tweettext.replace("#",".")
                if "," in tweettext : #On modifie quelques trucs du tweet pour éviter de remarquer trop facilement le plagiat
                    tweettext = tweettext.replace("#"," ")
                if re.search("ait\s", tweettext):        #On fais exprès de faire des fautes
                    tweettext = re.sub("ait\s","ais ", tweettext)
                if re.search("er\s", tweettext): 
                    tweettext = re.sub("er\s","é ", tweettext)

                api.update_status(tweettext)
                time.sleep(10)
    except tweepy.TweepError as e:
        if e.api_code == 185 :
            print("Message en attente, on a envoyé trop de message")
            time.sleep(1500)
        elif (e.api_code == 187) or (e.api_code == 327) or (e.api_code == 186) or (e.api_code == 326):
            pass
        else :
            print(e.reason)
