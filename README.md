# Project-R-Titanic
analyse des données du naufrage du Titanic (1912). avec le language R

## Description des données

## Question 1
Utilisation du data frame train contient un ´echantillon de passagers du
Titanic

load('C:/Users/admin/Documents/titanic_train.Rdata')
head(train)
```

## Question 2 

dim(train)
nrow(train) #nombre observations nbobs[1]
ncol(train)#nombre variables ou bien nbvar
names(train)# Nom des variables 
sum(is.na(train))# nombre de valeurs manquantes ou bien table(is.na(train))[2]
 
summary(train) #affiche le nombre de NA c'est la varibale Cabine qui a le max de 463 NA
```


## Question 3


# pour changer le nom des colonnes on peut utiliserla commande colnames(train)[colnames(train)=="Survived"]="S"

summary(train$Age)

#décrire la variable Sex

summary(train$Age)

barplot(table(train$Sex),main="La survive par Sex",xlab="Sex",col=c("orange","blue"))

summary(train$Sex)

prop.table(table(train$Sex))

#décrire la variable Sex
summary(train$Age)

barplot(table(train$Sex),main="Le Sex",col=c("orange","blue"))

summary(train$Sex)

table(train$Sex)

 #décrire la variable Survived
 
barplot(table(train$Survived),main="La Survie",col=c("orange","blue"))

table(train$Survived)

  #décrire la variable Pclasse
  
  barplot(table(train$Pclass),main="La Classe",col=c("orange","blue"))
  
 table(train$Pclass)
 
                   
  barplot(table(train$Pclass),main="La Classe",col=c("orange","blue"))
 
   #décrire la variable Age
   
  summary(train$Age)
  
barplot(table(train$Age))

sd(table(train$Age),na.rm = T)

sum(is.na(train$Age))/nrow(train)
  
```


## Question 4

#La nouvelle variable cAge qui cat´egorises Age `a l’aide de la fonction cut()


train$cAge=cut(train$Age,breaks=(0:4)*20)

barplot(table(train$cAge),main="Répartition des Classes d'age",col=c("blue","orange"))

floor(prop.table(table(train$cAge))*100)

summary(train$cAge)

cA=cAge
```


## Question 5


#Lien entre Sx et S

  table(train$Sex,train$Survived)
  
plot(table(train$Sex,train$Survived),main="La survie par Sex",xlab="Sex",ylab="Survived",col=c("orange","blue"))

prop.table(table(train$Sex,train$Survived),1)

#Lien entre P et S

table(train$Pclass,train$Survived)

plot(table(train$Pclass,train$Survived),main="La survie par Classe",xlab="Classe",ylab="Survived",col=c("orange","blue"))

prop.table(table(train$Pclass,train$Survived),1)

#Lien entre A et S

table(train$cAge,train$Survived)

plot(table(train$cAge,train$Survived),main="La survie par Classe d'age",xlab="Classe d'age",ylab="Survived",col=c("orange","blue"))

table(train$cAge,train$Survived)

#lien entre Age et S

prop.table(table(train$Age,train$Survived),margin=1)
```


## Question 7


#Proba(S/Sx) Problabilit´e de survie sachant sex

prop.table(table(train$Sex,train$Survived),margin=1)

#Proba(S/Pclass) Probabilit´e de survie sachant la classe

prop.table(table(train$Pclass,train$Survived),margin=1)

#Proba(S=1/cAge)  Probabilit´e de survie sachant Classe Age

prop.table(table(train$cAge,train$Survived),margin=1)
```

Question 8

#Creation de la variable S P la probabilit´e de la survie sachant Pclass

S_P=prop.table(table(train$Pclass,train$Survived),margin=2)

colnames(S_P)=c('Unsurvived','survived')

rownames(S_P)=c('Classe 1','Classe 2','Classe 3')

S_P

#contruire la variable proba S sachant Sx 

S_Sx=prop.table(table(train$Sex,train$Survived),margin=2)

colnames(S_Sx)=c('Unsurvived','survived')

S_Sx

#contruire la variable proba S sachant Age 

S_Age=prop.table(table(train$cAge,train$Survived),margin=2)

colnames(S_Age)=c('Unsurvived','survived')

#contruire la variable proba de Survie

probsurv
probsurv=prop.table(table(train$Survived))
```


## Question 9


# modéliser la fonction de probabilité de BAYES
  prob_prediction=function(Sx,P,cAge){
  
  classage=ceiling(cAge/20)
  return ((S_Sx[Sx,"survived"]*S_P[P,"survived"]*S_Age[classage,"survived"]*probsurv[2])
          /((S_Sx[Sx,"survived"]*S_P[P,"survived"]*probsurv[2]*S_Age[classage,"survived"] )+ 
              (S_Sx[Sx,"Unsurvived"]*S_P[P,"Unsurvived"]*probsurv[1]*S_Age[classage,"Unsurvived"])))}
 # Phase de test
prob_prediction("female",1,23)
prob_prediction("male",3,55)
