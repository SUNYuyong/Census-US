#Exercice us_census_full

#importer les fichiers d'apprentissage 
census_appren <- read.csv("census_income_learn.csv", header=F)

#importer les fichiers de test 
census_test <- read.csv("census_income_test.csv", header=F)

#clarifier le sens de chaque colonne en fonction du fichier census_income_metadata.txt
#Cette étape me permet d'étudier la qualité de toutes les données 
#et ceci m'aide à choisir les variables appropriétés pour la modélisation
census_appren[,1]#age
census_appren[,2]#class of worker
census_appren[,3]#detailed industry recode
census_appren[,4]#detailed occupation recode
census_appren[,5]#education
census_appren[,6]#wage per hour
census_appren[,7]#enroll in edu inst last wk
census_appren[,8]#marital stat
census_appren[,9]#major industry code
census_appren[,10]#major occupation code
census_appren[,11]#race
census_appren[,12]#hispanic origin
census_appren[,13]#sex
census_appren[,14]#member of a labor union
census_appren[,15]#reason for unemployment
census_appren[,16]#full or part time employment stat
census_appren[,17]#capital gains
census_appren[,18]#capital losses: continuous
census_appren[,19]#dividends from stocks
census_appren[,20]#tax filer stat
census_appren[,21]#region of previous residence
census_appren[,22]#state of previous residence
census_appren[,23]#detailed household and family stat
census_appren[,24]#detailed household summary in household
census_appren[,25]#instance weight
census_appren[,26]#migration code-change in msa
census_appren[,27]#migration code-change in reg
census_appren[,28]#migration code-move within reg
census_appren[,29]#live in this house 1 year ago
census_appren[,30]#migration prev res in sunbelt?
census_appren[,31]#num persons worked for employer
census_appren[,32]#family members under 18
census_appren[,33]#country of birth father?
census_appren[,34]#country of birth mother?
census_appren[,35]#country of birth self?
census_appren[,36]#citizenship
census_appren[,37]#own business or self employed
census_appren[,38]#fill inc questionnaire for veteran's admin
census_appren[,39]#veterans benefits
census_appren[,40]#weeks worked in year
census_appren[,41]#year: 94, 95.
census_appren[,42]#gagne_an

#sélectionner des variables intuitivement et ça peut risquer de perdre des bonnes variables 
#Le modèle n'est pas encore fixé et il est donc possible de raffiner ou modifier les choix des variables 
census_partiel <- census_appren[c(1,2,5,8,9,10,13,16,19,36,37,39,40,41,42)]


#affecter les titres des colonnes en fonction du fichier census_income_metadata.txt
#utiliser les noms des colonnes de base de données du Census Bureau
#YEAR pour year, GAGNE pour gagne_an
names(census_partiel) <- c("AAGE", "ACLSWKR", "AHGA", "AMARITL", "AMJIND", "AMJOCC", "ASEX", "AWKSTAT", "DIVVAL", "PRCITSHP", "SEOTR", "VETYN", "WKSWORK", "YEAR", "GAGNE")

#attacher les colonnes aux noms pour simplification
#par exemple, pour faire appel les colonnes "GAGNE" au lieu de "census_partiel$GAGNE" 
attach(census_partiel)

#visualiser les variables
#pour les variables "continuous", visualiser par boxplot (boite de moustache)
#pour les variables "nominal", visualiser par barplot (histogramme)

#la distribution de 'age' selon -50000/+50000
A=AAGE[GAGNE== " - 50000."]
B=AAGE[GAGNE== " 50000+."]
boxplot(A,B,main="La distribution de 'age' selon -50000/+50000", xlab="-50000/+50000", ylab="Age")

#la comparaison de 'class of worker' selon -50000/+50000
C=ACLSWKR[GAGNE== " - 50000."]
D=ACLSWKR[GAGNE== " 50000+."]
mat<-rbind(summary(C),summary(D))
barplot(mat, main="La comparaison 'class of worker'", space=1, xlab="class of worker", ylab="-50000     +50000")

#la comparaison de 'education' selon -50000/+50000
E=AHGA[GAGNE== " - 50000."]
F=AHGA[GAGNE== " 50000+."]
mat<-rbind(summary(E),summary(F))
barplot(mat, main="La comparaison 'education'", space=1,xlab="education", ylab="-50000     +50000")

#la comparaison de 'marital stat' selon -50000/+50000
G=AMARITL[GAGNE== " - 50000."]
H=AMARITL[GAGNE== " 50000+."]
mat<-rbind(summary(G),summary(H))
barplot(mat, main="La comparaison 'marital stat'", space=1,xlab="marital stat", ylab="-50000     +50000")

#la comparaison de 'major industry code' selon -50000/+50000
H=AMJIND[GAGNE== " - 50000."]
I=AMJIND[GAGNE== " 50000+."]
mat<-rbind(summary(H),summary(I))
barplot(mat, main="La comparaison 'major industry code'", space=1,xlab="major industry code", ylab="-50000     +50000")

#la comparaison de 'major occupation code' selon -50000/+50000
J=AMJOCC[GAGNE== " - 50000."]
K=AMJOCC[GAGNE== " 50000+."]
mat<-rbind(summary(J),summary(K))
barplot(mat, main="La comparaison 'major occupation code'", space=1,xlab="major occupation code", ylab="-50000     +50000")

#la comparaison de 'sex' selon -50000/+50000
L=ASEX[GAGNE== " - 50000."]
M=ASEX[GAGNE== " 50000+."]
mat<-rbind(summary(L),summary(M))
barplot(mat, main="La comparaison 'sex'", space=1,xlab="sex", ylab="-50000     +50000")

#la comparaison de 'full or part time employment stat' selon -50000/+50000
N=AWKSTAT[GAGNE== " - 50000."]
O=AWKSTAT[GAGNE== " 50000+."]
mat<-rbind(summary(N),summary(O))
barplot(mat, main="La comparaison 'full or part time employment stat'", space=1,xlab="full or part time employment stat", ylab="-50000     +50000")

#la distribution de 'dividends from stocks' selon -50000/+50000
P=DIVVAL[GAGNE== " - 50000."]
Q=DIVVAL[GAGNE== " 50000+."]
boxplot(P,Q,main="La distribution de 'dividends from stocks' selon -50000/+50000", xlab="-50000/+50000", ylab="dividends from stocks")

#la comparaison de 'citizenship' selon -50000/+50000
R=PRCITSHP[GAGNE== " - 50000."]
S=PRCITSHP[GAGNE== " 50000+."]
mat<-rbind(summary(R),summary(S))
barplot(mat, main="La comparaison 'citizenship'", space=1,xlab="citizenship", ylab="-50000     +50000")

#la comparaison de 'own business or self employed' selon -50000/+50000
#'own business or self employed' a les valeurs 0,1,2, factoriser cette variable
SEOTR<-factor(SEOTR)
T=SEOTR[GAGNE== " - 50000."]
U=SEOTR[GAGNE== " 50000+."]
mat<-rbind(summary(T),summary(U))
barplot(mat, main="La comparaison 'own business or self employed'", space=1,xlab="own business or self employed", ylab="-50000     +50000")

#la comparaison de 'veterans benefits' selon -50000/+50000
VETYN<-factor(VETYN)
V=VETYN[GAGNE== " - 50000."]
W=VETYN[GAGNE== " 50000+."]
mat<-rbind(summary(V),summary(W))
barplot(mat, main="La comparaison 'veterans benefits'", space=1,xlab="veterans benefits", ylab="-50000     +50000")

#la distribution de 'weeks worked in year' selon -50000/+50000
X=WKSWORK[GAGNE== " - 50000."]
Y=WKSWORK[GAGNE== " 50000+."]
boxplot(X,Y,main="La distribution de 'weeks worked in year' selon -50000/+50000", xlab="-50000/+50000", ylab="weeks worked in year")

#la comparaison de 'year' selon -50000/+50000
YEAR<-factor(YEAR)
V=YEAR[GAGNE== " - 50000."]
W=YEAR[GAGNE== " 50000+."]
mat<-rbind(summary(V),summary(W))
barplot(mat, main="La comparaison 'year'", space=1,xlab="veterans benefits", ylab="-50000     +50000")



###################################################################################
census_5 <- read.csv("census_income_learn_50000.csv", header=F)


census_5[,37]<-factor(census_5[,37])#own business or self employed 37
census_5[,39]<-factor(census_5[,39])#veterans benefits 39
summary(census_5[,37])
summary(census_5[,39])


#1,2,5,8,9,10,13,16,19,36,37,39,40,41
summary(census_5[,1])
summary(census_5[,2])
summary(census_5[,5])
summary(census_5[,8])
summary(census_5[,9])
summary(census_5[,10])
summary(census_5[,13])
summary(census_5[,16])
summary(census_5[,19])
summary(census_5[,36])###
summary(census_5[,37])
summary(census_5[,39])
summary(census_5[,40])
summary(census_5[,41])
summary(census_5[,42])
#######################################################################################"

census_5_test <- read.csv("census_income_test_50000.csv", header=F)


census_5_test[,37]<-factor(census_5_test[,37])#own business or self employed 37
census_5_test[,39]<-factor(census_5_test[,39])#veterans benefits 39
summary(census_5_test[,37])
summary(census_5_test[,39])


#1,2,5,8,9,10,13,16,19,36,37,39,40,41
summary(census_5_test[,1])
summary(census_5_test[,2])
summary(census_5_test[,5])
summary(census_5_test[,8])
summary(census_5_test[,9])
summary(census_5_test[,10])
summary(census_5_test[,13])
summary(census_5_test[,16])
summary(census_5_test[,19])
summary(census_5_test[,36])###
summary(census_5_test[,37])#
summary(census_5_test[,39])#2
summary(census_5_test[,40])#x
summary(census_5_test[,41])#x
summary(census_5_test[,42])
####################################################################################



#répéter la sélection des variables pour reinitialiser les données
census_partiel <- census_appren[c(1,2,5,8,9,10,13,16,19,36,37,39,40,41,42)]
summary(census_appren[,1])
census_modele = matrix(0, nrow=15000,ncol=15)
k=0
for(i in 1:nrow(census_partiel))
{
	if(census_partiel[i,15]== " 50000+.")
 	{
		for(j in 1:ncol(census_partiel))
		{
			census_modele[k,j] = census_partiel[i,j]
		}
		k = k+1
	}
}
k
