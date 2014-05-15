#Exercice us_census_full

#importer les fichiers d'apprentissage  getOption("max.print") 
census_appren <- read.csv("census_income_learn.csv", header=F)

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



#Selon les variables sélectionnées et les résultats de visualisations, 
#le profil des gens qui gagnent plus de 50K$:
#age: leur ages se situe entre 38 et 53 et les valeurs sont plus convergent que la population
#class of worker: Private ou Self-employed-not incorporated ou Self-employed-incorporated
#education: Bachelors degree(BA AB BS) ou Masters degree(MA MS MEng MEd MSW MBA) ou High school graduate ou Some college but no degree
#marital stat: Married-civilian spouse present
#major occupation code: Executive admin and managerial ou Professional specialty
#sex: male a un poid trois fois plus que female
#full or part time employment stat:Children or Armed Forces ou Full-time schedules
#citizenship: Native- Born in the United States
#own business or self employed et veterans benefits: 0 et 2 (satisfaire en même temps)

#Les aspects à améliorer: la complexité de l'algorithme

#importer les fichiers de test 
census_test <- read.csv("census_income_test.csv", header=F)

#concevoir et appliquer algorithme pour census_income_test.csv

	#initialise deux matrice cal pour sauvegarder les informations des gens +50000
	#caloth pour sauvegarder les informations des gens -50000

	cal = matrix(0, nrow=1,ncol=nrow(census_test))
	calindex = 1
	caloth = matrix(0, nrow=1,ncol=nrow(census_test))
	calothindex = 1
	#nombre d'erreur à retouner par cette fonction
	k=0

	#privilège des variables sélectionnées décidées par les visualisations réalisées précedentes
	#plus une variable différencie entre -50000 et +50000, plus haute cette variable a une privilège 
	age = 7
	classWorker = 6
	education = 3
	marital = 8
	occupation = 3
	sex = 7
	fullPart = 5
	citizenship = 10
	ownSelf = 8
	benefit = 10	

	for(i in 1:nrow(census_test))
	{
		poid=0

		#Selon les différentes valeurs d'une variable, ajouter différentes points à poid

		#age
		if(census_test[i,1]>38 & census_test[i,1]<45)
		{
			poid = poid + 70*age
		}
		else
		{
			poid = poid + 30*age

		}

		#class of worker
     		if(census_test[i,2]==" Private")
		{
			poid = poid + 60*classWorker
		}
		else if(census_test[i,2]==" Self-employed-not incorporated" | census_test[i,2]==" Self-employed-incorporated" )
		{
			poid = poid + 8*classWorker
		}
		else if(census_test[i,2]==" Local government" | census_test[i,2]==" Not in universe")
		{
			poid = poid + 6*classWorker
		}
		else if(census_test[i,2]==" Federal government" | census_test[i,2]==" State government")
		{
			poid = poid + 4*classWorker
		}

		#education
		if(census_test[i,5]==" Bachelors degree(BA AB BS)")
		{
			poid = poid + 32*education
		}
		else if(census_test[i,5]==" Masters degree(MA MS MEng MEd MSW MBA)" )
		{
			poid = poid + 16*education
		}
		else if(census_test[i,5]==" High school graduate" | census_test[i,5]==" Some college but no degree")
		{
			poid = poid + 14*education
		}
		else if(census_test[i,5]==" Prof school degree (MD DDS DVM LLB JD)")
		{
			poid = poid + 8*education
		}
		else if(census_test[i,5]==" Doctorate degree(PhD EdD)")
		{
			poid = poid + 5*education
		}
		else if(census_test[i,5]==" Associates degree-occup /vocational" | census_test[i,5]==" Associates degree-academic program")
		{
			poid = poid + 3*education
		}

		#marital stat
		if(census_test[i,8]==" Married-civilian spouse present")
		{
			poid = poid + 78*marital 
		}
		else if(census_test[i,8]==" Divorced" | census_test[i,8]==" Never married" )
		{
			poid = poid + 8*marital
		}
		else if(census_test[i,8]==" Widowed")
		{
			poid = poid + 3*marital
		}
		
		#major occupation code
		if(census_test[i,10]==" Executive admin and managerial" | census_test[i,10]==" Professional specialty")
		{
			poid = poid + 28*occupation
		}
 		else if(census_test[i,10]==" Sales")
		{
			poid = poid + 12*occupation
		}
		else if(census_test[i,10]==" Not in universe" | census_test[i,10]==" Precision production craft & repair")
		{
			poid = poid + 8*occupation
		}
		else if(census_test[i,10]==" Adm support including clerical")
		{
			poid = poid + 4*occupation
		}
		
		#sex
		if(census_test[i,13]==" Female")
		{
			poid = poid + 21*sex
		}
 		else 
		{
			poid = poid + 79*sex
		}
		
		#full or part time employment stat
		if(census_test[i,16]==" Children or Armed Forces" | census_test[i,16]==" Full-time schedules")
		{
			poid = poid + 45*fullPart
		}
 		else if(census_test[i,16]==" Not in labor force" | census_test[i,16]==" PT for non-econ reasons usually FT")
		{
			poid = poid + 3*fullPart
		}

		#citizenship
		if(census_test[i,36]==" Native- Born in the United States")
		{
			poid = poid + 90*citizenship
		}
 		else if(census_test[i,16]==" Foreign born- Not a citizen of U S" | census_test[i,16]==" Foreign born- U S citizen by naturalization")
		{
			poid = poid + 4*citizenship
		}

		#own business or self employed
		if(census_test[i,37]==0)
		{
			poid = poid + 84*ownSelf
		}

		#veterans benefits
		if(census_test[i,39]==2)
		{
			poid = poid + 98*citizenship
		}
		
		if(census_test[i,42]==" 50000+.")
		{
			cal[1,calindex]=poid
			calindex = calindex +1
		}	
		else	
		{
			caloth[1,calothindex]=poid
			calothindex = calothindex +1
		}

            #fixer un seuil pour "poid" = 4000, si poid > 4000, prévoir que cette personne gagne +50000 sinon -50000
		#vérifier la 42eme colonne, si poid > 4000 et la 42eme colonne est -50000, c'est un erreur et incrémente k

		if(poid > 4000 & census_test[i,42]==" 50000+.")
		{
			k=k+1
		}
	}

