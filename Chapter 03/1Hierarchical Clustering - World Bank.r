R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
Copyright (C) 2015 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Workspace loaded from ~/.RData]

 
 wbclust=read.csv("d:/WBClust2013.csv",header=T)
 head(wbclust)
 
 wbnorm<-scale(wbclust[,2:13])
 wbnorm
 
 rownames(wbnorm)=wbclust[,1]
 rownames(wbnorm)
 
 dist1<-dist(wbnorm, method="euclidean")
 
 clust1<-hclust(dist1,method="ward.D")
 clust1
 
 plot(clust1,labels= wbclust$Country, cex=0.7, xlab="",ylab="Distance",main="Clustering for 80 Most Populous Countries")
 
 
 rect.hclust(clust1,k=5)
 
 cuts=cutree(clust1,k=5)
 
 cuts
               China                India        United States            Indonesia 
                   1                    2                    1                    2 
              Brazil             Pakistan              Nigeria           Bangladesh 
                   2                    3                    4                    3 
  Russian Federation                Japan               Mexico          Philippines 
                   1                    1                    2                    2 
            Ethiopia              Vietnam     Egypt, Arab Rep.              Germany 
                   4                    5                    2                    5 
              Turkey             Thailand               France       United Kingdom 
                   1                    5                    1                    1 
               Italy         South Africa          Korea, Rep.             Tanzania 
                   1                    1                    5                    4 
            Colombia                Spain              Ukraine                Kenya 
                   2                    1                    5                    4 
             Algeria                Sudan               Canada                 Iraq 
                   2                    3                    1                    2 
             Morocco                 Peru           Uzbekistan             Malaysia 
                   2                    2                    2                    5 
        Saudi Arabia                Nepal                Ghana           Mozambique 
                   1                    3                    3                    4 
           Australia             Cameroon               Angola            Sri Lanka 
                   1                    4                    4                    3 
       Cote d'Ivoire                Chile           Kazakhstan          Netherlands 
                   4                    1                    1                    5 
             Ecuador            Guatemala             Cambodia               Zambia 
                   2                    2                    3                    4 
            Zimbabwe              Senegal              Belgium               Greece 
                   3                    3                    5                    1 
             Tunisia              Bolivia       Czech Republic             Portugal 
                   5                    2                    5                    1 
  Dominican Republic                Benin                Haiti              Hungary 
                   2                    3                    3                    5 
              Sweden              Belarus           Azerbaijan United Arab Emirates 
                   5                    5                    2                    5 
             Austria           Tajikistan             Honduras          Switzerland 
                   5                    3                    3                    5 
              Israel             Bulgaria               Serbia                 Togo 
                   1                    5                    1                    4 
            Paraguay               Jordan          El Salvador            Nicaragua 
                   3                    5                    2                    3 
 
 for (i in 1:5){
     print(paste("Countries in Cluster ",i))
     print(wbclust$Country[cuts==i])
     print (" ")
 }
[1] "Countries in Cluster  1"
 [1] China              United States      Russian Federation Japan             
 [5] Turkey             France             United Kingdom     Italy             
 [9] South Africa       Spain              Canada             Saudi Arabia      
[13] Australia          Chile              Kazakhstan         Greece            
[17] Portugal           Israel             Serbia            
80 Levels: Algeria Angola Australia Austria Azerbaijan Bangladesh Belarus ... Zimbabwe
[1] " "
[1] "Countries in Cluster  2"
 [1] India              Indonesia          Brazil             Mexico            
 [5] Philippines        Egypt, Arab Rep.   Colombia           Algeria           
 [9] Iraq               Morocco            Peru               Uzbekistan        
[13] Ecuador            Guatemala          Bolivia            Dominican Republic
[17] Azerbaijan         El Salvador       
80 Levels: Algeria Angola Australia Austria Azerbaijan Bangladesh Belarus ... Zimbabwe
[1] " "
[1] "Countries in Cluster  3"
 [1] Pakistan   Bangladesh Sudan      Nepal      Ghana      Sri Lanka  Cambodia  
 [8] Zimbabwe   Senegal    Benin      Haiti      Tajikistan Honduras   Paraguay  
[15] Nicaragua 
80 Levels: Algeria Angola Australia Austria Azerbaijan Bangladesh Belarus ... Zimbabwe
[1] " "
[1] "Countries in Cluster  4"
 [1] Nigeria       Ethiopia      Tanzania      Kenya         Mozambique   
 [6] Cameroon      Angola        Cote d'Ivoire Zambia        Togo         
80 Levels: Algeria Angola Australia Austria Azerbaijan Bangladesh Belarus ... Zimbabwe
[1] " "
[1] "Countries in Cluster  5"
 [1] Vietnam              Germany              Thailand            
 [4] Korea, Rep.          Ukraine              Malaysia            
 [7] Netherlands          Belgium              Tunisia             
[10] Czech Republic       Hungary              Sweden              
[13] Belarus              United Arab Emirates Austria             
[16] Switzerland          Bulgaria             Jordan              
80 Levels: Algeria Angola Australia Austria Azerbaijan Bangladesh Belarus ... Zimbabwe
[1] " "