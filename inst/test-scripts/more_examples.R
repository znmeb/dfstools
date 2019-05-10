#archetypoids.Rd
#SPORTIVE EXAMPLE:
#Database:
if(nzchar(system.file(package = "SportsAnalytics"))){
  data("NBAPlayerStatistics0910", package = "SportsAnalytics")
}      
mat <- NBAPlayerStatistics0910[,c("TotalMinutesPlayed","FieldGoalsMade")]
rownames(mat) <- NULL

#Calculating archetypes by using the archetype algorithm:
#Data preprocessing:
preproc <- preprocessing(mat,stand=TRUE,percAccomm=1)

#For reproducing results, seed for randomness:
set.seed(4321)
#Run archetype algorithm repeatedly from 1 to 15 archetypes:
numArch <- 15
lass15 <- stepArchetypesRawData(data=preproc$data,numArch=1:numArch,numRep=20,verbose=FALSE)
screeplot(lass15) 

#Calculating real archetypes:
numArchoid <- 3 #number of archetypoids.
res_ns <- archetypoids(numArchoid,preproc$data,huge=200,step=FALSE,ArchObj=lass15,
                       nearest="cand_ns",sequ=TRUE)
arquets_ns <- NBAPlayerStatistics0910[res_ns[[1]],c("Name","TotalMinutesPlayed","FieldGoalsMade")]

res_alpha <- archetypoids(numArchoid,preproc$data,huge=200,step=FALSE,ArchObj=lass15,
                          nearest="cand_alpha",sequ=TRUE)
arquets_alpha <- NBAPlayerStatistics0910[res_alpha[[1]],
                                         c("Name","TotalMinutesPlayed","FieldGoalsMade")]

res_beta <- archetypoids(numArchoid,preproc$data,huge=200,step=FALSE,ArchObj=lass15,
                         nearest="cand_beta",sequ=TRUE)
arquets_beta <- NBAPlayerStatistics0910[res_beta[[1]],
                                        c("Name","TotalMinutesPlayed","FieldGoalsMade")]

col_pal <- RColorBrewer::brewer.pal(7, "Set1")
col_black <- rgb(0, 0, 0, 0.2)

plot(mat, pch = 1, col = col_black, xlim = c(0,3500), main = "NBA archetypal basketball         
     players \n obtained in Eugster (2012) \n and with our proposal", 
     xlab = "Total minutes played", ylab = "Field goals made")
points(mat[as.numeric(rownames(arquets_ns)),], pch = 4, col = col_pal[1]) 
points(mat[as.numeric(rownames(arquets_alpha)),], pch = 4, col = col_pal[1]) 
points(mat[as.numeric(rownames(arquets_beta)),], pch = 4, col = col_pal[1]) 
plotrix::textbox(c(50,800), 50, "Travis Diener") 
plotrix::textbox(c(2800,3500), 780, "Kevin Durant", col = "blue")
plotrix::textbox(c(2800,3500), 270, "Jason Kidd", col = "blue")
legend("topleft",c("archetypes of Eugster","archetypes of our proposal"), 
       lty= c(1,NA), pch = c(NA,22), col = c("blue","black"))


#If a specific number of archetypes is computed only:
numArchoid <- 3
set.seed(4321)
lass3 <- stepArchetypesRawData(data=preproc$data,numArch=numArchoid,numRep=3,verbose=FALSE)
res3 <- archetypoids(numArchoid,preproc$data,huge=200,step=FALSE,ArchObj=lass3,nearest="cand_ns",
                     sequ=FALSE,aux=2)
arquets3 <- NBAPlayerStatistics0910[res3[[1]],c("Name","TotalMinutesPlayed","FieldGoalsMade")]



######
#HartiganShapes.Rd
#SIMULATION STUDY:
#Definition of the cluster of cubes:
Ms_cube <- cube8landm
#Ms_cube <- cube34landm #for the case of 34 landmarks.
colMeans(Ms_cube)
dim(Ms_cube) 
shapes::plotshapes(Ms_cube[,,1])

#Number of landmarks and variables:
k_cube <- dim(Ms_cube)[1]
vars_cube <- k_cube * dim(Ms_cube)[2] 

#Covariance matrix (0.01, 9, 36):
sigma_cube <- 0.01
Sigma_cube <- diag(sigma_cube,vars_cube)

#Sample size of each cluster (25, 250, 450):
n_cube <- 25

#Cluster of cubes:
simu1_cube <- mvtnorm::rmvt(n_cube,Sigma_cube,df=99)[,c(1 : k_cube * dim(Ms_cube)[2] 
                                                        - 2, 1 : k_cube * dim(Ms_cube)[2] - 1, 1 : k_cube * dim(Ms_cube)[2])]
Simu1_cube <- as.vector(Ms_cube) + t(simu1_cube) 
dim(Simu1_cube) 

#Labels vector to identify the elements in the cluster of cubes:
etiqs_cl1 <- paste("cube_", 1:n_cube, sep = "")

#First cluster:
cl1 <- array(Simu1_cube, dim = c(k_cube, dim(Ms_cube)[2], n_cube), 
             dimnames = list(NULL, NULL, etiqs_cl1))
colMeans(cl1) 
dim(cl1) 

#Definition of the cluster of parallelepipeds:
Ms_paral <- parallelep8landm
#Ms_paral <- parallelep34landm #for the case of 34 landmarks.
colMeans(Ms_paral)
dim(Ms_paral) 

#Number of landmarks and variables:
k_paral <- dim(Ms_paral)[1] 
vars_paral <- k_paral * dim(Ms_paral)[2] 

#Covariance matrix (0.01, 9, 36):
sigma_paral <- 0.01
Sigma_paral <- diag(sigma_paral,vars_paral)

#Sample size of each cluster (25, 250, 450):
n_paral <- 25

#Cluster of parallelepipeds:
simu1_paral <- mvtnorm::rmvt(n_paral, Sigma_paral, df = 99)[,c(1 : k_paral * 
                                                                 dim(Ms_paral)[2] - 2, 1 : k_paral * dim(Ms_paral)[2] - 
                                                                 1, 1 : k_paral * dim(Ms_paral)[2])]
Simu1_paral <- as.vector(Ms_paral) + t(simu1_paral) 
dim(Simu1_paral) 

#Labels vector to identify the elements in the cluster of parallelepipeds:
etiqs_cl2 <- paste("Parallelepiped_", 1:n_paral, sep = "")

#Second cluster:
cl2 <- array(Simu1_paral, dim = c(k_paral, dim(Ms_paral)[2], n_paral), 
             dimnames = list(NULL, NULL, etiqs_cl2))
colMeans(cl2) 
dim(cl2) 

#Combine both clusters: 
array3D <- abind::abind(cl1,cl2)
str(array3D)
shapes3dShapes(array3D[,,1], loop = 0, type = "p", color = 2, joinline = c(1:1), 
               axes3 = TRUE, rglopen = TRUE, main = "First figure")

#First, the Lloyd algorithm is executed and then the Hartigan algorithm with 
#the same initial values used by the Lloy algorithm is executed:
numClust <- 2 ; algSteps <- 3 ; niter <- 3 ; stopCr <- 0.0001
resLLSim <- LloydShapes(array3D,numClust,algSteps,niter,stopCr,TRUE,TRUE)
resHASim <- HartiganShapes(array3D,numClust,algSteps,niter,stopCr,TRUE,TRUE,resLLSim$initials,TRUE)



######
#LloydShapes.Rd
#SIMULATION STUDY:
#Definition of the cluster of cubes:
Ms_cube <- cube8landm
#Ms_cube <- cube34landm #for the case of 34 landmarks.
colMeans(Ms_cube)
dim(Ms_cube) 
shapes::plotshapes(Ms_cube[,,1])

#Number of landmarks and variables:
k_cube <- dim(Ms_cube)[1]
vars_cube <- k_cube * dim(Ms_cube)[2] 

#Covariance matrix (0.01, 9, 36):
sigma_cube <- 0.01
Sigma_cube <- diag(sigma_cube,vars_cube)

#Sample size of each cluster (25, 250, 450):
n_cube <- 25

#Cluster of cubes:
simu1_cube <- mvtnorm::rmvt(n_cube,Sigma_cube,df=99)[,c(1 : k_cube * dim(Ms_cube)[2] 
                                                        - 2, 1 : k_cube * dim(Ms_cube)[2] - 1, 1 : k_cube * dim(Ms_cube)[2])]
Simu1_cube <- as.vector(Ms_cube) + t(simu1_cube) 
dim(Simu1_cube) 

#Labels vector to identify the elements in the cluster of cubes:
etiqs_cl1 <- paste("cube_", 1:n_cube, sep = "")

#First cluster:
cl1 <- array(Simu1_cube, dim = c(k_cube, dim(Ms_cube)[2], n_cube), 
             dimnames = list(NULL, NULL, etiqs_cl1))
colMeans(cl1) 
dim(cl1) 

#Definition of the cluster of parallelepipeds:
Ms_paral <- parallelep8landm
#Ms_paral <- parallelep34landm #for the case of 34 landmarks.
colMeans(Ms_paral)
dim(Ms_paral) 

#Number of landmarks and variables:
k_paral <- dim(Ms_paral)[1] 
vars_paral <- k_paral * dim(Ms_paral)[2] 

#Covariance matrix (0.01, 9, 36):
sigma_paral <- 0.01
Sigma_paral <- diag(sigma_paral,vars_paral)

#Sample size of each cluster (25, 250, 450):
n_paral <- 25

#Cluster of parallelepipeds:
simu1_paral <- mvtnorm::rmvt(n_paral, Sigma_paral, df = 99)[,c(1 : k_paral * 
                                                                 dim(Ms_paral)[2] - 2, 1 : k_paral * dim(Ms_paral)[2] - 1, 
                                                               1 : k_paral * dim(Ms_paral)[2])]
Simu1_paral <- as.vector(Ms_paral) + t(simu1_paral) 
dim(Simu1_paral) 

#Labels vector to identify the elements in the cluster of parallelepipeds:
etiqs_cl2 <- paste("Parallelepiped_", 1:n_paral, sep = "")

#Second cluster:
cl2 <- array(Simu1_paral, dim = c(k_paral, dim(Ms_paral)[2], n_paral), 
             dimnames = list(NULL, NULL, etiqs_cl2))
colMeans(cl2) 
dim(cl2) 

#Combine both clusters: 
array3D <- abind::abind(cl1,cl2)
str(array3D)
shapes3dShapes(array3D[,,1], loop = 0, type = "p", color = 2, joinline = c(1:1),
               axes3 = TRUE, rglopen = TRUE, main = "First figure")

numClust <- 2 ; algSteps <- 5 ; niter <- 5 ; stopCr <- 0.0001
resLLSim <- LloydShapes(array3D, numClust, algSteps, niter, stopCr, TRUE, TRUE)



######
#plotPrototypes.Rd
variable <- "hip"
range(dataTrimowa[,variable])
#[1] 83.6 152.1
ylim <- c(80,160)
title <- "Prototypes \n bust vs hip"

plotPrototypes(dataTrimowa, prototypes, numSizes, bustVariable, 
               variable, color, xlim, ylim, title, FALSE)
plotPrototypes(dataTrimowa, prototypes, numSizes, bustVariable, 
               variable, color, xlim, ylim, title, TRUE)

bustVariable <- "bust"
xlim <- c(72, 132)
color <- c("black", "red", "green", "blue", "cyan", "brown", "gray", 
           "deeppink3", "orange", "springgreen4", "khaki3", "steelblue1")

variable <- "necktoground"
range(dataTrimowa[,variable])
#[1] 117.6 154.9
ylim <- c(116, 156)
title <- "Prototypes \n bust vs neck to ground"

plotPrototypes(dataTrimowa, prototypes, numSizes, bustVariable, 
               variable, color, xlim, ylim, title, FALSE)
plotPrototypes(dataTrimowa, prototypes, numSizes, bustVariable, 
               variable, color, xlim, ylim, title, TRUE)

variable <- "waist"
range(dataTrimowa[,variable])
#[1]  58.6 133.0
ylim <- c(56,136)
title <- "Prototypes \n bust vs waist"

plotPrototypes(dataTrimowa, prototypes, numSizes, bustVariable, 
               variable, color, xlim, ylim, title, FALSE)
plotPrototypes(dataTrimowa, prototypes, numSizes, bustVariable, 
               variable, color, xlim, ylim, title, TRUE)


#AN EXAMPLE FOR HIPAM ALGORITHM:
dataHipam <- sampleSpanishSurvey
bust <- dataHipam$bust
bustSizes <- bustSizesStandard(seq(74, 102, 4), seq(107, 131, 6))

type <- "IMO" #type <- "MO" for $HIPAM_{MO}$
maxsplit <- 5 ; orness <- 0.7 
ah <- c(23, 28, 20, 25, 25)

set.seed(2013)
res_hipam <- list() ; class(res_hipam) <- "hipamAnthropom"
numSizes <- bustSizes$nsizes
for(i in 1 : (numSizes - 1)){
  data =  dataHipam[(bust >= bustSizes$bustCirc[i]) & (bust < bustSizes$bustCirc[i + 1]), ]   
  dataMat <- as.matrix(data)
  res_hipam[[i]] <- hipamAnthropom(dataMat, maxsplit = maxsplit, orness = orness, type = type,
                                   ah = ah, verbose = FALSE) 
}  
#str(res_hipam)

fitmodels <- anthrCases(res_hipam, oneSize = FALSE, numSizes)

bustVariable <- "bust"
xlim <- c(72, 132)
color <- c("black", "red", "green", "blue", "cyan", "brown", "gray", 
           "deeppink3", "orange", "springgreen4", "khaki3", "steelblue1")

variable <- "hip"
ylim <- c(80, 160)
title <- "Fit models HIPAM_IMO \n bust vs hip"

plotPrototypes(dataHipam, fitmodels, numSizes, bustVariable, 
               variable, color, xlim, ylim, title, FALSE)



######
#plotTrimmOutl.Rd
variable <- "hip"
range(dataTrimowa[,variable])
#[1] 83.6 152.1
ylim <- c(80,160)
main <- "Trimmed women \n bust vs hip"

plotTrimmOutl(dataTrimowa,trimmed, numSizes, bustVariable, variable, col, xlim, ylim, main)

variable <- "necktoground"
range(dataTrimowa[,variable])
#[1] 117.6 154.9
ylim = c(110,160)
main <- "Trimmed women \n bust vs neck to ground"

plotTrimmOutl(dataTrimowa,trimmed, numSizes, bustVariable, variable, col, xlim, ylim, main)

variable <- "waist"
range(dataTrimowa[,variable])
#[1]  58.6 133.0
ylim <- c(50,140)
main <- "Trimmed women \n bust vs waist"

plotTrimmOutl(dataTrimowa,trimmed, numSizes, bustVariable, variable, col, xlim, ylim, main)


#AN EXAMPLE FOR HIPAM ALGORITHM:
dataHipam <- sampleSpanishSurvey
bust <- dataHipam$bust
bustSizes <- bustSizesStandard(seq(74, 102, 4), seq(107, 131, 6))

type <- "IMO"
maxsplit <- 5 ; orness <- 0.7
ah <- c(23, 28, 20, 25, 25)

set.seed(2013)
res_hipam <- list() ; class(res_hipam) <- "hipamAnthropom"
for(i in 1 : (bustSizes$nsizes - 1)){
  data = dataHipam[(bust >= bustSizes$bustCirc[i]) & (bust < bustSizes$bustCirc[i + 1]), ]
  dataMat <- as.matrix(data)
  res_hipam[[i]] <- hipamAnthropom(dataMat, maxsplit = maxsplit, orness = orness, type = type,
                                   ah = ah, verbose = FALSE)
}
outliers <- trimmOutl(res_hipam, oneSize = FALSE, numSizes)

bustVariable <- "bust"
xlim <- c(72, 132)
color <- c("black", "red", "green", "blue", "cyan", "brown", "gray", "deeppink3", "orange", 
           "springgreen4", "khaki3", "steelblue1")
variable <- "hip"
ylim <- c(80, 160)
title_outl <- "Outlier women HIPAM_IMO \n bust vs hip"

plotTrimmOutl(dataHipam, outliers, numSizes, bustVariable, variable, color, 
              xlim, ylim, title_outl)



######
#skeletonsArchetypal.Rd
#AN EXAMPLE FOR THE ARCHETYPOIDS:
#COCKPIT DESIGN PROBLEM:
USAFSurvey_First50 <- USAFSurvey[1 : 25, ]
#Variable selection:
variabl_sel <- c(48, 40, 39, 33, 34, 36)
#Changing to inches: 
USAFSurvey_First50_inch <- USAFSurvey_First50[,variabl_sel] / (10 * 2.54)

#Data preprocessing:
USAFSurvey_preproc <- preprocessing(USAFSurvey_First50_inch, TRUE, 0.95, TRUE)

set.seed(2010)
numArch <- 5 ; numRep <- 2
lass <- stepArchetypesRawData(data = USAFSurvey_preproc$data, 
                              numArch=1:numArch, numRep = numRep, 
                              verbose = FALSE)
screeplot(lass)

numArchoid <- 3
res_ns <- archetypoids(numArchoid, USAFSurvey_preproc$data, huge = 200, step = FALSE, 
                       ArchObj = lass, nearest = "cand_ns" , sequ = TRUE)

#Looking for the individuals in the non standardized database: 
aux <- USAFSurvey_First50_inch[setdiff(1:dim(USAFSurvey_First50_inch)[1],
                                       USAFSurvey_preproc$indivNo),]
rownames(aux) <- 1:dim(USAFSurvey_preproc$data)[1]

skeletonsArchetypal(aux[res_ns[[1]][1],], "Archetypoid 1")