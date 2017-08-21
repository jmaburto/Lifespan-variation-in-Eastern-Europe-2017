########### Check if I get same mortality rates from Alyson, this code can be ran after 5_4_Grogorev_LE
####### Years to compare 1994 and 1997
####### I will compare 3 sources, the dirst one as Alyson's, the second one HMD combined with
####### structure from HoC.
####### And the third one directly obtained from HoC
Age           <-	c(0,1,seq(5,85,5))
#1994
HeartDisease.1      <- c(0.0001,	0.00001,	0.00001,	0.00001,	0.00006,	0.00015,	0.00036,	0.00092,	0.00184,	0.0034,	0.00556,	0.00842,	0.01136,	0.01646,	0.02126,	0.02885,	0.04219,	0.06054,	0.09822)
Othercirculatory.1	<- c(0.00003,	0,	0,	0,	0.00002,	0.00004,	0.00007,	0.00015,	0.0003,	0.00067,	0.00131,	0.00259,	0.00429,	0.00849,	0.01368,	0.02173,	0.03683,	0.0591,	0.09989)
Smokingcancer.1	    <- c(0,	0,	0,	0,	0,	0,	0,	0.00001,	0.00003,	0.00011,	0.00027,	0.00051,	0.00066,	0.00088,	0.00097,	0.00094,	0.00089,	0.00096,	0.00098)
Othercancer.1	      <- c(0.00003,	0.00004,	0.00004,	0.00003,	0.00005,	0.00006,	0.00007,	0.00009,	0.00016,	0.00033,	0.00059,	0.00114,	0.00184,	0.00287,	0.00396,	0.00494,	0.00581,	0.00562,	0.00435)
Dx.External.1       <- c(	0.00037,	0.00029,	0.0002,	0.00027,	0.00134,	0.00271,	0.00346,	0.00419,	0.00486,	0.0056,	0.00596,	0.00625,	0.00548,	0.00457,	0.00356,	0.00269,	0.0028,	0.00331,	0.00431)
Dx.Illdefined.1	    <- c(0.00058,	0.00003,	0.00001,	0.00001,	0.00005,	0.00011,	0.00015,	0.00025,	0.00036,	0.00051,	0.00062,	0.00086,	0.00079,	0.00074,	0.00063,	0.00086,	0.00298,	0.01286, 0.03001)
Other.1	            <- c(0.02051,	0.0008,	0.0004,	0.00033,	0.00062,	0.00102,	0.00142,	0.00207,	0.00312,	0.00456,	0.00645,	0.00924,	0.01176,	0.01564,	0.01865,	0.02119,	0.02516,	0.02664,	0.02979)

Russia.S1.1994 <- cbind(HeartDisease.1,Othercirculatory.1,Smokingcancer.1,Othercancer.1,Dx.External.1,Dx.Illdefined.1,Other.1)

#1997

HeartDisease.2      <- 	c(0.00008	,	0.00001	,	0.00001	,	0.00001	,	0.00006	,	0.00015	,	0.00029	,	0.0006	,	0.00121	,	0.00225	,	0.00376	,	0.0057	,	0.00871	,	0.01273	,	0.01844	,	0.02574	,	0.03795	,	0.05487	,	0.08744	)
Othercirculatory.2  <- 	c(0.00003	,	0	,	0	,	0	,	0.00002	,	0.00003	,	0.00006	,	0.00012	,	0.00024	,	0.0005	,	0.00102	,	0.00196	,	0.00376	,	0.00694	,	0.01273	,	0.01992	,	0.0323	,	0.0539	,	0.09127	)
Smokingcancer.2     <- 	c(0	,	0	,	0	,	0	,	0	,	0	,	0	,	0.00001	,	0.00002	,	0.00008	,	0.00023	,	0.00042	,	0.00061	,	0.0008	,	0.00089	,	0.00098	,	0.00094	,	0.00084	,	0.00084	)
Othercancer.2       <- 	c(0.00003	,	0.00004	,	0.00003	,	0.00003	,	0.00004	,	0.00005	,	0.00006	,	0.00009	,	0.00015	,	0.00028	,	0.00052	,	0.00091	,	0.00169	,	0.00253	,	0.0037	,	0.00476	,	0.00557	,	0.00571	,	0.0046	)
Dx.External.2       <- 	c(0.00035	,	0.00023	,	0.00015	,	0.00022	,	0.00112	,	0.00246	,	0.00275	,	0.00311	,	0.00341	,	0.00377	,	0.00394	,	0.00411	,	0.0039	,	0.0033	,	0.00301	,	0.00239	,	0.00234	,	0.00297	,	0.00379	)
Dx.Illdefined.2     <- 	c(0.00066	,	0.00003	,	0.00001	,	0.00001	,	0.00005	,	0.00012	,	0.00014	,	0.00019	,	0.00023	,	0.00032	,	0.00041	,	0.00051	,	0.00056	,	0.00059	,	0.00052	,	0.0007	,	0.00239	,	0.01342	,	0.03106	)
Other.2             <- 	c(0.01839	,	0.00077	,	0.00036	,	0.0003	,	0.00058	,	0.00104	,	0.00129	,	0.00176	,	0.00242	,	0.00347	,	0.00482	,	0.00664	,	0.00988	,	0.01268	,	0.01675	,	0.01963	,	0.0228	,	0.02476	,	0.02691	)

Russia.S1.1997 <- cbind(HeartDisease.2,Othercirculatory.2,Smokingcancer.2,Othercancer.2,Dx.External.2,Dx.Illdefined.2,Other.2)

gdata::keep(Russia.S1.1994,Russia.S1.1997,sure=T)

########### Second source
XYZ <- c("BLR","CZE","EST","LTU","LVA","POL","RUS","UKR")
k <- XYZ[7]
#i <- 1
#j <- 1994

Mx.Cause <- NULL
for (k in XYZ){
  l <- 2010
  if (k=="POL") l <- 2009
  for (i in 1:2){
    for (j in 1994:l){
      if (i == 1) Sx <- "m"
      if (i == 3) Sx <- "f"
      D  <- subset(COD.structure,Country==k & Sex==i & Year==j)
      E  <- subset(Eastern_LT_5,PopName==k & Sex==Sx & Year==j)
      M1 <- as.matrix(D[,5:9])
      v1 <- matrix(c(rep(0,20),rep(1,5)),nrow = 5,ncol = 5)
      M2 <- rbind(M1,v1)
      cMx <- M2*E$mx
      row.names(cMx)<-NULL
      cMx <- as.data.frame(cMx)
      M <- cbind(Country=rep(k,dim(E)[1]),Sex=rep(i,dim(E)[1]),Year=rep(j,dim(E)[1]))
      M <- as.data.frame(M)
      M$Country <- as.character(M$Country)
      M$Sex <- as.numeric(levels(M$Sex))[M$Sex]
      M$Year <- as.numeric(levels(M$Year))[M$Year]
      M$Age <- E$Age
      M$ax <-E$ax
      Mx <- cbind(M,cMx)
      Mx.Cause <- rbind(Mx.Cause,Mx)
    }
  }
}


########### Third source
setwd("/Users/josemanuelaf/Desktop/Aburto_vanRaalte_2016/")
RussiaMx <- read.csv(file = "R/Comparison with Grigorev 2014 PDR/RUS_m_interm_idr.csv", header = T)
RussiaMx <- subset(RussiaMx, year==1994 | year==1997 & sex==1)
