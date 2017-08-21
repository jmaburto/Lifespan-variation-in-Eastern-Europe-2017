
############# Create a function for estimate differences in life expectancy
absolute.change <- function(x=edagger){
  x2 <- diff(x=x,lag=1)
  return(x2)
}



relative.change <- function(x=edagger){
  x1 <- x[-length(x)]
  x2 <- diff(x=x,lag=1)
  x3 <- x2/x1
  return(x3)
}

#col.fig.F1b <- c("gray0","gray28","gray19", "gray28", "gray47"
#                 ,"gray54","gray65","gray78","gray0","gray19","gray0","gray0")

col.fig.F1b <- c("red","blue","yellow", "green", "darkblue"
                 ,"magenta","gray65","cadetblue1","black","chartreuse1","coral1","brown2")

pch.fig.F1b <- c(9:20)



d_le <- function(ex){
  dif <- c(diff(ex),0)
}

Y <- function(Year,lag.2){  
  seq(min(Year),max(Year)-lag.2,1)
}


e.dagger <- function(fx,ex,ax=ax){
  l <- length(ex)
  v <- (sum(fx[-l]* (ex[-l] + ax[-l]*(ex[-1]-ex[-l]) )) + ex[l])
  return(v)         
}


K.h <- function(fx,ex,ax=ax){
  l <- length(ex)
  v <- (sum(fx[-l]* (ex[-l] + ax[-l]*(ex[-1]-ex[-l]) )) + ex[l])
  k <- v/ex[1]
  return(k)
}

makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

my.settings1 <- list(  
  strip.background=list(col="grey"),
  strip.border=list(col="black")
 ,axis.line=list(col=c(0))
)

my.settings1.1 <- list(  
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)



my.settings2 <- list(  
  strip.background=list(col="grey"),
  strip.border=list(col="black")  
)

my.settings <- list(  
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)



my.panel.bands<-function(x, y, upper,lower,age,fill,col,subscripts, ..., font,fontface)
{ 
  up <- na.omit(upper[subscripts])
  low <- na.omit(lower[subscripts])
  xx<-age[subscripts][1:length(up)]
  panel.polygon(c(xx, rev(xx)), c(low, rev(up)),col = fill, border = FALSE,...)
}

d_le <- function(ex){
  dif <- c(diff(ex),0)
}

d_le.2 <- function(ex){
  dif <- c(diff(ex),0)
}


mycol <- c(makeTransparent("red", alpha=90),makeTransparent("lightgreen", alpha=90),makeTransparent("blue", alpha=90),
           makeTransparent("magenta", alpha=50))


my.fill2 <- c("blue","black","red")

dx.Fig <- function(Data1=subset(dx3,dx3$Sex=="M"),lcolor="blue",fillcol=makeTransparent("blue",90),
                   upci=dx3$up.ci,lowci=dx3$low.ci,a=dx3$Age,main="Distribution of male lifetable-deaths (dx)",...){   
  xyplot(dx.p~Age|Country,data=Data1,groups=Year,type='l',
         main=main,lwd=1.8,
         xlab=list("Age",cex=1.2),
         layout=c(4,3),
         par.strip.text=list(cex=1.2),
         xlim=c(0,110),ylab=list("Proportion of life table deaths",cex=1.2),ylim=c(0,.043),
         par.settings=my.settings,col.line=lcolor,
         scales=list(x=list(cex=1.2,at=c(seq(0,110,20))),
                     y=list(cex=1.2,at=c(seq(0,.04,.01)))), 
         fill = fillcol,
         upper = upci,
         lower = lowci,
         age = a,
         key = list(x=.01,y=.94, background="white", text=list(c("1970","1994","2009"))
                    ,cex=1.2,
                    lines=list(col=my.fill2[])),
         panel = function(x, y, ...){   
           panel.grid(h=-1,v=0,col='dark grey',lty=3)
           panel.abline(v=c(seq(0,110,15)),col='dark grey',lty=3)
           panel.superpose(x, y, panel.groups = 'my.panel.bands', ...)
           panel.xyplot(x, y, ...)           
         }           
  )  
}

##########ggplots edagger vs life expectancy

#F3 <- ggplot(data = edagger, aes(x = e0, y = edagger,color=Period)) +  
#  geom_point()+
#  scale_x_continuous("Life expectancy at birth", limits=c(55,84))+
#  scale_y_continuous("Expected years lost due to death", limits=c(8,24))+
#  theme(legend.key.height=unit(3,"line"))+
#  theme(legend.position = c(0.8, 0.7))+
#  ggtitle("a) Expected years lost due to death (e dagger) ")+
#  theme(text = element_text(size = 15))+
#  geom_smooth(data=edagger,aes(x = e0, y = edagger,color=D), method = "lm", se=FALSE,col="black") +
#  annotate("text", label = paste("R-squared=", as.character( round(summary(f)$r.squared,digits=2)),sep = " "), x = 55, y = 24, size = 5, colour = "black")+
#  theme(axis.line = element_line(colour = "black"),
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        panel.border = element_blank(),
#        panel.background = element_blank()) 
#  
#previous_theme <- theme_set(theme_bw())
#
#F3

#require(gridExtra)
#pdf(file="Latex/F3.pdf",width=9,height=8,pointsize=6)
#grid.arrange(F3, ncol=1)
#dev.off()


#F4 <- ggplot(data = K.entropy, aes(x = e0, y = K.entropy,color=Period)) +  
# geom_point()+
#  scale_x_continuous("Life expectancy at birth", limits=c(52,85))+
#  scale_y_continuous("Keyfitz Entropy", limits=c(0.08,.45))+
#  theme(legend.key.height=unit(3,"line"))+
#  ggtitle("b) Keyfitz entropy")+
#  theme(text = element_text(size = 15))+
#  theme(legend.position="none")+
#  geom_smooth(data=K.entropy,aes(x = e0, y = K.entropy,color=D), method = "lm", se=FALSE,col="black") +
#  annotate("text", label = paste("R-squared=", as.character( round(summary(f)$r.squared,digits=1)),sep = " "), x = 55, y = .45, size = 5, colour = "black")+
#  theme(axis.line = element_line(colour = "black"),
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        panel.border = element_blank(),
#        panel.background = element_blank()) #
#F4




#XYZ <- getHMDcountries()
#us <- "jmaburto@colmex.mx"
#pw <- "kolmogorov"
#Deaths_HMDL <- do.call(rbind,lapply(XYZ, function(x, us, pw){
#  cat(x,"\n")
#  Deaths        <- readHMDweb(x,"Deaths_1x1",username=us,password=pw)
#  #Females      <- readHMDweb(x,"fltper_1x1",username=us,password=pw)
#  #Males$Sex    <- "m"
#  #Females$Sex  <- "f"  
#  Deaths$PopName <- x
#  Deaths    
#}, us = us, pw = pw))

#Deaths_HMDL <- data.table(HMDL)
#save(Deaths_HMDL,file="Data/HMD_Deaths.RData")

#Exp_HMDL <- do.call(rbind,lapply(XYZ, function(x, us, pw){
#  cat(x,"\n")
#  Deaths        <- readHMDweb(x,"Exposures_1x1",username=us,password=pw)
#  #Females      <- readHMDweb(x,"fltper_1x1",username=us,password=pw)
#  #Males$Sex    <- "m"
#  #Females$Sex  <- "f"  
#  Deaths$PopName <- x
#  Deaths    
#}, us = us, pw = pw))

#Exp_HMDL <- data.table(Exp_HMDL)
#save(Exp_HMDL,file="Data/HMD_Exp.RData")


#levs <- as.vector(quantile(Eastern_Data$m.mx,c(0,0.005,0.01,0.02,0.03,.05,.1,.5,1),na.rm = T))
#levq <- seq(min(levs), max(levs), length = 8) 

#brewer.div <- colorRampPalette(brewer.pal(11,"BrBG"),interpolate="spline")
#f.level.m <- levelplot(m.mx ~ Year*Age|Country,data=subset(Eastern_Data,Year>=1960),
 #                      ylab.right = list("Life table death rate per 1,000 people",cex=1.2),
#                       par.settings=my.settings,
#                       xlab=list("Year",cex=1.2),
#                       ylab=list("Age",cex=1.2),
#                       scales=list(x=list(cex=1.2),
#                                   y=list(alternating=1,cex=1.2)),
#                       par.strip.text=list(cex=1.2),
#                       at=c(do.breaks(c(0,0.005),20), 
#                            do.breaks(c(0.0051,0.01),20),
#                            do.breaks(c(0.01001,0.02),20),
#                            do.breaks(c(0.0201,0.03),20),
#                            do.breaks(c(0.03001,0.05),20),
#                            do.breaks(c(0.0501,0.1),20),
#                            do.breaks(c(0.1001,.5),20),
#                            do.breaks(c(0.51,1),20)),
#                       col.regions=colorRampPalette(c("slategray2","royalblue1","royalblue3","yellow",
#                                                      "orange","orangered","red","red4")),          
#                       layout=c(4,3),
#                       colorkey=list(at=levq, 
#                                     labels=list(at=levq, 
#                                                 labels=c(0,5,10,20,30,50,100,500,1000))
#                       ))
#f.level.m

#pdf(file="Latex/leveplot_males.fig.pdf",width=16,height=10,pointsize=4)
#print(f.level.m)
#dev.off()

#brewer.div <- colorRampPalette(brewer.pal(11,"Spectral"),interpolate="spline")
#levelplot(ro.s ~ Year*Age|Country, data=ro.est,
#          cuts=199,col.regions=brewer.div(200))

mycol.1 <- c(makeTransparent("red", alpha=90),makeTransparent("lightgreen", alpha=90),makeTransparent("blue", alpha=90),
             makeTransparent("magenta", alpha=50))

