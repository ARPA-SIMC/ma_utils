# Script per fare le rose dei venti
# dai dati estratti da estra_orari

# preliminari
options(warn=-1)                           # suppress warning messages

#legge opzioni
options  <-  scan(file = "windrose.inp", what = "", flush = TRUE)
filein   <-  options[1]
cod      <-  options[2]
title    <-  options[3]
type     <-  options[4]
nsett    <-  options[5]
rlimit   <-  as.integer(options[6])
color    <-  options[7]
layout   <-  options[8]
wrtype   <-  options[9]
palette  <-  options[10]
mmstr    <-  options[11]
mms      <-  as.list(as.numeric(strsplit(mmstr,",")[[1]]))
hhstr    <-  options[12]
hhs      <-  as.list(as.numeric(strsplit(hhstr,",")[[1]]))
level    <-  options[13]
fileout  <-  options[14]

# gestisce sottotitolo
if (mmstr=="1,2,3,4,5,6,7,8,9,10,11,12") 
   {subtitle=""} else {subtitle=paste("months:",mmstr)}
if (hhstr=="0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23") 
   {subtitle=subtitle} else {subtitle=paste(subtitle,"hours:",hhstr)}
subtitle=paste(subtitle,"level:",level)

#colori ecc.
if (palette == "classic") {
  palcols <- c("purple","red","orange","darkgreen","blue","black")}
if (palette == "light") {
  palcols <- c("purple","red","yellow","green","skyblue","black")}
if (palette == "gray") {
  palcols <- gray(c(1:6)/6)}

if (wrtype == "bars") {
  method      <- 4
  lines.lwd   <- 1
  lp.col      <- rep("black",6)
  polygon.col <- palcols
  legend.lwd  <- 20
}
if (wrtype == "lines") {
  method      <- 2
  lines.lwd   <- 5
  lp.col      <- palcols
  polygon.col <- rep(NA,6)
  legend.lwd  <- lines.lwd
}
if (layout == "minimal") {grid.col=color} else {grid.col="darkgray"}

#importa i dati
if (type == "ossmeteo") {
 line   <- scan(file = filein,skip=2,what="",nlines=1,sep="#")
 nvar   <- (nchar(line)-13)/11
 header <- read.fwf(file = filein,skip=2,widths=c(4,3,3,3,rep(11,nvar)),n=1,
           colClasses = "character",strip.white=TRUE)
 dum    <- read.fwf(file = filein,na.strings="    -9999.0",
                  widths=c(4,3,3,3,rep(11,nvar)),skip=3,col.names=t(header))
 nrows  <- dim(dum)[1]
# dum2   <- as.vector(dum)
# dum2   <- subset(dum2,select=c(aaaa,mm,gg,hh,FF.ist,DD.ist))
 dum2   <- subset(dum,select=c(aaaa,mm,gg,hh,FF.ist,DD.ist))
 dum2   <- subset(dum2,!is.na(match(mm,mms)))
 dum2   <- subset(dum2,!is.na(match(hh,hhs)))
 dati   <- dum2
 fn     <- "FF.ist"
 dn     <- "DD.ist"
}
if (type == "seriet") {
 line   <- scan(file = filein,skip=5,what="",nlines=1,sep="#")
 nvar   <- (nchar(line)-17)/11
 header <- read.fwf(file = filein,skip=5,widths=c(2,1,2,1,4,3,4,rep(11,nvar)),n=1,
           colClasses = "character",strip.white=TRUE)
 levs   <- read.fwf(file = filein,skip=3,widths=c(2,1,2,1,4,3,4,rep(11,nvar)),n=1,
           colClasses = "character",strip.white=TRUE)[-1:-7]
 header <- c(header[1:7],paste(header[-1:-7],levs,sep="."))
 dum    <- read.fwf(file = filein,na.strings=list("    -9999.0","     -9999."),
                   widths=c(2,1,2,1,4,3,4,rep(11,nvar)),skip=6,col.names=header)
 fn     <- paste("Mod.wind",level,sep=".")
 dn     <- paste("Dir.wind",level,sep=".")
 nrows  <- dim(dum)[1]
# dum2   <- as.vector(dum)
# dum2   <- subset(dum2,select=c(aaaa,mm,gg,hh,get(fn),get(dn)))
 dum2   <- subset(dum,select=c(aaaa,mm,gg,hh,get(fn),get(dn)))
 dum2   <- subset(dum2,!is.na(match(mm,mms)))
 dum2   <- subset(dum2,!is.na(match(hh,hhs)))
 dati   <- dum2
}
paste("uso variabili ",fn," e ",dn,sep="")

#funzione polar plot
polarplotR <- Sys.getenv("polarplotR")
source(polarplotR)

#-------------------------
# Inizio funzione windrose

wr<-function(dati,cod,title,subtitle,fn,dn,nsett,rlimit,color,layout,fileout){
  width <- 1200
  if(fileout == "nil") {
     fileout=paste("wrose_",cod,".png",sep="")
     }
  png(filename=fileout,
       width = width, height = width/5*4,
      pointsize = width/50, bg = color)

  ff<-subset(dati[,fn],(!is.na(dati[,fn]) & !is.na(dati[,dn])))
  dd<-subset(dati[,dn],(!is.na(dati[,fn]) & !is.na(dati[,dn])))

  #parametri per wind rose
  if (nsett == "s8") {
      nd=8
      incrd=45
      }
  if (nsett == "s16") {
      nd=16
      incrd=22.5
      }
  if (nsett == "s8") {cend<-c(0,incrd*1,incrd*2,incrd*3,incrd*4,incrd*5,incrd*6,incrd*7)
      }
  if (nsett == "s16") {cend<-c(0,incrd*1,incrd*2,incrd*3,incrd*4,incrd*5,incrd*6,incrd*7,
               incrd*8,incrd*9,incrd*10,incrd*11,incrd*12,incrd*13,incrd*14,incrd*15)
      }
  limd<-cend+incrd/2
  limf<-c(-1,1,2,4,7,10,20)
  nf=7
  if(rlimit<=0) {rlimits<-as.null(rlimit)} else {rlimits<-c(0,rlimit)}

  #calcola frequenze percentuali di intensita'
  tot<-table(cut(ff,c(-1,999)))
  freqf<-table(cut(ff,limf))/tot[1]*100

  #prepara il testo della legenda
  legwr<-c(paste("<",limf[2],sep=""),
    	   paste("<",limf[3],sep=""),
    	   paste("<",limf[4],sep=""),
    	   paste("<",limf[5],sep=""),
    	   paste("<",limf[6],sep=""),
    	   paste("<",limf[7],sep=""))
  legfr<-c(paste("<",limf[2],": ",signif(freqf[1],2),"%",sep=""),
    	   paste(limf[2],"-",limf[3],": ",signif(freqf[2],2),"%",sep=""),
    	   paste(limf[3],"-",limf[4],": ",signif(freqf[3],2),"%",sep=""),
    	   paste(limf[4],"-",limf[5],": ",signif(freqf[4],2),"%",sep=""),
    	   paste(limf[5],"-",limf[6],": ",signif(freqf[5],2),"%",sep=""),
    	   paste(limf[6],"-",limf[7],": ",signif(freqf[6],2),"%",sep=""))

  #costruisce input per wr
  dum<-table(cut(ff,limf),cut(dd,c(0,limd,360)))
  if (nsett == "s8") {
    r<-matrix(c((dum[,1]+dum[,9]),dum[,2:8],(dum[,1]+dum[,9])),ncol=nd+1,nrow=nf-1)
    settori<-c("N","NE","E","SE","S","SW","W","NW")
    ndir=8
    }
  if (nsett == "s16") {
    r<-matrix(c((dum[,1]+dum[,17]),dum[,2:16],(dum[,1]+dum[,17])),ncol=nd+1,nrow=nf-1)
    settori<-c("N","NNE","NE","ENE","E","ESE","SE","SSE",
               "S","SSW","SW","WSW","W","WNW","NW","NNW")
    ndir=16
    }
  r[1,] <- sum(r[1,])/dim(r)[2]     # spalma le calme fra tutti i settori
  dum   <- cend/180*pi
  theta <- c(dum,2*pi)

 #opzioni layout plot polari
  if(layout=="full" | layout=="light") {
    title<-paste(title,", wind rose",sep="")
  } else {
    title<-""
    subtitle<-""
  }
  if(layout=="minimal") {
    rlab=0
    tlab=color
  } else {
    rlab=2
    tlab="black"
  }

 #plot polari
  sectmin <- which.min(r[1,]+r[2,]+r[3,]+r[4,]+r[5,]+r[6,])
  dirmin  <- pi/2 - theta[sectmin]
  if(! layout=="onlylegend") {
    polar.plot(r[1,]+r[2,]+r[3,]+r[4,]+r[5,]+r[6,],
      theta, theta.clw = TRUE, 
      theta.zero = pi/2, 
#      text.lab = settori,
      text.lab = rep("",ndir),
      dir=ndir,
      pi2.lab = FALSE, lines.lwd = lines.lwd, grid.lwd = 1, 
      tlabel.col = tlab, points.cex = 0.8, 
      points.pch = 21, tlabel.cex = 1.4, 
      tlabel.offset = 0.3, lp.col = lp.col[1],
      grid.col = grid.col, rlabel.method = rlab,
      rlabel.axis = dirmin, rlabel.cex = .8,
      rlabel.pos = NULL, rlabel.col = "red",
      polygon.col = polygon.col[1],
      polygon.bottom=TRUE,
      method= method,
      rlimits=rlimits,
      main=title,
      sub=subtitle)
    polar.plot(r[1,]+r[2,]+r[3,]+r[4,]+r[5,],
      theta, theta.clw = TRUE, 
      overlay=1,
      theta.zero = pi/2,  
      lines.lwd = lines.lwd, grid.lwd = 1, 
      lp.col = lp.col[2],
      polygon.col = polygon.col[2],
      polygon.bottom=TRUE,
      rlimits=rlimits,
      method= method)
    polar.plot(r[1,]+r[2,]+r[3,]+r[4,],
      theta, theta.clw = TRUE, 
      overlay=1,
      theta.zero = pi/2,  
      lines.lwd = lines.lwd, grid.lwd = 1, 
      lp.col = lp.col[3],
      polygon.col = polygon.col[3],
      polygon.bottom=TRUE,
      rlimits=rlimits,
      method= method)
    polar.plot(r[1,]+r[2,]+r[3,],
      theta, theta.clw = TRUE, 
      overlay=1,
      theta.zero = pi/2,  
      lines.lwd = lines.lwd, grid.lwd = 1, 
      lp.col = lp.col[4],
      polygon.col = polygon.col[4],
      polygon.bottom=TRUE,
      rlimits=rlimits,
      method= method)
    polar.plot(r[1,]+r[2,],
      theta, theta.clw = TRUE, 
      overlay=1,
      theta.zero = pi/2,  
      lines.lwd = lines.lwd, grid.lwd = 1, 
      lp.col = lp.col[5],
      polygon.col = polygon.col[5],
      polygon.bottom=TRUE,
      rlimits=rlimits,
      method= method)    	   
    polar.plot(r[1,],
      theta, theta.clw = TRUE, 
      overlay=1,
      theta.zero = pi/2,  
      lines.lwd = lines.lwd, grid.lwd = 1, 
      lp.col = lp.col[6],
      polygon.col = polygon.col[6],
      polygon.bottom=TRUE,
      rlimits=rlimits,
      method= method)

# mette le label
    polar.plot(r[1,]+r[2,]+r[3,]+r[4,]+r[5,]+r[6,],
      theta, theta.clw = TRUE, 
      theta.zero = pi/2, 
      text.lab = settori,
      dir=ndir,
      overlay=2,
      pi2.lab = FALSE, lines.lwd = lines.lwd, grid.lwd = 1, 
      tlabel.col = tlab, points.cex = 0.8, 
      points.pch = 21, tlabel.cex = 1.4, 
      tlabel.offset = 0.3,
      grid.col = grid.col, rlabel.method = rlab,
      rlabel.axis = dirmin, rlabel.cex = .8,
      rlabel.pos = NULL, rlabel.col = "red",
      method= 0,
      rlimits=rlimits,
      main=title,
      sub=subtitle)
  }

  #legende
 if (layout=="onlylegend") {plot.new()}
 if (layout=="full" | layout=="light" | layout=="onlylegend") {
  plot.window(c(-400,400),c(-400,400))
  text(-445,410,"wind speed [m/s]",pos=4)
  par(lend="square")
  legend(-450,400, 
    legend=legwr, 
    col = rev(palcols),
    text.col= "black",merge=FALSE,
    lty = 1, lwd=legend.lwd, pch = -1, bty="n")
 }
 if (layout=="full") {
  text(-445,-100,"frequencies",pos=4) 
  legend(-485,-110, 
    legend=legfr, 
    col = rep(color,6), 
    text.col= "black",
    lty = 1, lwd=3, pch = -1, bg='gray90', bty="n")
  text(0,-350,"occurencies",adj=0.5,col="red") 
  text(445,410,paste("total no. of data:",tot),pos=2) 
  dev.off()
 }
}

# Fine funzione windrose
#-------------------------

wr(dati,cod,title,subtitle,fn,dn,nsett,rlimit,color,layout,fileout)

