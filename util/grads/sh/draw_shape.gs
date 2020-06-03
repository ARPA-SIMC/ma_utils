function drwshp(args)
****************************************************************************
* Script per disegnare una poligonale in una mappa GRADS.
* Uso: draw_shape filein
*
* gestisce anche file generati con lo script Arcview shp2gen.ave
*
*                                 Versione 3.0.0, Enrico & Johnny 29/01/2020
****************************************************************************

*-------------------
* Gestione parametri 
filein=subwrd(args,1)

if(filein="-h" | filein="")
 say ""
 say "usage: draw_shape filein [-k] [-l] [-d]"
 say ""
 say "options: -k divides per 1000 lat and lon"
 say "         -l reads x first, then y"
 say "         -d manages DOS generated files with commas"
 say ""
 say "with option -f filein is the complete path, otherwise use:"
 say "/autofs/nfshomes/eminguzzi/server/util/grads/dat/shape_{filein}.dat"
 return
endif

optlon=0
optkm=0
optdos=0
io=2
dum=subwrd(args,io)
while(dum!="")
 if(dum="-l")
  optlon=1
 endif
 if(dum="-k")
  optkm=1
 endif
 if(dum="-d")
  optdos=1
 endif
 io=io+1
 dum=subwrd(args,io)
endwhile

* Apro il file: se esiste interpeto filein come path completo, altrimenti lo
* cerco nella dir di default
idum1=read(filein)
if(sublin(idum1,1)!=0)
 filein2='/autofs/nfshomes/eminguzzi/server/util/grads/dat/shape_'filein'.dat'
 idum2=read(filein2)
 if(sublin(idum2,1)!=0)
  say "Shape file non trovato:"
  say filein
  say filein2
  return
 endif
 filein=filein2
endif
say "Leggo dal file "filein
idum=close(filein)

if(optlon & optkm & !optdos)
 say ""
 say "se stai usando un file generato con ArcView"
 say "probabilmente devi usare l'opzione -d"
 say ""
endif

*---------------------------------
* Lettura coordinate vertici
* e disegno della figura

cnt=0
kret=0
maxlat=-999999999
minlat=-maxlat
maxlon=maxlat
minlon=minlat

'q gxinfo'
xlin=sublin(result,3)
ylin=sublin(result,4)
plin=sublin(result,6)
xmin=subwrd(xlin,4)
xmax=subwrd(xlin,6)
ymin=subwrd(ylin,4)
ymax=subwrd(ylin,6)
proj=subwrd(plin,3)

* se proiezione lat/lon (anche UTM),
* imposta per accelerare i conti
if(proj=2)
 'q dim'
 lonlin=sublin(result,2)
 latlin=sublin(result,3)
 lonwest=subwrd(lonlin,6)
 loneast=subwrd(lonlin,8)
 latsouth=subwrd(latlin,6)
 latnorth=subwrd(latlin,8)
endif

*toglie eventuali virgole e caratteri dos dal file
if(optdos)
 "! /autofs/nfshomes/eminguzzi/bin/read_write.exe < "filein" > coord.tmp1"
 fileintmp='coord.tmp2'
 "! cat coord.tmp1 | sed s/\,//g | sed 's/$/\ /g' > "fileintmp
 "! rm coord.tmp1"
else
 fileintmp=filein
endif

while(kret=0)

  dum=read(fileintmp)
  kret=sublin(dum,1)
  line=sublin(dum,2)
  type="undefined"
  latb=maxlat
  lonb=maxlon

  if (kret=0)
* cerca l'ultima parola della linea
    iw=0
    dum="_"
    while(iw<99 & dum!="")
     iw=iw+1
     dum=subwrd(line,iw)
     if(dum!="");last=dum;endif
    endwhile
* distingue tra linee e poligoni
    if(last="AUTO");type="polygon";endif
    if(last="POLYGON");type="polygon";endif
    if(last="LINE");type="line";endif
* cerca le interruzioni di poligoni/linee
    if (last != "" & last != "END" & last != "AUTO" & last != "POLYGON" & last != "LINE")
        latb=subwrd(line,1)
        lonb=subwrd(line,2)
        if(optlon)
          tmp=lonb
          lonb=latb
          latb=tmp
        endif
        if(optkm)
          lonb=lonb*0.001
          latb=latb*0.001
        endif
* cerca coordinate estreme
        if(latb>maxlat)
          maxlat=latb
        endif
        if(lonb>maxlon)
          maxlon=lonb
        endif
        if(latb<minlat)
          minlat=latb
        endif
        if(lonb<minlon)
          minlon=lonb
        endif
* chiude i poligoni
    else
        if(cnt>1 & type="polygon" & xin>xmin & xin<xmax & x2>xmin & x2<xmax & yin>ymin & yin<ymax & y2>ymin & y2<ymax)
         'draw line 'xin' 'yin' 'x2' 'y2
     	endif
        cnt=-1
    endif 

    if(cnt>0)
      ok1=0
      ok2=0
*lat/lon e UTM
      if(proj=2)
        if(lona>lonwest & lona<loneast & lata<latnorth & lata>latsouth);ok1=1;endif
        if(lonb>lonwest & lonb<loneast & latb<latnorth & latb>latsouth);ok2=1;endif
        if(ok2=1)
          'q w2xy 'lonb' 'latb
          x2=subwrd(result,3)
          y2=subwrd(result,6)
        else
          x2=xmax+1
          y2=ymax+1
        endif
*altre proiezioni
      else
        'q w2xy 'lonb' 'latb
        x2=subwrd(result,3)
        y2=subwrd(result,6)
        if(x1>xmin & x1<xmax & y1>ymin & y1<ymax);ok1=1;endif
        if(x2>xmin & x2<xmax & y2>ymin & y2<ymax);ok2=1;endif
      endif
      if(ok1=1 & ok2=1)
        'draw line 'x1' 'y1' 'x2' 'y2
      endif
      lata=latb
      lona=lonb
      x1=x2
      y1=y2
    endif

    if(cnt=0)
      lata=latb
      lona=lonb 
      'q w2xy 'lonb' 'latb
      x2=subwrd(result,3)
      y2=subwrd(result,6)
      xin=x2
      yin=y2
      x1=x2
      y1=y2  
    endif
    cnt=cnt+1
  endif
endwhile

if(type="polygon" & xin>xmin & xin<xmax & x2>xmin & x2<xmax & yin>ymin & yin<ymax & y2>ymin & y2<ymax)
  'draw line 'xin' 'yin' 'x2' 'y2
else
*  say 'warning! xin:'xin' yin:'yin' x2:'x2' y2:'y2
endif

*'! rm 'fileintmp

output='minlat = 'minlat' maxlat = 'maxlat' minlon = 'minlon' maxlon = 'maxlon
say output
return output


**********************************************
*  function dist(lona,lata,lonb,latb)
** Distance between two points on the Earth surface
*
*    _At=6371229
*    _PI=3.141592654
*    _D2R=_PI/180
*    _R2D=180/_PI
*
*    phi=lona*_D2R;theta=(90-lata)*_D2R
*    'd sin('theta')*cos('phi')'
*    x1=subwrd(result,4)
*    'd sin('theta')*sin('phi')'
*    y1=subwrd(result,4)
*    'd cos('theta')'
*    z1=subwrd(result,4)
*    phi=lonb*_D2R;theta=(90-latb)*_D2R
*    'd sin('theta')*cos('phi')'
*    x2=subwrd(result,4)
*    'd sin('theta')*sin('phi')'
*    y2=subwrd(result,4)
*    'd cos('theta')'
*    z2=subwrd(result,4)
*    x=y1*z2-y2*z1
*    y=x2*z1-x1*z2
*    z=x1*y2-x2*y1
*    d2=x*x+y*y+z*z
*    'd asin(sqrt('d2'))'
*  return (subwrd(result,4)*_At)

