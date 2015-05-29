function wrom(args)

* impostazioni
'reinit'
'set mpdset hires'
'white'
'set parea 0.5 10.5 0.5 7'
'c'

* argomenti
typ=subwrd(args,1)
area=subwrd(args,2)
if(typ="") ;typ=4 ;endif
if(area="");area=5;endif

* ritaglia area, disegna orografia
'draw_orog 'typ' 'area
'c'
ok=0
j=0
minlon=99999
maxlon=-99999
minlat=99999
maxlat=-99999
while(ok=0)
  j=j+1
  res=read("wrom.lst")
  ok=sublin(res,1)
  if(ok=0)
    line=sublin(res,2)
    lon=subwrd(line,2)
    lat=subwrd(line,3)
    if(lon<minlon);minlon=lon;endif
    if(lon>maxlon);maxlon=lon;endif
    if(lat<minlat);minlat=lat;endif
    if(lat>maxlat);maxlat=lat;endif
  endif
endwhile
minx=minlon-0.1*(maxlon-minlon)
maxx=maxlon+0.1*(maxlon-minlon)
miny=minlat-0.1*(maxlat-minlat)
maxy=maxlat+0.1*(maxlat-minlat)
latox=(maxx-minx)
latoy=(maxy-miny)
prop=latox/latoy
ideal=10/4.8
if(prop>ideal)
  maxy=maxy+latoy*((prop-ideal)/ideal)/2
  miny=miny-latoy*((prop-ideal)/ideal)/2
else
  maxx=maxx+latox*((ideal-prop)/ideal)/2
  minx=minx-latox*((ideal-prop)/ideal)/2
endif
'set lon 'minx' 'maxx
'set lat 'miny' 'maxy
'set grads off'
'draw_orog 'typ' 'area
res=close("wrom.lst")

* individua posizione legenda
'q gxinfo'
line=sublin(result,2)
xpag=subwrd(line,4)
ypag=subwrd(line,6)
'q xy2w 'xpag' 'ypag
xtr=subwrd(result,3)
ytr=subwrd(result,6)
'q xy2w 0 0'
xbl=subwrd(result,3)
ybl=subwrd(result,6)
legx=xbl+0.35*(xtr-xbl)
legy=ybl+0.7*(ytr-ybl)

* aggiunge eventuali dettagli
* eseguendo i comandi di tmp.gs
res=read(wrtmp.gs)
ok=sublin(res,1)
if(ok=0);'wrtmp';endif

* sovrappone rose dei venti e legenda
ok=0
j=0
flag=0
while(ok=0)
  j=j+1
  res=read("wrom.lst")
  ok=sublin(res,1)
  if(ok=0)
    line=sublin(res,2)
    lon=subwrd(line,2)
    lat=subwrd(line,3)
    file=j'.png'
    respng=read(file)
    okpng=sublin(respng,1)
    if(flag=0)
     'overlay -i legend -o wrom -D 1000 -d 700 -s -7 -4 -p 'legx' 'legy
     flag=1
    endif
    if(okpng=0)  
     'overlay -i 'j' -o wrom -r wrom -D 1000 -d 300 -s -7 -4 -p 'lon' 'lat
    endif
  endif
endwhile
'quit'

return
