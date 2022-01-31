function wrom(args)
********************************************************************************
* Script per disegnare rose dei venti su mappa; chiamato da wrom.sh.
*
* Uso: wrom.gs [-t typ -a area -xo xout]
* I parametri typ e area vengono passati a draw_orog
* Il parametro xout determina la dimesione della mappa di output, che dipende
*   dal numero di wroses da inserire nella mappa: con una decina di wroses, va
*   bene il default (1000), con un centinaio (es. NIta) provare fra 3000 e 5000.
*
* Altri input:
* wrom.lst: nomi dei files con le windroses da sovrapporre (a sfondo trasparente)
*  e coordinate delle stazioni; obbligatorio.
* wrtmp.gs: istruzioni grafiche, eseguite prima di sovrapporre le windrose; opzionale
* wrom_marks.dat: coordinate e nomi delle stazioni; opzionale: se esiste, viene 
*  prodotta anche una mappa con i nomi delle stazioni
* 
* NOTE:
* Lo script "overlay.gs" (by Johnny) aggiunge le wroses e salva il png; si basa 
* sui programmi composite, pngtopnm, pnmscale e pnmtopng (pacchetti ImageMagick
* e netpbm, netpbm-progs.x86_64).
* Il programma pngtopnm e' obsoleto, e dovrebbe essere sostituito con pngtopam
* Links: http://netpbm.sourceforge.net; https://sourceforge.net/projects/netpbm/
*
*                                      Versione 2.1.1 Jhonny & Enrico 03/09/2019
********************************************************************************

********************************************************************************
* 1) Preliminari

* Abilito funzioni .gsf
rc=gsfallow('on')

* Argomenti da riga comando
ok_typ=parseopt(args,'-','t','ttt')
ok_area=parseopt(args,'-','a','aaa')
ok_xout=parseopt(args,'-','xo','xxx')

if(ok_typ=1);  typ=_.ttt.1;  else; typ=4;  endif
if(ok_area=1); area=_.aaa.1; else; area=7; endif
if(ok_xout=1); xout=_.xxx.1; else; xout=1000; endif

say 'typ='typ', area='area', xout='xout

* Leggo da wrom.lst le coordinate estreme delle stazioni  
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
res=close("wrom.lst")

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

* Inizializzo ambiente grafico
'reinit'
'set mpdset nil'
'set grads off'
'set parea 0.5 10.5 0.5 7'
'white'
'c'

********************************************************************************
* 2) Se esiste il file wrom_marks.dat, plot mappa con i nomi delle stazioni

res=read(wrom_marks.dat)
ok=sublin(res,1)
if(ok=0)
  say "plot mappa stazioni"
  'run ./draw_orog.gs 'typ' 'area' './
  'set lon 'minx' 'maxx
  'set lat 'miny' 'maxy
  'c'
  'run ./draw_orog.gs 'typ' 'area' './
  'set strsiz 0.03'
  'draw_marks ./wrom_marks.dat'

* Aggiungo eventuali dettagli (esegue i comandi di wrtmp.gs)
  res=read(wrtmp.gs)
  ok=sublin(res,1)
  if(ok=0); say 'plot wrtmp.gs'; 'wrtmp'; endif

  'save_png wrom_marks -x 3000 -y 2100'

else
  say "non faccio mappa stazioni"
endif

********************************************************************************
* 3) Plot mappa con le rose dei venti

'c'
'draw_orog 'typ' 'area
'set lon 'minx' 'maxx
'set lat 'miny' 'maxy
'c'
'draw_orog 'typ' 'area

* Individuo posizione legenda
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

* Aggiungo eventuali dettagli (esegue i comandi di wrtmp.gs)
res=read(wrtmp.gs)
ok=sublin(res,1)
if(ok=0); say 'plot wrtmp.gs'; 'wrtmp'; endif

* sovrappone rose dei venti e legenda
ok=0
j=0
flag=0
while(ok=0)
  j=j+1
  say 'wrom.gs: elaboro wrose 'j
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
      'run ./overlay.gs -i legend -o wrom -D 'xout' -d 700 -s -7 -4 -p 'legx' 'legy
      flag=1
    endif
    if(okpng=0)  
      'run ./overlay.gs -i 'j' -o wrom -r wrom -D 'xout' -d 300 -s -7 -4 -p 'lon' 'lat
    endif
  endif
endwhile
'quit'

return
