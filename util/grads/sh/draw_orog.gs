****************************************************************************
* Script per disegnare un'orografia di background per una mappa GRADS
*
*                                 Versione 2.0.0, Johnny & Enrico 29/01/2020
****************************************************************************
function drworog(args)

filegeo.1=eur20k_orog
filegeo.2=nita1k_orog
filegeo.3=COS2I_orog
filegeo.4=COS5MITR_orog
filegeo.5=LAMAZ_orog
filegeo.6=nita5kutm_orog

*-------------------
* Gestione parametri

opt=subwrd(args,1)

if (opt='-h' | opt='h' | opt='')
  say 'uso: draw_orog opzione area [geo_dir]'
  say ''
  say 'opzioni implementate:'
  say ' 1 : solo contour, nero'
  say ' 2 : shaded, scala di grigi grossolana'
  say ' 3 : solo contour, bianco'
  say ' 4 : shaded, scala di grigi dettagliata'
  say ' 5 : solo contour, grigio, dettagliato'
  say ' 6 : shaded, colori, dettagliato'
  say ' 7 : solo contour, grigio chiaro, dettagliato'
  say ''
  say 'aree implementate:'
  say ' 1 : Europa, 20 km, lat-lon'
  say ' 2 : Nord Italia, 1 km, lat-lon'
  say ' 3 : COSMO2I, 2 km, ruotata new'
  say ' 4 : COSMO5M, 5 km, ruotata new'
  say ' 5 : LAMAZ, 7 km, ruotata old'
  say ' 6 : Nord Italia, 5km, UTM'
  say ''
  say ' geodir: directory con i grib orografia '
  say '   (default: /usr/share/ma_utils)'

  return
endif

area=subwrd(args,2)
file_name=filegeo.area

geodir=subwrd(args,3)
if (geodir='')
  geodir='/usr/share/ma_utils'
endif

file_orog=geodir'/'file_name
say 'uso orografia: 'file_orog

*---------------------
* Conto i files aperti 

cnt_file=0
kret=0
'q files'

while (kret=0)
  nl=(cnt_file+1)*3
  if(sublin(result,nl)='')
    kret=-1
  else
    cnt_file=cnt_file+1
  endif
endwhile

'q file'
act_file=subwrd(result,2)

if(cnt_file=0)
  say 'nessun file aperto, alla fine tengo aperto il file orografico'
else
  say 'ci sono 'cnt_file' files aperti, est attivo il 'act_file' , opt 'opt
endif
done=0

*---------------------------------
* memorizzo il tempo per reimpostarlo alla fine

'q time'
time=subwrd(result,3)

'set mpdset hires'

*---------------------------------
* orografia, contour, in nero
if (opt=1)
  'open 'file_orog
  'set dfile 'cnt_file+1
  'set t '1
  'set gxout 'contour
  'set clab 'off
  'set ccolor '0
  'set clevs '100' '500' '1000
  'd z'
  done=1
endif

*---------------------------------
* orografia, shaded, scala di grigi grossolana
if (opt=2)
  'open 'file_orog
  'set dfile 'cnt_file+1
  'set t '1
  'set gxout 'shaded
  'set clab 'off
  'set rgb '16' '230' '230' '230
  'set rgb '17' '200' '200' '200
  'set rgb '18' '150' '150' '150
  'set ccols '1' '16' '17' '18
  'set clevs '100' '500' '1000
  'd z'
  done=1
endif

*---------------------------------
* orografia, contour, in bianco
if (opt=3)
  'open 'file_orog
  'set dfile 'cnt_file+1
  'set t '1
  'set gxout 'contour
  'set clab 'off
  'set ccolor '1
  'set clevs '100' '500' '1000
  'd z'
  done=1
endif

*---------------------------------
* orografia, shaded, scala di grigi dettagliata
if (opt=4)
  'open 'file_orog
  'set dfile 'cnt_file+1
  'set t '1
  'set gxout 'shaded
  'set clab 'off
  'define_colors'
  'set ccols 1  82  83  84  85  86   87   88   0'
  'set clevs  50 100 200 400 800 1500 2000 3000'
  'd z'
  done=1
endif

*---------------------------------
* orografia, contour grigio dettagliato
if (opt=5)
  'open 'file_orog
  'set dfile 'cnt_file+1
  'set t '1
  'set gxout 'contour
  'set clab 'off
  'set ccolor '15
  'set clevs 50 100 200 400 800 1500 2000 3000'
  'd z'
  done=1
endif

*---------------------------------
* orografia, shaded, scala di colori dettagliata
if (opt=6)
  'open 'file_orog
  'set dfile 'cnt_file+1
  'set t '1
  'set gxout 'shaded
  'set clab 'off
  'define_colors'
  'set ccols 44 36 34  32  22  24  73   76   84  87'
  'set clevs   0 50 100 200 400 800 1500 2000 3000'
  'd z'
  done=1
endif

*---------------------------------
* orografia, shaded, scala di grigi chiari dettagliata
if (opt=7)
  'open 'file_orog
  'set dfile 'cnt_file+1
  'set t '1
  'set gxout 'shaded
  'set clab 'off
  'set rgb 82 234 234 234'
  'set rgb 83 212 212 212'
  'set rgb 84 190 190 190'
  'set rgb 85 168 168 168'
  'set rgb 86 146 146 146 '
  'set rgb 87 124 124 124 '
  'set rgb 88 102 102 102 '
  'set rgb 89  80  80  80 '
  'set ccols 1  82  83  84  85  86   87   88   89'
  'set clevs  50 100 200 400 800 1500 2000 3000'
  'd z'
  done=1
endif

*---------------------------------
* orografia, shaded, scala di grigi chiari dettagliata
if (opt=8)
  'open 'file_orog
  'set dfile 'cnt_file+1
  'set t '1
  'set gxout 'shaded
  'set clab 'off
  'define_colors'
  'set ccols 1  81  82  83  84  85   86   87   88'
  'set clevs  50 100 200 400 800 1500 2000 3000'
  'd z'
  done=1
endif

if(done & cnt_file>0)
  'close 'cnt_file+1
  'set dfile 'act_file
  'set time 'time
endif
'set gxout 'contour
'set ccolor 'rainbow
'set clab 'on

return 




