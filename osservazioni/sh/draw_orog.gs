****************************************************************************
* Script per disegnare un'orografia di background per una mappa GRADS
*
*                                 Versione 1.0.1, Johnny & Enrico 03/09/2019
****************************************************************************
function drworog(args)

file_5km='orog_calmet_5km'
file_20km='orog_eur_20km'
file_7km='orog_lmbpa_7km'
file_2p8km='orog_lmbpa_2p8km'
file_1p1km='orog_lmbpa_1p1km'
file_5kmutm='orog_bpa_5km_utm'
file_lmsmr4='orog_lmsmr4_7km'
file_NIta2Km='orog_NIta2Km'

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
  say ' 1 : bacino padano adriatico 5km (default)'
  say ' 2 : Europa 20 km (area ECMWF)'
  say ' 3 : bacino padano adriatico 7km'
  say ' 4 : bacino padano adriatico 2.8km'
  say ' 5 : bacino padano adriatico 1.1km'
  say ' 6 : bacino padano adriatico 5km UTM'
  say ' 7 : dominio COSMO lmsmr4 7km'
  say ' 8 : Nord Italia, lat-lon, 2km'
  say ''
  say ' geo_dir: directory con i grib orografia '
  say '   (default: /autofs/nfshomes/eminguzzi/server/util/grads/dat)'

  return
endif

area=subwrd(args,2)
file_name=file_5km
if (area='2')
  file_name=file_20km
endif
if (area='3')
  file_name=file_7km
endif
if (area='4')
  file_name=file_2p8km
endif
if (area='5')
  file_name=file_1p1km
endif
if (area='6')
  file_name=file_5kmutm
endif
if (area='7')
  file_name=file_lmsmr4
endif
if (area='8')
  file_name=file_NIta2Km
endif

geo_dir=subwrd(args,3)
if (geo_dir='')
  geo_dir='/autofs/nfshomes/eminguzzi/server/util/grads/dat'
endif

file_orog=geo_dir'/'file_name

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
  'd 'distsfc
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
  'd 'distsfc
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
  'd 'distsfc
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
  'd 'distsfc
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
  'd 'distsfc
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
  'd 'distsfc
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
  'd 'distsfc
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
  'd 'distsfc
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




