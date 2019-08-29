function main(args)
* Disegna una mappa dell'orografia circostante e un punto
* Uso: plot_local_orog.gs file proj np

*** Parametri
file=subwrd(args,1)
proj=subwrd(args,2)
np=subwrd(args,3)

opt_out="c"
*opt_out="n"

if (opt_out="c")
  delta=7
endif
if (opt_out="n")
  delta=3
endif

if(file='' | proj='' | np='' | file='-h')
  say "Uso: plot_local_orog.gs file proj np"
  quit
endif
if(proj!='UTM' & proj!='GEO' & proj!='ROT')
  say "Proiezione "proj" non gestita"
  quit
endif

*** Apro il file, trovo nx,ny
'open 'file
'q file'
line=sublin(result,5)
nx=subwrd(line,3)
ny=subwrd(line,6)

* Calcolo indici e coordinate del punto richiesto
jj=math_int(np/nx)+1
ii=np-(jj-1)*nx
'set x 'ii
'set y 'jj
'q dims'
line=sublin(result,2)
xxworld=subwrd(line,6)
'q dims'
line=sublin(result,3)
yyworld=subwrd(line,6)
say "Indici punto "ii" "jj" ("xxworld" "yyworld")"

* Plot orografia
'define_colors'
'white'
'c'
'set x 'ii-delta' 'ii+delta
'set y 'jj-delta' 'jj+delta
'set gxout grfill'
if(proj="UTM")
  'set mproj off'
endif

if (opt_out="c")
  'set clevs 0 100 200 500 750 1000 2000 3000'
  'set ccols 45 37 35 32 72 74 76 84 87'
  'd z'
* 'run 'colbar_white.gs
  'run 'cbarn_white.gs
  'set clevs -0.02 0.02 100 200 500 750 1000 2000 3000'
  'set ccols 37 45 37 35 32 72 74 76 84 87'
  'd z'
endif

if (opt_out="n")
  'set gxout grid'
  'set ccolor 2'
  'set digsiz 0.15'
  'd z'
endif

if(proj="GEO")
  'draw_shape regit'
endif
if(proj="ROT")
  'draw_shape regitrot'
endif
if(proj="UTM")
  'draw_shape regitutm -k -l'
endif

* Plot punto richiesto
'q w2xy 'xxworld' 'yyworld
line=result
xx=subwrd(line,3)
yy=subwrd(line,6)
'draw mark 1 'xx' 'yy' 0.8'
'draw mark 2 'xx' 'yy' 0.2'

* Salvo mappa
'save_png orog med'

quit
