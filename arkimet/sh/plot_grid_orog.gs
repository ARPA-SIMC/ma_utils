function main(args)
* Disegna una mappa dell'orografia sull'intero dominio
* Uso: plot_grid_orog.gs file proj

*** Parametri
file=subwrd(args,1)
proj=subwrd(args,2)

if(file='' | proj='' | file='-h')
  say "Uso: plot_grid_orog.gs file proj"
  quit
endif
if(proj='UTM' & proj!='GEO' & proj!='ROT1' & proj!='ROT2' & proj!='ROT3')
  say "plot_grid_orog.gs: proiezione "proj" non gestita"
  quit
endif

if(proj='GEO' | proj='ROT1' | proj='ROT2' | proj='ROT3')
  geoshape=regit
else
  geoshape=nil
endif

if(proj='GEO' | proj='UTM')
  gxfmt=grfill
else
  gxfmt=shaded
endif

*** Plot orografia
'open 'file
'white'
'c'
'set mpdset hires'
'set gxout 'gxfmt
'set clevs 1 10 100 200 500 1000 2000'

* NON funziona:
* 'q vars'
* pvar=sublin(result,1)

'q file'
line=sublin(result,7)
pvar=subwrd(line,1)
say "var: "pvar
'd 'pvar

'draw_shape 'geoshape

'cbarn_white'
'save_png orog med'

quit
