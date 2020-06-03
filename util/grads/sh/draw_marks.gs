****************************************************************************
* Script per disegnare un insieme di mark e stringhe in una mappa GRADS.
*
* NOTE:
* - Legge coordinate e labels dal file filein, che deve contenere un record 
*   per ogni punto, con 3(5) campi: lat, lon, label (opzionali: posizione, angolo)
* - Prima di lanciarlo, bisogna che sia stato disegnato un campo
*
*                                            V3.0.0, Johnny & Enr 29/01/2020
****************************************************************************
function drwmrk(args)

*-------------------
* Gestione parametro 

filein=subwrd(args,1)
marker=subwrd(args,2)
marksize=subwrd(args,3)
markcol=subwrd(args,4)
labelcol=subwrd(args,5)

if (filein='-h' | filein='h')
  say 'uso: draw_marks filein <marker <marksize> <markcolor> <labelcolor>>'
  say 'filein contiene un record per punto, con i campi:'
  say 'lat, lon, label[, posiz.label[, angolo.label]]'
  say '(separati da spazi)'
  say ''
  say 'files predefiniti:'
  say '- emr  (capoluoghi di provincia Emilia Romagna)'
  say '- nord (citta principali del Nord Italia)'
  say '- null (non disegna nulla)'
  say ''
  say 'opzione marker:'
  say 'corrisponde al codice grads'
  say '(0=nessuno, 2=cerchio vuoto, 3=cerchio pieno, ...)'
  say 'se negativo non scrive le label'
  say ''
  say 'campo posizione (in filein):'
  say 'indica la posizione della label rispetto al punto'
  say 'per cambiare la dimensione delle label: set strsiz width [height]'
  say ' -1 = sovrapposta al punto'
  say '  0 = a destra sopra'
  say '  1 = a sinistra sopra'
  say '  2 = a sinistra sotto'
  say '  3 = a destra sotto'
  say ''
  say 'campo angolo (in filein):'
  say '  0 = orizzontale'
  say '  45 = obliqua di 45gradi in su'
  return
endif

* Apro il file: se esiste interpeto filein come path completo, altrimenti lo
* cerco nella dir di default
idum1=read(filein)
if(sublin(idum1,1)!=0)
 filein2='/autofs/nfshomes/eminguzzi/server/util/grads/dat/marks_'filein'.dat'
 idum2=read(filein2)
 if(sublin(idum2,1)!=0)
  say "Shape file non trovato:"
  say filein
  say filein2
  return
 endif
 filein=filein2
endif
idum=close(filein)

if (marker='')
  marker=0
endif

if (marksize='')
  marksize=0.1
endif

if (markcol='')
  markcol=-1
endif

if (labelcol='')
  labelcol=markcol
endif

label=1
if(marker<0)
  marker=-marker
  label=0
endif


*---------------------------------
* Lettura coordinate e plot labels

'q gxinfo'
xlin=sublin(result,3)
ylin=sublin(result,4)
xmin=subwrd(xlin,4)
xmax=subwrd(xlin,6)
ymin=subwrd(ylin,4)
ymax=subwrd(ylin,6)

*'set mpdset 'mres

cnt=0
kret=0
while(kret=0)

* Lettura file
  dum=read(filein)
  kret=sublin(dum,1)

  if (kret = 0) 
    dum2=sublin(dum,2)
    mylat=subwrd(dum2,1)
    mylon=subwrd(dum2,2)
    mylab=subwrd(dum2,3)

* Posizione e direzione delle label
    mylabpos=''
    mylabdir=0
    mylabpos=subwrd(dum2,4)
    if(mylabpos!=0 & mylabpos!=1 & mylabpos!=2 & mylabpos!=3)
      mylabpos=0
    else
      mylabdir=subwrd(dum2,5)
    endif
    if(mylabdir='')
      mylabdir=0
    endif
*   say mylab' 'mylabpos' 'mylabdir

    shift=0.05
    if(mylabpos=0-1)
      shiftx=0
      shifty=0
      jus='c'
    endif
    if(mylabpos=0)
      shiftx=shift
      shifty=shift
      jus='l'
    endif
    if(mylabpos=1)
      shiftx=-shift
      shifty=shift
      jus='r'
    endif
    if(mylabpos=3)
      shiftx=shift
      shifty=-shift
      jus='l'
    endif
    if(mylabpos=2)
      shiftx=-shift
      shifty=-shift
      jus='r'
    endif
say mylab' lon='mylon' lat='mylat' pos='mylabpos' just='jus' angle='mylabdir
    'set string 'labelcol' 'jus' -1 'mylabdir
    'set line 'markcol

* Conversione coordinate
    'q w2xy 'mylon' 'mylat
    myx=subwrd(result,3)
    myy=subwrd(result,6)

* Disegna    
    if(myx>xmin & myx<xmax & myy>ymin & myy<ymax)
      cnt=cnt+1

      if(marker>0)
        'draw mark 'marker' 'myx' 'myy' 'marksize
*       say mylab' 'jus' 'shiftx' 'shifty
        myx=myx+shiftx
        myy=myy+shifty
      endif 

      if(label=1)
        'draw string 'myx' 'myy' 'mylab 
      endif
    endif
             

  endif

endwhile

say 'scritte 'cnt' labels'
return 


