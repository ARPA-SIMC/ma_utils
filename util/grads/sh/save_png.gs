function savepng(args)

default=web
flag=0

if(args='')
 prompt 'nome file (senza estensione)? '
 pull name
 resol=default
else
 name=subwrd(args,1)
 resol=subwrd(args,2)
 if(resol='');resol=default;endif
 if(subwrd(args,2)='-x' & subwrd(args,4)='-y')
  x=subwrd(args,3)
  y=subwrd(args,5)
  flag=1
  resol=user
 endif
 if(subwrd(args,2)='-y' & subwrd(args,4)='-x')
  y=subwrd(args,3)
  x=subwrd(args,5)
  flag=1
  resol=user
 endif
endif

if(name='-h')
  usage()
  return
endif

if(resol=high)
  'printim 'name'.png x1584 y1124'
  flag=1
endif
if(resol=med | resol=medium )
  'printim 'name'.png x1188 y843'
  flag=1
endif
if(resol=low)
  'printim 'name'.png x792 y562' 
  flag=1
endif
if(resol=web)
  'printim 'name'.png x640 y480'
  flag=1
endif
if(resol=pic)
  'printim 'name'.png x200 y150'
  flag=1
endif
if(resol=user)
  'printim 'name'.png x'x' y'y
  flag=1
endif

if(flag)
  say 'ho creato il file 'name'.png'
else
  say
  say "non riconosco l'opzione "resol
  usage()
  return
endif

*************
function usage()
  say
  say "uso: save_png nomefile [resol] [-x xdim -y ydim]"
  say "     resol = high,med,low,web,pic"
  say "     xdim  = dimensioni x in pixel"
  say "     ydim  = dimensioni y in pixel"
  say
return
