*
* Script che sovrappone a una schermata GrADS
* un'immagine in formato PNG
*
* Richiede gli applicativi:
* - pngtopnm
* - pnmscale
* - pnmtopng
* - composite
* nella cartella /usr/bin/
* 
*                     Johnny   28/11/2005

function overlay(arg)
rc=gsfallow('on')

* gestisce argomenti

ok_img=parseopt(arg,'-','i','fi')
ok_pos=parseopt(arg,'-','p','pos')
ok_din=parseopt(arg,'-','d','dimg')
ok_dout=parseopt(arg,'-','D','dout')
ok_fout=parseopt(arg,'-','o','fo')
ok_map=parseopt(arg,'-','r','fm')
ok_shft=parseopt(arg,'-','s','shft')

if(ok_img<1 | ok_pos<2)
  usage()
  return
else
  fileimg=_.fi.1
endif
  
if(ok_fout>=1)
  fileout=_.fo.1
else
  fileout=out
endif

if(ok_map>=1)
  filemap=_.fm.1
else
  filemap=_tmp
  '!if [ -f _tmp.png ];then rm -f _tmp.png; fi'
endif

if(ok_shft>=2)
  xshft=_.shft.1
  yshft=_.shft.2
else
  xshft=0
  yshft=0
endif

* definisce dimensioni immagine

'q gxinfo'
line=sublin(result,2)
xpage=subwrd(line,4)
ypage=subwrd(line,6)

ximg=0
yimg=0
xout=0
yout=0
if(ok_din>0);ximg=_.dimg.1;endif  
if(ok_dout>0);xout=_.dout.1;endif  
if(ok_din>1);yimg=_.dimg.2;endif  
if(ok_dout>1);yout=_.dout.2;endif 
if(ximg=0 & xout>0);ximg=math_nint(xout/5);endif
if(ximg=0);ximg=120;endif
if(ximg>0 & xout=0);xout=ximg*5;endif
if(yimg=0);yimg=math_nint(ximg/xpage*ypage);endif
if(yout=0);yout=math_nint(xout/xpage*ypage);endif

* definisce posizione dell'immagine sulla mappa

lon=_.pos.1
lat=_.pos.2
'q w2xy 'lon' 'lat
line=result
xpos=subwrd(line,3)
ypos=subwrd(line,6)
xpos=xout/xpage*xpos-(ximg/2)+xshft
ypos=yout/ypage*(ypage-ypos)-(yimg/2)-yshft
xpos=math_nint(xpos)
ypos=math_nint(ypos)

* salva immagine corrente

dum=read(_tmp.png)
nomap=sublin(dum,1)
if(nomap!=0 & ok_map!=1)
  'save_png _tmp -x 'xout' -y 'yout
endif

* ridimensiona immagine di overlay

'!/usr/bin/pngtopnm -mix -background=white 'fileimg'.png > _1.pnm'
'!/usr/bin/pnmscale -xsize 'ximg' -ysize 'yimg' _1.pnm > _2.pnm'
'!/usr/bin/pnmtopng -transparent white _2.pnm > _1.png'

* colloca immagine di overlay

if(xpos<0)
 xsign=""
else
 xsign="+"
endif
if(ypos<0)
 ysign=""
else
 ysign="+"
endif
'!/usr/bin/composite -geometry 'xsign''xpos''ysign''ypos' _1.png 'filemap'.png 'fileout'.png'

* fa pulizia

'!if [ -f _1.png   ] ; then rm _1.png  ;fi'
'!if [ -f _tmp.png ] ; then rm _tmp.png;fi'
'!if [ -f _1.pnm   ] ; then rm _1.pnm  ;fi'
'!if [ -f _2.pnm   ] ; then rm _2.pnm  ;fi'



*********************
function usage()
say ''
say ' usage: overlay -i fileimg -p lon lat [-d ximg yimg] '
say '                [-D xout yout] [-o fileout] [-r filein]'
say '                [-s xshft yshft]'
say ''
say ' fileimg = input file name, without .png extension'
say ' lon,lat = position of the image on the map'
say ' x-,yimg = dimension of the input image, in pixel'
say ' x-,yout = dimension of the output image, in pixel'
say ' x-,yshft= additional shift to lat lon, in pixel'
say '           (for windrose: -7 -4, if xout=1000)'
say ' fileout = output file name, without .png extension'
say ' filein  = map to be overlayed, without .png extension'
say '           (default is the present view)'
say ''
say ' NB Coordinates are calculated on the present map'
say ''
say ' NB Anything white in the overlaying image,'
say '    becomes transparent in the final image'
say ''

