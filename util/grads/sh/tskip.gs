function tskip(args)

* plotta le inversioni calcolate da trasp_temp
* sulla base dei TEMP di radiosondaggio
*

* controlla l'intervallo temporale

'q dim'
dum=sublin(result,5)
start_time=subwrd(dum,11)
end_time=0
end_time=subwrd(dum,13)

if(end_time<=start_time)
 say 'please set time:'
 say 'end time must be'
 say 'greater than start time'
 return
endif

* gestisce gli argomenti della funzione

var=""
var=subwrd(args,1)
if(var="")
 say 'usage: tskip var int [opt]'
 say
 say 'parameters:'
 say '            var = variable to process'
 say '            int = skipping interval'
 say '            opt = 0 do not calculates complementar'
 say '                  1 calculates complementar, default'
 say
 say 'variable?'
 pull var
endif

int=0
int=subwrd(args,2)
while(int<2)
 say 'skipping interval? (>1)'
 pull int
endwhile

opt=subwrd(args,3)
if(opt='');opt=1;endif

* costruisce la mask

tmax=end_time
t=start_time-int
dum=-1
count=0
'set t 't' 't
'define mask='dum

while(t <= tmax)
 if(count=int)
  dum=1
  count=1
 else
  count=count+1
 endif

 'set t 'start_time-int' 't
 'define mask=const(mask,'dum',-u)'

 if(dum=1)
  dum=-1
 endif
 t=t+1
endwhile

* definisce la variabile skippata
* e la sua complementare

'define 'var'tsk=maskout('var',mask)'
if (opt=1)
  'define 'var'ctsk=maskout('var',-mask)'
endif
'undefine mask'

say 'defined 'var'tsk'
say 'as 'var' skipped through time'
say 'with interval 'int
say 'starting from timestep 'start_time
say 'ending with timestep 'end_time';'
if (opt=1)
  say 'also defined its complementar'
  say var'ctsk'
endif
'set t 'start_time' 'end_time

