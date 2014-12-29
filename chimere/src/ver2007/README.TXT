--------------------------------------------------------------------------------
Note relative alla compilazione di diagmet - versione 2007
Ultima modifica: 04/12/2014

- diagmet.exe versione 2007 contiene alcuni dimensionamenti inchiavardati nel 
  codice, per cui deve essere ricompilato ogni volta che cambia il dominio.
- I parametri da cambiare sono contenuti nel modulo "chimere_params", che e' nel
  file src/ver2007/modules/chimere_params.F90
- Questo modulo tuttavia e' usato da una serie di altri files: di tutti questi 
  devono esistere copie separate per ciascun dominio, altrimenti "make" li
  compila una volta sola, e attribuisce a tutte le versioni di diagmet.exe gli 
  stessi diemnsionamenti.
- Se si prova a ricompilare una sola versione di diagmet, spesso si creano 
  problemi: l'unico modo che sembra sicuro, e' ricompilare l'intera directory
  chimere/src (make clean, make)

Per aggiungere un nuovo dominio:
- creare il file src/ver2007/modules/chimere_params.NEWDOM.F90
- nella dir src/ver2007/diag,  creare una copia con estensione .NEWDOM.F90 per 
  ciascuno dei files: diagXXX diagXXX_common diagXXX_science (XXX = met, bio)
- modificare i Makefile.am nelle dir src e src/ver2007
