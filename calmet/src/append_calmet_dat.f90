PROGRAM append_calmet_dat
!--------------------------------------------------------------------------
! Programma per leggere l'output di una serie di run calmet consecutivi 
! (files calmet.dat) e riscriverlo in un unico file.
!
! Uso: append_calmet_dat filelst fileout [-h]
! Gestisce le versioni di Calmet 5.0 e 5.2, e riscrive nello stesso formato
! Legge l'elenco dei files da eleaborare da filelst
!
! NOTE: 
! - I files devono essere temporalmente consecutivi (se c'e' sovrapposizione
!   tra gli istanti di due files, vengono scritti i dati del primo)
! - Viene scritto il numero di istanti richiesto, indipendentemente dal 
!   contnuto dei files di input (questo perche' il numero di istanti deve
!   essere scritto nell'header del file di ouptut)
! - Il programma controlla solo le date e che la griglia sia la stessa in
!   tutti i files di input
!
!                                           Versione 1.0, Enrico 01/02/2008
!--------------------------------------------------------------------------

USE date_handler
IMPLICIT NONE       

!--------------------------------------------------------------------------
! 0) Dichiarazioni

! 0.1) Parametri che dimensionano al massimo gli array, contenuti nel file
!      params.met di calmet 

INTEGER, PARAMETER :: mxnx=150,mxny=150,mxnz=12
INTEGER, PARAMETER :: mxss=99,mxus=99,mxps=1,mxows=1
INTEGER, PARAMETER :: mxnzp1=mxnz+1

!       MXNX    - Maximum number of X grid cells
!       MXNY    - Maximum number of Y grid cells
!       MXNZ    - Maximum number of layers
!       MXNZP1  - Maximum number of face cells
!       MXSS    - Maximum number of surface meteorological stations
!       MXUS    - Maximum number of upper air stations
!       MXPS    - Maximum number of precipitation stations
!       MXOWS   - Maximum number of overwater stations

! 0.2) Dichiarazioni per lettura headers
REAL :: zfacem(mxnzp1),xssta(mxss),yssta(mxss),xusta(mxus),yusta(mxus), &
        xPsta(mxps),ypsta(mxps), &
        zo(mxnx,mxny),elev(mxnx,mxny),xlai(mxnx,mxny)

REAL :: dgrid,xorigr,yorigr
REAL :: xlat0,xlon0,conec,xlat1,xlat2,rlat0,rlon0

INTEGER :: ilandu(mxnx,mxny),nears(mxnx,mxny)
INTEGER :: ibyr,ibmo,ibdy,ibhr,ibtz,irlg,irtype
INTEGER :: nx,ny,nz,iutmzn,iwfcod,nssta
INTEGER :: nusta,npsta,nowsta,nlu,iwat1,iwat2
INTEGER :: idum(12)

CHARACTER (LEN=80) title(3)
CHARACTER (LEN=8)  ver,level,clab1,clab2,clab3,clab4,clab5,clab6, & 
                   clab7,clab8,clab9,clab10,clab11,clab12

LOGICAL :: lcalgrd,llconf
 
! 0.3) Dichiarazioni per lettura data records
REAL :: u(mxnx,mxny,mxnz),v(mxnx,mxny,mxnz),w(mxnx,mxny,mxnz)
REAL ::  ztemp(mxnx,mxny,mxnz)      
REAL :: ustar(mxnx,mxny),zi(mxnx,mxny),el(mxnx,mxny)
REAL :: wstar(mxnx,mxny),rmm(mxnx,mxny)
REAL :: tempk(mxss),rho(mxss),qsw(mxss)

INTEGER :: ipgt(mxnx,mxny)
INTEGER :: irh(mxss),ipcode(mxss)
INTEGER :: ndathr,i,j,k,ih
        
CHARACTER (LEN=8) :: clabu,clabv,clabw,clabt,clabsc,clabus,clabzi
CHARACTER (LEN=8) :: clabl,clabws,clabrmm,clabtk,clabd,clabq,clabrh
CHARACTER (LEN=8) :: clabpc 
  
! 0.4) Altre variabili del programma
TYPE (date) :: datac,datar,data2
REAL :: rgrid_desc(3),rver,rver_sav
INTEGER :: cnt_skip,cnt_files,nheadrec
INTEGER :: hhc,hhr,hh2,hht,igrid_desc(3),nhreq,yyyy,julday
INTEGER :: ios,eof,eor
CHARACTER (LEN=80) :: filein,fileout,filelst,chpar
CHARACTER (LEN=9) :: ch9

!==========================================================================
! 1) Preliminari

! Parametri da riga comandi
CALL getarg(1,filelst)
CALL getarg(2,chpar)
CALL getarg(3,fileout)
READ (chpar,*,IOSTAT=ios) nhreq

IF (filelst == "-h" .OR. filelst == "" .OR. fileout == "" .OR. &
  ios /= 0 .OR. nhreq < 0) THEN
  WRITE (*,*) "Uso: append_calmet_dat filelst nhreq fileout [-h]"
  WRITE (*,*) "filelst: contiene i nomi dei fiels calmet.da da appendere (un nome per riga)"
  WRITE (*,*) "nhreq:   numero totale di istanti da scrivere (24 per un run di 1 giorno)"
  STOP
ENDIF

! Apro i files
OPEN (UNIT=20, FILE=filelst, FORM="FORMATTED", STATUS="OLD", ACTION="READ", &
  ERR=9999)
OPEN (UNIT=40, FILE=fileout, FORM='UNFORMATTED', STATUS='REPLACE')

! Trovo codice EOF
CALL get_eof_eor(eof,eor)

!==========================================================================
! CICLO PRINCIPALE (INPUT FILES)

cnt_files = 0
cnt_skip = 0
files: DO

READ (20,'(a)',IOSTAT=ios) filein
IF (ios == eof) GOTO 9993
IF (ios /= 0) GOTO 9998

OPEN (UNIT=30, FILE=filein, FORM="UNFORMATTED", STATUS="OLD", ACTION="READ", &
  ERR=9997)
WRITE (*,*) "Elaboro file: ",TRIM(filein)
cnt_files = cnt_files + 1

!==========================================================================
! 2) ELaboro Headers

IF (cnt_files == 1) THEN

!--------------------------------------------------------------------------
! 2.1) Se sto elaborando il primo file, leggo e riscrivo gli header record
!      e salvo i parametri della griglia

!--------------------------------------------------------------------------
! 2.1.1 Lettura

! Lettura header record 1: run title
READ (30) title

! Lettura header record 2: run and grid information
READ (30) ver,level,ibyr,ibmo,ibdy,ibhr,ibtz,irlg,irtype, &
  nx,ny,nz,dgrid,xorigr,yorigr,iutmzn,iwfcod,nssta, &
  nusta,npsta,nowsta,nlu,iwat1,iwat2,lcalgrd 

READ (ver,*) rver ! numero della versione (reale)
WRITE (*,'(a,f4.1)') "Versione calmet : ",rver

! Lettura header record 3 (-): Lambert Conformal parameters
IF (rver >= 5.2) THEN
  READ (30) xlat0,xlon0,llconf,conec,xlat1,xlat2,rlat0,rlon0
ENDIF

! Lettura header record 4 (3): vertical cell face height
READ (30) clab1,idum(1),(zfacem(j),j=1,nz+1) 

! Lettura header record 5-6 (4-5): surface station coordinates
IF (nssta >= 1) THEN
  READ (30) clab2,idum(2),(xssta(j),j=1,nssta) 
  READ (30) clab3,idum(3),(yssta(j),j=1,nssta) 
ENDIF

! Lettura header record 7-8 (6-7): upper air station coordinates
IF (nusta >= 1) THEN
  READ (30) clab4,idum(4),(xusta(j),j=1,nusta) 
  READ (30) clab5,idum(5),(yusta(j),j=1,nusta) 
ENDIF

! Lettura header record 9-10 (8-9): precipitation station coordinates
IF (npsta >= 1) THEN
  READ (30) clab6,idum(6),(xpsta(j),j=1,npsta) 
  READ (30) clab7,idum(7),(ypsta(j),j=1,npsta) 
ENDIF

! Lettura header record 11 (10): surface roughness lenghts
READ (30) clab8,idum(8),((zo(i,j),i=1,nx),j=1,ny)

! Lettura header record 12 (11): land use categories
READ (30) clab9,idum(9),((ilandu(i,j),i=1,nx),j=1,ny)

! Lettura header record 13 (12): terrain elevations
READ (30) clab10,idum(10),((elev(i,j),i=1,nx),j=1,ny)

! Lettura header record 14 (13): leaf area indexes
READ (30) clab11,idum(11),((xlai(i,j),i=1,nx),j=1,ny)

! Lettura header record 15 (14): nearest surface station to each grid point
READ (30) clab12,idum(12),((nears(i,j),i=1,nx),j=1,ny)

!--------------------------------------------------------------------------
! 2.1.2 Elaborazioni dipendenti dagli headers

! Salvo la data iniziale
datac = date(ibdy,ibmo,ibyr)
hhc = ibhr

! Salvo i parametri descrittivi della griglia e la versione Calmet
igrid_desc(1:3) = (/nx,ny,nz/)
rgrid_desc(1:3) = (/dgrid,xorigr,yorigr/)
rver_sav = rver

! Salvo il numero di header records
IF (rver >= 5.2) THEN
  nheadrec = 15
ELSE
  nheadrec = 14
ENDIF

! Calcolo l'ultima data attesa
hht = hhc + nhreq - 1
data2 = datac + nhreq / 24
hh2 = MOD(nhreq,24)
WRITE (*,'(2(a,i4.4,3(1x,i2.2)))') "Istanti richiesti: da ", &
  datac%yy,datac%mm,datac%dd,hhc," a ",data2%yy,data2%mm,data2%dd,hh2

!--------------------------------------------------------------------------
! 2.1.3 Scrittura

! Scrittura header record 1: run title
WRITE (40) title

! Scrittura header record 2: run and grid information
! NB: write nhreq instead of irlg
WRITE (40) ver,level,ibyr,ibmo,ibdy,ibhr,ibtz,nhreq,irtype, &
  nx,ny,nz,dgrid,xorigr,yorigr,iutmzn,iwfcod,nssta, &
  nusta,npsta,nowsta,nlu,iwat1,iwat2,lcalgrd 

! Scrittura header record 3 (-): Lambert Conformal parameters
IF (rver >= 5.2) THEN
  WRITE (40) xlat0,xlon0,llconf,conec,xlat1,xlat2,rlat0,rlon0
ENDIF

! Scrittura header record 4 (3): vertical cell face height
WRITE (40) clab1,idum(1),(zfacem(j),j=1,nz+1) 

! Scrittura header record 5-6 (4-5): surface station coordinates
IF (nssta >= 1) THEN
  WRITE (40) clab2,idum(2),(xssta(j),j=1,nssta) 
  WRITE (40) clab3,idum(3),(yssta(j),j=1,nssta) 
ENDIF

! Scrittura header record 7-8 (6-7): upper air station coordinates
IF (nusta >= 1) THEN
  WRITE (40) clab4,idum(4),(xusta(j),j=1,nusta) 
  WRITE (40) clab5,idum(5),(yusta(j),j=1,nusta) 
ENDIF

! Scrittura header record 9-10 (8-9): precipitation station coordinates
IF (npsta >= 1) THEN
  WRITE (40) clab6,idum(6),(xpsta(j),j=1,npsta) 
  WRITE (40) clab7,idum(7),(ypsta(j),j=1,npsta) 
ENDIF

! Scrittura 11 (10): surface roughness lenghts
WRITE (40) clab8,idum(8),((zo(i,j),i=1,nx),j=1,ny)

! Scrittura 12 (11): land use categories
WRITE (40) clab9,idum(9),((ilandu(i,j),i=1,nx),j=1,ny)

! Scrittura header record 13 (12): terrain elevations
WRITE (40) clab10,idum(10),((elev(i,j),i=1,nx),j=1,ny)

! Scrittura header record 14 (13): leaf area indexes
WRITE (40) clab11,idum(11),((xlai(i,j),i=1,nx),j=1,ny)

! Scrittura header record 15 (14): nearest surface station to each g.p.
WRITE (40) clab12,idum(12),((nears(i,j),i=1,nx),j=1,ny)

ELSE

!--------------------------------------------------------------------------
! 2.2) Se non e' il primo file, leggo dall'header solo i parametri della 
!      griglia, per assicurarmi che non siano cambiati

! Leggo gli headers
READ (30)
READ (30) ver,level,ibyr,ibmo,ibdy,ibhr,ibtz,irlg,irtype, &
  nx,ny,nz,dgrid,xorigr,yorigr,iutmzn,iwfcod,nssta, &
  nusta,npsta,nowsta,nlu,iwat1,iwat2,lcalgrd 
IF (rver >= 5.2) THEN
  READ (30) 
ENDIF
READ (30) 
IF (nssta >= 1) THEN
  READ (30) 
  READ (30) 
ENDIF
IF (nusta >= 1) THEN
  READ (30)
  READ (30)
ENDIF
IF (npsta >= 1) THEN
  READ (30)
  READ (30)
ENDIF
READ (30)
READ (30)
READ (30)
READ (30)
READ (30)

! Verifico che siano conformi a quelli del primo file
IF (ANY((/nx,ny,nz/) /= igrid_desc(1:3)) .OR.&
    ANY((/dgrid,xorigr,yorigr/) /= rgrid_desc(1:3))) GOTO 9996
READ (ver,*) rver
IF (rver /= rver_sav) GOTO 9995

ENDIF

!==========================================================================
! 3) Elaboro data records (ciclo sulle scadenze)

DO ih = 1, irlg

!--------------------------------------------------------------------------
! 3.1 Lettura

! Wind: U,V,W components 
  DO k=1,nz
    READ (30) clabu,ndathr,((u(i,j,k),i=1,nx),j=1,ny)       
    READ (30) clabv,ndathr,((v(i,j,k),i=1,nx),j=1,ny)       
    IF (lcalgrd) READ (30) clabw,ndathr,((w(i,j,k),i=1,nx),j=1,ny)  
  ENDDO
              
! 3-D temperature field 
  IF (lcalgrd .AND. irtype == 1) THEN
    DO k=1,nz
      READ (30) clabt,ndathr,((ztemp(i,j,k),i=1,nx),j=1,ny)       
    ENDDO
  ENDIF

! 2-D meteorological fields 
  IF (irtype == 1) THEN 
    READ (30) clabsc,ndathr,((ipgt(i,j),i=1,nx),j=1,ny)
    READ (30) clabus,ndathr,((ustar(i,j),i=1,nx),j=1,ny)
    READ (30) clabzi,ndathr,((zi(i,j),i=1,nx),j=1,ny)
    READ (30) clabl,ndathr,((el(i,j),i=1,nx),j=1,ny)
    READ (30) clabws,ndathr,((wstar(i,j),i=1,nx),j=1,ny)
    IF (npsta.gt.0) READ (30) clabrmm,ndathr, &
       ((rmm(i,j),i=1,nx),j=1,ny)           ! RMM non usato per calgrid
  ENDIF
 
! 1-D meteorological fields 
  IF (irtype.eq.1) THEN 
    READ (30) clabtk,ndathr,(tempk(i),i=1,nssta)
    READ (30) clabd,ndathr,(rho(i),i=1,nssta)
    READ (30) clabq,ndathr,(qsw(i),i=1,nssta)
    READ (30) clabrh,ndathr,(irh(i),i=1,nssta)
    IF (npsta.gt.0) READ (30) clabpc,ndathr,(ipcode(i),i=1,nssta)
  ENDIF
                   
!--------------------------------------------------------------------------
! 3.2 Controllo la data trovata

  WRITE (ch9,'(i9)') ndathr
  READ (ch9,'(i4,i3,i2)') yyyy,julday,hhr
  datar = greg(julday,yyyy)
  IF ((datar==datac .AND. hhr>hhc) .OR. (datar>datac)) THEN
    GOTO 9994
  ELSE IF ((datar==datac .AND. hhr<hhc) .OR. (datar<datac)) THEN
    cnt_skip = cnt_skip + 1
    CYCLE
  ENDIF

!--------------------------------------------------------------------------
! 3.3 Scrittura

! Wind: U,V,W components 
  DO k=1,nz
    WRITE (40) clabu,ndathr,((u(i,j,k),i=1,nx),j=1,ny)       
    WRITE (40) clabv,ndathr,((v(i,j,k),i=1,nx),j=1,ny)       
    IF (lcalgrd) WRITE (40) clabw,ndathr,((w(i,j,k),i=1,nx),j=1,ny)  
  ENDDO
              
! 3-D temperature field 
  IF (lcalgrd .AND. irtype == 1) THEN
    DO k=1,nz
      WRITE (40) clabt,ndathr,((ztemp(i,j,k),i=1,nx),j=1,ny)
    ENDDO
  ENDIF

! 2-D meteorological fields 
  IF (irtype == 1) THEN 
    WRITE (40) clabsc,ndathr,((ipgt(i,j),i=1,nx),j=1,ny)
    WRITE (40) clabus,ndathr,((ustar(i,j),i=1,nx),j=1,ny)
    WRITE (40) clabzi,ndathr,((zi(i,j),i=1,nx),j=1,ny)
    WRITE (40) clabl,ndathr,((el(i,j),i=1,nx),j=1,ny)
    WRITE (40) clabws,ndathr,((wstar(i,j),i=1,nx),j=1,ny)
    IF (npsta.gt.0) WRITE (40) clabrmm,ndathr, &
       ((rmm(i,j),i=1,nx),j=1,ny)           ! RMM non usato per calgrid
  ENDIF
 
! 1-D meteorological fields 
  IF (irtype.eq.1) THEN 
    WRITE (40) clabtk,ndathr,(tempk(i),i=1,nssta)
    WRITE (40) clabd,ndathr,(rho(i),i=1,nssta)
    WRITE (40) clabq,ndathr,(qsw(i),i=1,nssta)
    WRITE (40) clabrh,ndathr,(irh(i),i=1,nssta)
    IF (npsta.gt.0) WRITE (40) clabpc,ndathr,(ipcode(i),i=1,nssta)
  ENDIF

!--------------------------------------------------------------------------
! 3.4 Calcolo la prossima data

! Se ho raggiunto l'ultima data richiesta, mi fermo
  IF (datac==data2 .AND. hhc==hh2) EXIT files

! Calcolo la prossima data attesa
  hhc = hhc + 1
  IF (hhc == 24) THEN
    hhc = 0
    datac = datac + 1
  ENDIF

ENDDO

!==========================================================================
! Chiudo il ciclo sui files

CLOSE (30)
ENDDO files

!==========================================================================
! 4) Conclusione

WRITE (*,*) "Apend_calmet_dat: elaborazioni terminate"
WRITE (*,*) "Elaborati ",cnt_files," files"
WRITE (*,*) "Istanti di sovrapposizione skippati: ",cnt_skip

CLOSE(20)
CLOSE(40)
STOP

!==========================================================================
! 5) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filelst)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filelst)
STOP

9997 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9996 CONTINUE
WRITE (*,*) "Griglia diversa nel file ",TRIM(filein)
STOP

9995 CONTINUE
WRITE (*,*) "Versione Calmet diversa nel file ",TRIM(filein)
WRITE (*,*) "Attesa ",rver_sav," trovata ",rver
STOP

9994 CONTINUE
WRITE (*,*) "Salto di data nel file ",TRIM(filein)
WRITE (*,'(2(a,i5,3i2))') "Data attesa: ",datac%yy,datac%mm,datac%dd,hhc, &
  " trovata: ",datar%yy,datar%mm,datar%dd,hhr
STOP

9993 CONTINUE
WRITE (*,*) "Files di input terminati senza raggiungere l'ultima data richiesta"
WRITE (*,*) "Il file ",TRIM(fileout)," e' inconsistente"
WRITE (*,'(a,i4.4,3(1x,i2.2))') "Prima data non trovata: ", &
  datac%yy,datac%mm,datac%dd,hhc

END PROGRAM append_calmet_dat

!=========================================================================

SUBROUTINE get_eof_eor(eof, eor)
!-------------------------------------------------------------------------
! Ritorna i codici di errore macchina-dipendenti corrispondenti alle 
! condizioni di EOF e EOR nella lettura di un file sequenziale formattato
!
! Secondo manuale, questi sono gli unici due casi in cui IOSTAT ritorna
! con un valore negativo. 
! Si noti che EOR riguarda solo non-advancinag READ
!-------------------------------------------------------------------------
IMPLICIT NONE

INTEGER, INTENT(OUT) :: eof,eor

INTEGER :: k, ios, idummy=0, iun=0
LOGICAL :: l1 = .TRUE.


! Cerco un'unita' libera per aprire il file di prova
DO k = 10,99
  INQUIRE (UNIT=k, OPENED=l1, IOSTAT=ios)
  IF (.NOT. l1 .AND. ios==0) THEN
    iun = k
    EXIT
  ENDIF
ENDDO
IF (iun == 0) GOTO 9999   ! non ho torvato nessuna unita' libera
!WRITE (*,*) "uso unita ",iun

! Cerco codice di errore per EOF
OPEN (unit=k, STATUS="SCRATCH", FORM="FORMATTED", ACCESS="SEQUENTIAL", &
  PAD="NO", ERR=9999)
ENDFILE (k)
REWIND (k)
READ (k,*,IOSTAT=eof)
CLOSE(k)

! Cerco codice di errore per EOR
OPEN (unit=k, STATUS="SCRATCH", FORM="FORMATTED", ACCESS="SEQUENTIAL", &
  PAD="NO", ERR=9999)
WRITE (k,'(a1)') "1" 
WRITE (k,'(a1)') "2"
REWIND (k)
READ (k,'(i1)',ADVANCE="NO",ERR=9999) idummy
READ (k,'(i1)',ADVANCE="NO",IOSTAT=eor) idummy
CLOSE(k)

!write (*,*) "eof,eor ",eof,eor
RETURN

! Gestione errori
9999 CONTINUE
WRITE (*,*) "Errore in subroutine get_eof_eor, usero' valori di default"
eof = -1
eor = -2
RETURN

END SUBROUTINE get_eof_eor

