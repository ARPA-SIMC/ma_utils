PROGRAM proc_seriet_surf
!--------------------------------------------------------------------------
! Programma della catena Calmet
! Legge un file in formato seriet e lo riscrive in formato st2.
! Uso: proc_seriet_surf.exe filein fileout [-test] [-h] 
!
! Note:
! - Il nome del file di output deve essere nella forma: NNN_SSSSS.st2
! - Il file di input (seriet) deve contenere i campi: 
!   Ps,T02,Td02,dd10m,ff10m,tp,tcc,lcl,mcl,hcl
! - Tracciato file di output (.st2): 1 record header, poi (6iN, 8f8.1): 
!   staz,net,anno,mese,giorno,ora, ff, dd, ceil,  tcc, tt, rh, prs,cod_prc 
!                                 [m/s grd ft*100 10i  K   %   mb  Calmet ]
!
!                                         Versione 2.0.0, Enrico 28/12/2012
!--------------------------------------------------------------------------
IMPLICIT NONE

INTEGER, PARAMETER :: fw = 10         ! ampiezza dei campi nei files seriet
INTEGER, PARAMETER :: nf_in = 10      ! n.ro di parametri richiesti in input
REAL, PARAMETER :: rmis_in = -9999.   ! dato mancante nei files seriet
REAL, PARAMETER :: rmis_out = 9999.   ! dato mancante nei files .st2a

! Label seriet dei parametri in input
CHARACTER(LEN=fw) :: reqlab(nf_in) = &
  (/"pr        ","Temp      ","td        ","Dir-wind  ","Mod-wind  ", &
    "tp        ","tcc       ","clcl      ","clcm      ","clch      "/)
INTEGER, PARAMETER :: reqlev(nf_in) = (/0,2,2,10,10,0,0,0,0,0/)

! Header files .st2 (output)
CHARACTER(LEN=87), PARAMETER :: head_out = &
  " staz net aaaa mm gg hh      ff      dd    ceil     tcc      tt      rh     prs cod_prc"

! Altre variabili del programma
REAL, ALLOCATABLE :: field(:)
REAL :: ceilh
REAL :: pr,tt,td,dd,ff,tp,tcc,cll,clm,clh,ceil,rh,idprc,esat_tt,esat_td
INTEGER :: idf_in(nf_in),kpar,kr,kp,k2,cnt_par,ios,eof,eor
INTEGER :: gg,mm,yy,hh,net,staz,npar_tot,levid
CHARACTER(LEN=10000) :: header_lev,header_par,rec_in
CHARACTER(LEN=87) :: rec_out
CHARACTER(LEN=80) :: filein,fileout,chpar
CHARACTER(LEN=fw) :: par_lab,lev_lab
LOGICAL :: ltest,luv

!--------------------------------------------------------------------------
! Parametri da riga comandi

ltest = .FALSE.
luv = .TRUE.
cnt_par = 0
DO kpar = 1,HUGE(0)
  CALL getarg(kpar,chpar)
  IF (chpar == "") THEN
    EXIT
  ELSE IF (TRIM(chpar) == "-h") THEN
    CALL scrive_help
    STOP
  ELSE IF (TRIM(chpar) == "-test") THEN
    ltest = .TRUE.
  ELSE IF (TRIM(chpar) == "-nouv") THEN
    luv = .FALSE.
  ELSE IF (cnt_par == 0) THEN
    cnt_par = 1
    filein = chpar
  ELSE IF (cnt_par == 1) THEN
    cnt_par = 2
    fileout = chpar
  ELSE
    CALL scrive_help
    STOP
  ENDIF
ENDDO

READ (fileout,'(i3,1x,i5)',IOSTAT=ios) net,staz
IF (ios /= 0 .OR. cnt_par /= 2) THEN
  CALL scrive_help
  STOP
ENDIF  

!--------------------------------------------------------------------------
! Altri preliminari

! Codice EOF
CALL get_eof_eor(eof, eor)

! Apro filein e leggo gli headers
OPEN (UNIT=30, FILE=filein, STATUS="OLD", ACTION="READ", IOSTAT=ios)
IF (ios /= 0) GOTO 9999
READ (30,*)
READ (30,*)
READ (30,*)
READ (30,'(17x,a)',ERR=9998) header_lev
READ (30,*,ERR=9998)
READ (30,'(17x,a)',ERR=9998) header_par

IF (MOD(LEN(TRIM(header_lev)), (fw+1)) /= 0) THEN
  GOTO 9998
ELSE
  npar_tot = LEN(TRIM(header_lev)) / (fw+1)
ENDIF

! Cerco negli headers i campi relativi ai parametri richiesti
idf_in(:) = -9999
DO kp = 1,npar_tot
  lev_lab = ADJUSTL(header_lev((kp-1)*(fw+1)+1:kp*(fw+1)))
  par_lab = ADJUSTL(header_par((kp-1)*(fw+1)+1:kp*(fw+1)))
  READ (lev_lab,*,ERR=9998) levid

  DO k2 = 1,10
    IF (TRIM(par_lab) == TRIM(reqlab(k2)) .AND. levid == reqlev(k2)) &
      idf_in(k2) = kp
  ENDDO
ENDDO

IF (ANY(idf_in(:) == -9999)) GOTO 9997 

! Altre operazioni
OPEN (UNIT=31, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")
WRITE (31,'(a)') head_out

ALLOCATE(field(1:npar_tot))

!--------------------------------------------------------------------------
! Lettura - scrittura (ciclo sugli istanti)

DO kr = 1,HUGE(0)

! Leggo dati da filein
  READ (30,'(a)',IOSTAT=ios) rec_in
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9996
 
  READ (rec_in,'(i2,1x,i2,1x,i4,1x,i2)',IOSTAT=ios) gg,mm,yy,hh
  IF (ios /= 0) GOTO 9996
  READ (rec_in(18:),*,IOSTAT=ios) field(1:npar_tot)
  IF (ios /= 0) GOTO 9996

  pr = field(idf_in(1))
  tt = field(idf_in(2))
  td = field(idf_in(3))
  dd = field(idf_in(4))
  ff = field(idf_in(5))
  tp = field(idf_in(6))
  tcc = field(idf_in(7))
  cll = field(idf_in(8))
  clm = field(idf_in(9))
  clh = field(idf_in(10))

! Converto unita' di misura e calcolo grandezze derivate
  IF (ff == rmis_in) ff=rmis_out

  IF (dd == rmis_in) dd=rmis_out

  ceil = ceilh(tcc,cll,clm,clh,rmis_in,rmis_out)

  IF (tcc == rmis_in) THEN
    tcc = rmis_out
  ELSE
    tcc = tcc / 10.
  ENDIF

  IF (tt == rmis_in) tt=rmis_out

  IF (td > tt) td = tt
  esat_tt = 6.1078 * EXP((17.2693882*(tt-273.15))/((tt-273.15)+237.3)) 
  esat_td = 6.1078 * EXP((17.2693882*(td-273.15))/((td-273.15)+237.3)) 
  rh = esat_td / esat_tt * 100.
  rh = MIN(MAX(rh,0.),100.)

  IF (pr == rmis_in) THEN
    pr=rmis_out
  ELSE
    pr = pr / 100.
  ENDIF

  IF (tp < 0.1) THEN
    idprc = 0.               ! no pioggia
  ELSE IF (tt < 273.15) THEN
    idprc = 19.              ! neve
  ELSE IF (tp < 1) THEN
    idprc = 1.               ! pioggia leggera
  ELSE 
    idprc = 7.               ! pioggia
  ENDIF

! USER MODIFICATION
! Se richiesta l'opzione -test, metto il vento a un valore fittizio, 
! altrimenti lo metto a dato mancante (voglio che calmet prenda il vento 
! solo dai profili)
  IF (ltest) THEN
    ff = 100.
    dd = 45.
  ELSE IF (.NOT. luv) THEN
    ff = 9999.
    dd = 9999.
  ENDIF

! Scrivo su fileout
  WRITE (31,'(i5.5,1x,i3.3,1x,i4.4,3(1x,i2.2),8f8.1)') &
    staz,net,yy,mm,gg,hh,ff,dd,ceil,tcc,tt,rh,pr,idprc

ENDDO

WRITE (*,*) "proc_seriet_surf: elaborati ",kr-1," istanti"
STOP

!--------------------------------------------------------------------------
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo headers ",TRIM(filein)
STOP

9997 CONTINUE
WRITE (*,*) "Errore, ",TRIM(filein)," non contiene alcuni i parametri richiesti:"
DO kp = 1, 10
  IF (idf_in(kp) == -9999) WRITE (*,*) TRIM(reqlab(kp))
ENDDO
STOP

9996 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," riga ",kr+6
STOP

END PROGRAM proc_seriet_surf

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION ceilh(tcc,cll,clm,clh,rmis_in,rmis_out) RESULT (ceil)

IMPLICIT NONE
REAL, INTENT(IN) :: tcc,cll,clm,clh,rmis_in,rmis_out
REAL :: ceil
!
REAL, PARAMETER :: hlow = 20.
REAL, PARAMETER :: hmed = 120.
REAL, PARAMETER :: hhig = 170.
REAL, PARAMETER :: hclear = 999.
!--------------------------------------------------------------------------

IF (tcc==rmis_in .OR. cll==rmis_in .OR. clm==rmis_in .OR. clh==rmis_in) THEN
  ceil = rmis_out
  RETURN
ENDIF

IF (tcc <= 50. ) THEN
  ceil = hclear
ELSE IF (cll >= 50. ) THEN
  ceil = hlow
ELSE IF (clm >= 50. .OR. 1.-(1.-0.01*cll)*(1.-0.01*clm) > .5) THEN
  ceil = hmed
ELSE 
  ceil = hhig
ENDIF

RETURN
END FUNCTION ceilh

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE get_eof_eor(eof, eor)
!-------------------------------------------------------------------------
! Ritorna i codici di errore macchina-dipendenti corrispondenti alle 
! condizioni di EOF e EOR nella lettura di un file sequenziale formattato
!
! Secondo manuale, questi sono gli unici due casi in cui IOSTAT ritorna
! con un valore negativo. 
! Si noti che EOR riguarda solo non-advancinag READ
!-------------------------------------------------------------------------

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

RETURN

! Gestione errori
9999 CONTINUE
WRITE (*,*) "Errore in subroutine get_eof_eor, usero' valori di default"
eof = -1
eor = -2
RETURN

END SUBROUTINE get_eof_eor

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Scrive a schermo un breve help
!
IMPLICIT NONE

WRITE (*,*) "Uso: proc_seriet_surf.exe filein fileout [-nouv] [-test] [-h]"
WRITE (*,*) "Filein: in formato seriet"
WRITE (*,*) "Fileout: nella forma NNN_SSSSS.st2"
WRITE (*,*) "  -nouv: mette mancanti i dati di vento (per prenderli solo da profili)"
WRITE (*,*) "  -test: sostituisce alcuni dati con valori fittizi (per debug Calmet)"

RETURN
END SUBROUTINE scrive_help

