PROGRAM split_qrs
!--------------------------------------------------------------------------
! Programma che stima QR e QS a partire da QRS e T
! Legge da files grib separati
! Scrive 2 files grib: qr.grb e qs.grb
! Usa i coefficenti medi calcolati con stat_qrs_temp.exe, relativi al mese 
!   di ottobre 2007.
!
! Note:
! Tutti i grib devono essere definiti sulla stessa area.
! Data, scadenza e livello nei files di input devono corrispondere; e' 
!   ammesso che il file T contenga dei grib in piu' (che vengono saltati),
!   purche' i campi da usare siano nello stesso ordine del file QRS.
!
!                                           Versione 2.0, Enrico 12/11/2007
!--------------------------------------------------------------------------

IMPLICIT NONE

! Altri parametri costanti (user modification)
REAL, PARAMETER :: rmis = -1.E20       ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 300000  ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: ksec1_sav(1024),ksec2_sav(1024)
INTEGER :: kbuffer(maxdim), klen, kret
REAL :: psec2(512),psec3(2)
REAL :: field(maxdim)

! Coefficenti per la ripartizione tra Qr e Qs in funzione di T
REAL, PARAMETER :: tcl1 = 253., tcl2 = 283., tclstep = 1.
INTEGER, PARAMETER :: ntcl = INT( (tcl2-tcl1)/tclstep ) + 2
REAL, PARAMETER :: fraz_rain(ntcl) = (/0.00, &                  ! < 253
  0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.01, 0.01, 0.01, 0.01, & ! < 263
  0.01, 0.02, 0.02, 0.03, 0.05, 0.07, 0.09, 0.12, 0.17, 0.23, & ! < 273
  0.44, 0.74, 0.84, 0.95, 0.97, 0.99, 1.00, 1.00, 1.00, 1.00, & ! < 283
  1.00/)                                                        ! >= 283

! Altre variabili del programma
REAL, ALLOCATABLE :: qrs(:),tt(:),qr(:),qs(:)
REAL :: fave1,fave2
INTEGER, ALLOCATABLE :: idcl(:)
INTEGER :: iuin(2),iuout(2),np,kfin,kg,nok1,nok2,hh_dum,hhc,ier,k,kp
INTEGER :: nskipt,cnt_out
CHARACTER (LEN=80) :: filein(2),fileout(2)
CHARACTER (LEN=2) :: hum
LOGICAL :: wait

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
CALL getarg(1,filein(1))
CALL getarg(2,filein(2))

IF (ANY(filein(1:2) == "") .OR. TRIM(filein(1)) == "-h") THEN
  WRITE (*,*) "Uso: calc_qrc_qis.exe [-h] file_Qrs file_T" 
  STOP
ENDIF

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
DO kfin = 1,2
  CALL PBOPEN (iuin(kfin),filein(kfin),'R',kret)
  IF (kret /= 0) GOTO 9999
ENDDO

fileout(1) = "qr.grb"
fileout(2) = "qs.grb"

CALL PBOPEN (iuout(1),fileout(1),'W',kret)
CALL PBOPEN (iuout(2),fileout(2),'W',kret)

!--------------------------------------------------------------------------
! 2) Lettura - Scrittura (ciclo su istanti/livelli)

nskipt = 0
cnt_out = 0
wait = .FALSE.

grb: DO kg = 1,HUGE(0)

!--------------------------------------------------------------------------
! 2.1) Lettura e controlli (ciclo sui file di input)

  DO kfin = 1,2

    IF (wait .AND. kfin == 1) THEN
      CYCLE
    ELSE
      wait = .FALSE.
    ENDIF

!   Leggo e decodifico i grib di input
    CALL PBGRIB(iuin(kfin),kbuffer,maxdim*4,klen,kret)
    IF (kret == -1 .AND. kfin == 1) THEN 
      EXIT grb
    ELSE IF (kret == -1) THEN
      GOTO 9998
    ELSE IF (kret < -1) THEN
      WRITE(*,*) "Error pbgrib: kret ",kret
      STOP
    ENDIF

    psec3(2) = rmis                           ! forzo dati mancanti = rmis
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'D',kret)
    IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

!   Se e' il primo grib, alloco le variabili e salvo sezione 2 
    IF (kfin == 1 .AND. kg == 1) THEN
      ksec2_sav(:) = ksec2(:)
      np = ksec4(1)
      ALLOCATE (qrs(np),tt(np),qr(np),qs(np),idcl(np))
    ENDIF

!   Controlli su scadenza, livello e area
    IF (ANY(ksec2(:) /= ksec2_sav(:)) .OR. ksec4(1) /= np) GOTO 9997

    IF (kfin == 1) THEN
      ksec1_sav(:) = ksec1(:)
      IF (ANY(ksec1(15:18)/=(/1,ksec1(16),0,0/)) .AND. &   ! LM
          ANY(ksec1(15:18)/=(/1,0,0,13/)) ) &              ! LAMA
        GOTO 9996
    ELSE
      IF (ANY(ksec1(7:18) /= ksec1_sav(7:18)) .OR. &
        ksec1(21) /= ksec1_sav(21)) THEN
        wait = .TRUE.
        nskipt = nskipt + 1
        CYCLE grb
      ENDIF
    ENDIF

!   Controlli sul parametro e salvo il campo
    SELECT CASE(kfin)
    CASE(1)                                          ! QRS
      IF (ksec1(1)/=201 .OR. (ksec1(6)/=39 .AND. ksec1(6)/=99)) GOTO 9994
      qrs(:) = field(:)
    CASE(2)                                          ! T
      IF (ksec1(1)/=2 .OR. ksec1(6)/=11) GOTO 9994
      tt(:) = field(:)
    END SELECT      

  ENDDO

!--------------------------------------------------------------------------
! 2.2) Calcoli

  DO kp = 1,np
    IF (tt(kp) < tcl1)  THEN
      idcl(kp) = 1
    ELSE IF (tt(kp) < tcl2) THEN
      idcl(kp) = INT((tt(kp)-tcl1)/tclstep) + 2
    ELSE
      idcl(kp) = ntcl
    ENDIF
  ENDDO

  WHERE (qrs(1:np) /= rmis)
    qr(1:np) = qrs(1:np) * fraz_rain(idcl(1:np))
    qs(1:np) = qrs(1:np) * (1. - fraz_rain(idcl(1:np)))
  ELSEWHERE
    qr(1:np) = rmis
    qs(1:np) = rmis
  ENDWHERE
      
!--------------------------------------------------------------------------
! 2.3) Scrittura output

! Scrivo grib QR
  ksec1(1) = 201                                    ! tabella
  ksec1(6) = 35                                     ! parametro
  ksec1(7:9) = ksec1_sav(7:9)                       ! livello
  ksec1(10:18) = ksec1_sav(10:18)                   ! data e scadenza
  ksec2(:) = ksec2_sav(:)                           ! griglia
  psec3(2) = rmis                                   ! codice dati mancanti
  ksec4(2) = 16                                     ! nbit
  field(1:np) = qr(1:np)

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout(1),kbuffer,ksec0(1),kret)
  
! Scrivo grib QS
  ksec1(1) = 201                                    ! tabella
  ksec1(6) = 36                                     ! parametro
  ksec1(7:9) = ksec1_sav(7:9)                       ! livello
  ksec1(10:18) = ksec1_sav(10:18)                   ! data e scadenza
  ksec2(:) = ksec2_sav(:)                           ! griglia
  psec3(2) = rmis                                   ! codice dati mancanti
  ksec4(2) = 16                                     ! nbit
  field(1:np) = qs(1:np)

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout(2),kbuffer,ksec0(1),kret)

  cnt_out = cnt_out + 1
  IF (MOD(kg,1000) == 0) WRITE (*,*) "Elaborato campo ",kg
ENDDO grb

!--------------------------------------------------------------------------
! 3) Conclusione

WRITE (*,*) "split_qrs.exe: campi elaborati ",kg-1," scritti ",cnt_out
IF (nskipt /= 0) WRITE (*,*) "Skippati ",nskipt," campi in ",TRIM(filein(2))
IF (kfin /= 1) WRITE (*,*) "Warning: ",TRIM(filein(2)), &
  " non contiente tutti i campi richiesti"

CALL PBCLOSE (iuin(1),kret)
CALL PBCLOSE (iuin(2),kret)
CALL PBCLOSE (iuout,kret)
CLOSE (96)
CLOSE (97)

STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE(*,*) "Errore aprendo ",TRIM(filein(kfin))," kret ",kret
STOP

9998 CONTINUE
WRITE(*,*) "Il file ",TRIM(filein(kfin))," contiene meno campi"
STOP

9997 CONTINUE
WRITE(*,*) "Area diversa in ",TRIM(filein(kfin))," campo ",kg
IF (ksec4(   1) /= np) WRITE (*,'(a,i10,a,i10)') &
  "ksec4(1) attesto ",np," trovato ",ksec4(1)
DO k = 1,1024
  IF (ksec2_sav(k) /= ksec2(k)) WRITE (*,'(a,i4,2(a,i10))') &
    "ksec2(",k,") attesto ",ksec2_sav(k)," trovato ",ksec2(k)
ENDDO

STOP

9996 CONTINUE
WRITE(*,'(2a,4i4)') "Scadenza non gestita in ",TRIM(filein(kfin)), &
  ksec1(15:18)
STOP

9995 CONTINUE
WRITE(*,*) "Data/livello/scadenza disallineata in ",TRIM(filein(kfin)), &
  " campo ",kg
WRITE(*,'(a,3i4,5x,6i5,5x,4i4)') "Richiesto: ", &
  ksec1_sav(7:9),ksec1_sav(10:14),ksec1_sav(21),ksec1_sav(15:18)
WRITE(*,'(a,3i4,5x,6i5,5x,4i4)') "Trovato:   ", &
  ksec1(7:9),ksec1(10:14),ksec1(21),ksec1(15:18)
STOP

9994 CONTINUE
WRITE(*,'(3a,3i4)') "Parametro errato in ",TRIM(filein(kfin)), &
  " file,tab,var ",kfin,ksec1(1),ksec1(6)
STOP

END PROGRAM split_qrs

