PROGRAM grib_dist_freq
!--------------------------------------------------------------------------
! Programma che legge un file con molti grib e calcola, per ciascun punto,
! la frazione di dati che appartengono a un certo intervallo di valori.
! Se richesto, scrive un campo per ciascuna ora del giorno.
! 
! Note:
! - I grib devono essere definiti sulla stessa area (sez. 2 coincidente)
! - La sezione 1 e' presa dal primo grib del file
! - Le altre sezioni sono messe a valori standard
! - I punti che risultano mancanti in un qualsiasi GRIB sono messi
!   mancanti; e' ammesso che ci sia un diverso numero di dati mancanti nei
!   diversi grib
!
!                                         Versione 2.0.1, Enrico 13/01/2014
!--------------------------------------------------------------------------

USE date_handler
IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.           ! valore per dati mancanti
!REAL, PARAMETER :: rmis = -HUGE(0.)       ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 100000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: ksec2_sav(1024),ksec1_sav(1024)
INTEGER :: kbuffer(maxdim),klen,kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Altre variabili del programma
REAL, ALLOCATABLE :: val_bin(:)                    ! (nbin+1)
INTEGER (KIND=8), ALLOCATABLE :: count_binh(:,:,:) ! (maxdim, 0:nbin+1,0:23)
REAL, ALLOCATABLE :: fraz_binh(:,:,:)              ! (maxdim, 0:nbin+1,0:23)
REAL, ALLOCATABLE :: fraz_bint(:,:)                ! (maxdim, 0:nbin+1)

TYPE(date) :: data
REAL :: fave
INTEGER (KIND=8) :: count_okh(maxdim,0:23),tot_ok
INTEGER :: nbin,ngribinh(0:23),iuin,iuout,nok,np_sav,kbin,p,hh,ier,karg,kg,kh
CHARACTER (LEN=200) :: filein,fileout,file_root,charg
CHARACTER (LEN=80) :: filepar="grib_dist_freq.inp"
LOGICAL :: fraz_missing,rq_mday                 

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
fraz_missing = .FALSE.
rq_mday = .FALSE.

DO karg = 1,HUGE(0)
  CALL getarg(karg,charg)
  IF (trim(charg) == "-h") THEN
    CALL scrive_help
    STOP
  ELSE IF (TRIM(charg) == "-c") THEN
    CALL crea_esempio_namelist
    STOP
  ELSE IF (TRIM(charg) == "-mday") THEN
    rq_mday = .TRUE.
  ELSE IF (TRIM(charg) == "-miss") THEN
    fraz_missing = .TRUE.
  ELSE IF (TRIM(charg) == "") THEN
    EXIT
  ELSE
    filein = charg
  ENDIF
ENDDO

IF (filein == "") THEN
  CALL scrive_help
  STOP
ENDIF

p = INDEX(filein,".")
IF (p > 0) THEN
  file_root = filein(1:p-1)
ELSE
  file_root = filein
ENDIF

! 1.2 Leggo gli intervalli richiesti (da grib_dist_freq.inp) e alloco le 
!     variabili relative
OPEN (UNIT=30,FILE=filepar,STATUS="OLD",FORM="FORMATTED",ACTION="READ", &
  ERR=9999)
READ (30,*,ERR=9998) nbin
ALLOCATE (val_bin(nbin))
READ (30,*,ERR=9998) val_bin(1:nbin)
CLOSE (30)

ALLOCATE (count_binh(maxdim,0:nbin+1,0:23))
ALLOCATE (fraz_binh(maxdim,0:nbin+1,0:23))
ALLOCATE (fraz_bint(maxdim,0:nbin+1))

! 1.3 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.4 Apro i files (input e log)
CALL PBOPEN (iuin,filein,'R',kret)
IF (kret /= 0) THEN 
  WRITE(*,*) "Errore aprendo ",filein," kret ",kret
  STOP
ENDIF

OPEN (UNIT=96, FILE="grib_dist_freq.log", STATUS="REPLACE", ACTION="WRITE")

!--------------------------------------------------------------------------
! 2) Lettura (ciclo sui grib)

count_binh(:,:,:) = 0
ngribinh(:) = 0

grib: DO kg = 1,HUGE(0)

! 2.1 Leggo il grib e lo decodifico
  CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)
  IF (kret.eq.-1) THEN 
    EXIT grib
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  psec3(2) = rmis                !impongo di mettere i dati mancanti = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
  IF (kg == 1) ksec1_sav(:) = ksec1(:)

! 2.2 Controllo che area e n.ro di punti siano giusti
  IF (kg == 1) THEN
    ksec2_sav(:) = ksec2(:)
    np_sav = ksec4(1) 
  ELSE IF (ANY( ksec2(:) /= ksec2_sav(:) ) .OR. ksec4(1) /= np_sav) THEN
    WRITE (*,*) "Errore, grib ",kg," definito su un'area diversa!"
    STOP
  ENDIF  

  nok = COUNT(field(1:np_sav) /= rmis)
  IF (nok > 0) THEN
    fave = SUM(field(1:np_sav), MASK = field(1:np_sav)/=rmis) / REAL(nok)
  ELSE
    fave = rmis
  ENDIF
  WRITE (96,*) "Letto 1 grib: par, dati ok, media ",ksec1(6),nok,fave

! 2.3 Aggiorno i contatori

! calcolo ora di validita' del grib appena letto
  CALL ksec1_valid(ksec1,data,hh,ier)
  IF (ier /= 0) GOTO 9997
  ngribinh(hh) = ngribinh(hh) + 1

! valori <= 1a soglia
  WHERE (field(:) <= val_bin(1) .AND. field(:) /= rmis)
    count_binh(:,1,hh) = count_binh(:,1,hh) + 1 
  ENDWHERE

! valori tra 2 soglie
  DO kbin = 2,nbin
    WHERE (field(:) > val_bin(kbin-1) .AND. field(:) <= val_bin(kbin) &
           .AND. field(:) /= rmis)
      count_binh(:,kbin,hh) = count_binh(:,kbin,hh) + 1 
    ENDWHERE
  ENDDO

! valori > ultima soglia
  WHERE (field(:) > val_bin(nbin) .AND. field(:) /= rmis)
    count_binh(:,nbin+1,hh) = count_binh(:,nbin+1,hh) + 1 
  ENDWHERE

! dati mancanti
  WHERE (field(:) == rmis)
    count_binh(:,0,hh) = count_binh(:,0,hh) + 1 
  ENDWHERE

ENDDO grib
IF (SUM(ngribinh(:)) /= kg-1) &
  WRITE (*,*) "Warning: n.ro grib inconsistente: ",SUM(ngribinh(:),kg-1)

CALL PBCLOSE (iuin,kret)
CLOSE (96)

!--------------------------------------------------------------------------
! 3) Calcolo i campi "% di dati in ciascun intervallo" e li scrivo

! 3.1 Log: n.ro totale di dati in ciascun intervallo, tot. dati buoni
WRITE (*,*)
WRITE (*,*) "Punti in un grib: ",np_sav
WRITE (*,*) "Grib letti:       ",kg-1
WRITE (*,*) "Soglie richieste: ",nbin 
IF (fraz_missing) THEN
  WRITE (*,*) "Files da scrivere: ",nbin+2
ELSE
  WRITE (*,*) "Files da scrivere: ",nbin+1
ENDIF
WRITE (*,*) "N.ro totale di dati in ciascun intervallo:"
DO kbin = 1,nbin
  WRITE (*,'(a,e12.5,a,i8)') "<= ",val_bin(kbin)," : ", &
    SUM(count_binh(1:np_sav,kbin,0:23))
ENDDO
WRITE (*,'(a,e12.5,a,i8)') " > ",val_bin(nbin)," : ", &
  SUM(count_binh(1:np_sav,nbin+1,0:23))

WRITE (*,*)
WRITE (*,'(a,i8)') "Mancanti:         ", SUM(count_binh(1:np_sav,0,0:23))

count_okh(1:np_sav,0:23) = SUM(count_binh(1:np_sav,1:nbin+1,0:23),DIM=2)
tot_ok = SUM(count_okh(1:np_sav,0:23))
WRITE (*,'(a,f6.2,a)') "Totale dati buoni:  ", &
  100. * REAL(tot_ok) / REAL(np_sav*SUM(ngribinh(:)))," %"

IF (SUM(count_binh(1:np_sav,:,:)) /= np_sav*SUM(ngribinh(:))) THEN 
  WRITE (*,*) "Warning, il n.ro totale di punti non corrisponde:"
  WRITE (*,*) SUM(count_binh(1:np_sav,:,:)),np_sav*SUM(ngribinh(:))
ENDIF

! 3.2 Calcolo le %
IF (fraz_missing) THEN
  DO kh = 0,23
    fraz_binh(1:np_sav,0:nbin+1,kh) = &
      REAL(count_binh(1:np_sav,0:nbin+1,kh)) / &
      REAL(ngribinh(kh))
  ENDDO
  fraz_bint(1:np_sav,0:nbin+1) = &
    REAL(SUM(count_binh(1:np_sav,0:nbin+1,0:23),DIM=3)) / &
    REAL(SUM(ngribinh(0:23)))

ELSE
  DO kbin = 1,nbin+1
    fraz_binh(1:np_sav,kbin,0:23) = &
      REAL(count_binh(1:np_sav,kbin,0:23)) / REAL(count_okh(1:np_sav,0:23))
    fraz_bint(1:np_sav,kbin) = &
      REAL(SUM(count_binh(1:np_sav,kbin,0:23),DIM=2)) / &
      REAL(SUM(count_okh(1:np_sav,0:23),DIM=2))
  ENDDO
ENDIF

! 3.3 Scrivo i grib 
ksec1_sav(5) = 128             ! includo area ma non bitmap
ksec3(:) = 0
psec3(2) = rmis                ! dati mancanti = rmis (ma non ce ne sono!)
ksec4(1) = np_sav              ! numero di punti
ksec4(2) = 24                  ! n.ro di bit: massimo
ksec4(3:) = 0

DO kbin = 0,nbin+1
  IF (.NOT. fraz_missing .AND. kbin == 0) CYCLE

  IF (kbin == 0) THEN
    WRITE(*,'(a,i2,a,e12.5)') "Dati mancanti: ",kbin," fraz. media = ", &
      SUM(fraz_bint(1:np_sav,kbin))/REAL(np_sav)
  ELSE
    WRITE(*,'(a,i2,a,e12.5)') "Intervallo nr: ",kbin," fraz. media = ", &
      SUM(fraz_bint(1:np_sav,kbin))/REAL(np_sav)
  ENDIF

  WRITE (fileout,'(a,i2.2,a)') TRIM(file_root),kbin,".grb"
  CALL PBOPEN (iuout,fileout,'W',kret)

  IF (rq_mday) THEN
    DO kh = 0,23
      field(1:np_sav) = fraz_binh(1:np_sav,kbin,kh)
      ksec1_sav(13) = kh
      CALL GRIBEX (ksec0,ksec1_sav,ksec2_sav,psec2,ksec3,psec3,ksec4, &
        field(1:np_sav),maxdim,kbuffer,maxdim,klen,'C',kret)
      IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
      CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
      IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
    ENDDO

  ELSE
    field(1:np_sav) = fraz_bint(1:np_sav,kbin)
    CALL GRIBEX (ksec0,ksec1_sav,ksec2_sav,psec2,ksec3,psec3,ksec4, &
      field(1:np_sav),maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
    IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

  ENDIF

  CALL PBCLOSE (iuout,kret)

ENDDO

STOP

!--------------------------------------------------------------------------
! 4) Gestione errori I/O

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filepar)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filepar)
STOP

9997 CONTINUE
WRITE (*,*) "Errore ksec1_valid, file ",TRIM(filein)
WRITE (*,*) "ksec1 ",ksec1(1:25)
STOP

END PROGRAM grib_dist_freq

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE ksec1_valid(ksec1,data,hh,ier)
!--------------------------------------------------------------------------
! Data la sezione 1 di un GRIB, ritorna la data e l'ora di validita'
!--------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: ksec1(*)
TYPE(date), INTENT(OUT) :: data
INTEGER, INTENT(OUT) :: hh,ier
!
TYPE(date) :: datar
INTEGER :: hhr,hh_tot,sca

!--------------------------------------------------------------------------

datar%dd = ksec1(12)
datar%mm = ksec1(11)
datar%yy = ksec1(10) + 100 * (ksec1(21) - 1)
hhr = ksec1(13)

! Gestione time range & time unit
SELECT CASE(ksec1(18))
CASE(0,10)
  IF (ksec1(16) == 0) THEN      ! unintialised analysis
    sca=0
  ELSE                          ! forecast
    IF (ksec1(15) == 1) THEN
      sca = ksec1(16)
    ELSE
      GOTO 9999
    ENDIF
  ENDIF

CASE(1)                         ! initialised analysis
  sca = 0

CASE(2:5)                       ! prodotto riferito a un intervallo
  IF (ksec1(15) == 1) THEN
    sca = ksec1(17)
  ELSE
    GOTO 9999
  ENDIF

CASE(13)                        ! analisi LAMA
  IF (ksec1(15) == 1 .AND. ksec1(16) == 0) THEN
    sca = 0
  ELSE
    GOTO 9999
  ENDIF

CASE DEFAULT                    ! time range non gestio
  GOTO 9998

END SELECT

! Calcolo data-ora di validita'
hh_tot = hhr + sca
data = datar + (hh_tot/24)
hh = MOD(hh_tot,24)
ier = 0

RETURN

! Messaggi d'errore
9999 CONTINUE
WRITE (*,*) "Errore ksec1_valid: time unit indicator non gestito ", &
  ksec1(15)
ier = 1
RETURN

9998 CONTINUE
WRITE (*,*) "Errore ksec1_valid: time range indicator non gestito ", &
  ksec1(18)
ier = 2
RETURN

END SUBROUTINE ksec1_valid

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE crea_esempio_namelist
!
! crea  un file grib_dist_freq.inp di esempio
!
OPEN (UNIT=60, FILE="grib_dist_freq.inp", STATUS="REPLACE", &
  FORM="FORMATTED")

WRITE (60,'(a)') "6                      ! numero bin"
WRITE (60,'(a)') "0.01,0.1,0.,1.,10.100. ! valore bin (<=)"

CLOSE(60)
RETURN

END SUBROUTINE crea_esempio_namelist

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!                123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,'(a)') "Uso: grib_dist_freq.exe [-miss] [-mday] [-c] [-h] filein" 
WRITE (*,'(a)') "     -c: crea file grib_dist_freq.inp di esempio"
WRITE (*,'(a)') "     -h: visualizza questo help"
WRITE (*,'(a)') "     -mday: scrive le % relative a cisacuna ora (24 grib in ogni output file)"
WRITE (*,'(a)') "     -miss: nel calcolo delle % considera i dati mancanti come una classe"
WRITE (*,'(a)') "            (di default, le % sono relative ai soli dati validi)" 
WRITE (*,'(a)') "" 
WRITE (*,'(a)') "Legge tutti i grib di filein e calcola i campi con la frazione di dati"
WRITE (*,'(a)') "  che appartengono a un certo intervallo di valori. Gli intervalli sono "
WRITE (*,'(a)') "  definiti da un insieme di soglie specificate nel .inp"
WRITE (*,'(a)') "Se le soglie sono N, il programma scrive N+2 files:"
WRITE (*,'(a)') " - NOME_00.grb : frequenza di dati mancanti"
WRITE (*,'(a)') " - NOME_01.grb : frequenza di dati <= soglia 1"
WRITE (*,'(a)') " - NOME_xx.grb : frequenza di dati <= soglia xx e > soglia xx-1"
WRITE (*,'(a)') " - NOME_(N+1).grb : frequenza di dati > ultima soglia"
WRITE (*,'(a)') ""
!                123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
