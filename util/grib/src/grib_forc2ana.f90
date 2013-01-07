PROGRAM grib_forc2ana
!--------------------------------------------------------------------------
! Legge un file con molti grib e lo riscrive, trasformado tutte le 
! previsioni in analisi relative alla data-ora di emissione. 
! I parametri relativi a un intervallo (es .precipitazione) vengono 
! attribuiti all'istante finale dell'intervallo.
!
! Serve per:
! - importare in grads
! - confrontare i campi di prima scelta per Calmet con Calmet stesso
!
! Derivato da rw_grib.f90
!                                           Versione 1.0, Enrico 04/06/2003
!--------------------------------------------------------------------------

USE date_handler

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.           ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 200000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim), klen, kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Altre variabili del programma
TYPE(date) :: datar, dataw
INTEGER :: sca,hhr,hhw,hh_tot
INTEGER :: iuin,iuout,cnt_tot,cnt_cng,cnt_err
CHARACTER (LEN=80) :: filein,fileout
LOGICAL :: modif

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)

IF (filein == "" .OR. fileout == "" .OR. TRIM(filein) == "-h") THEN
  WRITE (*,*) "Uso: grib_forc2ana.exe [-h] filein fileout" 
  STOP
ENDIF

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
CALL PBOPEN (iuin,filein,'R',kret)
CALL PBOPEN (iuout,fileout,'W',kret)

!--------------------------------------------------------------------------
! 2) Leggo / Scrivo (ciclo sui grib)

cnt_tot = 0 
cnt_cng = 0 
cnt_err = 0 

DO 

! 2.1 Leggo il grib 
  CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)
  IF (kret == -1) THEN
    EXIT
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! 2.2 Trasformo data-scad da previsione ad analisi
  datar = date(ksec1(12),ksec1(11),ksec1(10))
  hhr = ksec1(13)


! Gestione time range & time unit
  SELECT CASE(ksec1(18))
  CASE(0)
    IF (ksec1(16) == 0) THEN      ! unintialised analysis
      modif = .FALSE.
    ELSE                          ! forecast
      IF (ksec1(15) == 1) THEN
        modif = .TRUE.
        sca = ksec1(16)
      ELSE
        modif = .FALSE.
        WRITE (*,'(a,i3,a)') "Time unit indicator ",ksec1(15), &
          " non gestito, non modifico il grib"
        cnt_err = cnt_err + 1
      ENDIF
    ENDIF

  CASE(1)                         ! initialised analysis
    modif = .TRUE.
    sca = 0

  CASE(2:5)                       ! prodotto riferito a un intervallo
    IF (ksec1(15) == 1) THEN
      modif = .TRUE.
      sca = ksec1(17)
    ELSE
      modif = .FALSE.
      WRITE (*,'(a,i3,a)') "Time unit indicator ",ksec1(15), &
       " non gestito, non modifico il grib"
      cnt_err = cnt_err + 1
    ENDIF

  CASE DEFAULT                    ! time range non gestio
    modif = .FALSE.
    WRITE (*,'(a,i3,a)') "Time range indicator ",ksec1(18), &
      " non gestito, non modifico il grib"
    cnt_err = cnt_err + 1
  END SELECT

! Se necessario, modifico data e scadenza
  IF (modif) THEN
    hh_tot = hhr + sca
    dataw = datar + (hh_tot/24)
    hhw = MOD(hh_tot,24)

    ksec1(10) = dataw%yy
    ksec1(11) = dataw%mm
    ksec1(12) = dataw%dd
    ksec1(13) = hhw

    ksec1(15) = 1
    ksec1(16) = 0
    ksec1(17) = 0
    ksec1(18) = 0

    cnt_cng = cnt_cng + 1
  ENDIF

! 2.3 Lo riscrivo
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
  cnt_tot = cnt_tot + 1

ENDDO
!--------------------------------------------------------------------------
! 3) Chiudo i files e termino

CALL PBCLOSE (iuin,kret)
CALL PBCLOSE (iuout,kret)

WRITE (*,*) "Grib scritti        ",cnt_tot
WRITE (*,*) "Grib modificati:    ",cnt_cng
WRITE (*,*) "Grib non gestibili: ",cnt_err

STOP

END PROGRAM grib_forc2ana
