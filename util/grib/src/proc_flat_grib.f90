PROGRAM proc_flat_grib
!--------------------------------------------------------------------------
! Legge un file con molti grib e lo riscrive; se trova un grib con valori
! costanti, modifica il valore del primo punto.
!
! Note:
! - Programma utile per leggere con GRIBEX i grib costanti scritti da 
!   GRIB-API, minimzzando i danni.
! - Se mi limito ad aggiungere al primo punto un valore molto piccolo, 
!   quando il campo di ingresso e' costante e > 0 GRIBEX non funziona. 
!
!                                         Versione 1.0.0, Enrico 22/06/2012
!--------------------------------------------------------------------------

USE grib_api
IMPLICIT NONE

! Numero di bit nel grib di output (per compatibilita' GRIBEX, mettere >=8)
INTEGER, PARAMETER :: bpv_out = 8

! Varibili locali
REAL, ALLOCATABLE :: field(:)
REAL :: avg
INTEGER :: ifin,ifout,igin,igout,iret,kg
INTEGER :: ni,nj,ic,bpv_in,cnt_const
CHARACTER(LEN=80) :: filein,fileout

!--------------------------------------------------------------------------
! 1) Preliminari

! Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)
IF (TRIM(filein) == "" .OR. TRIM(fileout) == "" .OR. &
  TRIM(filein) == "-h" .OR. TRIM(filein) == "--help") THEN
  WRITE (*,*) "Uso: proc_flat_grib.exe filein fileout"
  STOP
ENDIF

! Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifout,fileout,"w")

!--------------------------------------------------------------------------
! 2) Ciclo sui grib

cnt_const = 0
DO kg = 1,HUGE(0)

! Leggo il prossimo campo
  igin = -1
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998
  CALL grib_clone(igin,igout)

  CALL grib_get(igin,"isConstant",ic)
  CALL grib_get(igin,"bitsPerValue",bpv_in)
  IF (ic == 1) THEN
    cnt_const = cnt_const + 1
    CALL grib_get(igin,"average",avg)
    CALL grib_get(igin,"numberOfPointsAlongAParallel",ni)
    CALL grib_get(igin,"numberOfPointsAlongAMeridian",nj)
    ALLOCATE (field(ni*nj))
    CALL grib_get(igin,"values",field)

!   Lo modifico
    field(1) = 1.e-10 + avg * 1.001   
    CALL grib_set(igout,"bitsPerValue",bpv_out)
    CALL grib_set(igout,"values",field)

  ELSE IF (bpv_in == 0) THEN
    WRITE (*,*) "Warning: trovato un campo non costante scritto con 0 bit"

  ENDIF

! Lo scrivo in output
  CALL grib_write (igout,ifout)
  CALL grib_release(igout)
  CALL grib_release(igin)
  IF (ic == 1) DEALLOCATE(field)

ENDDO

WRITE (*,*) "Elaborazioni completate, elaborati ",kg-1," campi, di cui ", &
  cnt_const," costanti"
STOP

!--------------------------------------------------------------------------
! 3) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib n.ro " ,kg
STOP

END PROGRAM proc_flat_grib
