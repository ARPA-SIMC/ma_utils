PROGRAM grib_forc2ana
!--------------------------------------------------------------------------
! Legge un file con molti grib e riscrive tutti i campi come analisi
! istantanee, relative alla data-ora di validita' o di emissione.
! Gestisce GRIB1 e GRIB2.
! I parametri relativi a un intervallo (es .precipitazione) vengono 
! attribuiti all'istante finale dell'intervallo.
!
!                                         Versione 1.0.0, Enrico 16/01/2014
!--------------------------------------------------------------------------

USE grib2_utilities
USE datetime_class
USE grib_api
IMPLICIT NONE

TYPE (datetime) :: rtime_in,vtime_in,rtime_out
INTEGER :: ifin=0,ifout=0,igin=0,igout=0
INTEGER :: kg,idp,kp,iret,yy,mon,dd,hh,min,en,yoc,cortod
CHARACTER(LEN=200) :: filein,fileout,chdum
CHARACTER(LEN=1) :: out_times

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
out_times = "V"
idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1
  ELSE IF (TRIM(chdum) == "-rtime") THEN
    out_times = "R"

  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      fileout = chdum
    CASE DEFAULT
      CALL write_help
      STOP 1
    END SELECT
  ENDIF
ENDDO

! Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifout,fileout,"w")

!--------------------------------------------------------------------------
! 2) Esecuzione (ciclo sui grib)

DO kg = 1,HUGE(0)

! 2.1 Leggo il prossimo campo
  igin = -1
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998

! 2.2 Calcolo il nuovo reference time
  CALL grib_get(igin,"editionNumber",en)
  CALL get_grib_time(igin, RTIME=rtime_in, VTIME=vtime_in, IRET=iret)
  IF (iret /= 0) GOTO 9997

  IF (out_times == "V") THEN
    rtime_out = vtime_in
  ELSE IF (out_times == "R") THEN
    rtime_out = rtime_in
  ENDIF
  CALL getval(rtime_out, YEAR=yy, MONTH=mon, DAY=dd, HOUR=hh, MINUTE=min)

! 2.3 Costruisco il grib modificato
  CALL grib_clone(igin,igout)

  IF (en == 1) THEN
    yoc = MOD((yy-1),100) + 1
    cortod = (yy-1)/100 + 1
    CALL grib_set(igout,"yearOfCentury",yoc)
    CALL grib_set(igout,"month",mon)
    CALL grib_set(igout,"day",dd)
    CALL grib_set(igout,"hour",hh)
    CALL grib_set(igout,"minute",min)
    CALL grib_set(igout,"centuryOfReferenceTimeOfData",cortod)
    CALL grib_set(igout,"unitOfTimeRange",1)
    CALL grib_set(igout,"P1",0)
    CALL grib_set(igout,"P2",0)
    CALL grib_set(igout,"timeRangeIndicator",0)

  ELSE IF (en == 2) THEN
    CALL grib_set(igout,"year",yy)
    CALL grib_set(igout,"month",mon)
    CALL grib_set(igout,"day",dd)
    CALL grib_set(igout,"hour",hh)
    CALL grib_set(igout,"minute",min)
    CALL grib_set(igout,"significanceOfReferenceTime",0)
    CALL grib_set(igout,"typeOfProcessedData",0)
    CALL grib_set(igout,"productDefinitionTemplateNumber",0)
    CALL grib_set(igout,"typeOfGeneratingProcess",0)
    CALL grib_set(igout,"forecastTime",0)

  ENDIF

! 2.4 Lo scrivo su fileout
  CALL grib_write (igout,ifout)
  CALL grib_release(igin)
  CALL grib_release(igout)

ENDDO

!--------------------------------------------------------------------------
! 3) Conclusione

WRITE (*,*) "Elaborazioni completate, letti e riscritti ",kg-1," campi"

CALL grib_close_file(ifin)
CALL grib_close_file(ifout)
STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 2

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib n.ro " ,kg
STOP 2

9997 CONTINUE
WRITE (*,*) "Data o scadenza non gestite in input: ",TRIM(filein)," grib n.ro " ,kg
STOP 2

END PROGRAM grib_forc2ana

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: grib_forc2ana.exe [-h] filein fileout [-rtime]"
WRITE (*,*) "Legge un file con molti grib e riscrive tutti i campi come analisi"
WRITE (*,*) "istantanee, relative alla data-ora di validita' o di emissione."
WRITE (*,*)
WRITE (*,*) "-h    : visualizza questo help"
WRITE (*,*) "filein, fileout: in formato GRIB1 o GRIB2"
WRITE (*,*) "-rtime: scrive analisi con data uguale al reference time (default: "
WRITE (*,*) "        usa il verification time)"
WRITE (*,*) ""
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
