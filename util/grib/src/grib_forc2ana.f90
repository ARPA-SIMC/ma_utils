PROGRAM grib_forc2ana
!--------------------------------------------------------------------------
! Legge un file con molti grib e riscrive tutti i campi come analisi
! istantanee, relative alla data-ora di validita' o di emissione.
! Gestisce GRIB1 e GRIB2.
! Per i campi non istantanei, usa le convenzioni standard WMO:
!   grib1: reference time = fine dell'intervallo di elaborazione
!   grib2: reference time = inizio dell'intervallo di elaborazione
!
! ToDo: opzione -force per scrivere comunque qualcosa nei casi non gestiti
!
!                                         Versione 2.0.0, Enrico 09/12/2024
!--------------------------------------------------------------------------

USE grib2_utilities
USE datetime_class
USE grib_api
IMPLICIT NONE

TYPE (datetime) :: rtime_in,vtime1_in,vtime2_in,rtime_out
INTEGER :: ifin=0,ifout=0,igin=0,igout=0
INTEGER :: kg,idp,kp,iret,yy,mon,dd,hh,min,en,yoc,cortod,scad(4),pdtn_in
CHARACTER(LEN=200) :: filein,fileout,chdum
CHARACTER(LEN=1) :: out_times

!debug INTEGER :: sortm,topd,pdtn,togp,ft,tosp,toti,lotr

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
  ELSE IF (TRIM(chdum) == "-proc") THEN
    out_times = "P"

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
  CALL grib_get(igin,"editionNumber",en)

! 2.2 Calcolo il reference time in output
  CALL get_grib_time(igin, RTIME=rtime_in, VTIME1=vtime1_in, &
                     VTIME2=vtime2_in, IRET=iret)
  IF (iret /= 0) GOTO 9997

  IF (out_times == "V") THEN
    rtime_out = vtime2_in
  ELSE IF (out_times == "R") THEN
    rtime_out = rtime_in
  ELSE IF (out_times == "P" .AND. en == 1) THEN
    rtime_out = vtime2_in
  ELSE IF (out_times == "P" .AND. en == 2) THEN
    rtime_out = vtime1_in
  ENDIF
  CALL getval(rtime_out, YEAR=yy, MONTH=mon, DAY=dd, HOUR=hh, MINUTE=min)

! 2.3 Costruisco il grib modificato
  CALL grib_clone(igin,igout)

! 2.4 Assegno il reference time
  IF (en == 1) THEN
    yoc = MOD((yy-1),100) + 1
    cortod = (yy-1)/100 + 1
    CALL grib_set(igout,"yearOfCentury",yoc)
    CALL grib_set(igout,"month",mon)
    CALL grib_set(igout,"day",dd)
    CALL grib_set(igout,"hour",hh)
    CALL grib_set(igout,"minute",min)
    CALL grib_set(igout,"centuryOfReferenceTimeOfData",cortod)

  ELSE IF (en == 2) THEN
    CALL grib_set(igout,"year",yy)
    CALL grib_set(igout,"month",mon)
    CALL grib_set(igout,"day",dd)
    CALL grib_set(igout,"hour",hh)
    CALL grib_set(igout,"minute",min)

  ENDIF

! 2.5 Assegno il timerange
  IF (out_times == "V" .OR. out_times == "R") THEN   ! Sempre istantaneo
    IF (en == 1) THEN
      CALL grib_set(igout,"unitOfTimeRange",1)
      CALL grib_set(igout,"P1",0)
      CALL grib_set(igout,"P2",0)
      CALL grib_set(igout,"timeRangeIndicator",0)
  
    ELSE IF (en == 2) THEN
! Patch, 10/12/2024
! SE il grib di input non e' istantaneo, prima di convertirlo a istantaneo bisogna mettere
! mancante l'elaborazione statistica, altrimenti grib_api da' errore.
! La funzione "grib_is_defined" secondo documentazione dovrebbe ritornare 0 (=GRIB_SUCCESS)
! se la chiave e' definita, invece apparentemente fa il contrario (ritorna 0 se la chiave e' mancante)
      CALL grib_is_defined(igout,"typeOfStatisticalProcessing",iret)
      IF (iret /= 0) CALL grib_set(igout,"typeOfStatisticalProcessing",255)
!     IF (iret == GRIB_SUCCESS) CALL grib_set(igout,"typeOfStatisticalProcessing",255)

      CALL grib_set(igout,"productDefinitionTemplateNumber",0)
      CALL grib_set(igout,"significanceOfReferenceTime",0)
      CALL grib_set(igout,"typeOfProcessedData",0)
      CALL grib_set(igout,"typeOfGeneratingProcess",0)
      CALL grib_set(igout,"forecastTime",0)
  
    ENDIF

  ELSE IF (out_times == "P") THEN
    CALL get_grib1_header(igin, SCAD=scad, IRET=iret)
    IF (iret /= 0) GOTO 9997
    
    IF (en == 1) THEN
      SELECT CASE (scad(4))
      CASE(0)    
        CALL grib_set(igout,"unitOfTimeRange",1)
        CALL grib_set(igout,"P1",0)
        CALL grib_set(igout,"P2",0)
        CALL grib_set(igout,"timeRangeIndicator",0)

     CASE(3,4,6,7,12,14,15,16,17)    
        CALL grib_set(igout,"unitOfTimeRange",1)
        CALL grib_set(igout,"P1",0)
        CALL grib_set(igout,"P2",scad(3)-scad(2))
        CALL grib_set(igout,"timeRangeIndicator",13)

     CASE DEFAULT
        GOTO 9996

     END SELECT
        
    ELSE IF (en == 2) THEN
      IF (scad(4) == 0) THEN    ! Campo istantaneo
        CALL grib_set(igout,"productDefinitionTemplateNumber",0)
        CALL grib_set(igout,"significanceOfReferenceTime",0)
        CALL grib_set(igout,"typeOfProcessedData",0)
        CALL grib_set(igout,"typeOfGeneratingProcess",0)
        CALL grib_set(igout,"forecastTime",0)

      ELSE                      ! Campo elaborato
        CALL grib_set(igout,"productDefinitionTemplateNumber",8)
        CALL grib_set(igout,"significanceOfReferenceTime",0)
        CALL grib_set(igout,"typeOfProcessedData",0)
        CALL grib_set(igout,"typeOfGeneratingProcess",0)
        CALL grib_set(igout,"forecastTime",0)
        CALL grib_set(igout,"typeOfTimeIncrement",1)
        CALL grib_set(igout,"lengthOfTimeRange",scad(3)-scad(2))

        SELECT CASE (scad(4))
        CASE(3,14)              ! media
          CALL grib_set(igout,"typeOfStatisticalProcessing",0)
        CASE(4,15)              ! cumulata
          CALL grib_set(igout,"typeOfStatisticalProcessing",1)
        CASE(6,16)              ! massimo
          CALL grib_set(igout,"typeOfStatisticalProcessing",2)
       CASE DEFAULT
          GOTO 9996
       END SELECT

     ENDIF
        
    ENDIF

  ENDIF

! debug
!  CALL grib_get(igout,"significanceOfReferenceTime",sortm)
!  CALL grib_get(igout,"typeOfProcessedData",topd)
!  CALL grib_get(igout,"productDefinitionTemplateNumber",pdtn)
!  CALL grib_get(igout,"typeOfGeneratingProcess",togp)
!  CALL grib_get(igout,"forecastTime",ft)
!  CALL grib_get(igout,"typeOfStatisticalProcessing",tosp)
!  CALL grib_get(igout,"typeOfTimeIncrement",toti)
!  CALL grib_get(igout,"lengthOfTimeRange",lotr)
!  print *,"sort,topd,pdtn,togp,ft,tosp,toti,lotr ",sortm,topd,pdtn,togp,ft,tosp,toti,lotr
  
! 2.6 Scrivo su fileout
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

9996 CONTINUE
WRITE (*,*) "Errore converisone timerange ",TRIM(filein)," grib n.ro " ,kg

END PROGRAM grib_forc2ana

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: grib_forc2ana.exe [-h] filein fileout [-rtime/-proc]"
WRITE (*,*) "Legge un file con molti grib e riscrive tutti i campi come analisi, "
WRITE (*,*) "modificando reference time e timerange. Gestisce GRIB1 e GRIB2"
WRITE (*,*) ""
WRITE (*,*) "Default: scrive sempre campi istantanei, data uguale al verification time (fine intervallo)"
WRITE (*,*) "-rtime:  scrive sempre campi istantanei, data uguale al reference time"
WRITE (*,*) "-proc:   i campi istantanei hanno data uguale al verification time."
WRITE (*,*) "         per i campi elaborati, scrive un'analisi elaborata secondo le convenzioni WMO"
WRITE (*,*) "         grib1: retfime = fine intervallo; grib2: reftime = inizio intervallo"
WRITE (*,*)
WRITE (*,*) "#####    VEDI ANCHE: grib2_forc2ana.exe     #####"
WRITE (*,*)
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
