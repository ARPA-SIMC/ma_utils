PROGRAM grib_forc2ana
!--------------------------------------------------------------------------
! Legge un file con molti grib (edizione 2) lo riscrive come analisi.
!
! Nota 25/04/2024:
! nei campi Kenda istantanei, productDefinitionTemplateNumber=1 (individual ensemble forecast; table 4.0).
! Se la forzo a 0 (analisi o forecast), le chiavi relative ai livelli delle nubi vengono corrotte.
! Potrebbe essere un bug della versione corrente di grib-api, ma nel dubbio riduco al minimo le
! modifiche a questa chiave, anche se l'archivio iama risualta meno omogeneo...
!
!                                         Versione 1.0.0, Enrico 25/04/2024
!--------------------------------------------------------------------------

USE datetime_class
USE grib_api
IMPLICIT NONE

TYPE (datetime) :: rtime_in,rtime_out
INTEGER :: ifin=0,ifout=0,igin=0,igout=0
INTEGER :: kg,idp,kp,iret,yy,mon,dd,hh,min,en,pdtn,iouotr,iouftr,ft,lotr
INTEGER :: cnt_ist,cnt_proc,fth,pdtn_out,lotrh
CHARACTER(LEN=200) :: filein,fileout,chdum
CHARACTER(LEN=4) :: tr_proc
LOGICAL :: force_tmpl

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
force_tmpl = .FALSE.
tr_proc = "proc"
idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1
  ELSE IF (TRIM(chdum) == "-isti") THEN
    tr_proc = "isti"
  ELSE IF (TRIM(chdum) == "-istf") THEN
    tr_proc = "istf"
  ELSE IF (TRIM(chdum) == "-ftmp") THEN
    force_tmpl = .TRUE.

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

cnt_ist=0
cnt_proc=0

grib: DO kg = 1,HUGE(0)

! 2.1 Leggo il prossimo campo
  igin = -1
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998

! 2.2 Controlli  
  CALL grib_get(igin,"editionNumber",en)
  IF ( en /= 2 ) THEN
    WRITE (*,*) "Trovato campo grib1, scrivo invariato"
    CALL grib_write (igin,ifout)
    CALL grib_release(igin)
    CYCLE grib
  ENDIF
    
! 2.3 Trovo reference time e forecast time
  CALL grib_get(igin,"year",yy)
  CALL grib_get(igin,"month",mon)                             
  CALL grib_get(igin,"day",dd)                               
  CALL grib_get(igin,"hour",hh)                              
  CALL grib_get(igin,"minute",min)                              
  rtime_in = datetime_new(YEAR=yy, MONTH=mon, DAY=dd, HOUR=hh, MINUTE=min)

  CALL grib_get(igin,"productDefinitionTemplateNumber",pdtn)
  CALL grib_get(igin,"indicatorOfUnitOfTimeRange",iouotr)
  CALL grib_get(igin,"forecastTime",ft)
  IF (iouotr == 0) THEN       ! minutes
     IF (MOD(ft,60) /= 0) GOTO 9995
     fth = ft / 60.
  ELSE IF (iouotr == 1) THEN  ! hours
    fth = ft
  ELSE
    GOTO 9997
  ENDIF   
  
! 2.4 Duplico il campo, assegno le chiavi che sono uguali per tutte le analisi
  CALL grib_clone(igin,igout)
  CALL grib_set(igout,"forecastTime",0)
  CALL grib_set(igout,"significanceOfReferenceTime",0)         ! table 1.2
  CALL grib_set(igout,"typeOfProcessedData",0)                 ! table 1.4
  CALL grib_set(igout,"typeOfGeneratingProcess",0)             ! table 4.3
  
! 2.5 assegno le chiavi specifiche del tipo di dato

! 2.5.1 Campo istantaneo
  IF (pdtn == 0 .OR. pdtn == 1 .OR. pdtn == 40 .OR. pdtn == 41) THEN  
    cnt_ist = cnt_ist + 1

    rtime_out = rtime_in + timedelta_new(hour=fth)
    CALL getval(rtime_out, YEAR=yy, MONTH=mon, DAY=dd, HOUR=hh, MINUTE=min)
    CALL grib_set(igout,"year",yy)
    CALL grib_set(igout,"month",mon)
    CALL grib_set(igout,"day",dd)
    CALL grib_set(igout,"hour",hh)
    CALL grib_set(igout,"minute",min)
    IF (force_tmpl) CALL grib_set(igout,"productDefinitionTemplateNumber",0) ! table 4.0
    
  ELSE IF (pdtn == 8 .OR. pdtn == 11 .OR. pdtn == 42 .OR. pdtn == 43) THEN  
    cnt_proc = cnt_proc + 1

! 2.5.2 Campo elaborato: trange="elaborato", reftime="inizio intervallo", forecastTime=0
    IF (tr_proc == "proc") THEN
      rtime_out = rtime_in + timedelta_new(hour=fth)
      CALL getval(rtime_out, YEAR=yy, MONTH=mon, DAY=dd, HOUR=hh, MINUTE=min)
      CALL grib_set(igout,"year",yy)
      CALL grib_set(igout,"month",mon)
      CALL grib_set(igout,"day",dd)
      CALL grib_set(igout,"hour",hh)
      CALL grib_set(igout,"minute",min)

      IF (force_tmpl) CALL grib_set(igout,"productDefinitionTemplateNumber",8) ! table 4.0
      CALL grib_set(igout,"typeOfTimeIncrement",1)             ! table 4.11

! 2.5.3 Campo elaborato: trange="istantaneo", reftime="inizio intervallo", forecastTime=0, 
    ELSE IF (tr_proc == "isti") THEN
      rtime_out = rtime_in + timedelta_new(hour=fth)
      CALL getval(rtime_out, YEAR=yy, MONTH=mon, DAY=dd, HOUR=hh, MINUTE=min)
      CALL grib_set(igout,"year",yy)
      CALL grib_set(igout,"month",mon)
      CALL grib_set(igout,"day",dd)
      CALL grib_set(igout,"hour",hh)
      CALL grib_set(igout,"minute",min)

      CALL grib_set(igout,"typeOfStatisticalProcessing",255)

      IF (force_tmpl) THEN
        pdtn_out = 0
      ELSE
        IF (pdtn == 8) pdtn_out = 0
        IF (pdtn == 11) pdtn_out = 1
        IF (pdtn == 42) pdtn_out = 40
        IF (pdtn == 43) pdtn_out = 41
      ENDIF
      CALL grib_set(igout,"productDefinitionTemplateNumber",pdtn_out)
      
! 2.5.4 Campo elaborato: trange=istantaneo, reftime="fine intervallo", forecastTime=0,
    ELSE IF (tr_proc == "istf") THEN
      CALL grib_get(igin,"lengthOfTimeRange",lotr)
      CALL grib_get(igin,"indicatorOfUnitForTimeRange",iouftr)
      IF (iouftr == 0) THEN       ! minutes
        IF (MOD(lotr,60) /= 0) GOTO 9995
        lotrh = lotr / 60.
      ELSE IF (iouftr == 1) THEN  ! hours
        lotrh = lotr
      ELSE
        GOTO 9997
      ENDIF   
      rtime_out = rtime_in + timedelta_new(hour=fth) + timedelta_new(hour=lotrh)
      CALL getval(rtime_out, YEAR=yy, MONTH=mon, DAY=dd, HOUR=hh, MINUTE=min)
  
      CALL grib_set(igout,"year",yy)
      CALL grib_set(igout,"month",mon)
      CALL grib_set(igout,"day",dd)
      CALL grib_set(igout,"hour",hh)
      CALL grib_set(igout,"minute",min)

      CALL grib_set(igout,"typeOfStatisticalProcessing",255)

      IF (force_tmpl) THEN
        pdtn_out = 0
      ELSE
        IF (pdtn == 8) pdtn_out = 0
        IF (pdtn == 11) pdtn_out = 1
        IF (pdtn == 42) pdtn_out = 40
        IF (pdtn == 43) pdtn_out = 41
      ENDIF
      CALL grib_set(igout,"productDefinitionTemplateNumber",pdtn_out)

    ENDIF

  ELSE
    GOTO 9996

  ENDIF

  CALL grib_write (igout,ifout)
  CALL grib_release (igin)
  CALL grib_release (igout)
  
ENDDO grib

!--------------------------------------------------------------------------
! 3) Conclusione

WRITE (*,*) "Elaborazioni completate, letti e riscritti ",kg-1," campi"
WRITE (*,*) "GRIB2 istantanei: ",cnt_ist
WRITE (*,*) "GRIB2 elaborati: ",cnt_proc

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
WRITE (*,*) "iouotr non gestito ",iouotr
STOP 3

9996 CONTINUE
WRITE (*,*) "pdtn non gestito ",pdtn
STOP 3

9995 CONTINUE
WRITE (*,*) "ft in minuti, non multiplo di ore ",ft
STOP 3

END PROGRAM grib_forc2ana

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: grib2_forc2ana.exe [-h] filein fileout [-isti/-istf] [-ftmp]"
WRITE (*,*) "Legge un file con molti grib (edizione 2) e lo riscrive come analisi"
WRITE (*,*) "I campi non istantnei sono scritti come:"
WRITE (*,*) "  [default]: analisi elaborate, con reftime=inizio intervallo di elaborazione"
WRITE (*,*) "  [-isti]: analisi istantanee, con reftime=inizio intervallo di elaborazione"
WRITE (*,*) "  [-istf]: analisi istantanee, con reftime=fine intervallo di elaborazione"
WRITE (*,*) "  [-ftmp]: forza la chiave grib ""productDefinitionTemplateNumber"" a 0 (ist)"
WRITE (*,*) "           oppure 8 (proc). Rende i dati di output piu' omogenei, ma in rari"
WRITE (*,*) "           casi corrompe altre chbiavi dei grib (es. livelli delle nubi su"
WRITE (*,*) "           strati, livelli nel suolo). Usare con cautela"
WRITE (*,*)
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
