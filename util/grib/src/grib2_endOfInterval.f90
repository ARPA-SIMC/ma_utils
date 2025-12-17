PROGRAM grib2_endOfInterval
!--------------------------------------------------------------------------
! Legge un file con molti grib, riscrive i grib2 non istantanei con reftime 
! o forecast time relativo alla fine dell'intetvallo di elaborazione.
!
! QUESTO PROGRAMMA SCRIVE DEI GRIB SCORRETTI, CHE DEVONO ESSERE ELABORATI 
! SOLO CON PROGRAMMI SPECIFICI, E SOLO COME ULTIMA ELABORAZIONE.
!
! Vedi SUBR. write_help per maggiori dettagli
!
!                                         Versione 1.0.0, Enrico 03/12/2025
!--------------------------------------------------------------------------

USE datetime_class
USE grib_api
USE  grib2_utilities
IMPLICIT NONE

TYPE (datetime) :: rt_out
INTEGER :: ifin=0,ifout=0,igin=0,igout=0
INTEGER :: en,pdtn,lotr,ft_in,ft_out,yy,mon,dd,hh,min
INTEGER :: kg,idp,cnt_g1,cnt_ist,cnt_mod,iret,kp
CHARACTER(LEN=200) :: filein,fileout,chdum
CHARACTER(LEN=3) :: opt

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1

  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      fileout = chdum
    CASE (3)
      opt = chdum
    CASE DEFAULT
      CALL write_help
      STOP 1
    END SELECT
  ENDIF
ENDDO

IF (opt /= "ana" .AND. opt /= "for") THEN
  CALL write_help
  STOP 1 
ENDIF

! Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifout,fileout,"w")

!--------------------------------------------------------------------------
! 2) Esecuzione (ciclo sui grib)

cnt_g1 = 0
cnt_ist = 0
cnt_mod = 0

grib: DO kg = 1,HUGE(0)

! 2.1 Leggo il prossimo campo
  igin = -1
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998

! 2.2 Skip campi grib1 e istantanei
  CALL grib_get(igin,"editionNumber",en)
  IF ( en /= 2 ) THEN
    cnt_g1 = cnt_g1 + 1
    CALL grib_write (igin,ifout)
    CALL grib_release(igin)
    CYCLE grib
  ENDIF

  CALL grib_get(igin,"productDefinitionTemplateNumber",pdtn)
  IF ( pdtn == 0 .OR. pdtn == 1 .OR. pdtn == 40 ) THEN
    cnt_ist = cnt_ist + 1
    CALL grib_write (igin,ifout)
    CALL grib_release(igin)
    CYCLE grib
  ENDIF

  IF (en /= 2 .OR. (pdtn /= 8 .AND. pdtn /= 11)) GOTO 9997

! 2.3 Modifico i grib2 non istantanei
  cnt_mod = cnt_mod + 1
  CALL grib_clone(igin,igout)
  CALL grib_get(igin,"forecastTime",ft_in)

  IF (ft_in == 0 .AND. opt == "ana") THEN     ! cambio reftime
    CALL get_grib_time(igin, vtime2=rt_out, iret=iret)
    IF (iret /= 0) GOTO 9996 
    CALL getval(rt_out, YEAR=yy, MONTH=mon, DAY=dd, HOUR=hh, MINUTE=min)
    CALL grib_set(igout,"year",yy)
    CALL grib_set(igout,"month",mon)
    CALL grib_set(igout,"day",dd)
    CALL grib_set(igout,"hour",hh)
    CALL grib_set(igout,"minute",min)
 
  ELSE                                        ! cambio forecast time
    CALL grib_get(igin,"lengthOfTimeRange",lotr)
    ft_out = ft_in + lotr
    CALL grib_set (igout,"forecastTime",ft_out)

  ENDIF

  CALL grib_write(igout,ifout)
  CALL grib_release(igin)
  CALL grib_release(igout)

ENDDO grib

!--------------------------------------------------------------------------
! 3) Conclusione

WRITE (*,*) "Grib invariati perche' edition 1:  ",cnt_g1
WRITE (*,*) "Grib invariati perche' istantanei: ",cnt_ist
WRITE (*,*) "Grib modificati:                   ",cnt_mod

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
WRITE (*,*) "Errore ",TRIM(filein)," grib n.ro " ,kg
WRITE (*,*) "  en o pdtn non gestiti: ",en,pdtn
STOP 3

9996 CONTINUE
WRITE (*,*) "Errore ",TRIM(filein)," grib n.ro " ,kg
WRITE (*,*) " Errore nella funzione get_grib_time "
STOP 3

END PROGRAM  grib2_endOfInterval

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: grib2_endOfInterval.exe [-h] filein fileout ana/for"
WRITE (*,*) "Legge un file con molti grib (edizione 2) e lo riscrive spostando il "
WRITE (*,*) "  reference time (analisi) o il forecast time (previsioni) dei campi "
WRITE (*,*) "  non istantanei alla fine dell'intervallo di elaborazione"
WRITE (*,*) "Il timerange non viene modificato"
WRITE (*,*) "I campi istantanei e i campi grib1 sono riscritti senza modifiche"
WRITE (*,*) ""
WRITE (*,*) "L'opzione ana/for codifica i dati non istantanei con forecastTime = 0"
WRITE (*,*) "  ana: i dati sono trattati come analisi => cambia il reference time"
WRITE (*,*) "  for: i dati sono trattati come forecast => cambia il forecast time"
WRITE (*,*) ""
WRITE (*,*) "A differenza dei grib1, nei grib2, il reference time (per le analisi) e il "
WRITE (*,*) "forecast time (per le previsioni) si riferiscono all'inizio dell'intervallo"
WRITE (*,*) "di elaborazione"
WRITE (*,*) ""
WRITE (*,*) "QUESTO PROGRAMMA SCRIVE QUINDI DEI GRIB SCORRETTI, CHE DEVONO ESSERE ELABORATI"
WRITE (*,*) "       SOLO CON PROGRAMMI SPECIFICI, E SOLO COME ULTIMA ELABORAZIONE."
WRITE (*,*) ""
WRITE (*,*) " Per esempio:"
WRITE (*,*) "- preparazioone dell'input Calmet a partra da dati Icon (3ddat.f90)"
WRITE (*,*) "- estrazone di serie temporai su punto, per confronto con dati osservati (seriet.f90)"
WRITE (*,*)
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
