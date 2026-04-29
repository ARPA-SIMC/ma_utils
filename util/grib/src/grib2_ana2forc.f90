PROGRAM grib2_ana2forc
!--------------------------------------------------------------------------
! Legge una serie di analisi orarie di 1 par/liv, ordinate per reftime
! Le riscrive come previsioni previsioni, con reftime uguale all'istante iniziale
! Programma pensato per riscrivere le analisi Iama come se fossero forecast:
! accetta in input solo queste, con contolli rigidi (se utile, potrebbe essere
! generalizzato)
! Manca inoltre la gestione dei dati mancanti in input/output
!
! Gestione dei campi istantanei:
!
! Analisi (ft=0)         Forecast (reft=ist.iniziale)
! campo  reft   vt       campo  ft     vt
! 1      00h    00       1      +00h   +00
! 2      01h    01       2      +01h   +01
! 3      02h    02       3      +02h   +02
! ...                
! 24     23h    23       24     +23h   +23
! 25     0h d+1 0h d+1   25     +24h   +24
!
! Gestione dei campi non istantanei:
!
! Analisi (ft=0,lotr=1)  Forecast (ft=0, reft=ist.iniziale)
! campo  reft   vt       campo  vt     lotr
! 1      00h    00-01    1      nil    0
!                        2      +00-01 1
! 2      01h    01-02    3      +00-02 2
! 3      02h    02-03    4      +00-03 3
! ...
! 24     23h    23-24    25     +00-24 24
! 25     0h d+1 24-01    26     +00-25 25 [campo da filtrare!]
!
! All'istante iniziale (reftime, scrive due campi: "zeri" all'istante iniziale
! Nelle analisi, i campi elaborati si riferiscono all'ora successiva;
! nei forecast, si riferiscono al periodo fra analisi e forecast time corrente
!
! Comando per debug:
! grib_get -f -w typeOfLevel=surface -p productDefinitionTemplateNumber,hour,forecastTime,lengthOfTimeRange,typeOfStatisticalProcessing,shortName,minimum,average,maximum
!  
!                                          Versione 1.0.0 Enrico 03/04/2026
!--------------------------------------------------------------------------

USE grib_api
USE datetime_class
USE grib2_utilities
IMPLICIT NONE

TYPE(datetime) :: rtime_in,rtime_out
TYPE(timedelta) :: tdelta
REAL, ALLOCATABLE :: values_in(:),values_prog(:),values_out(:)
INTEGER :: ifin,ifout,igin=0,igout=0,ig0=0,iret,kg,clret(0:5),ier
INTEGER :: ni,nj,pdtn,ft_in,rt_yy,rt_mm,rt_dd,rt_hh,lotr,tosp,en
INTEGER :: pdtn2,lotr2,lotr_out,ft_out,lotr_prog,tosp2

CHARACTER(LEN=200) :: filein,fileout
LOGICAL :: lverbose = .TRUE.
INTEGER :: ktest = 1000

!--------------------------------------------------------------------------
! Parametri da riga comando

CALL getarg(1,filein)
CALL getarg(2,fileout)

IF (TRIM(filein) == "" .OR. TRIM(fileout) == "" .OR. &
  TRIM(filein) == "-h" .OR. TRIM(filein) == "--help") THEN
  WRITE (*,*) "Uso: grib2_ana2forc.exe filein fileout"
  STOP
ENDIF

! Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifout,fileout,"w")

!--------------------------------------------------------------------------
! Primo campo

CALL grib_new_from_file(ifin,ig0,iret)
IF (iret /= GRIB_SUCCESS) GOTO 9998

CALL grib_get(ig0,"editionNumber",en)
IF (en /= 2) GOTO 9997
CALL grib_get(ig0,"forecastTime",ft_in)
IF (ft_in /= 0) GOTO 9993

CALL grib_get(ig0,"Ni",ni)
CALL grib_get(ig0,"Nj",nj)
CALL grib_get(ig0,"productDefinitionTemplateNumber",pdtn)
CALL grib_get(ig0,"year",rt_yy)
CALL grib_get(ig0,"month",rt_mm)
CALL grib_get(ig0,"day",rt_dd)
CALL grib_get(ig0,"hour",rt_hh)
rtime_out = datetime_new(YEAR=rt_yy, MONTH=rt_mm, DAY=rt_dd, HOUR=rt_hh, MINUTE=0)
WRITE (*,*) "Output reftime: ",rt_yy,rt_mm,rt_dd,rt_hh

ALLOCATE (values_in(ni*nj))
IF (pdtn==8 .OR. pdtn==11) THEN   ! campo elaborato
  CALL grib_get(ig0,"lengthOfTimeRange",lotr)
 
  IF (lotr /= 1) GOTO 9992
  CALL grib_get(ig0,"typeOfStatisticalProcessing",tosp)
  ALLOCATE (values_out(ni*nj),values_prog(ni*nj))

  CALL grib_clone(ig0,igout)
  values_out(:) = 0.
  CALL grib_set(igout,"lengthOfTimeRange",0)
  CALL grib_set(igout,"values",values_out(:))
  CALL set_fc_keys(igout)
  CALL grib_write (igout,ifout)

  IF (lverbose) WRITE (*,*) "decum: , kg,lotr_prog,lotr_out,prog,in,out ", &
        0,-999,0,-999.,-999.,values_out(ktest)

  CALL grib_clone(ig0,igout)
  CALL grib_set(igout,"lengthOfTimeRange",1)
  CALL set_fc_keys(igout)
  CALL grib_write (igout,ifout)

  CALL grib_get(ig0,"values",values_prog)
  lotr_prog = 1

  IF (lverbose) WRITE (*,*) "decum: , kg,lotr_prog,lotr_out,prog,in,out ", &
        1,lotr_prog,1,values_prog(ktest),values_prog(ktest),values_prog(ktest)

ELSE                              ! campo istantaneo
  CALL set_fc_keys(ig0)
  CALL grib_write (ig0,ifout)

ENDIF

!--------------------------------------------------------------------------
! Campi successivi (ciclo principale)

DO kg = 2,HUGE(0)
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998
  CALL grib_get(igin,"editionNumber",en)
  IF (en /= 2) GOTO 9997
  CALL grib_get(igin,"forecastTime",ft_in)
  IF (ft_in /= 0) GOTO 9993

  CALL check_consistency(igin,ig0,.TRUE.,.FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,clret(0:5),ier)
  IF (ier == 1) GOTO 9996
  IF (ier == 2) GOTO 9995
  CALL grib_get(ig0,"productDefinitionTemplateNumber",pdtn2)
  IF (pdtn2 /= pdtn) GOTO 9994
  
  CALL get_grib_time(igin,vtime1=rtime_in)

  CALL grib_clone(igin,igout)
  CALL grib_set(igout,"year",rt_yy)
  CALL grib_set(igout,"month",rt_mm)
  CALL grib_set(igout,"day",rt_dd)
  CALL grib_set(igout,"hour",rt_hh)

! Campo elaborato
  IF (pdtn==8 .OR. pdtn==11) THEN   
    CALL grib_get(igin,"typeOfStatisticalProcessing",tosp2)
    IF (tosp2 /= tosp) GOTO 9990
    CALL grib_get(igin,"lengthOfTimeRange",lotr2)
    IF (lotr2 /= lotr) GOTO 9992

    tdelta = rtime_in - rtime_out + timedelta_new(hour=lotr)
    CALL getval(tdelta, AHOUR=lotr_out)
    CALL grib_set(igout,"lengthOfTimeRange",lotr_out)

    CALL grib_get(igin,"values", values_in)
    IF (tosp == 0) THEN      ! average
      values_out = (values_prog * REAL(lotr_prog) + values_in) / REAL(lotr_out)
    ELSE IF (tosp == 1) THEN ! accumulation
      values_out = values_prog + values_in
    ELSE IF (tosp == 2) THEN ! maximum
      values_out = MAX(values_prog, values_in)
    ELSE
      GOTO 9991
    ENDIF  
    CALL grib_set(igout,"values",values_out)

    IF (lverbose) WRITE (*,*) "decum: , kg,lotr_prog,lotr_out,prog,in,out ", &
      kg,lotr_prog,lotr_out,values_prog(ktest),values_in(ktest),values_out(ktest)
    
    lotr_prog = lotr_out
    values_prog = values_out

! Campo istantaneo
  ELSE
    tdelta = rtime_in - rtime_out
    CALL getval(tdelta, AHOUR=ft_out)
    CALL grib_set(igout,"forecastTime",ft_out)
    
  ENDIF

  CALL set_fc_keys(igout)
  CALL grib_write (igout,ifout)
  CALL grib_release(igin)
  CALL grib_release(igout)

ENDDO

!--------------------------------------------------------------------------
! Conclusione

WRITE (*,*) "Elaborazioni completate, grib elaborati ",kg-1
STOP

!--------------------------------------------------------------------------

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib n.ro " ,kg
STOP

9997 CONTINUE
WRITE (*,*) "Grib1 non supportati. Campo ",kg
STOP

9996 CONTINUE
WRITE (*,*) "Errore: messaggio ",kg+1," incompatibile con il primo"
STOP

9995 CONTINUE
WRITE (*,*) "Errore: check consistency"
STOP

9994 CONTINUE
WRITE (*,*) "Errore: diversi pdtn in input: campo,pdtn ",kg,pdtn2," iniziale ",pdtn
STOP

9993 CONTINUE
WRITE (*,*) "Input forecast time diverso da 0, non gestito; grib, ft_in ",kg,ft_in
STOP

9992 CONTINUE
WRITE (*,*) "Input lotr diverso da 1, non gestito; grib, lotr ",kg,lotr
STOP

9991 CONTINUE
WRITE (*,*) "Input tosp non gestito; grib, lotr ",kg,tosp
STOP

9990 CONTINUE
WRITE (*,*) "Errore: diversi tosp in input: campo,tosp ",kg,tosp2," iniziale ",tosp
STOP

END PROGRAM grib2_ana2forc

SUBROUTINE set_fc_keys(ig)
USE grib_api
IMPLICIT NONE

INTEGER, INTENT(IN) :: ig
INTEGER :: pdtn

CALL grib_set(ig,"significanceOfReferenceTime",1)
CALL grib_set(ig,"typeOfProcessedData",1)
CALL grib_set(ig,"typeOfGeneratingProcess",2)

CALL grib_get(ig,"productDefinitionTemplateNumber",pdtn)
IF (pdtn == 11) CALL grib_set(ig,"typeOfTimeIncrement",2)

RETURN
END SUBROUTINE set_fc_keys
