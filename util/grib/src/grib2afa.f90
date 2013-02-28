PROGRAM grib2afa
!--------------------------------------------------------------------------
! Legge un file con molti grib e lo riscrive nell'obsoleto formato AFA.
! replicando tutte le convenzioni usate in cong.
! Per un'eventuale estensione a GRIB2, bisogna solo gestire le aree 
! (eg: drt -> gdtn ....)
!
!                                         Versione 1.0.1, Enrico 27/03/2013
!--------------------------------------------------------------------------

USE grib_api
USE grib2_utilities
USE missing_values
IMPLICIT NONE

! Chiavi Grib-API
INTEGER :: en,yy,mm,dd,hh,min,drt,ni,nj,sm, par(3),lev(3),scad(4),dig
INTEGER :: gnov,nom,nocv
REAL :: latofgp,latolgp,lonofgp,lonolgp,dx,dy,lonosp,latosp
REAL, ALLOCATABLE :: field(:)
CHARACTER (LEN=3) :: sm_out

! Altre variabili locali
INTEGER :: ifin,igin,iret,kg,ngout
CHARACTER(LEN=160) :: filein,fileout

!--------------------------------------------------------------------------
! 1) Preliminari

! Parametri da riga comando

CALL getarg(1,filein)
CALL getarg(2,fileout)
IF (TRIM(filein) == "" .OR. TRIM(fileout) == "" .OR. &
  TRIM(filein) == "-h" .OR. TRIM(filein) == "--help") THEN
  WRITE (*,*) "Uso: grib2afa.exe filein fileout"
  STOP
ENDIF

! Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
OPEN (UNIT=30, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")

!--------------------------------------------------------------------------
! 2) Elaborazioni (ciclo sui grib)

ngout = 0
DO kg = 1,HUGE(0)

! 2.1 Leggo il prossimo campo
  igin = -1
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998

  CALL grib_get(igin,"editionNumber",en)
  IF (en /= 1) THEN
    WRITE (*,*) "GRIB edizione 2 non gestiti"
    CYCLE
  ENDIF

! 2.2 Leggo le chiavi che servono

! Sezione 1
  CALL grib_get(igin,"year",yy)
  CALL grib_get(igin,"month",mm)
  CALL grib_get(igin,"day",dd)
  CALL grib_get(igin,"hour",hh)
  CALL grib_get(igin,"minute",min)
  CALL get_grib1_header(igin,par,lev,scad,iret)
  IF (iret /= 0) THEN
    WRITE (*,*) "Errore get_grib1_header, skip"
    CYCLE
  ENDIF
  WHERE (.NOT. c_e(lev(1:3)))
    lev(1:3) = 0
  ENDWHERE

! Sezione 2
  CALL grib_get(igin,"dataRepresentationType",drt)
  IF (drt == 10) THEN
    CALL grib_get(igin,"latitudeOfSouthernPoleInDegrees",latosp)
    CALL grib_get(igin,"longitudeOfSouthernPoleInDegrees",lonosp)
  ELSE
    latosp = 0.
    lonosp = 0.
  ENDIF
  CALL grib_get(igin,"latitudeOfFirstGridPointInDegrees",latofgp)
  CALL grib_get(igin,"latitudeOfLastGridPointInDegrees",latolgp)
  CALL grib_get(igin,"numberOfPointsAlongAMeridian",nj)
  CALL grib_get(igin,"longitudeOfFirstGridPointInDegrees",lonofgp)
  CALL grib_get(igin,"longitudeOfLastGridPointInDegrees",lonolgp)
  CALL grib_get(igin,"numberOfPointsAlongAParallel",ni)
  CALL grib_get(igin,"ijDirectionIncrementGiven",dig)
  IF (dig == 1) THEN
    CALL grib_get(igin,"iDirectionIncrementInDegrees",dx)
    CALL grib_get(igin,"jDirectionIncrementInDegrees",dy)
  ELSE
    dx = 0.
    dy = 0.
  ENDIF

  CALL grib_get(igin,"scanningMode",sm)
  IF (sm == 64) THEN
    sm_out = "010"
  ELSE 
    sm_out = "000"
    IF (sm == 0) WRITE (*,*) "Warning: scanning mode non gestito ",sm," metto 000"
  ENDIF

! Sezione 4
  CALL grib_get(igin,"getNumberOfValues",gnov)    ! totale di punti nel grib
  CALL grib_get(igin,"numberOfMissing",nom)       ! n.ro dati mancanti
  CALL grib_get(igin,"numberOfCodedValues",nocv)  ! n.ro dati validi
  ALLOCATE (field(ni*nj))
  IF (nocv == 0) THEN
    field(:) = rmiss
  ELSE
    CALL grib_set(igin,"missingValue",rmiss)
    CALL grib_get(igin,"values",field(:))
  ENDIF
  IF (nom + nocv /= gnov .OR. &
    (nocv /= 0 .AND. nocv /= COUNT(field(1:ni*nj) /= rmiss))) GOTO 9995

! 2.3 Scrivo sul file di output
  WRITE (30,'(a)') filein
  WRITE (30,'(i4.4,4(1x,i2.2))') yy,mm,dd,hh,min
  WRITE (30,'(i3.3,1x,i5.5,1x,i3.3,1x,i3.3)') scad(1:4)
  WRITE (30,'(i3.3,1x,i5.5,3x,i3.3)') lev(1:3)
  WRITE (30,'(i3.3,5(1x,i3.3))') par(1:3),0,0,0
  WRITE (30,'(i3)') drt
  IF (drt == 10) THEN
    WRITE (30,'(4(1x,f8.3))') latosp+90.,lonosp,0.
  ELSE
    WRITE (30,'(4(1x,f8.3))') 0.,0.,0.
  ENDIF
  WRITE (30,'(3(1x,f8.3),1x,i8)') latofgp,latolgp,dy,nj
  WRITE (30,'(3(1x,f8.3),1x,i8)') lonofgp,lonolgp,dx,ni
  WRITE (30,'(i8,1x,a3,1x,i3,1x,e12.5,1x,i16,1x,i1)') &
    ni*nj,sm_out,0,rmiss,imiss,0
  WRITE (30,*) field(1:ni*nj)

! 2.4 Dealloco il puntatore al grib e l'array dei valori
  CALL grib_release(igin)
  DEALLOCATE (field)

ENDDO

WRITE (*,*) "Elaborazioni completate, elaborati ",kg-1," campi"
STOP

!--------------------------------------------------------------------------
! 3) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib n.ro " ,kg
STOP

9995 CONTINUE
WRITE (*,*) "Errore nelle chiavi realtive ai dati mancanti"
WRITE (*,*) "Dati totali (getNumberOfValues):   ",gnov
WRITE (*,*) "Dati validi (numberOfCodedValues): ",nocv
WRITE (*,*) "Dati mancanti (numberOfMissing):   ",nom
WRITE (*,*) "Dati mancanti (matrice grib):      ",COUNT(field(1:ni*nj) /= rmiss)
STOP

END PROGRAM grib2afa
