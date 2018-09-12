PROGRAM ma_grib1_grib2
!--------------------------------------------------------------------------
! Legge un file in formato GRIB1 in proiezione geografica o UTM-SIM 
! (lat-lon con estremi in metri) e lo riscrive in formato GRIB2
!
! Uso: utm_grib1_grib2 [-h] [-forc] [-macc] filein fileout
!
! Todo (eventuali):
! - gestione dati Cosmo (aspettare che DVD decida come scrivere i GRIB2?)
! - opzione -ana (per scrivere i dati previsti come analisi con lo stesso 
!   verification time)
!
!                                         Versione 2.5.0, Enrico 13/10/2015
!--------------------------------------------------------------------------

USE grib_api
USE datetime_class
USE missing_values
USE grib2_utilities
IMPLICIT NONE

! Variabili locali
REAL,ALLOCATABLE :: values(:)
REAL :: xi_in,yi_in,xf_in,yf_in,dx_in,dy_in,xrot_in,yrot_in
REAL :: xi_out,yi_out,xf_out,yf_out,dx_out,dy_out,xrot_out,yrot_out
INTEGER :: cnt_par,kg,kpar,iret,cnt_utm,cnt_geo,cnt_rot,cnt_skp,cnt_gr2
INTEGER :: ifin,ifout,igin=0,igout=0,igtemp_utm=0,igtemp_geo=0,igtemp_rot=0
INTEGER :: datah_in(4),datah_ref(4),datah_end(4)
INTEGER :: scad(4),lev(3),ni,nj,ier
INTEGER :: cem,sc,par,tab,sortt,togp,igen,gd,topd,dig,uvrtg,sm,pdtn,dd, &
  dt,ft,toffs,sfoffs,svoffs,tosfs,sfosfs,svosfs,toti,lotr,bp,bpv,bmi, &
  nv,tosp,ct,en,gnov,nocv,nom
CHARACTER(LEN=200) :: chpar,filein,fileout
CHARACTER(LEN=80) :: grid_type
LOGICAL :: lforc,lmacc
TYPE(datetime) :: datet_in,datet_ref,datet_end

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1) Parametri da riga comando
lforc = .FALSE.
lmacc = .FALSE.

cnt_par = 0
DO kpar = 1,HUGE(0)
  CALL getarg(kpar,chpar)
  IF (chpar == "") THEN
    EXIT
  ELSE IF (TRIM(chpar) == "-h") THEN
    CALL scrive_help
    STOP 1
  ELSE IF (TRIM(chpar) == "-forc") THEN
    lforc = .TRUE.
  ELSE IF (TRIM(chpar) == "-macc") THEN
    lmacc = .TRUE.
  ELSE IF (cnt_par == 0) THEN
    cnt_par = 1
    filein = chpar
  ELSE IF (cnt_par == 1) THEN
    cnt_par = 2
    fileout = chpar
  ENDIF
ENDDO

IF (cnt_par /= 2) THEN
  CALL scrive_help
  STOP 1
ENDIF  

! 1.2) Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999

CALL grib_open_file(ifout,fileout,"w",iret)

CALL grib_new_from_samples(igtemp_utm,"utm_grib2",iret)
CALL grib_new_from_samples(igtemp_geo,"regular_ll_sfc_grib2",iret)
CALL grib_new_from_samples(igtemp_rot,"rotated_ll_sfc_grib2",iret)

IF (iret /= GRIB_SUCCESS) GOTO 9998

!==========================================================================
! Elaborazioni (ciclo sui grib in input)

cnt_utm = 0
cnt_geo = 0
cnt_rot = 0
cnt_gr2 = 0 
cnt_skp = 0 
DO kg = 1,HUGE(0)

!--------------------------------------------------------------------------
! 2) Leggo dal prossimo GRIB le informazioni necessarie

! 2.1) Leggo il GRIB
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9997

! 2.2) Se non e' un GRIB1 in proiezione UTM, GEO o ROT, passo oltre
  CALL grib_get(igin,"editionNumber",en)
  CALL grib_get(igin,"gridType",grid_type)

  IF (en /= 1) THEN
    cnt_gr2 = cnt_gr2 + 1 
    CALL grib_write (igin,ifout)
    CYCLE

  ELSE IF (grid_type /= "regular_ll" .AND. grid_type /= "rotated_ll") THEN
    cnt_skp = cnt_skp + 1
    CYCLE

  ELSE
    CALL grib_get(igin,"longitudeOfFirstGridPointInDegrees",xi_in)
    CALL grib_get(igin,"longitudeOfLastGridPointInDegrees",xf_in)
    CALL grib_get(igin,"latitudeOfFirstGridPointInDegrees",yi_in)
    CALL grib_get(igin,"latitudeOfLastGridPointInDegrees",yf_in)

    IF (grid_type == "regular_ll" .AND. (yi_in>90. .OR. yf_in>90.)) THEN
      grid_type = "UTM"
      cnt_utm = cnt_utm + 1
    ELSE IF (grid_type == "regular_ll") THEN
      cnt_geo = cnt_geo + 1
    ELSE IF (grid_type == "rotated_ll") THEN
      cnt_rot = cnt_rot + 1
    ENDIF
  ENDIF

! 2.3) Leggo le chiavi che mi servono

! 2.3.1 Section 1 (Product)
  CALL grib_get(igin,"centre",cem)
  CALL grib_get(igin,"subCentre",sc)
  CALL grib_get(igin,"generatingProcessIdentifier",igen)
  CALL grib_get(igin,"gridDefinition",gd)
  CALL grib_get(igin,"indicatorOfParameter",par)
  CALL grib_get(igin,"table2Version",tab)

  CALL grib_get(igin,"unitOfTimeRange",scad(1))
  CALL grib_get(igin,"P1",scad(2))
  CALL grib_get(igin,"P2",scad(3))
  CALL grib_get(igin,"timeRangeIndicator",scad(4))
  CALL grib_get(igin,"dataDate",dd)
  CALL grib_get(igin,"dataTime",dt)

  CALL grib_get(igin,"indicatorOfTypeOfLevel",lev(1))
  IF (lev(1) == 1 .OR. lev(1) == 105 .OR. lev(1) == 109) THEN
    CALL grib_get(igin,"level",lev(2))
    lev(3) = 0
  ELSE IF (lev(1) == 110) THEN
    CALL grib_get(igin,"bottomLevel",lev(2))
    CALL grib_get(igin,"topLevel",lev(3))
  ENDIF

  CALL grib_get(igin,"bitmapPresent",bp)

! 2.3.2 Section 2 (Grid)
  CALL grib_get(igin,"numberOfVerticalCoordinateValues",nv)         
  CALL grib_get(igin,"numberOfPointsAlongAParallel",ni)
  CALL grib_get(igin,"numberOfPointsAlongAMeridian",nj)
  CALL grib_get(igin,"ijDirectionIncrementGiven",dig)
  IF (grid_type == "rotated_ll") THEN
    CALL grib_get(igin,"longitudeOfSouthernPoleInDegrees",xrot_in)
    CALL grib_get(igin,"latitudeOfSouthernPoleInDegrees",yrot_in)
  ENDIF
  IF (dig == 1) THEN
    CALL grib_get(igin,"iDirectionIncrementInDegrees",dx_in)
    CALL grib_get(igin,"jDirectionIncrementInDegrees",dy_in)
  ENDIF
  CALL grib_get(igin,"uvRelativeToGrid",uvrtg)
  CALL grib_get(igin,"scanningMode",sm)
  IF (nv /= 0) THEN
    WRITE (*,*) "Vert. Coordinate Values non gestiti (non saranno scritti)"
    nv = 0
  ENDIF

! 2.3.4 Sections 3 (Bit Map) e 4 (Binary Data)
! Bug grib-api (18/03/2013): se un campo e' interamente mancante, values(:) e'
! messo a 0 invece che a rmiss, quindi devo gestire questo caso separatamente

  CALL grib_get(igin,"bitsPerValue",bpv)
  CALL grib_get(igin,"getNumberOfValues",gnov)    ! totale di punti nel grib
  CALL grib_get(igin,"numberOfMissing",nom)       ! n.ro dati mancanti
  CALL grib_get(igin,"numberOfCodedValues",nocv)  ! n.ro dati validi

  ALLOCATE (values(ni*nj))
  IF (nocv == 0) THEN
    values(:) = rmiss
  ELSE
    CALL grib_set(igin,"missingValue",rmiss)
    CALL grib_get(igin,"values",values(:))
  ENDIF
  IF (nom + nocv /= gnov .OR. &
    (nocv /= 0 .AND. nocv /= COUNT(values(1:ni*nj) /= rmiss))) GOTO 9995

! 2.4) Se trovo una chiave con un valore non gestito mi fermo
  IF (scad(1)/=1 .OR. &                     ! unit of timernage is not hour
      MOD(dt,100)/=0  .OR. &                ! minutes /= 0
      gd /= 255 .OR. &                      ! section 2 not included
      sm /= 64 .OR. &                       ! scanning mode anomalo
      (scad(4)/=0 .AND. scad(4)/=10 .AND. scad(2)>scad(3)) .OR. &
      (lev(1)/=1 .AND. lev(1)/=105 .AND. lev(1)/=109 .AND. lev(1)/=110) &
      ) GOTO 9996

!--------------------------------------------------------------------------
! 3) Calcolo le chiavi GRIB2 non contenute nel GRIB1

! 3.1) Timerange
  CALL calc_grib2_trange(scad,lforc,sortt,topd,pdtn,togp,ft,tosp,toti,lotr,ier)
  IF (ier /= 0) GOTO 9994

! 3.2) Reference time; end of overall time interval
  datah_in(1) = dd/10000
  datah_in(2) = MOD(dd/100,100)
  datah_in(3) = MOD(dd,100)
  datah_in(4) = dt / 100
  datet_in = datetime_new(YEAR=datah_in(1), MONTH=datah_in(2), &
    DAY=datah_in(3), HOUR=datah_in(4))

  datet_ref = datet_in
  CALL getval(datet_ref, YEAR=datah_ref(1), MONTH=datah_ref(2), &
    DAY=datah_ref(3), HOUR=datah_ref(4))
  
  IF (scad(4)==3  .OR. scad(4)==4  .OR. scad(4)==6  .OR. scad(4)==7 .OR. &
      scad(4)==14 .OR. scad(4)==15 .OR. scad(4)==16 .OR. scad(4)==17) THEN
    datet_end = datet_in + timedelta_new(HOUR=scad(3))
    CALL getval(datet_end, YEAR=datah_end(1), MONTH=datah_end(2), &
      DAY=datah_end(3), HOUR=datah_end(4))
  ENDIF

! 3.3) Level
  SELECT CASE(lev(1))
  CASE(1)            ! Surface
    toffs = 1
    sfoffs = 0
    svoffs = lev(2)
    tosfs = 255

  CASE(105)          ! Specified height level above ground m
    toffs = 103
    sfoffs = 3
    svoffs = 1000 * lev(2)
    tosfs = 255

  CASE(109)          ! Hybrid level
    toffs = 105
    sfoffs = 0
    svoffs = lev(2)
    tosfs = 255

  CASE(110)          ! Hybrid layer
    toffs = 105
    sfoffs = 0
    svoffs = lev(2)
    tosfs = 105
    sfosfs = 0
    svosfs = lev(3)

  END SELECT

! 3.4) Altro

! Presenza della bitmap
  IF (bp == 0) THEN                 ! Bitmap not present
    bmi = 255
  ELSE IF (bp == 1) THEN            ! Bitmap present
    bmi = 0
  ENDIF

! Dal file di input ho letto coordinate estreme e passo griglia espressi in 
! gradi (chiavi *InDegrees).
! - nella codifica GRIB1-UTM i gradi corrispondono ai km, quindi per avere
!   i valori in m come richiesto dal GRIB2-UTM devo moltiplicare per 1000.
! - nei GRIB2 geo e rot l'unita di misura raccomandata e' deg*10^6

  IF (grid_type == "UTM") THEN
    xi_out = xi_in * 1000.
    xf_out = xf_in * 1000.
    yi_out = yi_in * 1000.
    yf_out = yf_in * 1000.
    IF (dig == 1) THEN
      dx_out = dx_in * 1000.
      dy_out = dy_in * 1000.
    ENDIF

  ELSE
    xi_out = xi_in * 1000000.
    xf_out = xf_in * 1000000.
    yi_out = yi_in * 1000000.
    yf_out = yf_in * 1000000.
    IF (grid_type == "rotated_ll") THEN
      xrot_out = xrot_in * 1000000.
      yrot_out = yrot_in * 1000000.
    ENDIF
    IF (dig == 1) THEN
      dx_out = dx_in * 1000000.
      dy_out = dy_in * 1000000.
    ENDIF

  ENDIF

! 3.5) Se e' richiesto l'output per MACC, modifico le chiavi necessarie;
!      agisco solo sui campi relativi a concentrazioni di inquinanati.

  IF (lmacc .AND. tab == 200 .AND. ANY(par == (/151,153,220,221/))) THEN
    pdtn = 40
    tab = 20
    SELECT CASE (par) ! (t4.230 - diversa da quella su web ECMWF !!!)
    CASE (151) ! O3
      ct = 1
    CASE (153) ! NO2
      ct = 6
    CASE (220) ! PM10
      ct = 40008
    CASE (221) ! PM25
      ct = 40009
    END SELECT  
    par = 0

  ENDIF

!--------------------------------------------------------------------------
! 4) Assegno le chiavi GRIB2 che possono avere valori diversi rispetto al
!    template

  IF (grid_type == "UTM") THEN
    CALL grib_clone(igtemp_utm,igout)
  ELSE IF (grid_type == "regular_ll") THEN
    CALL grib_clone(igtemp_geo,igout)
  ELSE IF (grid_type == "rotated_ll") THEN
    CALL grib_clone(igtemp_rot,igout)
  ENDIF

! 4.1 Section 1 (Identificator)                         
  CALL grib_set(igout,"centre",cem)
  CALL grib_set(igout,"subCentre",sc)
  CALL grib_set(igout,"significanceOfReferenceTime",sortt)
  CALL grib_set(igout,"year",datah_ref(1))
  CALL grib_set(igout,"month",datah_ref(2))                             
  CALL grib_set(igout,"day",datah_ref(3))                               
  CALL grib_set(igout,"hour",datah_ref(4))                              
  CALL grib_set(igout,"minute",0)                            
  CALL grib_set(igout,"second",0)                            
  CALL grib_set(igout,"typeOfProcessedData",topd)
                                                        
! 4.2 Section 3 (Grid)                                  
  CALL grib_set(igout,"sourceOfGridDefinition",0)            
  CALL grib_set(igout,"Ni",ni)
  CALL grib_set(igout,"Nj",nj)
  CALL grib_set(igout,"numberOfDataPoints",ni*nj)            
  CALL grib_set(igout,"iDirectionIncrementGiven",dig)
  CALL grib_set(igout,"jDirectionIncrementGiven",dig)
  IF (dig == 1) THEN
    CALL grib_set(igout,"iDirectionIncrement",dx_out)
    CALL grib_set(igout,"jDirectionIncrement",dy_out)
  ELSE
    CALL grib_set_missing(igout,"iDirectionIncrement")
    CALL grib_set_missing(igout,"jDirectionIncrement")
  ENDIF
  CALL grib_set(igout,"uvRelativeToGrid",uvrtg)
  CALL grib_set(igout,"scanningMode",sm)

  IF (grid_type == "UTM") THEN
    CALL grib_set(igout,"zone",32)
    CALL grib_set(igout,"falseEasting",500000)
    CALL grib_set(igout,"eastingOfFirstGridPoint",xi_out)
    CALL grib_set(igout,"eastingOfLastGridPoint",xf_out)
    CALL grib_set(igout,"northingOfFirstGridPoint",yi_out)
    CALL grib_set(igout,"northingOfLastGridPoint",yf_out)

  ELSE IF (grid_type == "regular_ll") THEN
    CALL grib_set(igout,"longitudeOfFirstGridPoint",xi_out)
    CALL grib_set(igout,"longitudeOfLastGridPoint",xf_out)
    CALL grib_set(igout,"latitudeOfFirstGridPoint",yi_out)
    CALL grib_set(igout,"latitudeOfLastGridPoint",yf_out)

  ELSE IF (grid_type == "rotated_ll") THEN
    CALL grib_set(igout,"longitudeOfFirstGridPoint",xi_out)
    CALL grib_set(igout,"longitudeOfLastGridPoint",xf_out)
    CALL grib_set(igout,"latitudeOfFirstGridPoint",yi_out)
    CALL grib_set(igout,"latitudeOfLastGridPoint",yf_out)
    CALL grib_set(igout,"longitudeOfSouthernPole",xrot_out)
    CALL grib_set(igout,"latitudeOfSouthernPole",yrot_out)

  ENDIF

! 4.3 Section 4 (product; includes parameter, timerange, level)
  CALL grib_set(igout,"NV",nv)
  CALL grib_set(igout,"productDefinitionTemplateNumber",pdtn)
  CALL grib_set(igout,"parameterCategory",tab)
  CALL grib_set(igout,"parameterNumber",par)
  CALL grib_set(igout,"typeOfGeneratingProcess",togp)
  CALL grib_set(igout,"generatingProcessIdentifier",igen)
  CALL grib_set(igout,"indicatorOfUnitOfTimeRange",1)
  CALL grib_set(igout,"forecastTime",ft)

! NB: se assegno i parametri relativi alla "SecondFixedSurface" dopo
!     quelli della "FirstFixedSurface", questi ultimi vengono messi a 0 
!     (probabile bug GRIB-API, 10/12/2010) 
  CALL grib_set(igout,"typeOfFirstFixedSurface",toffs)
  CALL grib_set(igout,"scaleFactorOfFirstFixedSurface",sfoffs)
  CALL grib_set(igout,"scaledValueOfFirstFixedSurface",svoffs)
  CALL grib_set(igout,"typeOfSecondFixedSurface",tosfs)

  IF (tosfs == 255) THEN
    CALL grib_set_missing(igout,"scaleFactorOfSecondFixedSurface")
    CALL grib_set_missing(igout,"scaledValueOfSecondFixedSurface")
  ELSE
    CALL grib_set(igout,"scaleFactorOfSecondFixedSurface",sfosfs)
    CALL grib_set(igout,"scaledValueOfSecondFixedSurface",svosfs)
  ENDIF

  IF (pdtn == 8) THEN
    CALL grib_set(igout,"yearOfEndOfOverallTimeInterval",datah_end(1))
    CALL grib_set(igout,"monthOfEndOfOverallTimeInterval",datah_end(2))
    CALL grib_set(igout,"dayOfEndOfOverallTimeInterval",datah_end(3))
    CALL grib_set(igout,"hourOfEndOfOverallTimeInterval",datah_end(4))
    CALL grib_set(igout,"minuteOfEndOfOverallTimeInterval",0)
    CALL grib_set(igout,"secondOfEndOfOverallTimeInterval",0)
    CALL grib_set(igout,"typeOfStatisticalProcessing",tosp)
    CALL grib_set(igout,"typeOfTimeIncrement",toti)
    CALL grib_set(igout,"indicatorOfUnitForTimeRange",1)
    CALL grib_set(igout,"lengthOfTimeRange",lotr)
    CALL grib_set(igout,"indicatorOfUnitForTimeIncrement",1)
    CALL grib_set(igout,"timeIncrement",1)
  ENDIF

  IF (lmacc .AND. pdtn == 40) THEN
    CALL grib_set(igout,"constituentType",ct)
  ENDIF

! 2.2.4 Sections 5-7 (data representation, bitmap, data)
  CALL grib_set(igout,"numberOfValues",ni*nj)
  CALL grib_set(igout,"bitMapIndicator",bmi)
  CALL grib_set(igout,"missingValue",rmiss)

  IF (lmacc) THEN
    CALL grib_set(igout,"dataRepresentationTemplateNumber",4)
    CALL grib_set(igout,"precision",1) ! t5.7
    CALL grib_set(igout,"values",values(:) * 0.000000001)
  ELSE
    CALL grib_set(igout,"bitsPerValue",bpv)
    CALL grib_set(igout,"values",values(:))
  ENDIF

!--------------------------------------------------------------------------
! 5) Scrivo il grib modificato

  CALL grib_write (igout,ifout)

! Libero memoria
  CALL grib_release(igin)
  CALL grib_release(igout)
  DEALLOCATE (values)
ENDDO

!==========================================================================
! Conclusione

CALL grib_close_file(ifin)
CALL grib_close_file(ifout)

WRITE (*,*)"Operazioni completate, grib elaborati ",kg-1
IF (cnt_utm > 0) WRITE (*,*) "  in proiezione utm ",cnt_utm
IF (cnt_geo > 0) WRITE (*,*) "  in proiezione geo ",cnt_geo
IF (cnt_rot > 0) WRITE (*,*) "  in proiezione rot ",cnt_rot
IF (cnt_skp > 0) WRITE (*,*) "  Grib 1 non gstiti ",cnt_skp
IF (cnt_gr2 > 0) WRITE (*,*) "  Grib 2 in input   ",cnt_gr2
IF (kg-1 /= cnt_utm + cnt_geo + cnt_rot + cnt_gr2 + cnt_skp) &
  WRITE (*,*) "I conti non tornano!!"
STOP

!==========================================================================
! 6) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 2

9998 CONTINUE
WRITE (*,*) "Template GRIB2 UTM-SIMC non trovato"
WRITE (*,*) "Verificare che esista il file /usr/share/grib_api/samples/utm_grib2.tmpl,"
WRITE (*,*) "oppure mettere il suo path nella varibile d'ambiente GRIB_SAMPLES_PATH"
STOP 3

9997 CONTINUE
WRITE (*,*) "Errore ",iret," leggendo ",TRIM(filein)," grib n.ro ",kg
STOP 4

9996 CONTINUE
WRITE (*,*) "Chiave non gestita nel grib n.ro ",kg
IF (scad(1)/=1) WRITE (*,*) "Unit of timernage is not hour but ",scad(1)
IF (MOD(dt,100)/=0) WRITE (*,*) "Minutes in refernce time are not 0 but ",&
  MOD(dt,100)
IF (sm /= 64) WRITE (*,*) "Scanning mode non gestito ",sm
IF (scad(4)/=0 .AND. scad(4)/=3 .AND. scad(4)/=4 .AND. scad(4)/=10 .AND. &
  scad(4)/=14) WRITE (*,*) "Timerange non gestito: ",scad(4)
IF (lev(1)/=1 .AND. lev(1)/=105 .AND. lev(1)/=109 .AND. lev(1)/=110) &
   WRITE (*,*) "Tipo di lvello non gestito ",lev(1)
IF (scad(4)/=0 .AND. scad(4)/=10 .AND. scad(2)>scad(3)) WRITE (*,*) &
  "timerange con P1 > P2 ",scad(1:4)
STOP 5

9995 CONTINUE
WRITE (*,*) "Errore nelle chiavi relative ai dati mancanti"
WRITE (*,*) "Dati totali (getNumberOfValues):   ",gnov
WRITE (*,*) "Dati validi (numberOfCodedValues): ",nocv
WRITE (*,*) "Dati mancanti (numberOfMissing):   ",nom
WRITE (*,*) "Dati mancanti (matrice grib):      ",COUNT(values(1:ni*nj) /= rmiss)
STOP 6

9994 CONTINUE
WRITE (*,*) "Timernage non gestito ",scad(4)
STOP 7

END PROGRAM ma_grib1_grib2

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Scrive a schermo l'help del programma
!
IMPLICIT NONE

!                12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*)
WRITE (*,'(a)') "Uso: ma_grib1_grib2 [-h] [-forc] [-macc] filein fileout"
WRITE (*,'(a)') "Legge un file con molti grib1 e lo riscrive in formato GRIB2"
WRITE (*,'(a)') "Gestisce proiezioni UTM-SIMC, geo, rot; per ora non gestisce nudging COSMO"
WRITE (*,*) "-forc: scrive i campi relativi ad analisi (istantanee o elaborate) come forecast +0"
WRITE (*,*) "-macc: scrive i campi relativi a conc. inquinanti secondo le convenzioni MACC;"
WRITE (*,*) "       agisce sulle sezioni 4, 5 e 7; non modifica livello e scadenza"

RETURN
END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
