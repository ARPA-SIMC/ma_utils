PROGRAM ma_grib2_grib1
!--------------------------------------------------------------------------
! Legge un file con molti grib, convertendo i campi GRIB2 in formato GRIB1
! - Deriva da ma_grib1_grib2.f90 V3
! - Usa grib_api per leggere, e scrivere"
!   utile perche' gribex non riesce a leggere correttamente i grib "degeneri"
!   (ie. con tutti i valori uguali o mancanti) scritti da grib-api.
! - Gestisce proiezioni geo, rot, utm (i campi utm sono riscritti nel vecchio"
!   formato GRIB1-SIMC)
!  - Gestisce dati SIMC e AQ-MACC
!
! Uso: ma_grib2_grib1.exe [-h] filein fileout
!
!                                         Versione 3.0.0, Enrico 08/03/2017
!--------------------------------------------------------------------------

USE grib_api
USE grib2_utilities
USE missing_values
IMPLICIT NONE

! Variabili locali
REAL,ALLOCATABLE :: values(:)
REAL :: xi_in,yi_in,xf_in,yf_in,dx_in,dy_in,fe,fn,xrot,yrot
REAL :: xi_out,yi_out,xf_out,yf_out,dx_out,dy_out
INTEGER :: cnt_par,kg,kpar,iret,cnt_utm,cnt_geo,cnt_rot,cnt_nok
INTEGER :: ifin,ifout,igin=0,igout=0,igtemp=0,iu
INTEGER :: par(3),lev(3),scad(4),datah_ref(4),min,sec,yoc,cortod
INTEGER :: sc,igen,drt,nv,bp,bmi,bpv,ni,nj,sogd,dig,idig, &
  jdig,rf,cf,uvrtg,sm,s1f,z,drtn,gnov,nocv,nom
CHARACTER(LEN=200) ::  chpar,filein,fileout
CHARACTER(LEN=80) :: grid_type

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1) Parametri da riga comando
cnt_par = 0
DO kpar = 1,HUGE(0)
  CALL getarg(kpar,chpar)
  IF (chpar == "") THEN
    EXIT
  ELSE IF (TRIM(chpar) == "-h") THEN
    CALL scrive_help
    STOP 1
  ELSE IF (cnt_par == 0) THEN
    cnt_par = 1
    filein = chpar
  ELSE IF (cnt_par == 1) THEN
    cnt_par = 2
    fileout = chpar
  ELSE
    WRITE (*,*) "Warning: parametro non gestito, ignoro: ",TRIM(chpar)
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
CALL grib_new_from_samples(igtemp,"GRIB1",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9998

!==========================================================================
! Elaborazioni (ciclo sui grib in input)

cnt_utm = 0
cnt_geo = 0
cnt_rot = 0
cnt_nok = 0 
DO kg = 1,HUGE(0)

!--------------------------------------------------------------------------
! 2) Leggo il prossimo GRIB; calcolo i parametri dell'header in stile GRIB1

! 2.1) Leggo il GRIB
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9997

! 2.2) Se non e'un GRIB2 in proiezione UTM o geo, lo riscrivo cosi' com'e'
  CALL grib_get(igin,"gridType",grid_type)
  IF (grid_type == "UTM") THEN
    CALL grib_get(igin,"falseEasting",fe)
    CALL grib_get(igin,"falseNorthing",fn)
    CALL grib_get(igin,"zone",z)
    IF (z /= 32 .OR. NINT(fe) /= 500000 .OR. NINT(fn) /= 0) THEN
      WRITE (*,*) "Warning: trovato grib UTM non SIMC, scrivo immutato"
      CALL grib_write (igin,ifout)
      cnt_nok = cnt_nok + 1
      CYCLE
    ELSE
      cnt_utm = cnt_utm + 1
    ENDIF
  ELSE IF (grid_type == "regular_ll") THEN
    cnt_geo = cnt_geo + 1
  ELSE IF (grid_type == "rotated_ll") THEN
    cnt_rot = cnt_rot + 1
  ELSE
    WRITE (*,*) "Warning: trovato in proiezione non gestita, scrivo immutato"
    CALL grib_write (igin,ifout)
    cnt_nok = cnt_nok + 1
    CYCLE
  ENDIF

! 2.3) Se e' un GRIB2 UTM o GEO, leggo le chiavi che mi servono e calcolo
!      le corrsipondenti chiavi GRIB1

! 2.3.1 Parametro, livello, timerange
  CALL get_grib1_header(igin, PAR=par, LEV=lev, SCAD=scad, IRET=iret)
  IF (iret /= 0) GOTO 9996

! 2.3.2 Section 1 (Identificator)                         
  CALL grib_get(igin,"subCentre",sc)
  CALL grib_get(igin,"year",datah_ref(1))
  CALL grib_get(igin,"month",datah_ref(2))                             
  CALL grib_get(igin,"day",datah_ref(3))                               
  CALL grib_get(igin,"hour",datah_ref(4))                              
  CALL grib_get(igin,"minute",min)                           
  CALL grib_get(igin,"second",sec)                           
  yoc = MOD((datah_ref(1)-1),100) + 1
  cortod = (datah_ref(1)-1)/100 + 1

! 2.3.3 Section 3 (Grid)                                  
  CALL grib_get(igin,"sourceOfGridDefinition",sogd)         
  CALL grib_get(igin,"Ni",ni)
  CALL grib_get(igin,"Nj",nj)

  IF (grid_type == "UTM") THEN
    CALL grib_get(igin,"eastingOfFirstGridPoint",xi_in)
    CALL grib_get(igin,"eastingOfLastGridPoint",xf_in)
    CALL grib_get(igin,"northingOfFirstGridPoint",yi_in)
    CALL grib_get(igin,"northingOfLastGridPoint",yf_in)
  ELSE IF (grid_type == "regular_ll" .OR. grid_type == "rotated_ll") THEN
    CALL grib_get(igin,"longitudeOfFirstGridPointInDegrees",xi_in)
    CALL grib_get(igin,"longitudeOfLastGridPointInDegrees",xf_in)
    CALL grib_get(igin,"latitudeOfFirstGridPointInDegrees",yi_in)
    CALL grib_get(igin,"latitudeOfLastGridPointInDegrees",yf_in)
  ENDIF
  IF (grid_type == "rotated_ll") THEN
    CALL grib_get(igin,"longitudeOfSouthernPoleInDegrees",xrot)
    CALL grib_get(igin,"latitudeOfSouthernPoleInDegrees",yrot)
  ENDIF

  CALL grib_get(igin,"iDirectionIncrementGiven",idig)
  CALL grib_get(igin,"jDirectionIncrementGiven",jdig)
  IF (idig == 1 .AND. jdig ==1) THEN
    IF (grid_type == "UTM") THEN
      CALL grib_get(igin,"iDirectionIncrement",dx_in)
      CALL grib_get(igin,"jDirectionIncrement",dy_in)
    ELSE IF (grid_type == "regular_ll" .OR. grid_type == "rotated_ll") THEN
      CALL grib_get(igin,"iDirectionIncrementInDegrees",dx_in)
      CALL grib_get(igin,"jDirectionIncrementInDegrees",dy_in)
    ENDIF  
  ENDIF

  CALL grib_get(igin,"uvRelativeToGrid",uvrtg)
  CALL grib_get(igin,"scanningMode",sm)

  IF (sogd/=0) GOTO 9996    ! grid definition not included in GRIB message
! IF (sm /= 64) GOTO 9994

  IF (uvrtg == 0) THEN
    cf = 0
  ELSE
    cf = 8
  ENDIF

! Nei GRIB UTM, l'unita' di misura di coordinate estreme e passo griglia e':
! - nel GRIB2:              metri
! - nel GRIB1-UTM grib_api: gradi (corrispondono ai km.), in quanto ho 
!                           scelto di usare sempre le chiavi *InDegrees
! - nel GRIB1-UTM gribex:   millesimi di grado (corrispondono ai m.)
!
! Per compatibilita' con GRADS, nei grib regolari o ruotati scrivo le
! longitudini ovest come negative
  
  IF (grid_type == "rotated_ll") THEN
    drt = 10
  ELSE
    drt = 0
  ENDIF

  IF (grid_type == "UTM") THEN
    xi_out = xi_in / 1000.
    xf_out = xf_in / 1000.
    yi_out = yi_in / 1000.
    yf_out = yf_in / 1000.
  ELSE IF (grid_type == "regular_ll" .OR. grid_type == "rotated_ll") THEN
    IF (xi_in < 180) THEN
      xi_out = xi_in
    ELSE
      xi_out = xi_in - 360.
    ENDIF
    IF (xf_in < 180) THEN
      xf_out = xf_in
    ELSE
      xf_out = xf_in - 360.
    ENDIF

    yi_out = yi_in
    yf_out = yf_in
  ENDIF
  
  IF (idig == 1 .AND. jdig ==1) THEN
    dig = 1
    rf = 128
    IF (grid_type == "UTM") THEN
      dx_out = dx_in / 1000.
      dy_out = dy_in / 1000.
    ELSE IF (grid_type == "regular_ll" .OR. grid_type == "rotated_ll") THEN
      dx_out = dx_in
      dy_out = dy_in
    ENDIF

  ELSE
    dig = 0
    rf = 0

  ENDIF   

! 2.3.4 Section 4-7 (product, data representation, bitmap, data)
  CALL grib_get(igin,"NV",nv)
  CALL grib_get(igin,"dataRepresentationTemplateNumber",drtn)
  CALL grib_get(igin,"generatingProcessIdentifier",igen)
  CALL grib_get(igin,"bitsPerValue",bpv)
  CALL grib_get(igin,"bitMapIndicator",bmi)
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

  IF (nv /= 0) THEN
    WRITE (*,*) "Vert. Coordinate Values non gestiti (non saranno scritti)"
    nv = 0
  ENDIF
  IF (bmi == 255) THEN              ! Bitmap not present
    bp = 0
    s1f = 128
  ELSE IF (bmi == 0) THEN           ! Bitmap present
    bp = 1
    s1f = 192
  ENDIF
  IF (drtn == 4 .AND. bpv == 0) bpv = 24

!--------------------------------------------------------------------------
! 4) Scrivo in formato GRIB1 usando la liberria Grib-API. Assegno solo le 
!    chiavi GRIB1 che possono avere valori diversi rispetto al template

  CALL grib_clone(igtemp,igout)
 
! 4.1 Section 1 (Product)
  CALL grib_set(igout,"table2Version",par(2))
  CALL grib_set(igout,"centre",par(1))
  CALL grib_set(igout,"subCentre",sc)
  CALL grib_set(igout,"generatingProcessIdentifier",igen)
  CALL grib_set(igout,"gridDefinition",255) 
  CALL grib_set(igout,"bitmapPresent",bp)
  CALL grib_set(igout,"indicatorOfParameter",par(3))
 
! Level
  CALL grib_set(igout,"indicatorOfTypeOfLevel",lev(1))
  IF (lev(1) == 1 .OR. lev(1) == 105 .OR. lev(1) == 109) THEN
    CALL grib_set(igout,"level",lev(2))
  ELSE IF (lev(1) == 110) THEN
    CALL grib_set(igout,"bottomLevel",lev(2))
    CALL grib_set(igout,"topLevel",lev(3))
  ENDIF
 
! Reference time
  CALL grib_set(igout,"yearOfCentury",yoc)
  CALL grib_set(igout,"month",datah_ref(2))                             
  CALL grib_set(igout,"day",datah_ref(3))                               
  CALL grib_set(igout,"hour",datah_ref(4))                              
  CALL grib_set(igout,"minute",0)                            
  CALL grib_set(igout,"centuryOfReferenceTimeOfData",cortod)
 
! Timerange
  CALL grib_set(igout,"unitOfTimeRange",scad(1))
  CALL grib_set(igout,"P1",scad(2))
  CALL grib_set(igout,"P2",scad(3))
  CALL grib_set(igout,"timeRangeIndicator",scad(4))
 
! 4.2 Section 2 (Grid)
  CALL grib_set(igout,"dataRepresentationType",drt)
  CALL grib_set(igout,"numberOfVerticalCoordinateValues",nv)         

  CALL grib_set(igout,"numberOfPointsAlongAParallel",ni)
  CALL grib_set(igout,"numberOfPointsAlongAMeridian",nj)
  CALL grib_set(igout,"longitudeOfFirstGridPointInDegrees",xi_out)
  CALL grib_set(igout,"longitudeOfLastGridPointInDegrees",xf_out)
  CALL grib_set(igout,"latitudeOfFirstGridPointInDegrees",yi_out)
  CALL grib_set(igout,"latitudeOfLastGridPointInDegrees",yf_out)
  IF (grid_type == "rotated_ll") THEN
    CALL grib_set(igout,"longitudeOfSouthernPoleInDegrees",xrot)
    CALL grib_set(igout,"latitudeOfSouthernPoleInDegrees",yrot)
  ENDIF
 
  CALL grib_set(igout,"ijDirectionIncrementGiven",dig)
  IF (dig == 1) THEN
    CALL grib_set(igout,"iDirectionIncrementInDegrees",dx_out)
    CALL grib_set(igout,"jDirectionIncrementInDegrees",dy_out)
  ELSE
    CALL grib_set_missing(igout,"iDirectionIncrement")
    CALL grib_set_missing(igout,"jDirectionIncrement")
  ENDIF
  CALL grib_set(igout,"uvRelativeToGrid",uvrtg)
  CALL grib_set(igout,"scanningMode",sm)
 
! 4.3 Section 3 (Bit Map)
  IF (bp /= 0) CALL grib_set(igout,"missingValue",rmiss)
 
! 4.4 Section 4 (Binary Data)
  CALL grib_set(igout,"bitsPerValue",bpv)
  CALL grib_set(igout,"values",values(:))
 
! 4.5 Scrivo in formato GRIB1
  CALL grib_write (igout,ifout)

!--------------------------------------------------------------------------
! 5) Libero memoria

  DEALLOCATE (values)
  CALL grib_release(igin)
  CALL grib_release(igout)

ENDDO

!==========================================================================
! Conclusione

CALL grib_close_file(ifin)
CALL grib_close_file(ifout)

WRITE (*,*)"Operazioni completate, grib elaborati ",kg-1
IF (cnt_utm > 0) WRITE (*,*) "  in proiezione utm ",cnt_utm
IF (cnt_geo > 0) WRITE (*,*) "  in proiezione geo ",cnt_geo
IF (cnt_rot > 0) WRITE (*,*) "  in proiezione rot ",cnt_rot
IF (cnt_nok > 0) WRITE (*,*) "  non gstiti        ",cnt_nok
STOP

!==========================================================================
! 6) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 2

9998 CONTINUE
WRITE (*,*) "Template GRIB1 non trovato"
WRITE (*,*) "Verificare che esista il file /usr/share/grib_api/samples/utm_grib2.tmpl,"
WRITE (*,*) "oppure mettere il suo path nella varibile d'ambiente GRIB_SAMPLES_PATH"
STOP 3

9997 CONTINUE
WRITE (*,*) "Errore ",iret," leggendo ",TRIM(filein)," grib n.ro ",kg
STOP 4

9996 CONTINUE
IF (sogd/=0) WRITE (*,*) "grid definition not included in GRIB message"
WRITE (*,*) "Codifica non gestita nel grib n.ro ",kg
STOP 5

9995 CONTINUE
WRITE (*,*) "Errore nelle chiavi realtive ai dati mancanti"
WRITE (*,*) "Dati totali (getNumberOfValues):   ",gnov
WRITE (*,*) "Dati validi (numberOfCodedValues): ",nocv
WRITE (*,*) "Dati mancanti (numberOfMissing):   ",nom
WRITE (*,*) "Dati mancanti (matrice grib):      ",COUNT(values(1:ni*nj) /= rmiss)
STOP 6

! 9994 CONTINUE
! WRITE (*,*) "Scanning mode non gestito ",sm

END PROGRAM ma_grib2_grib1

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Scrive a schermo l'help del programma
!
IMPLICIT NONE

!                12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*)
WRITE (*,'(a)') "Uso: ma_grib2_grib1.exe filein fileout [-h]"
WRITE (*,'(a)') "Legge un file con molti grib, convertendo i campi GRIB2 in formato GRIB1"
WRITE (*,'(a)') "  usa le librerie grib-api"
WRITE (*,'(a)') "  Gestisce proiezioni geo, rot, utm (i campi utm sono riscritti nel vecchio"
WRITE (*,'(a)') "  formato GRIB1-SIMC)"
WRITE (*,*)

RETURN
END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
