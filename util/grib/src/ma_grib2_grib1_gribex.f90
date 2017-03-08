PROGRAM ma_grib2_grib1_gribex
!--------------------------------------------------------------------------
! Legge un file con molti grib, convertendo i campi GRIB2 in formato GRIB1
! - Deriva da ma_grib1_grib2.f90 V3
! - Usa grib_api per leggere, gibex (vecchia libreria EMOS) per scrivere:
!   utile perche' gribex non riesce a leggere correttamente i grib "degeneri"
!   (ie. con tutti i valori uguali o mancanti) scritti da grib-api.
! - Gestisce proiezioni geo, rot, utm (i campi utm sono riscritti nel vecchio"
!   formato GRIB1-SIMC)
!  - Gestisce dati SIMC e AQ-MACC
!
! Uso: ma_grib2_grib1_gribex.exe [-h] filein fileout
!
!                                         Versione 3.0.0, Enrico 08/03/2017
!--------------------------------------------------------------------------

USE grib_api
USE grib2_utilities
USE missing_values
IMPLICIT NONE

! Dichiarazioni per GRIBEX
INTEGER, PARAMETER :: maxdim = 1000000
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim),kword,kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Variabili locali
REAL,ALLOCATABLE :: values(:)
REAL :: xi_in,yi_in,xf_in,yf_in,dx_in,dy_in,fe,fn,xrot,yrot
INTEGER :: xi_out_gbex,yi_out_gbex,xf_out_gbex,yf_out_gbex,dx_out_gbex, &
  dy_out_gbex,xrot_gbex,yrot_gbex
INTEGER :: cnt_par,kg,kpar,iret,cnt_utm,cnt_geo,cnt_rot,cnt_nok
INTEGER :: ifin,ifout,igin=0,iu
INTEGER :: par(3),lev(3),scad(4),datah_ref(4),min,sec,yoc,cortod
INTEGER :: sc,igen,drt,nv,bp,bp_gbex,bmi,bpv,bpv_gbex,ni,nj,sogd,dig,idig, &
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

CALL grsvck(0)
CALL PBOPEN (iu,fileout,'W',kret)

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

! 2.2) Se non e'un GRIB2 in proiezione UTM (SIMC-new) o geo, passo oltre
  CALL grib_get(igin,"gridType",grid_type)
  IF (grid_type == "UTM") THEN
    CALL grib_get(igin,"falseEasting",fe)
    CALL grib_get(igin,"falseNorthing",fn)
    CALL grib_get(igin,"zone",z)
    IF (z /= 32 .OR. NINT(fe) /= 500000 .OR. NINT(fn) /= 0) THEN
      WRITE (*,*) "Warning: trovato grib UTM non SIMC, skip"
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
    WRITE (*,*) "Warning: trovato in proiezione non gestita, skip"
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

  IF (grid_type == "rotated_ll") THEN
    drt = 10
    xrot_gbex = NINT(xrot * 1000.)
    yrot_gbex = NINT(yrot * 1000.)
  ELSE
    drt = 0
    xrot_gbex = 0
    yrot_gbex = 0
  ENDIF

  IF (grid_type == "UTM") THEN
    xi_out_gbex = NINT(xi_in)
    xf_out_gbex = NINT(xf_in)
    yi_out_gbex = NINT(yi_in)
    yf_out_gbex = NINT(yf_in)
  ELSE IF (grid_type == "regular_ll" .OR. grid_type == "rotated_ll") THEN
    xi_out_gbex = NINT(xi_in * 1000.)
    xf_out_gbex = NINT(xf_in * 1000.)
    yi_out_gbex = NINT(yi_in * 1000.)
    yf_out_gbex = NINT(yf_in * 1000.)
  ENDIF
  
  IF (idig == 1 .AND. jdig ==1) THEN
    dig = 1
    rf = 128
    IF (grid_type == "UTM") THEN
      dx_out_gbex = NINT(dx_in)
      dy_out_gbex = NINT(dy_in)
    ELSE IF (grid_type == "regular_ll" .OR. grid_type == "rotated_ll") THEN
      dx_out_gbex = NINT(dx_in * 1000.)
      dy_out_gbex = NINT(dy_in * 1000.)
    ENDIF

  ELSE
    dig = 0
    rf = 0
    dx_out_gbex = 0
    dy_out_gbex = 0

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
    bp_gbex = 1
  ELSE IF (bmi == 0) THEN           ! Bitmap present
    bp = 1
    s1f = 192
    bp_gbex = 0
  ENDIF
  IF (drtn == 4 .AND. bpv == 0) THEN
    bpv_gbex = 24
    bpv = 24
  ELSE IF (bpv == 0) THEN
    bpv_gbex = 8
  ELSE
    bpv_gbex = bpv
  ENDIF

!--------------------------------------------------------------------------
! 4) Scrivo in formato GRIB1 usando la libreria Emos (Gribex)

  ksec1(:) = 0
  ksec2(:) = 0
  psec2(:) = 0. 
  ksec3(:) = 0
  psec3(:) = 0.
  ksec4(:) = 0

  ksec1(1:24) = &
  (/par(2),      par(1),      igen,        255,         s1f,          &
    par(3),      lev(1),      lev(2),      lev(3),      yoc,          &
    datah_ref(2),datah_ref(3),datah_ref(4),0,           scad(1),      &
    scad(2),     scad(3),     scad(4),     0,           0,            &
    cortod,      sc,          0,           0/)

  ksec2(1:19) = &
  (/drt,         ni,          nj,          yi_out_gbex, xi_out_gbex,  &
    rf,          yf_out_gbex, xf_out_gbex, dx_out_gbex, dy_out_gbex,  &
    sm,          nv,          yrot_gbex,   xrot_gbex,   0,            &
    0,           0,           0,           cf/)

  ksec3(1) = bp_gbex
  psec3(2) = rmiss
  ksec4(1) = ni * nj
  ksec4(2) = bpv_gbex
  field(1:ni*nj) = values(1:ni*nj)
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
    field,ni*nj,kbuffer,maxdim,kword,'C',kret)
  CALL PBWRITE(iu,kbuffer,ksec0(1),kret)

!--------------------------------------------------------------------------
! 6) Libero memoria

  DEALLOCATE (values)
  CALL grib_release(igin)

ENDDO

!==========================================================================
! Conclusione

CALL grib_close_file(ifin)
CALL PBCLOSE (iu,kret)

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

END PROGRAM ma_grib2_grib1_gribex

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Scrive a schermo l'help del programma
!
IMPLICIT NONE

!                12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*)
WRITE (*,'(a)') "Uso: ma_grib2_grib1_gribex.exe filein fileout [-h]"
WRITE (*,'(a)') "Legge un file con molti grib, convertendo i campi GRIB2 in formato GRIB1"
WRITE (*,'(a)') "  legge con grib-api, scrive con gribex" 
WRITE (*,'(a)') "  utile perche gribex non riesce a leggere correttamente i grib degeneri"
WRITE (*,'(a)') "  (ie. con tutti i valori uguali o mancanti) scritti da grib-api."
WRITE (*,'(a)') "  Gestisce proiezioni geo, rot, utm (i campi utm sono riscritti nel vecchio"
WRITE (*,'(a)') "  formato GRIB1-SIMC)"
WRITE (*,*)

RETURN
END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
