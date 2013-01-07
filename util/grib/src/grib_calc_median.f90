PROGRAM grib_calc_median
!--------------------------------------------------------------------------
! Legge molti files grib con campi corrispondenti; scrive un file che ha,
! per ogni campo e per ogni punto, la mediana sui fiels di input.
! Note:
!
! - Non gestisce dati mancanti
!                                         Versione 1.0.2, Enrico 22/11/2011
!--------------------------------------------------------------------------

USE grib_api
IMPLICIT NONE

REAL,PARAMETER :: rmis = -9999.
INTEGER, PARAMETER :: maxfiles=15,pt=1
INTEGER :: ifin(maxfiles)=0,igin(maxfiles)=0,ifout=0,igout=0
INTEGER :: ni,nj,dd,p1,iop,lev,ni_sav,nj_sav,dd_sav,p1_sav,iop_sav,lev_sav
INTEGER :: nfiles,iret,kg,kf,ios,kk,k2
CHARACTER(LEN=80) :: filein(maxfiles)
CHARACTER(LEN=80) :: fileout,chfmt
LOGICAL :: ldeb = .FALSE.

REAL, ALLOCATABLE :: field_in(:,:),field_out(:),field_ave(:)
REAL, ALLOCATABLE :: field_in_sorted(:,:)
INTEGER, ALLOCATABLE :: idx_min(:)
LOGICAL, ALLOCATABLE :: unassigned(:,:)

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
CALL getarg(1,fileout)
IF (TRIM(fileout) == "" .OR. TRIM(fileout) == "-h") THEN
  WRITE (*,*) "Uso: grib_calc_median.exe fileout"
  WRITE (*,*) "Legge la lista dei files di input da grib_calc_median.lst"
  STOP
ENDIF

! Leggo la lista dei files in input
OPEN (20, file="grib_calc_median.lst", STATUS="OLD", FORM="FORMATTED", &
  IOSTAT=ios)
IF (ios /= 0) GOTO 9999
nfiles = 0
DO kf = 1,maxfiles
  READ (20,'(a)',IOSTAT=ios) filein(kf)
  IF (ios /= 0) EXIT
  nfiles = nfiles + 1
ENDDO
CLOSE (20)

! Apro i files
DO kf = 1,nfiles
  CALL grib_open_file(ifin(kf),filein(kf),"r",iret)
  IF (iret /= GRIB_SUCCESS) GOTO 9998
ENDDO
CALL grib_open_file(ifout,fileout,"w")

!--------------------------------------------------------------------------
! 2) Ciclo sui grib

fields: DO kg = 1,HUGE(0)

! 2.1 Leggo il prossimo campo da ciascuno dei files di input

  DO kf = 1,nfiles
!   Leggo il grib
    CALL grib_new_from_file(ifin(kf),igin(kf),iret)
    IF (iret == GRIB_END_OF_FILE) EXIT fields
    IF (iret /= GRIB_SUCCESS) GOTO 9997

!   Controlli
    CALL grib_get(igin(kf),"numberOfPointsAlongAParallel",ni)
    CALL grib_get(igin(kf),"numberOfPointsAlongAMeridian",nj)
    CALL grib_get(igin(kf),"dataDate",dd)               ! data
    CALL grib_get(igin(kf),"P1",p1)                     ! scad
    CALL grib_get(igin(kf),"indicatorOfParameter",iop)  ! var
    CALL grib_get(igin(kf),"level",lev)                 ! lev

    IF (kg == 1 .AND. kf == 1) THEN
      ALLOCATE (field_in(ni*nj,nfiles),field_out(ni*nj),field_ave(ni*nj))
      ALLOCATE (field_in_sorted(ni*nj,nfiles),unassigned(ni*nj,nfiles))
      ALLOCATE (idx_min(ni*nj))
      ni_sav = ni
      nj_sav = nj
    ELSE 
      IF (ni /= ni_sav .OR. nj /= nj_sav) GOTO 9996
    ENDIF

    IF (kf == 1) THEN
      dd_sav = dd
      p1_sav = p1
      iop_sav = iop
      lev_sav = lev
    ELSE
      IF (dd_sav /= dd .OR. p1_sav /= p1 .OR. iop_sav /= iop .OR. &
        lev_sav /= lev) GOTO 9996
    ENDIF

!   Salvo i valori
    CALL grib_get(igin(kf),"values",field_in(:,kf))

  ENDDO

! 2.2 Calcolo ensamble median ed ensamble mean
  unassigned(:,:) = .TRUE.
  field_in_sorted(:,:) = rmis
  DO kk = 1,nfiles
    field_in_sorted(:,kk) = MINVAL(field_in(:,:), DIM=2, MASK=unassigned(:,:))
    idx_min(:) = MINLOC(field_in(:,:), DIM=2, MASK=unassigned(:,:))
    DO k2 = 1,ni*nj
      unassigned(k2,idx_min(k2)) = .FALSE.
    ENDDO

    IF (ldeb) THEN
      WRITE (95,*) "Calcolo mediana: "
      WRITE (95,*) kk," --- ",idx_min(pt)
      WRITE (95,*) field_in(pt,:) 
      WRITE (95,*) field_in_sorted(pt,:) 
      WRITE (95,*) unassigned(pt,:)
    ENDIF

  ENDDO

  IF (MOD(nfiles,2) == 0) THEN
    field_out = (field_in_sorted(:,nfiles/2) + field_in_sorted(:,nfiles/2+1)) / 2.
  ELSE
    field_out = field_in_sorted(:,nfiles/2+1)
  ENDIF

  field_ave(:) = SUM(field_in(:,:),DIM=2) / REAL(nfiles)

! 2.3 Controlli
  IF (COUNT(field_in_sorted(:,:) == rmis) /= 0) WRITE (*,*) "Errore median (1)"
  IF (ldeb) THEN
    chfmt = '(a,i3,i4,i5,a,4(1x,e10.3))'
  ELSE
    chfmt = '(a,i3,i4,i5,a,4(1x,f7.1))'
  ENDIF

  WRITE (*,chfmt) &
    "Scad, var, lev ",p1,iop,lev," med,ave,min,max ", &
    SUM(field_out(:)) / REAL(ni*nj), &
    SUM(field_ave(:)) / REAL(ni*nj), &
    SUM(field_in_sorted(:,1)) / REAL(ni*nj), &
    SUM(field_in_sorted(:,nfiles)) / REAL(ni*nj)

! 2.5 Scrivo il grib "mediana" su fileout
  CALL grib_clone(igin(1),igout)
  CALL grib_set(igout,"values",field_out)
  CALL grib_write (igout,ifout)

! 2.6 Scrivo valori per il punto di test
  IF (ldeb) THEN
    WRITE (95,*) "Scad, var, lev ",p1,iop,lev
    WRITE (95,*) field_in(pt,:)
    WRITE (95,*) field_in_sorted(pt,:)
    WRITE (95,*) field_ave(pt)
    WRITE (95,*) field_out(pt)
  ENDIF

ENDDO fields

WRITE (*,*) "Elaborazioni completate, elaborati ",kg-1," campi"
STOP

!--------------------------------------------------------------------------

9999 CONTINUE
WRITE (*,*) "Errore aprendo grib_calc_median.lst"
STOP 2

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein(kf))
STOP 2

9997 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein(kf))," campo ",kg
STOP 2

9996 CONTINUE
WRITE (*,*) "Campi disallineati: file ",kf," grib ",kg
IF (ni /= ni_sav .OR. nj /= nj_sav) &
  WRITE (*,*) "ni,nj,ni_sav,nj_sav ",ni,nj,ni_sav,nj_sav
IF (dd_sav /= dd) WRITE (*,*) "dd,dd_sav ",dd,dd_sav
IF (p1_sav /= p1) WRITE (*,*) "p1,p1_sav ",p1,p1_sav
IF (iop_sav /= iop) WRITE (*,*) "iop,iop_sav ",iop,iop_sav
IF (lev_sav /= lev) WRITE (*,*) "lev,lev_sav ",lev,lev_sav
STOP 3

END PROGRAM grib_calc_median
