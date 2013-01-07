PROGRAM calc_ps_cosmo
!--------------------------------------------------------------------------
! Legge orografia e GPH sui pressure levels di un run Cosmo; scrive PS.
! Gestisce input triorario, ma scrive comunque campi orari
!
! L'errore finale nel calcolo di Ps e' di circa 0.1 hPa, ma in alcuni casi
! si possono verificare errori fino a 1 hPa (dovuti principalmente 
! all' interpolazione temporale di GPH)
!
!                                          Versione 1.0.1 Enrico 27/11/2012 
!--------------------------------------------------------------------------

USE grib_api
USE grib2_utilities
IMPLICIT NONE

REAL, ALLOCATABLE :: orog(:),gph_t1(:,:),gph_t2(:,:),gph_t(:,:),ps(:)
REAL :: w1,w2
INTEGER, ALLOCATABLE :: igin_first(:),plevs(:)
INTEGER :: idp,kp,gph_step,gph_levs,ios,ios1,ios2,iret,clret(0:5),ier
INTEGER :: ifin,ifgeo,ifout,igin,iggeo,igout
INTEGER :: kl,ksc,kout,cnt_out,nextrp
INTEGER :: t2v,iop,iotol,tri,dd,dt,ni,nj,np,p1
INTEGER :: dd_first,dt_first,p1_t1,p1_t2,p1_out
CHARACTER (LEN=250) :: filein,filegeo,fileout,chdum
CHARACTER (len=1) :: next_arg
LOGICAL, PARAMETER :: ldeb = .FALSE.
INTEGER, PARAMETER :: ptest = 13

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
gph_step = 3
gph_levs = 5
next_arg = ""
ios1 = 0
ios2 = 0
idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "-step") THEN
    next_arg = "s"
  ELSE IF (next_arg == "s") THEN
    READ (chdum,*,IOSTAT=ios1) gph_step
    next_arg = ""
  ELSE IF (TRIM(chdum) == "-levs") THEN
    next_arg = "l"
  ELSE IF (next_arg == "l") THEN
    READ (chdum,*,IOSTAT=ios2) gph_levs
    next_arg = ""
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      filegeo = chdum
    CASE (3)
      fileout = chdum
    CASE DEFAULT
      CALL write_help
      STOP
    END SELECT
  ENDIF
ENDDO

IF (idp /= 3 .OR. ios1 /= 0 .OR. ios2 /= 0) THEN
  CALL write_help
  STOP
ENDIF  

! Leggo orografia
ifgeo = 0
iggeo = 0
CALL grib_open_file(ifgeo,filegeo,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_new_from_file(ifgeo,iggeo,iret)

CALL grib_get (iggeo,"Ni",ni)
CALL grib_get (iggeo,"Nj",nj)
CALL grib_get (iggeo,"indicatorOfParameter",iop)
CALL grib_get (iggeo,"table2Version",t2v)
IF (iop /= 8 .OR. t2v /= 2) GOTO 9998
np = ni*nj

! Apro i files I/O
ifin = 0
ifout = 0
igin = 0
igout = 0
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9997
CALL grib_open_file(ifout,fileout,"w")

IF (ldeb) OPEN (UNIT=90, FILE="calc_ps_cosmo.log", STATUS="REPLACE", &
  FORM="FORMATTED")

! Alloco array
ALLOCATE (orog(np),ps(np))
ALLOCATE (gph_t1(gph_levs,np),gph_t2(gph_levs,np),gph_t(gph_levs,np))
ALLOCATE (igin_first(gph_levs),plevs(gph_levs))
igin_first(:) = 0
plevs(:) = 0

! Salvo dati orografia
CALL grib_get(iggeo,"values",orog(1:np))

!--------------------------------------------------------------------------
! 2) Ciclo sulle scadenze

!--------------------------------------------------------------------------
! 2.1 Elaboro la prima scadenza

cnt_out = 0
gph_t(:,:) = 0
gph_t1(:,:) = 0
gph_t2(:,:) = 0

ksc = 1
DO kl = 1, gph_levs
  CALL grib_new_from_file(ifin,igin_first(kl),iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9996

  CALL  check_consistency(igin_first(kl),iggeo, &
    .TRUE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,clret,ier)
  IF (ier /= 0) GOTO 9995
  CALL grib_get (igin_first(kl),"indicatorOfParameter",iop)
  CALL grib_get (igin_first(kl),"table2Version",t2v)
  CALL grib_get (igin_first(kl),"indicatorOfTypeOfLevel",iotol)
  CALL grib_get (igin_first(kl),"level",plevs(kl))
  CALL grib_get (igin_first(kl),"timeRangeIndicator",tri)
  IF (iop /= 6 .OR. t2v /= 2 .OR. iotol /= 100 .OR. tri /= 0) GOTO 9994

  CALL grib_get (igin_first(kl),"dataDate",dd)
  CALL grib_get (igin_first(kl),"dataTime",dt)
  IF (kl == 1) THEN
    dd_first = dd
    dt_first = dt
  ELSE
    IF (dd /= dd_first .OR. dt /= dt_first) GOTO 9993
  ENDIF

  CALL grib_get (igin_first(kl),"P1",p1)
  IF (kl == 1) THEN
    p1_t2 = p1    
  ELSE
    IF (p1 /= p1_t2) GOTO 9992
  ENDIF

  IF (kl > 1) THEN
    IF (plevs(kl) <= plevs(kl-1)) GOTO 9991
  ENDIF

  CALL grib_get(igin_first(kl),"values",gph_t1(kl,1:np))
ENDDO

! Calcolo PS
gph_t2(1:gph_levs,1:np) = gph_t1(1:gph_levs,1:np)
gph_t(1:gph_levs,1:np) = gph_t1(1:gph_levs,1:np)
CALL calc_ps(gph_t(1:gph_levs,1:np),orog(1:np),plevs(1:gph_levs), &
  np,gph_levs,ps,nextrp)

! Scrivo il grib in output
CALL grib_clone(igin_first(1),igout)
CALL grib_set(igout,"indicatorOfTypeOfLevel",1)
CALL grib_set(igout,"level",0)
CALL grib_set(igout,"table2Version",2)
CALL grib_set(igout,"indicatorOfParameter",1)
CALL grib_set(igout,"values",ps(1:np))
CALL grib_write(igout,ifout)
cnt_out = cnt_out + 1
WRITE (*,'(a,i8.8,i4.4,a2,i3.3,a,i5,a)') "Scritto Ps: ", &
  dd_first,dt_first," +",p1_t2," estrap. in ",nextrp," punti"

!--------------------------------------------------------------------------
! 2.2 Elaboro le scadenze successive

scad: DO ksc = 2,HUGE(0)
  gph_t(:,:) = 0
  gph_t1(:,:) = gph_t2(:,:)
  p1_t1 = p1_t2
  
! 2.2.1 Leggo GPH per tutti i livelli, relativo alla prossima scadenza in input
  DO kl = 1, gph_levs
    CALL grib_new_from_file(ifin,igin,iret)
    IF (iret == GRIB_END_OF_FILE) EXIT scad
    IF (iret /= GRIB_SUCCESS) GOTO 9996
    CALL check_consistency(igin,igin_first(kl), &
      .TRUE.,.FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,clret,ier)
    IF (ier /= 0) GOTO 9995

    CALL grib_get (igin,"dataDate",dd)
    CALL grib_get (igin,"dataTime",dt)
    IF (dd /= dd_first .OR. dt /= dt_first) GOTO 9993

    CALL grib_get (igin,"P1",p1)
    IF (kl == 1) THEN
      p1_t2 = p1    
    ELSE
      IF (p1 /= p1_t2) GOTO 9992
    ENDIF

    CALL grib_get(igin,"values",gph_t2(kl,1:np))
  ENDDO
  
! 2.2.2 Interplo nel tempo, calcolo PS, scrivo in output  
  DO kout = 1,gph_step

!   Interplo nel tempo
    w1 = REAL(gph_step - kout) / REAL(gph_step)
    w2 = REAL(kout) / REAL(gph_step)
    IF (ABS(w1 + w2 - 1.) > 0.0001) THEN
      print *,w1,w2,gph_step,kout
      STOP "Erroraccio W"
    ENDIF

    gph_t(1:gph_levs,1:np) = w1*gph_t1(1:gph_levs,1:np) + w2*gph_t2(1:gph_levs,1:np)
    p1_out = NINT(w1*REAL(p1_t1) + w2*REAL(p1_t2))

!   Calcolo PS
    CALL calc_ps(gph_t(1:gph_levs,1:np),orog(1:np),plevs(1:gph_levs), &
      np,gph_levs,ps,nextrp)
    
!   Scrivo il grib in output
    CALL grib_clone(igin_first(1),igout)
    CALL grib_set(igout,"indicatorOfTypeOfLevel",1)
    CALL grib_set(igout,"level",0)
    CALL grib_set(igout,"table2Version",2)
    CALL grib_set(igout,"indicatorOfParameter",1)
    CALL grib_set(igout,"P1",p1_out)
    CALL grib_set(igout,"values",ps(1:np))
    CALL grib_write (igout,ifout)

!   Log a schermo
    cnt_out = cnt_out + 1
    WRITE (*,'(a,i8.8,i4.4,a2,i3.3,a,i5,a)') "Scritto Ps: ", &
      dd_first,dt_first," +",p1_out," estrap. in ",nextrp," punti"


    IF (ldeb) THEN
      WRITE (90,*)
      WRITE (90,'(a,i8.8,i2.2,a,3(1x,i3.3))') &
        "data ",dd,dt," scad1,scad2,scad_out ",p1_t1,p1_t2,p1_out
      WRITE (90,*) "gph_t1 ",gph_t1(1:gph_levs,ptest)
      WRITE (90,*) "gph_t2 ",gph_t2(1:gph_levs,ptest)
      WRITE (90,*) "gph_t ",gph_t(1:gph_levs,ptest)
      WRITE (90,*) "w1,w2,ps ",w1,w2,ps(ptest)
    ENDIF

  ENDDO
ENDDO scad

!--------------------------------------------------------------------------
! 3) Conclusione

CALL grib_close_file(ifout)
CALL grib_close_file(ifgeo)
CALL grib_close_file(ifin)
WRite (*,*) "Scadenze in input ",ksc-1," campi in ouptut ",cnt_out
STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Il 1o campo del file ",TRIM(filegeo)," non e' orografia COSMO"
STOP

9997 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filegeo)
STOP

9996 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," scad ",ksc," lev ",kl
STOP

9995 CONTINUE
WRITE (*,*) "GRIB inatteso: ",TRIM(filein)," scad ",ksc," lev ",kl
IF (clret(1) == 1) WRITE (*,*) "Griglia inattesa"
IF (clret(1) == 4) WRITE (*,*) "Livello inatteso "
IF (clret(1) == 5) WRITE (*,*) "Parametro inatteso"
STOP

9994 CONTINUE
WRITE (*,*) "Parametro o tipo livello o timerange inatteso in ",TRIM(filein), &
  " scad ",ksc," lev ",kl
STOP

9993 CONTINUE
WRITE (*,*) "Reftime inattaeso in ",TRIM(filein), &
  " scad ",ksc," lev ",kl
STOP

9992 CONTINUE
WRITE (*,*) "Scadenza inattaesa in ",TRIM(filein), &
  " scad ",ksc," lev ",kl
WRITE (*,*) "Verificare il numero di livelli nel file input (opzione -levs)"
STOP

9991 CONTINUE
WRITE (*,*) "Scadenza 1, trovati livelli non ordinati dall'alto ",plevs(1:kl)
STOP

END PROGRAM calc_ps_cosmo

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE calc_ps(gph,orog,plevs,np,nlevs,ps,nextrp)

IMPLICIT NONE

INTEGER, INTENT(IN) :: np,nlevs,plevs(nlevs)
REAL, INTENT(IN) :: gph(nlevs,np),orog(np)
REAL, INTENT(OUT) :: ps(np)
INTEGER, INTENT(OUT) :: nextrp

REAL (KIND=8) :: z1,z2,zs,lnp1,lnp2,lnps
REAL, PARAMETER :: g = 9.8
INTEGER :: kp,lev1,lev2

!--------------------------------------------------------------------------

nextrp = 0
DO kp = 1,np

! Cerco l'ultimo livello isobarico sottoterra. I livelli sono ordinati dall'alto
  zs = DBLE(g*orog(kp))
  DO lev1 = 1,nlevs
    IF (gph(lev1,kp) < g*orog(kp)) EXIT
  ENDDO
  IF (lev1 >= nlevs) THEN
    lev1 = nlevs
    nextrp = nextrp + 1
  ELSE IF (lev1 == 1) THEN
    lev1 = 2
    nextrp = nextrp + 1
  ENDIF  
  lev2 = lev1 - 1

! Calcolo PS
  z1 = gph(lev1,kp)
  z2 = gph(lev2,kp)
  lnp1 = LOG(REAL(plevs(lev1)))
  lnp2 = LOG(REAL(plevs(lev2)))
  lnps = lnp1 + (zs-z1) * (lnp2-lnp1)/(z2-z1)
  ps(kp) = REAL(EXP(lnps)) * 100.

ENDDO

RETURN
END SUBROUTINE calc_ps

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: calc_aps_cosmo.exe [-h] filein filegeo fileout [-step N] [-levs N]"
WRITE (*,*) "  filein: GPH relativo a un run Cosmo sui livelli di pressione; ignora "
WRITE (*,*) "          gli altri parametri; deve essere ordinato per timerange,level"
WRITE (*,*) "          (arki-scan --data --sort=timerange,level filein > fileout)"
WRITE (*,*) "  filegeo: orografia Cosmo, sulla stessa area di filein"
WRITE (*,*) "  fileout: PS, sulla stessa area dei dati di input"
WRITE (*,*) "  -levs N: numero di pressure levels in input (default: 5)"
WRITE (*,*) "  -step N: step in ore dei dati in input (def: 3)" 
WRITE (*,*) ""

!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
