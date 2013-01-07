PROGRAM split_grib_par
!--------------------------------------------------------------------------
! Legge un file con molti grib e lo riscrive mettendo i grib relativi allo
! stesso parametro (o allo stesso parametro/tipo livelo) in files diversi.
!
! Note:
! Versioni <4.0 rinominate in split_gribex_par.f90
!
!                                           Versione 4.0, Enrico 01/03/2012
!--------------------------------------------------------------------------

USE grib_api
USE grib2_utilities
IMPLICIT NONE

! Parametri costanti
INTEGER, PARAMETER :: maxpar = 75       ! n.ro max di parametri diversi 
INTEGER, PARAMETER :: maxlev = 75       ! n.ro max di livelli diversi 

! Altre variabili del programma
INTEGER :: ifin,ifout(maxpar,maxlev),igrb
INTEGER :: cnt,kv,kl,kp,split_lev,iret,ier
INTEGER :: par_id(3,maxpar),lev_id(3,maxlev),par(3),lev(3)
INTEGER :: parc,levc,npar,nlev
CHARACTER (LEN=80) :: filein,fileout,chpar
LOGICAL :: opened(maxpar,maxlev)

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
split_lev = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chpar)
  IF (chpar == "") THEN
    EXIT
  ELSE IF (TRIM(chpar) == "-h") THEN
    CALL scrive_help
    STOP
  ELSE IF (TRIM(chpar) == "-lkind") THEN
    split_lev = 1
  ELSE IF (TRIM(chpar) == "-lev") THEN
    split_lev = 2
  ELSE IF (TRIM(chpar) == "-levl") THEN
    split_lev = 3
  ELSE
    filein = chpar    
  ENDIF
ENDDO

IF (filein == "") THEN
  CALL scrive_help
  STOP
ENDIF

! 1.2 Apro input file
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999

!--------------------------------------------------------------------------
! 2) Leggo / Scrivo (ciclo sui grib)

par_id(:,:) = -999
lev_id(:,:) = -999
opened(:,:) = .FALSE.
npar = 0
nlev = 0

DO cnt = 1,HUGE(cnt)

! 2.1 Leggo il prossimo grib 
  CALL grib_new_from_file(ifin,igrb,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9996
  CALL get_grib1_header(igrb, PAR=par, LEV=lev, IRET=ier)
  IF (ier /= 0) GOTO 9995

! 2.2 Verifico se ho trovato un nuovo parametro
  parc = -999
  DO kv = 1,npar
    IF (ALL(par(1:3) == par_id(1:3,kv))) THEN
      parc = kv
      EXIT
    ENDIF
  ENDDO

  IF (parc == -999) THEN                        ! nuovo parametro
    IF (npar >= maxpar) GOTO 9998
    npar = npar + 1
    parc = npar
    par_id(1:3,npar) = par(1:3)
  ENDIF

! 2.3 Se e' richiesto di distinguere tra livelli o tipi di livello, 
!     verifico se ne ho trovato uno nuovo

  IF (split_lev == 0) THEN
    levc = 1

  ELSE IF (split_lev == 1) THEN
    levc = -999
    DO kl = 1,nlev
      IF (lev(1) == lev_id(1,kl)) THEN
        levc = kl
        EXIT
      ENDIF
    ENDDO
  
    IF (levc == -999) THEN                      ! nuovo tipo di livello
      IF (nlev >= maxlev) GOTO 9997
      nlev = nlev + 1
      levc = nlev
      lev_id(1,nlev) = lev(1)
    ENDIF

  ELSE IF (split_lev == 2 .OR. split_lev == 3) THEN
    levc = -999
    DO kl = 1,nlev
      IF (ALL(lev(1:3) == lev_id(1:3,kl))) THEN
        levc = kl
        EXIT
      ENDIF
    ENDDO

    IF (levc == -999) THEN                      ! nuovo tipo di livello
      IF (nlev >= maxlev) GOTO 9997
      nlev = nlev + 1
      levc = nlev
      lev_id(1:3,nlev) = lev(1:3)
    ENDIF

  ENDIF
    
! 2.4 Se necessario, apro un nuovo file
  IF (.NOT. opened(parc,levc)) THEN
    IF (split_lev == 0) THEN
      WRITE (fileout,'(a,3(i3.3,a))') "sg_",par_id(1,parc),"_", &
        par_id(2,parc),"_",par_id(3,parc),".grb"
    ELSE IF (split_lev == 1) THEN
      WRITE (fileout,'(a,4(i3.3,a))') "sg_",par_id(1,parc),"_", &
        par_id(2,parc),"_",par_id(3,parc),"_",lev_id(1,levc),".grb"
    ELSE IF (split_lev == 2) THEN
      WRITE (fileout,'(a,6(i3.3,a))') "sg_", &
        par_id(1,parc),"_",par_id(2,parc),"_",par_id(3,parc),"_", &
        lev_id(1,levc),"_",lev_id(2,levc),"_",lev_id(3,levc),".grb"
    ELSE IF (split_lev == 3) THEN
      WRITE (fileout,'(a,4(i3.3,a),2(i4.4,a))') "sg_", &
        par_id(1,parc),"_",par_id(2,parc),"_",par_id(3,parc),"_", &
        lev_id(1,levc),"_",lev_id(2,levc),"_",lev_id(3,levc),".grb"
    ENDIF

    CALL grib_open_file(ifout(parc,levc),fileout,"w")
    opened(parc,levc) = .TRUE.
  ENDIF

! 2.5 Scrivo il grib sul file giusto
  CALL grib_write (igrb,ifout(parc,levc))
  CALL grib_release(igrb)

ENDDO

!--------------------------------------------------------------------------
! 3) Log e conclusione

WRITE (*,'(2(a,i3))') "Parametri diversi ",npar,", livelli diversi ",nlev
WRITE (*,'(a,i3,a,i8)') "Files scritti ",COUNT(opened(:,:)),", totale grib ",cnt-1
CALL grib_close_file(ifin)

STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Ci sono troppi parametri diversi, aumentare maxpar"
STOP

9997 CONTINUE
WRITE (*,*) "Ci sono troppi livelli diversi, aumentare maxlev"
STOP

9996 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," campo ",cnt
STOP

9995 CONTINUE
WRITE (*,*) "Errore get_grib1_header ",TRIM(filein)," campo ",cnt, &
  " ier ",ier
STOP

END PROGRAM split_grib_par

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Scrive a schermo un breve help
!
IMPLICIT NONE
!            12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) "Uso: split_grib_par filein [-lkind] [-lev] [-levl] [-h]"
WRITE (*,*) "Legge un file con molti grib; scrive ciascun parametro in un file diverso"
WRITE (*,*) "-lkind: distingue anche il tipo di livello "
WRITE (*,*) "-lev: distingue anche il livello"
WRITE (*,*) "-levl: distingue il livello e usa 4 cifre per scriverlo (output calmet)"

RETURN
END SUBROUTINE scrive_help
