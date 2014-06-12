PROGRAM grib_stat
!--------------------------------------------------------------------------
! Legge un file son molti grib; scrive il file STAT.grib, con intestazione
! uguale al primo file in input e valori relativi alla statistica richiesta.
!
!                                         Versione 2.1.0, Enrico 09/06/2014
!--------------------------------------------------------------------------

USE grib_api
USE missing_values
USE grib2_utilities
IMPLICIT NONE

INTEGER, ALLOCATABLE :: val_nok(:),val_nex(:)
INTEGER :: ifin=0,ifout=0,igin=0,igout=0
INTEGER :: iret,ier,ios,ios2,clret(0:5),kg,kp,ks,kpar
INTEGER :: iv_stat,idp,idp_req
INTEGER :: ni,nj,nocv,out_nok,kptest
REAL :: out_max,out_min,out_ave,rv_stat
REAL, ALLOCATABLE :: field_in(:),field_out(:)
REAL, ALLOCATABLE :: val_max(:),val_min(:),val_sum(:),val_sum2(:)
REAL, ALLOCATABLE :: val_nth(:,:),old_val(:)
CHARACTER(LEN=200) :: filein,fileout,chdum
CHARACTER(LEN=3) :: stat
CHARACTER(LEN=1) :: next_arg
LOGICAL :: ldeb

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
ldeb = .FALSE.
next_arg = ""
idp = 0
ios = 0
ios2 = 0
iv_stat=imiss
rv_stat=rmiss
DO kpar = 1,HUGE(0)
  CALL getarg(kpar,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1
  ELSE IF (TRIM(chdum) == "-deb") THEN
    ldeb = .TRUE.
    next_arg = "D"
  ELSE IF (next_arg == "D") THEN
    READ (chdum,*,IOSTAT=ios2) kptest
    next_arg = ""
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      stat = TRIM(ADJUSTL(chdum))
    CASE (3)
      IF (stat == "nth" .OR. stat == "ntl") THEN
        READ (chdum,*,IOSTAT=ios) iv_stat
      ELSE IF (stat == "nex") THEN
        READ (chdum,*,IOSTAT=ios) rv_stat
      ENDIF
    CASE DEFAULT
      CALL write_help
      STOP 1
    END SELECT
  ENDIF
ENDDO

! Controlli sui parametri
IF (stat/="max" .AND. stat/="min" .AND. stat/="ave" .AND. stat/="std" .AND. &
    stat/="nth" .AND. stat/="ntl" .AND.  stat/="nex") THEN
  CALL write_help
  STOP 1
ENDIF  

IF (stat=="nth" .OR. stat=="ntl" .OR. stat=="nex") THEN
  idp_req = 3
ELSE
  idp_req = 2
ENDIF
IF (idp /= idp_req .OR. ios /= 0 .OR. ios2 /= 0) THEN
  CALL write_help
  STOP 1
ENDIF  

! Apro i files input
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
WRITE (fileout,'(2a)') stat,".grib"
CALL grib_open_file(ifout,fileout,"w")
IF (ldeb) OPEN (UNIT=95, file="grib_stat.log", STATUS="REPLACE")

!--------------------------------------------------------------------------
! 2) Lettura

DO kg = 1,HUGE(0)

!--------------------------------------------------------------------------
! 2.1 Leggo il prossimo campo
  igin = -1
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998

  IF (kg == 1) THEN
!   Primo campo: alloco e inizializzo i contatori statistici
    CALL grib_get(igin,"Ni",ni)
    CALL grib_get(igin,"Nj",nj)
    ALLOCATE (field_in(ni*nj),field_out(ni*nj),val_nok(ni*nj))

    val_nok = 0
    IF (stat =="max") THEN
      ALLOCATE (val_max(ni*nj))
      val_max(:) = rmiss
    ELSE IF (stat =="min") THEN
      ALLOCATE (val_min(ni*nj))
      val_min(:) = rmiss
    ELSE IF (stat == "ave") THEN
      ALLOCATE (val_sum(ni*nj))
      val_sum(:) = 0.
    ELSE IF (stat == "std") THEN
      ALLOCATE (val_sum(ni*nj),val_sum2(ni*nj))
      val_sum(:) = 0.
      val_sum2(:) = 0.
    ELSE IF (stat == "nth" .OR. stat == "ntl") THEN
      ALLOCATE(val_nth(ni*nj,iv_stat),old_val(iv_stat))
      val_nth(:,:) = rmiss
    ELSE IF (stat == "nex") THEN
      ALLOCATE(val_nex(ni*nj))
      val_nex(:) = 0
    ENDIF

    CALL grib_clone(igin,igout)

  ELSE
!   Altri campi: controllo che la griglia non sia cambiata
    CALL check_consistency(igin,igout,.TRUE.,.FALSE.,.FALSE.,.FALSE., &
      .FALSE.,.FALSE.,clret,ier)
    IF (ier /= 0) GOTO 9997

  ENDIF

! Leggo i valori
  CALL grib_get(igin,"numberOfCodedValues",nocv)  ! n.ro dati validi
  IF (nocv == 0) THEN
    field_in(:) = rmiss
  ELSE
    CALL grib_set(igin,"missingValue",rmiss)
    CALL grib_get(igin,"values",field_in)
  ENDIF

!--------------------------------------------------------------------------
! 2.2 Aggiorno i contatori statistici

  WHERE (field_in(:) /= rmiss)
    val_nok(:) = val_nok(:) + 1
  ENDWHERE

  IF (stat =="max") THEN
    WHERE (field_in(:) /= rmiss .AND. &
           (field_in(:) > val_max(:) .OR. val_max(:) == rmiss))
      val_max(:) = field_in(:) 
    ENDWHERE

  ELSE IF (stat == "min") THEN
    WHERE (field_in(:) /= rmiss .AND. &
           (field_in(:) < val_min(:) .OR. val_min(:) == rmiss))
      val_min(:) = field_in(:) 
    ENDWHERE

  ELSE IF (stat == "ave") THEN
    WHERE (field_in(:) /= rmiss)
      val_sum(:) = val_sum(:) + field_in(:)
    ENDWHERE

  ELSE IF (stat == "std") THEN
    WHERE (field_in(:) /= rmiss)
      val_sum(:) = val_sum(:) + field_in(:)
      val_sum2(:) = val_sum2(:) + field_in(:)**2
    ENDWHERE

  ELSE IF (stat == "nth") THEN
    DO kp = 1,ni*nj
      nth: DO ks = 1,iv_stat
        IF (field_in(kp) /= rmiss .AND. &
            (field_in(kp) > val_nth(kp,ks) .OR. val_nth(kp,ks) == rmiss)) THEN
          old_val(ks:iv_stat-1) = val_nth(kp,ks:iv_stat-1)
          val_nth(kp,ks+1:iv_stat) =  old_val(ks:iv_stat-1)
          val_nth(kp,ks) = field_in(kp)  

          IF (ldeb .AND. kp == kptest) WRITE (95,*) kg,field_in(kp),"###", &
              val_nth(kp,1:iv_stat)
          EXIT nth
        ENDIF  
      ENDDO nth
    ENDDO

  ELSE IF (stat == "ntl") THEN
    DO kp = 1,ni*nj
      ntl: DO ks = 1,iv_stat
        IF (field_in(kp) /= rmiss .AND. &
            (field_in(kp) < val_nth(kp,ks) .OR. val_nth(kp,ks) == rmiss)) THEN
          old_val(ks:iv_stat-1) = val_nth(kp,ks:iv_stat-1)
          val_nth(kp,ks+1:iv_stat) =  old_val(ks:iv_stat-1)
          val_nth(kp,ks) = field_in(kp)  

          IF (ldeb .AND. kp == kptest) WRITE (95,*) kg,field_in(kp),"###", &
              val_nth(kp,1:iv_stat)
          EXIT ntl
        ENDIF  
      ENDDO ntl
    ENDDO

  ELSE IF (stat == "nex") THEN
    WHERE (field_in(:) /= rmiss .AND. field_in(:) > rv_stat)
      val_nex(:) = val_nex(:) + 1
    ENDWHERE

  ENDIF

  CALL grib_release(igin)
  IF (ldeb .AND. MOD(kg,1000)==0) WRITE (*,*) "Ealborato grib ",kg
ENDDO

!--------------------------------------------------------------------------
! 3) Calcolo le statistiche e scrivo in output

! Calcolo le statistiche
IF (stat =="max") THEN
  field_out(:) = val_max(:)

ELSE IF (stat =="min") THEN
  field_out(:) = val_min(:)

ELSE IF (stat == "ave") THEN
  WHERE (val_nok(:) > 0)
    field_out(:) = val_sum(:) / REAL(val_nok(:))
  ELSEWHERE
    field_out(:) = rmiss
  ENDWHERE

ELSE IF (stat == "std") THEN
  WHERE (val_nok(:) > 0)
    field_out(:) = SQRT(MAX( val_sum2(:)/REAL(val_nok(:)) - &
      (val_sum(:)/REAL(val_nok(:)))**2 ,0.))
  ELSEWHERE
    field_out(:) = rmiss
  ENDWHERE

ELSE IF (stat == "nth" .OR. stat == "ntl") THEN
  WHERE (val_nok(:) > 0)
    field_out(:) = val_nth(:,iv_stat)
  ELSEWHERE
    field_out(:) = rmiss
  ENDWHERE

ELSE IF (stat =="nex") THEN
  field_out(:) = REAL(val_nex(:))

ENDIF

! Assegno i valori al campo di output
IF (COUNT(field_out(:) == rmiss) > 0) THEN
  CALL grib_set(igout,"missingValue",rmiss)
  CALL grib_set(igout,"bitmapPresent",1)
ENDIF
CALL grib_set(igout,"values",field_out)

! Scrivo output
CALL grib_write (igout,ifout)

! Conclusione
CALL grib_get(igout,"maximum",out_max)
CALL grib_get(igout,"minimum",out_min)
CALL grib_get(igout,"average",out_ave)
CALL grib_get(igout,"numberOfCodedValues",out_nok)

WRITE (*,*) "Elaborazioni completate, elaborati ",kg-1," campi"
WRITE (*,*) "In output: dati validi: ",out_nok,"/",ni*nj
WRITE (*,*) "           ave,max,min: ",out_ave,out_max,out_min

CALL grib_release(igout)
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
WRITE (*,*) "Trovato comap con griglia diversa dal primo: ",kg
STOP 3

END PROGRAM grib_stat

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: grib_stat.exe [-h] [-deb] filein STAT [N]"
WRITE (*,*)
WRITE (*,*) "Legge un file son molti grib; scrive il file stat.grib, con intestazione"
WRITE (*,*) "uguale al primo file in input e valori relativi alla statistica richiesta."
WRITE (*,*) "Statistiche gestite:"
WRITE (*,*) "  max  : massimo"
WRITE (*,*) "  min  : minimo"
WRITE (*,*) "  ave  : media"
WRITE (*,*) "  std  : deviazione standard"
WRITE (*,*) "  nth N: N-mo valore piu' alto"
WRITE (*,*) "  ntl N: N-mo valore piu' basso"
WRITE (*,*) "  nex X: Numero di valori maggiori di X"
WRITE (*,*) ""
WRITE (*,*) "-h:    visualizza questo help"
WRITE (*,*) "-deb N scrive su grib_stat.log il debug relativo al punto N"
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
