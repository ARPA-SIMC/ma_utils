PROGRAM grib_set_scanning
!--------------------------------------------------------------------------
! Legge un file con molti grib, e lo riscrive cambiando l'ordine dei punti
! (scanning mode flags). 
! Per ora gestisce solo l'inverisone nord-sud (scanniong mode 000 e 010)
! 
!                                         Versione 1.0.0, Enrico 29/05/2015
!--------------------------------------------------------------------------

USE grib_api
IMPLICIT NONE

INTEGER :: ifin=0,ifout=0,igin=0,igout=0,iret,kg
CHARACTER(LEN=200) :: filein,fileout,chdum
REAL, ALLOCATABLE :: field_in(:),field_out(:)

CHARACTER(LEN=3) :: smf
INTEGER :: idp,kp,cnt_mod,k1_in,k2_in,k1_out,k2_out,jj,ios
INTEGER :: isn,jsp,jpac,isn_req,jsp_req,jpac_req
INTEGER :: ni,nj,ni_sav,nj_sav
REAL :: latf,latl,lonf,lonl

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
      smf = chdum
    CASE DEFAULT
      CALL write_help
      STOP 1
    END SELECT
  ENDIF
ENDDO

! Spacchetto lo scanning mode richiesto
READ (smf,'(3i1)',IOSTAT=ios) isn_req,jsp_req,jpac_req
IF (ios /= 0) GOTO 9999
IF ((isn_req/=0.AND.isn_req/=1) .OR. (isn_req/=0.AND.isn_req/=1) .OR. &
  (isn_req/=0.AND.isn_req/=1)) GOTO 9999
IF (isn_req /= 0 .OR. jpac_req /= 0) GOTO 9998

! Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9997
CALL grib_open_file(ifout,fileout,"w")

!--------------------------------------------------------------------------
! 2) Esecuzione (ciclo sui grib)


cnt_mod = 0 
DO kg = 1,HUGE(0)

! 2.1 Leggo il prossimo campo
  igin = -1
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9996

  CALL grib_get(igin,"numberOfPointsAlongAParallel",ni)
  CALL grib_get(igin,"numberOfPointsAlongAMeridian",nj)
  CALL grib_get(igin,"latitudeOfFirstGridPointInDegrees",latf)
  CALL grib_get(igin,"latitudeOfLastGridPointInDegrees",latl)
  CALL grib_get(igin,"longitudeOfFirstGridPointInDegrees",lonf)
  CALL grib_get(igin,"longitudeOfLastGridPointInDegrees",lonl)
  CALL grib_get(igin,"iScansNegatively",isn)
  CALL grib_get(igin,"jScansPositively",jsp)
  CALL grib_get(igin,"jPointsAreConsecutive",jpac)

  IF (isn /= 0 .OR. jpac /= 0) GOTO 9995

  IF (kg == 1) THEN
    ALLOCATE (field_in(ni*nj),field_out(ni*nj))
    ni_sav = ni
    nj_sav = nj
  ELSE IF (ni /= ni_sav .OR. nj /= nj_sav) THEN
    GOTO 9994
  ENDIF

! 2.2 Se necessario modifico l'ordine dei punti
  CALL grib_clone(igin,igout)

  IF (jsp /= jsp_req) THEN
    cnt_mod = cnt_mod + 1
    CALL grib_get(igin,"values",field_in(:))

    DO jj = 1,nj
      k1_in = (jj-1) * ni + 1
      k2_in = jj * ni
      k1_out = (nj-jj) * ni + 1
      k2_out = (nj-jj+1) * ni
      field_out(k1_out:k2_out) = field_in(k1_in:k2_in)
    ENDDO

    CALL grib_set(igout,"values",field_out(:))
    CALL grib_set(igout,"latitudeOfFirstGridPointInDegrees",latl)
    CALL grib_set(igout,"latitudeOfLastGridPointInDegrees",latf)
    CALL grib_set(igout,"jScansPositively",jsp_req)
  ENDIF

  CALL grib_write (igout,ifout)
  CALL grib_release(igin)
  CALL grib_release(igout)

ENDDO

!--------------------------------------------------------------------------
! 3) Conclusione

WRITE (*,*) "Elaborazioni completate: campi scritti ",kg-1," modificati ",cnt_mod

CALL grib_close_file(ifin)
CALL grib_close_file(ifout)
STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Richiesto sacnning mode illegale: ",smf
STOP 1

9998 CONTINUE
WRITE (*,*) "Richiesto sacnning mode non gestito: ",smf
STOP 1

9997 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 2

9996 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib n.ro " ,kg
STOP 2

9995 CONTINUE
WRITE (*,*) "Trovato sacnning mode non gestito: ",smf," campo ",kg
STOP 3

9994 CONTINUE
WRITE (*,*) "Trovato grib con numero di punti diverso, campo ",kg
WRITE (*,*) "atteso: ",ni_sav,nj_sav," trovato ",ni,nj
STOP 3

END PROGRAM grib_set_scanning

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: grib_set_scanning.exe [-h] filein fileout out_scanning"
WRITE (*,*) "out_scanning e' un intero a tre cifre IJK:"
WRITE (*,*) "I: chiave ""iScansNegatively"" (al momento solo 0)"
WRITE (*,*) "J: chiave ""jScansPositively"" (ECMWF: 0; COSMO: 1)"
WRITE (*,*) "K: chiave ""jPointsAreConsecutive"" (al momento solo 0)"
WRITE (*,*) ""
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
