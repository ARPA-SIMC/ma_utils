PROGRAM diff_header_grib
!--------------------------------------------------------------------------
! Legge da due files con molti grib e scrive le differenze tra gli header
!
!                                         Versione 2.1.1, Enrico 13/01/2014
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
INTEGER, PARAMETER :: maxdim = 1000000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0a(2),ksec1a(1024),ksec2a(1024),ksec3a(2),ksec4a(512)
INTEGER :: ksec0b(2),ksec1b(1024),ksec2b(1024),ksec3b(2),ksec4b(512)

INTEGER :: kbuffer(maxdim), klen, kret
REAL :: psec2a(512),psec3a(2),psec2b(512),psec3b(2)
REAL :: field_a(maxdim),field_b(maxdim)

! Altre variabili del programma
INTEGER :: ios(6),k,kg,ks1_max,ks2_max,ks3_max,ks4_max,ps2_max,ps3_max
INTEGER :: iuin1,iuin2,np,idp,kp
CHARACTER (LEN=200) :: filein1,filein2,chdum,opt
LOGICAL :: show_all,idx_all

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
filein1 = ""
filein2 = ""
show_all = .FALSE.
idx_all = .FALSE.

idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "-showall") THEN
    show_all = .TRUE.
  ELSE IF (TRIM(chdum) == "-idxall") THEN
    idx_all = .TRUE.
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein1 = chdum
    CASE (2)
      filein2 = chdum
    CASE DEFAULT
      CALL write_help
      STOP
    END SELECT
  ENDIF
ENDDO

IF (filein1 == "" .OR. filein2 == "") THEN
  CALL write_help
  STOP
ENDIF

IF (idx_all) THEN
  ks1_max = 1024
  ks2_max = 1024
  ks3_max = 2
  ks4_max = 512
  ps2_max = 512
  ps3_max = 2
ELSE
  ks1_max = 24
  ks2_max = 19
  ks3_max = 2
  ks4_max = 11
  ps2_max = 50
  ps3_max = 2
ENDIF

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
CALL PBOPEN (iuin1,filein1,'R',kret)
IF (kret /= 0) THEN 
  WRITE(*,*) "Errore aprendo ",filein1," kret ",kret
  STOP
ENDIF

CALL PBOPEN (iuin2,filein2,'R',kret)
IF (kret /= 0) THEN 
  WRITE(*,*) "Errore aprendo ",filein2," kret ",kret
  STOP
ENDIF

!--------------------------------------------------------------------------
! 2) Lettura e confronto

WRITE (*,'(a)') "Campo;sezione;chiave;valore-file1;valore-file2"

DO kg = 1, HUGE(0)

! 2.1) Leggo e decodifico il grib a
  CALL PBGRIB(iuin1,kbuffer,maxdim*4,klen,kret)
  IF (kret.eq.-1) THEN 
    EXIT
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF
  
  CALL GRIBEX (ksec0a,ksec1a,ksec2a,psec2a,ksec3a,psec3a,ksec4a, &
               field_a,maxdim,kbuffer,maxdim,klen,'D',kret)
  if (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! 2.2) Leggo & decodifico il grib b
  CALL PBGRIB(iuin2,kbuffer,maxdim*4,klen,kret)
  IF (kret.eq.-1) THEN 
    EXIT
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  CALL GRIBEX (ksec0b,ksec1b,ksec2b,psec2b,ksec3b,psec3b,ksec4b, &
               field_b,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! 2.3) Visulizzo differenze negli header
  DO k = 1,ks1_max
    IF (ksec1a(k) /= ksec1b(k) .OR. (show_all .AND. k <= 24)) &
      WRITE (*,'(i6,a,i4,a,i8,a,i8)') &
      kg,";ksec1;",k,";",ksec1a(k),";",ksec1b(k)
  ENDDO
  DO k = 1,ks2_max
    IF (ksec2a(k) /= ksec2b(k) .OR. (show_all .AND. k <= 19)) &
      WRITE (*,'(i6,a,i4,a,i8,a,i8)') &
      kg,";ksec2;",k,";",ksec2a(k),";",ksec2b(k)
  ENDDO
  DO k = 1,ks3_max
    IF (ksec3a(k) /= ksec3b(k) .OR. show_all) &
      WRITE (*,'(i6,a,i4,a,i8,a,i8)') &
      kg,";ksec3;",k,";",ksec3a(k),";",ksec3b(k)
  ENDDO
  DO k = 1,ks4_max
    IF (ksec4a(k) /= ksec4b(k) .OR. (show_all .AND. k <= 11)) &
      WRITE (*,'(i6,a,i4,a,i8,a,i8)') &
      kg,";ksec4;",k,";",ksec4a(k),";",ksec4b(k)
  ENDDO

  DO k = 1,ps2_max
    IF (psec2a(k) /= psec2b(k)) WRITE (*,'(i6,a,i4,a,e12.5,a,e12.5)') &
      kg,";psec2;",k,";",psec2a(k),";",psec2b(k)
  ENDDO
  DO k = 1,ps3_max
    IF (psec3a(k) /= psec3b(k) .OR. show_all) &
      WRITE (*,'(i6,a,i4,a,e12.5,a,e12.5)') &
      kg,";psec3;",k,";",psec3a(k),";",psec3b(k)
  ENDDO

ENDDO
  
!--------------------------------------------------------------------------
! 3) Conclusione

CALL PBCLOSE (iuin1,kret)
CALL PBCLOSE (iuin2,kret)

END PROGRAM diff_header_grib

!--------------------------------------------------------------------------

SUBROUTINE write_help

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: diff_header_grib.exe [-h] file1 file2 [-showall] [-idxall]"
WRITE (*,*) "Legge da due files con molti grib e scrive le differenze tra gli header"
WRITE (*,*) "  -showall: stampa tutti i valori degli header"
WRITE (*,*) "  -idxall:  esamina anche gli elementi non significativi deli header (indici alti)"
RETURN

END SUBROUTINE write_help
