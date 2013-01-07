PROGRAM replace_dat_grib
!--------------------------------------------------------------------------
! Pende il primo grib e lo riscrive, mettendo solo i valori del secondo.
!
!                                           Versione 1.0, Enrico 10/09/2009
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.           ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 1000000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0a(2),ksec1a(1024),ksec2a(1024),ksec3a(2),ksec4a(512)
INTEGER :: ksec0b(2),ksec1b(1024),ksec2b(1024),ksec3b(2),ksec4b(512)
INTEGER :: ksec0c(2),ksec1c(1024),ksec2c(1024),ksec3c(2),ksec4c(512)

INTEGER :: kbuffer(maxdim), klen, kret
REAL :: psec2a(512),psec3a(2),psec2b(512),psec3b(2),psec2c(512),psec3c(2)
REAL :: field_a(maxdim),field_b(maxdim),field_c(maxdim)

! Altre variabili del programma
REAL :: fk1,fk2
INTEGER :: ctv_out(3),ios(6),force_par_out,k
INTEGER :: iuin1,iuin2,iuout,np,kp,idp
CHARACTER (LEN=80) :: filein1,filein2,fileout,chdum
CHARACTER (LEN=3) :: next_arg

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
force_par_out = 0
ios(:) = 0
idp = 0
next_arg=""

DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "-ctv") THEN
    next_arg = "cem"
    force_par_out = 2
  ELSE IF (next_arg == "cem") THEN
    READ (chdum,*,IOSTAT=ios(1)) ctv_out(1)
    next_arg = "tab"
  ELSE IF (next_arg == "tab") THEN
    READ (chdum,*,IOSTAT=ios(2)) ctv_out(2)
    next_arg = "var"
  ELSE IF (next_arg == "var") THEN
    READ (chdum,*,IOSTAT=ios(3)) ctv_out(3)
    next_arg = ""
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein1 = chdum
    CASE (2)
      filein2 = chdum
    CASE (3)
      fileout = chdum
    CASE (4)
      READ (chdum,*,IOSTAT=ios(6)) ctv_out(3)
      force_par_out = 1
    CASE DEFAULT
      CALL write_help
      STOP
    END SELECT
  ENDIF
ENDDO

IF (ANY(ios(:) /= 0) .OR. idp < 3 .OR. &
    filein1 == "" .OR. filein2 == "" .OR. fileout == "" .OR. &
    (force_par_out > 0 .AND. (ctv_out(3) <= 0 .OR. ctv_out(3) > 255)) .OR. &
    (force_par_out == 2 .AND. &
     (ANY(ctv_out(1:3) <= 0) .OR. ANY(ctv_out(1:3) > 255)))  ) THEN
  CALL write_help
  STOP
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

CALL PBOPEN (iuout,fileout,'W',kret)

OPEN (UNIT=96, FILE="somma_grib.log", STATUS="REPLACE", ACTION="WRITE")

!--------------------------------------------------------------------------
! 2) Lettura - Scrittura

! 2.1) Leggo e decodifico il grib a
CALL PBGRIB(iuin1,kbuffer,maxdim*4,klen,kret)
IF (kret <= -1) THEN
  WRITE(*,*) "Error pbgrib: kret ",kret
  STOP
ENDIF

psec3a(2) = rmis                                   ! dati mancanti = rmis
CALL GRIBEX (ksec0a,ksec1a,ksec2a,psec2a,ksec3a,psec3a,ksec4a, &
             field_a,maxdim,kbuffer,maxdim,klen,'D',kret)
if (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! 2.2) Leggo & decodifico il grib b
CALL PBGRIB(iuin2,kbuffer,maxdim*4,klen,kret)
IF (kret.eq.-1) THEN 
  WRITE(*,*) "Errore, il file 2 continene meno grib del file 1"
  STOP
ELSE IF (kret < -1) THEN
  WRITE(*,*) "Error pbgrib: kret ",kret
  STOP
ENDIF

psec3b(2) = rmis                                   ! dati mancanti = rmis
CALL GRIBEX (ksec0b,ksec1b,ksec2b,psec2b,ksec3b,psec3b,ksec4b, &
             field_b,maxdim,kbuffer,maxdim,klen,'D',kret)
IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! 2.3) Diagnostica sulla compatibilita' dei campi
WRITE (*,*) "Confronto header:"
IF (ANY(ksec2a(:) /= ksec2b(:)) .OR. (ksec4a(1) /= ksec4b(1))) THEN
  DO k = 1,1024
    IF (ksec2a(k) /= ksec2b(k)) WRITE (*,'(a,i4,2(a,i8))') &
      "ksec2(",k,"): in file1 ",ksec2a(k)," in file2 ",ksec2b(k)
  ENDDO
ENDIF

IF (ANY(ksec1a(1:2) /= ksec1b(1:2)) .OR. ksec1a(6) /= ksec1b(6)) &
  WRITE (*,*) "Parametro diverso"
IF (ANY(ksec1a(7:9) /= ksec1b(7:9))) WRITE (*,*) "Livello diverso"
IF (ANY(ksec1a(15:18) /= ksec1b(15:18))) WRITE (*,*) "Scadenza diversa"

! 2.4) Riscrivo
np = ksec4a(1)
field_c(1:np) = field_b(1:np)

ksec1c(:) = ksec1a(:)
IF (force_par_out == 1) THEN
  ksec1c(6) = ctv_out(3)
ELSE IF (force_par_out == 2) THEN
  ksec1c(2) = ctv_out(1)
  ksec1c(1) = ctv_out(2)
  ksec1c(6) = ctv_out(3)
ENDIF

ksec2c(:) = ksec2a(:)
psec2c(:) = psec2a(:)
ksec3c(:) = ksec3a(:)
psec3c(2) = rmis                         ! dati mancanti = rmis
ksec4c(:) = ksec4a(:)

CALL GRIBEX (ksec0c,ksec1c,ksec2c,psec2c,ksec3c,psec3c,ksec4c, &
             field_c,maxdim,kbuffer,maxdim,klen,'C',kret)
IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
CALL PBWRITE (iuout,kbuffer,ksec0c(1),kret)
  
!--------------------------------------------------------------------------
! 3) Conclusione

CALL PBCLOSE (iuin1,kret)
CALL PBCLOSE (iuin2,kret)
CALL PBCLOSE (iuout,kret)
CLOSE (96)

END PROGRAM replace_dat_grib

!--------------------------------------------------------------------------

SUBROUTINE write_help

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: replace_dat_grib.exe [-h] file1 file2 file_out"
WRITE (*,*) "                          [par_out] [-ctv cem tab var]"
WRITE (*,*)
WRITE (*,*) "Legge il primo grib da due files"
WRITE (*,*) "Scrive un grib con gli header del primo file e i valori del secondo file."
WRITE (*,*) "par_out: se specificato, forza il codice parametro (ksec1(6)) in output" 
WRITE (*,*) "-ctv   : forza i valori di CEM, TAB, VAR nei gribi di output"
WRITE (*,*)
RETURN

END SUBROUTINE write_help
