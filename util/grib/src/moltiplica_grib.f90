PROGRAM moltiplica_grib
!--------------------------------------------------------------------------
! Programma che legge 2 file con molti grib (con tutto uguale tranne il 
! parametro) e scrive un file che contiene, per ciascun grib, il prodotto
! (o il quoziente) punto a punto dei due campi.
!
! PROGRAMMA RESO OBSOLETO DA math_grib.exe
!
!                                         Versione 3.1.1, Enrico 13/01/2014
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -1.E30           ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 100000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0a(2),ksec1a(1024),ksec2a(1024),ksec3a(2),ksec4a(512)
INTEGER :: ksec0b(2),ksec1b(1024),ksec2b(1024),ksec3b(2),ksec4b(512)
INTEGER :: ksec0c(2),ksec1c(1024),ksec2c(1024),ksec3c(2),ksec4c(512)

INTEGER :: kbuffer(maxdim), klen, kret
REAL :: psec2a(512),psec3a(2),psec2b(512),psec3b(2),psec2c(512),psec3c(2)
REAL :: field_a(maxdim),field_b(maxdim),field_c(maxdim)

! Altre variabili del programma
REAL :: fk1,fk2
INTEGER :: ctv_out(3),ios(6),force_par_out
INTEGER :: ngrib,iuin1,iuin2,iuout,np,kp,idp,kgb,k
CHARACTER (LEN=200) :: filein1,filein2,fileout,chdum
CHARACTER (LEN=3) :: next_arg,oper
LOGICAL :: ld_par,ld_lev,ld_date,ld_sca,ld_ksec1,lseek

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
force_par_out = 0
ios(:) = 0
idp = 0
next_arg=""
lseek = .FALSE.
oper = "mol"

DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "-seek") THEN
    lseek = .TRUE.
  ELSE IF (TRIM(chdum) == "-div") THEN
    oper = "div"
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

IF (lseek) OPEN (UNIT=90, FILE="moltiplica_grib.log", &
  STATUS="REPLACE", ACTION="WRITE")

! 1.4 Inizializzo flag
ld_par = .FALSE.
ld_lev = .FALSE.
ld_date = .FALSE.
ld_sca = .FALSE.
ld_ksec1 = .FALSE.

!--------------------------------------------------------------------------
! 2) Lettura - Scrittura (ciclo sui grib)

ngrib = 0

griba: DO

! 2.1) Leggo e decodifico il grib a
  CALL PBGRIB(iuin1,kbuffer,maxdim*4,klen,kret)
  IF (kret.eq.-1) THEN 
    EXIT griba
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  psec3a(2) = rmis                                   ! dati mancanti = rmis
  CALL GRIBEX (ksec0a,ksec1a,ksec2a,psec2a,ksec3a,psec3a,ksec4a, &
               field_a,maxdim,kbuffer,maxdim,klen,'D',kret)
  if (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! 2.2) Leggo & decodifico il grib b

  gribb: DO kgb = 1,HUGE(0)

    CALL PBGRIB(iuin2,kbuffer,maxdim*4,klen,kret)
    IF (kret.eq.-1 .AND. .NOT. lseek) THEN
      WRITE(*,*) "Errore, il file 2 contiene meno grib del file 1"
      STOP
    ELSE IF (kret.eq.-1 .AND. lseek) THEN
      EXIT griba
    ELSE IF (kret < -1) THEN
      WRITE(*,*) "Error pbgrib: kret ",kret
      STOP
    ENDIF
  
    psec3b(2) = rmis                                   ! dati mancanti = rmis
    CALL GRIBEX (ksec0b,ksec1b,ksec2b,psec2b,ksec3b,psec3b,ksec4b, &
                 field_b,maxdim,kbuffer,maxdim,klen,'D',kret)
    IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

!   2.3) Diagnostica sulla compatibilita' dei campi
    IF (ANY(ksec2a(1:11) /= ksec2b(1:11)) .OR. ANY(ksec2a(13:14) /= ksec2b(13:14)) .OR. &
      (ksec4a(1) /= ksec4b(1))) THEN
      GOTO 9999
    ELSE IF ((.NOT.lseek) .OR. ALL(ksec1a(10:18) == ksec1b(10:18)) ) THEN
      EXIT gribb
    ENDIF

    IF (kgb == 1) THEN
      WRITE (90,'(a)') "Skip: "
      WRITE (90,'(a,9i4)') "File 1: ",ksec1a(10:18)
    ENDIF
    WRITE (90,'(a,9i4)') "File 2: ",ksec1b(10:18)

  ENDDO gribb
  IF (kgb > 1) WRITE (90,*)

  WRITE (90,'(a,9i4)') "OK: ",ksec1a(10:18)

  IF (ANY(ksec1a(1:2) /= ksec1b(1:2)) .OR. ksec1a(6) /= ksec1b(6)) THEN
    ld_par = .TRUE.
  ELSE IF (ANY(ksec1a(7:9) /= ksec1b(7:9))) THEN
    ld_lev = .TRUE.
  ELSE IF (ANY(ksec1a(10:14) /= ksec1b(10:14))) THEN
    ld_date = .TRUE.
  ELSE IF (ANY(ksec1a(15:18) /= ksec1b(15:18))) THEN
    ld_sca = .TRUE.
  ELSE IF (ANY(ksec1a(3:5) /= ksec1b(3:5)) .OR. ANY(ksec1a(19:) /= ksec1b(19:))) THEN
    ld_ksec1 = .TRUE.
  ENDIF
  
  ngrib = ngrib +1

! 2.4) Calcolo il risultato dell'operazione richiesta e lo scrivo
  np = ksec4a(1)
  IF (oper == "mol") THEN
    WHERE (field_a(1:np) /= rmis .AND. field_b(1:np) /= rmis)
      field_c(1:np) = field_a(1:np) * field_b(1:np)
    ELSEWHERE
      field_c(1:np) = rmis
    ENDWHERE

  ELSE IF (oper == "div") THEN
    WHERE (field_a(1:np) /= rmis .AND. field_b(1:np) /= rmis .AND. field_b(1:np) /= 0.)
      field_c(1:np) = field_a(1:np) / field_b(1:np)
    ELSEWHERE
      field_c(1:np) = rmis
    ENDWHERE

  ENDIF

! Uso griglia del primo campo
  ksec2c(:) = ksec2a(:)
  psec2c(:) = psec2a(:)
  ksec3c(:) = ksec3a(:)

! Se richiesto, sovrascrivo identificativi parametro
  ksec1c(:) = ksec1a(:)
  IF (force_par_out == 1) THEN
    ksec1c(6) = ctv_out(3)
  ELSE IF (force_par_out == 2) THEN
    ksec1c(2) = ctv_out(1)
    ksec1c(1) = ctv_out(2)
    ksec1c(6) = ctv_out(3)
  ENDIF

! Se ci sono dati mancanti forzo la scrittura bitmap e valore per missing
  IF (ANY(field_c(1:np) == rmis)) THEN
    psec3c(2) = rmis
    IF (ksec1c(5) == 0 .OR. ksec1c(5) == 64) THEN
      ksec1c(5) = 64
    ELSE 
      ksec1c(5) = 192
    ENDIF
  ENDIF

! Gestione SOP e NBIT
  ksec4c(:) = ksec4a(:)
  ksec4c(2) = MAX(ksec4a(2),ksec4b(2))     ! metto nibt = max tra addendi
  IF (ksec4a(4) == 64 .AND. ksec4b(4) == 64) THEN
    IF (ksec4c(2) == 0) ksec4c(2) = 8      ! se SOP, escludo nbit=0
  ELSE IF (ksec4a(4) == 64 .OR. ksec4b(4) == 64) THEN
    ksec4c(4:) = 0                         ! se un solo addendo SOP, risultato FOP
  ENDIF

! Scrivo il campo
  CALL GRIBEX (ksec0c,ksec1c,ksec2c,psec2c,ksec3c,psec3c,ksec4c, &
               field_c,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0c(1),kret)
  
ENDDO griba

WRITE (*,*) "Analisi differenze tra i files in input (sezione 1):"
IF (ld_par)   WRITE (*,*) "Ci sono parametri diversi"
IF (ld_lev)   WRITE (*,*) "Ci sono livelli diversi"
IF (ld_date)  WRITE (*,*) "Ci sono date diverse"
IF (ld_sca)   WRITE (*,*) "Ci sono scadenze diverse"
IF (ld_ksec1) WRITE (*,*) "Ci sono altri campi di sezione 1"
WRITE (*,*)
WRITE (*,*) "Scritti: ",ngrib," campi"

!--------------------------------------------------------------------------
! 3) Conclusione

CALL PBCLOSE (iuin1,kret)
CALL PBCLOSE (iuin2,kret)
CALL PBCLOSE (iuout,kret)

STOP

!--------------------------------------------------------------------------
9999 CONTINUE

WRITE (*,*) "Files 1 e 2 hanno campi con griglie diverse, progr. ",ngrib + 1
WRITE (*,*) "File 1: ",TRIM(filein1)
WRITE (*,*) "File 2: ",TRIM(filein2)

DO k = 1, 14
  IF (ksec2a(k) /= ksec2b(k) .AND. k /= 12) THEN
    WRITE (*,'(a,i2,a,i10,a,i10)') &
      "ksec2(",k,"): in file1 ",ksec2a(k),", in file2 ",ksec2b(k)
  ENDIF
ENDDO
STOP

END PROGRAM moltiplica_grib

!--------------------------------------------------------------------------

SUBROUTINE write_help

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: moltiplica_grib.exe [-h] file1 file2 file_out "
WRITE (*,*) "                         [par_out] [-ctv cem tab var] [-seek] [-div]"
WRITE (*,*)
WRITE (*,*) "Per ciascuno dei GRIB contenuti in file1 e file2, calcola il risultato"
WRITE (*,*) "  dell'operazione GRIB1*GRIB2 (o GRIB1/GRIB2) e lo scrive in file_out;"
WRITE (*,*) "  i grib in file1 e file2 devono essere definiti sulla stessa griglia;"
WRITE (*,*) "  di default, non fa controlli sul contenuto della sez. 1 in file1 e file2"
WRITE (*,*) 
WRITE (*,*) "par_out: se specificato, forza il codice parametro (ksec1(6)) in output" 
WRITE (*,*) "-ctv   : forza i valori di CEM, TAB, VAR nei gribi di output"
WRITE (*,*) "-seek  : se i GRIB in file1 e file2 hanno data/scad diversa, scorre file2"
WRITE (*,*) "         finche' non trova il grib corrispondente"
WRITE (*,*) "-div   : esegue l'operazione GRIB1/GRIB2 (default: GRIB1*GRIB2)"
WRITE (*,*)
RETURN

END SUBROUTINE write_help
