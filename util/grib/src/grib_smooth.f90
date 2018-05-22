PROGRAM grib_smooth
!--------------------------------------------------------------------------
! Legge un file con molti grib, lo riscrive filtrando ciascun campo
!
!                                         Verisone 1.0.0, Enrico 13/02/2018
!--------------------------------------------------------------------------

USE grib_api
USE missing_values
IMPLICIT NONE

INTEGER, ALLOCATABLE :: idx(:,:)
REAL, ALLOCATABLE :: field(:),field2(:)
REAL :: radius,radius2,dd,sumf,out_value
INTEGER :: ifin=0,ifout=0,igin=0,igout=0,iret,kg
INTEGER :: k1,i1,j1,k2,i2,j2,npf,npf_min,npf_max,mxidx
CHARACTER(LEN=1) :: next_arg
CHARACTER(LEN=3) :: smooth
CHARACTER(LEN=4) :: mode
CHARACTER(LEN=200) :: filein,fileout,chdum

INTEGER :: idp,kp
INTEGER :: ni,nj,ni1,nj1,bpv,gnov,nom,nocv

!--------------------------------------------------------------------------
! 1) Preliminari

! La variabile "mode" determina la strategia infomratica per selezionare
! gli indici dei punti da usare nel filtro:
! - mode = "lig": calcola la lista per ogni campo (usa poca memoria, utile
!   per pochi cmapi e grandi raggi d'influenza)
! - mode = "fas": calcola la lista una volta per tutte (piu' veloce, ma puo'
!   richiedere molta memeoria; utile per piccoli raggi d'influenza)

! 1.1 Parametri da riga comando
mode=""
smooth = ""
next_arg = ""
idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1
  ELSE IF (TRIM(chdum) == "-r") THEN
    next_arg = "r"
  ELSE IF (next_arg == "r") THEN
    smooth = "rad"
    READ (chdum,*) radius
    next_arg = "m"
  ELSE IF (next_arg == "m") THEN
    mode = chdum(1:4)
    next_arg = ""
  ELSE IF (TRIM(chdum) == "-c") THEN
    next_arg = "c"
  ELSE IF (next_arg == "c") THEN
    READ (chdum,*) out_value
    next_arg = ""
    smooth = "con"
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      fileout = chdum
    CASE DEFAULT
      CALL write_help
      STOP 1
    END SELECT
  ENDIF
ENDDO

IF (idp /= 2) THEN
  CALL write_help
  STOP 1
ENDIF
IF (smooth == "rad" .AND. mode /= "fast" .AND. mode /= "ligh") THEN
  CALL write_help
  STOP 1
ENDIF
  
IF (radius < 1.) GOTO 9999
radius2 = radius * radius

! 1.2 Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9998
CALL grib_open_file(ifout,fileout,"w")

!--------------------------------------------------------------------------
! 2) Esecuzione (ciclo sui grib)

DO kg = 1,HUGE(0)

! 2.1 Leggo il prossimo campo
  igin = -1
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9997

  CALL grib_get(igin,"Ni",ni)
  CALL grib_get(igin,"Nj",nj)

  IF (kg == 1) THEN
    ni1 = ni
    nj1 = nj
    ALLOCATE (field(ni*nj))
    IF (smooth == "rad") THEN
      ALLOCATE (field2(ni*nj))
      IF (mode == "fast") THEN
        mxidx = MIN(ni*nj, 4*(INT(radius)+1)**2)
        ALLOCATE (idx(ni*nj,mxidx))
      ENDIF
    ENDIF
  ELSE IF (ni /= ni1 .OR. nj /= nj1) THEN
    GOTO 9996
  ENDIF

! 2.2 se e' il primo campo ed e' richiesta l'opzione "-r" calcolo, per
!     ciascuna cella, i punti da usare per lo smoothing

  IF (kg == 1 .AND. smooth == "rad" .AND. mode == "fast" ) THEN
    idx(:,:) = 0
    npf_min = HUGE(0)
    npf_max = 0
    DO k1 = 1, ni*nj
      i1 = MOD((k1-1),ni) + 1
      j1 = INT((k1-1)/ni) + 1
      npf = 0
      
      DO k2 = 1, ni*nj 
        i2 = MOD((k2-1),ni) + 1
        j2 = INT((k2-1)/ni) + 1
        dd = REAL((i1-i2)*(i1-i2) + (j1-j2)*(j1-j2))
        IF (dd <= radius2 .AND. field(k2) /= rmiss) THEN
          npf = npf + 1
          IF (npf > mxidx) GOTO 9993
          idx(k1,npf) = k2
        ENDIF
      ENDDO
      IF (npf < npf_min) npf_min = npf
      IF (npf > npf_max) npf_max = npf
    ENDDO
    WRITE (*,*) "Definito il filtro. Punti usati: min ",npf_min," max ",npf_max, &
                " teorici ",3.1415*radius2
  ENDIF

! 2.3 Leggo il campo
  CALL grib_get(igin,"getNumberOfValues",gnov)    ! totale di punti nel grib
  CALL grib_get(igin,"numberOfMissing",nom)       ! n.ro dati mancanti
  CALL grib_get(igin,"numberOfCodedValues",nocv)  ! n.ro dati validi
  IF (nocv == 0) THEN
    field(:) = rmiss
  ELSE
    CALL grib_set(igin,"missingValue",rmiss)
    CALL grib_get(igin,"values",field)
  ENDIF
  IF (nom + nocv /= gnov .OR. &
    (nocv /= 0 .AND. nocv /= COUNT(field(:) /= rmiss))) GOTO 9995

  CALL grib_clone(igin,igout)

! 2.4.1 Filtro veloce
  IF (smooth == "rad" .AND. mode == "fast") THEN
    DO k1 = 1, ni*nj
      IF (field(k1) == rmiss) THEN
        field2(k1) = rmiss
      ELSE
        sumf = 0.
        npf = 0
        DO k2 = 1, npf_max
          IF (idx(k1,k2) > 0 .AND. field(idx(k1,k2)) /= rmiss) THEN
            sumf = sumf + field(idx(k1,k2))
            npf = npf + 1
          ENDIF
        ENDDO
        IF (npf > 0) THEN
          field2(k1) = sumf / REAL(npf)
        ELSE
          field2(k1) = rmiss
        ENDIF
      ENDIF
    ENDDO
    WRITE (*,*) "Mancanti in input ",COUNT(field(:) == rmiss)," in output: ", &
                 COUNT(field2(:) == rmiss)," su ",ni*nj
    field(:) = field2(:)

! 2.4.2 Filtro leggero
  ELSE IF (smooth == "rad" .AND. mode == "ligh") THEN
    npf_min = HUGE(0)
    npf_max = 0
    DO k1 = 1, ni*nj
      i1 = MOD((k1-1),ni) + 1
      j1 = INT((k1-1)/ni) + 1
      npf = 0
      sumf = 0.

      DO k2 = 1, ni*nj 
        i2 = MOD((k2-1),ni) + 1
        j2 = INT((k2-1)/ni) + 1
        dd = REAL((i1-i2)*(i1-i2) + (j1-j2)*(j1-j2))
        IF (dd <= radius2 .AND. field(k2) /= rmiss) THEN
          sumf = sumf + field(k2)
          npf = npf +1
        ENDIF
      ENDDO
      IF (npf < npf_min) npf_min = npf
      IF (npf > npf_max) npf_max = npf
      IF (field(k1) == rmiss .OR. npf == 0) THEN
        field2(k1) = rmiss
      ELSE
        field2(k1) = sumf / REAL(npf)
      ENDIF
    ENDDO
    WRITE (*,*) "Campo filtrato. Punti usati: min ",npf_min," max ",npf_max, &
                " teorici ",3.1415*radius2
    WRITE (*,*) "Mancanti in input ",COUNT(field(:) == rmiss)," in output: ", &
                 COUNT(field2(:) == rmiss)," su ",ni*nj
    field(:) = field2(:)
    
  ELSE IF (smooth == "con") THEN
    WHERE (field(:) /= rmiss) field(:) = out_value
      
  ENDIF


  IF (COUNT(field(:) == rmiss) /= 0) THEN
    CALL grib_set(igout,"missingValue",rmiss)
    CALL grib_set(igout,"bitmapPresent",1)
  ENDIF
  CALL grib_set(igout,"values",field)

! 2.4 Riscrivo
  CALL grib_write (igout,ifout)
  CALL grib_release(igin)
  CALL grib_release(igout)
ENDDO

!--------------------------------------------------------------------------
! 3) Conclusione

WRITE (*,*) "Elaborazioni completate, elaborati ",kg-1," campi"

CALL grib_close_file(ifin)
CALL grib_close_file(ifout)
STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "radius deve essere >= 1."
STOP 2

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 3

9997 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib n.ro " ,kg
STOP 4

9996 CONTINUE
WRITE (*,*) "Trovato campo con numero di punti diverso, mi fermo"
STOP 5

9995 CONTINUE
WRITE (*,*) "Errore nelle chiavi realtive ai dati mancanti"
WRITE (*,*) "Dati totali (getNumberOfValues):   ",gnov
WRITE (*,*) "Dati validi (numberOfCodedValues): ",nocv
WRITE (*,*) "Dati mancanti (numberOfMissing):   ",nom
WRITE (*,*) "Dati mancanti (matrice grib):      ",COUNT(field(:) /= rmiss)
STOP 6

9994 CONTINUE
WRITE (*,*) "Errore smoothing"
STOP 7

9993 CONTINUE
WRITE (*,*) "Errore calcolo idx"
STOP 7

END PROGRAM grib_smooth

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: grib_smooth.exe [-h] filein fileout [-r radius mode] [-c value]"
WRITE (*,*) "Legge un file con molti grib e lo riscrive filtrato:"
WRITE (*,*) "I grib devono avere lo stesso numero di punti (nx e ny)"
WRITE (*,*) "Con opzione -r e mode ""fast"", devono anche avere la stessa bitmap"
WRITE (*,*) ""
WRITE (*,*) "Con opzione -r, mette al posto di ogni valore la media tra i punti che"
WRITE (*,*) "  distano meno del raggio d'influenza specificato (radius; in passi griglia)"
WRITE (*,*) "  mode puo essere: ligh (usa poca memeoria) o fast (piu'veloce)"
WRITE (*,*) "Con opzione -c, mette tutti i dati validi al valore costante value"
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
