PROGRAM lm_levels2layers
!--------------------------------------------------------------------------
! Legge un file con le quote dei model levels/layers Icon/cosmo, e calcola 
! le quote dei layers/levels.
! 
!                                         Versione 1.0.0, Enrico 21/04/2026
!--------------------------------------------------------------------------

USE grib_api
IMPLICIT NONE

REAL, ALLOCATABLE :: topo(:),z_in(:,:),z_out(:,:)
INTEGER :: ifin=0,ifout=0,ifgeo=0,igin=0,igout=0,iggeo=0,igt=0,iret,kg,kl,kp
INTEGER :: nodp,nodp_sav,nodp_geo,toffs,svoffs,tosfs,svosfs,dis,pc,pn
INTEGER :: nl_in,nlev,nlay,l1_in,l2_in,l1_out,l2_out,idp,l1,l2,stride
CHARACTER(LEN=200) :: filein,fileout,filegeo,chdum
CHARACTER(LEN=5) :: opt
CHARACTER(LEN=2) :: next_arg
LOGICAL :: lgeo,lrev,lsal

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
idp = 0
next_arg = ""
opt = ""
lgeo = .FALSE.
lrev = .FALSE.
lsal = .FALSE.
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1
  ELSE IF (TRIM(chdum) == "-rev") THEN
    lrev = .TRUE.
  ELSE IF (TRIM(chdum) == "-sal") THEN
    lsal = .TRUE.
  ELSE IF (next_arg == "fg") THEN
    filegeo = chdum
    lgeo = .TRUE.
    next_arg = ""
  ELSE IF (TRIM(chdum) == "-hsurf") THEN
    next_arg = "fg"
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      fileout = chdum
    CASE (3)
      opt = chdum
    CASE DEFAULT
      CALL write_help
      STOP 1
    END SELECT
  ENDIF
ENDDO

IF (opt /= "lv2ly" .AND. opt /= "ly2lv") THEN
  CALL write_help
  STOP 1
ENDIF  

! Se richiesto leggo topo
IF (lgeo) THEN
  CALL grib_open_file(ifgeo,filegeo,"r",iret)
  IF (iret /= GRIB_SUCCESS) GOTO 9998
  CALL grib_new_from_file(ifgeo,iggeo,iret)
  IF (iret /= GRIB_SUCCESS) GOTO 9997
  CALL grib_get(iggeo,"numberOfDataPoints",nodp_geo)
  ALLOCATE (topo(nodp_geo))
  CALL grib_get(iggeo,"values",topo)
ENDIF

! Scan file di input per leggere la lista dei livelli (min, max, number)
! Controllo numero di punti e parametro = h
! Salvo un livello diverso dalla superficie da usare come template per l'output
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999

nl_in = 0
l1_in = HUGE(0)
l2_in = -HUGE(0)
DO kg = 1,HUGE(0)
  igin = -1
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9996

  CALL grib_get(igin,"numberOfDataPoints",nodp)
  IF (lgeo .AND. nodp /= nodp_geo) GOTO 9995
  IF (kg == 1) nodp_sav = nodp
  IF (kg > 1 .AND. nodp /= nodp_sav) GOTO 9994
  
  CALL grib_get(igin,"discipline",dis)
  CALL grib_get(igin,"parameterCategory",pc)
  CALL grib_get(igin,"parameterNumber",pn)
  IF (dis /= 0 .OR. pc /= 3 .OR. pn /= 6) GOTO 9993
  
  CALL grib_get(igin,"typeOfFirstFixedSurface",toffs)
  CALL grib_get(igin,"scaledValueOfFirstFixedSurface",svoffs)
  CALL grib_get(igin,"typeOfSecondFixedSurface",tosfs)
  CALL grib_get(igin,"scaledValueOfSecondFixedSurface",svosfs)
  IF (opt == "lv2ly") THEN        ! input: levels
    IF ((toffs /= 150 .OR. tosfs /= 101 .OR. svosfs /= 0) .AND. &                    ! model level
        (toffs /= 1 .OR. svoffs /= 0 .OR. tosfs /= 101 .OR. svosfs /= 0) ) GOTO 9990 ! surface
  ELSE IF (opt == "ly2lv") THEN   ! input: layers
    IF (toffs /= 150 .OR. tosfs /= 150 .OR. svosfs /= svoffs+1) GOTO 9990
  ENDIF  
  l1_in = MIN(l1_in,svoffs)
  l2_in = MAX(l2_in,svoffs)
  nl_in = nl_in + 1
  IF (svoffs /= 0 ) CALL grib_clone(igin,igt)
  CALL grib_release(igin)
ENDDO

IF (l2_in - l1_in + 1 /= nl_in) GOTO 9989      ! In input non si possono saltare dei livelli
IF (opt == "ly2lv" .AND. l1_in == 0) GOTO 9986 ! Non ci puo' essre un layer "zero"

! ly2lv senza opzione "-sal": se la superficie e' scritta come level=0, la salvo come nlev_in+1
IF (.NOT. lsal .AND. l1_in == 0) THEN
  l1_in = 1
  l2_in = l2_in + 1
ENDIF

! Definisco gli indici estremi dei livelli in otput
IF (opt == "lv2ly") THEN        ! input: levels
  l1_out = l1_in
  l2_out = l2_in - 1
ELSE IF (opt == "ly2lv") THEN   ! input: layers
  l1_out = l1_in
  l2_out = l2_in + 1
ENDIF

WRITE (*,*) "Livelli in input ",nl_in," da ",l1_in," a ",l2_in
WRITE (*,*) "Livelli in output da ",l1_out," a ",l2_out

CALL grib_close_file(ifin,iret)

! Leggo input, salvo le quote.
! ly2lv senza opzione "-sal": se esiste il livello "0" (superficie) lo salvo come livello "nlev+1"
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999

ALLOCATE (z_in(nodp,l1_in:l2_in))
ALLOCATE (z_out(nodp,l1_out:l2_out))

DO kg = 1,nl_in
  igin = -1
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret /= GRIB_SUCCESS) GOTO 9996
  CALL grib_get(igin,"scaledValueOfFirstFixedSurface",svoffs)
  IF (.NOT. lsal .AND. opt == "lv2ly" .AND. svoffs == 0) svoffs = l2_in
  IF (svoffs < l1_in .OR. svoffs > l2_in) GOTO 9988
  IF (.NOT. lsal .AND. svoffs == 0) THEN
    CALL grib_get(igin,"values",z_in(:,nl_in+1))
  ELSE
    CALL grib_get(igin,"values",z_in(:,svoffs))
  ENDIF
ENDDO

! Calcolo le quote in output e scrivo.
! Se richiesto il calcolo dei model levels, devo necessariamente procedere dal basso verso l'alto!

! Apro file di output; ordine dei livelli richiesti in output
CALL grib_open_file(ifout,fileout,"w")
IF (lrev) THEN
  l1 = l2_out
  l2 = l1_out
  stride = -1
ELSE
  l1 = l1_out
  l2 = l2_out
  stride = 1
  ENDIF

! Richiesto output su model layers: calcolo e scrivo nell'ordine richisto per l'output
IF (opt == "lv2ly") THEN
  DO kl = l1, l2, stride 
    IF (kl < l1_out .OR. kl > l2_out) GOTO 9987
    z_out(:,kl) = (z_in(:,kl) + z_in(:,kl+1)) / 2.

    CALL grib_clone(igt,igout)
    CALL grib_set(igout,"typeOfFirstFixedSurface",150)
    CALL grib_set(igout,"scaledValueOfFirstFixedSurface",kl)
    CALL grib_set(igout,"typeOfSecondFixedSurface",150)
    CALL grib_set(igout,"scaledValueOfSecondFixedSurface",kl+1)
    CALL grib_set(igout,"values", z_out(:,kl))
    CALL grib_write (igout,ifout)
    CALL grib_release(igout)
    WRITE (*,*) "Output: ",kl,MINVAL(z_out(:,kl)),MAXVAL(z_out(:,kl))
 ENDDO

! Richiesto output su model levels: devo sempre calcolare dal basso verso l'alto
ELSE IF (opt == "ly2lv") THEN        ! output: levels

! Calcolo
  DO kl = l2_out, l1_out, -1
    IF (kl < l1_out .OR. kl > l2_out) GOTO 9987
    IF (kl == l2_out .AND. lgeo) THEN
      z_out(:,kl) = topo(:)
    ELSE IF (kl == l2_out .AND. .NOT. lgeo) THEN
      z_out(:,kl) = 0.
    ELSE
      z_out(:,kl) = z_in(:,kl) + (z_in(:,kl) - z_out(:,kl+1))
    ENDIF
  ENDDO

! print *,topo(11538)
! print *,z_in(11538,:)
! print *,z_out(11538,:)

! Scrivo. 
! Senza l'opzioe "-sal", il livello l2_out viene codificato come "superficie"
  DO kl = l1, l2, stride
    CALL grib_clone(igt,igout)
    IF (.NOT. lsal .AND. kl == l2_out) THEN  ! Primo livello scritto come superficie
      CALL grib_set(igout,"typeOfFirstFixedSurface",1)
      CALL grib_set(igout,"scaledValueOfFirstFixedSurface",0)
    ELSE
      CALL grib_set(igout,"typeOfFirstFixedSurface",150)
      CALL grib_set(igout,"scaledValueOfFirstFixedSurface",kl)
    ENDIF
    CALL grib_set(igout,"typeOfSecondFixedSurface",101)
    CALL grib_set(igout,"scaledValueOfSecondFixedSurface",0)
    CALL grib_set(igout,"values", z_out(:,kl))
    CALL grib_write (igout,ifout)
    CALL grib_release(igout)
    WRITE (*,*) "Output: ",kl,MINVAL(z_out(:,kl)),MAXVAL(z_out(:,kl))
  ENDDO

ENDIF

!--------------------------------------------------------------------------
! 3) Conclusione

WRITE (*,*) "Elaborazioni completate"

CALL grib_close_file(ifin)
CALL grib_close_file(ifout)
IF (lgeo) CALL grib_close_file(ifgeo)
STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 2

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filegeo)
STOP 2

9997 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filegeo)
STOP 2

9996 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib n.ro " ,kg
STOP 2

9995 CONTINUE
WRITE (*,*) "Numero di punti diverso per input e geo: ",nodp,nodp_geo
STOP 3

9994 CONTINUE
WRITE (*,*) "Trovato grib con numero di punti diverso, campo ",kg
WRITE (*,*) "atteso: ",nodp_sav," trovato ",nodp
STOP 3

9993 CONTINUE
WRITE (*,*) "Trovato grib con parametro che non e' altezza ",dis,pc,pn
STOP 3

9990 CONTINUE
WRITE (*,*) "COdifica livelli non gestita, opt ",opt
WRITE (*,*) "toffs,svoffs,tosfs,svosfs ",toffs,svoffs,tosfs,svosfs
STOP 4

9989 CONTINUE
WRITE (*,*) "Livelli non continui, termino"
STOP 4

9988 CONTINUE
WRITE (*,*) "Errore livelli in ",svoffs,l1_in,l2_in
STOP 4

9987 CONTINUE
WRITE (*,*) "Errore livelli out ",kl,l1_out,l2_out
STOP 4

9986 CONTINUE
WRITE (*,*) "Errore: livello zero in input con input da model layers "
STOP 4

END PROGRAM lm_levels2layers

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: lm_levels2layers.exe [-h] filein fileout lv2ly/ly2lv [-hsurf file_topo] [-rev] [-sal]"
WRITE (*,*) "filein: quote dei levels/layers in input"
WRITE (*,*) "lv2ly: legge le quote dei levels, scrive quelle dei layers."
WRITE (*,*) "  puo' elaborare le quote SLM o da superficie"
WRITE (*,*) "ly2lv: legge le quote dei layers, scrive quelle dei levels"
WRITE (*,*) "  se vengono elaborate le quote da superficie, il primo livello e' zero."
WRITE (*,*) "  se vengono elaborate le quote SLM, il primo livello e' l'orografia, che"
WRITE (*,*) "  deve essere specificat in file_topo"
WRITE (*,*) "-rev: scrive i livelli a partire dal basso, ie. dall'indice più alto"
WRITE (*,*) "  (nell'output Icon sono a partire dall'alto)"
WRITE (*,*) "-sal (Surface As Level): considera il ""livello zero"" (superficie) come model level nlev+1,"
WRITE (*,*) "  sia in input (non testato!) sia in output. E' il dafault di Cosmo, ma non di Icon."
WRITE (*,*)
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
