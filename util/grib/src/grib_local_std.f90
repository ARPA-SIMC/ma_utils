PROGRAM grib_local_std
!--------------------------------------------------------------------------
! Legge un grib e scrive un campo che contiene la deviazione standard o la 
! differenza rispetto alla media calcolata in un intorno di ciascun punto.
! Nei punti in cui non e' possibile il calcolo completo (vicino ai bordi o
! a dati mancanti), STD viene messa a dato mancante.
!
!                                           Versione 1.0, Enrico 17/07/2008
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = 1.e19            ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 5000000     ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim), klen, kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim),field_out(maxdim)

! Altre variabili del programma
REAL (KIND=8) :: sum,sum2
REAL :: radius,radius2,frq
INTEGER :: iradius,ncircle,nrq,ni,nj,idp
INTEGER :: iuin,iuout,ios1,ios2,i,j,k,i2,j2,k2,ii,jj,nok,cnt_ok,kp
CHARACTER (LEN=80) :: filein,fileout,charg
CHARACTER (LEN=3) :: oper
CHARACTER (LEN=1) :: next_arg
LOGICAL, ALLOCATABLE :: msk_circle(:,:)

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
oper = ""
frq = 1.
idp = 1
DO kp = 1,HUGE(0)
  CALL getarg(kp,charg)

  IF (charg == "") THEN
    EXIT

  ELSE IF (next_arg == "f") THEN
    READ (charg,*,IOSTAT=ios1) frq

  ELSE IF (TRIM(charg) == "-h") THEN
    CALL write_help
    STOP

  ELSE IF (TRIM(charg) == "-std") THEN
    oper = "std"

  ELSE IF (TRIM(charg) == "-dlt") THEN
    oper = "dlt"

  ELSE IF (TRIM(charg) == "-frq") THEN
    next_arg="f"

  ELSE IF (idp == 1) THEN
    filein = charg
    idp = idp + 1

  ELSE IF (idp == 2) THEN
    fileout = charg
    idp = idp + 1

  ELSE IF (idp == 3) THEN
    READ (charg,*,IOSTAT=ios2) radius
    idp = idp + 1

  ENDIF
ENDDO

IF (filein == "" .OR. fileout == "" .OR. TRIM(filein) == "-h" .OR. &
    ios1 /= 0 .OR. ios2 /= 0 .OR. &
    radius < 0. .OR. frq < 0. .OR. frq > 1. .OR. oper == "") THEN
  CALL write_help
  STOP
ENDIF

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
CALL PBOPEN (iuin,filein,'R',kret)
IF (kret /= 0) GOTO 9999
CALL PBOPEN (iuout,fileout,'W',kret)

!--------------------------------------------------------------------------
! 2) Elaborazioni

! 2.1 Leggo il grib di input
CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)
IF (kret < 0) THEN
  WRITE(*,*) "Error pbgrib: kret ",kret
  STOP
ENDIF

psec3(2) = rmis
CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
             field,maxdim,kbuffer,maxdim,klen,'D',kret)
IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

ni = ksec2(2)
nj = ksec2(3)
IF (ksec2(11) /= 64) GOTO 9997

! 2.2) Operazioni preliminari per cacolo STD
!
! Considero i punti in un quadrato di lato 2*iradius+1 attorno a ciscun 
! punto. La matrice msk_circle individua, in questo quadrato, i punti che 
! effettivmanete si trovano a meno di radius passi griglia dal punto 
! centrale.

iradius = INT(radius)
radius2 = radius*radius

ALLOCATE (msk_circle(-iradius:iradius,-iradius:iradius))

msk_circle(:,:) = .TRUE.
DO i = -iradius, iradius
DO j = -iradius, iradius
  IF (REAL(i*i+j*j) > radius2) msk_circle(i,j) = .FALSE.
ENDDO
ENDDO
ncircle = COUNT(msk_circle)
nrq = NINT(frq*REAL(ncircle))
WRITE (*,'(3a,i6,a,i6)') &
  "Calcolo ",oper," usando almeno ",nrq," valori su ",ncircle

! 2.3) Calcolo STD
!
! i,j,k:    indici del punto in cui sto calcolando STD (punto centrale)
! i2,j2,k2: indici (nel grib) del punto di cui calcolo il contributo a STD
! ii,jj:    indici (relativi al punto centrale) del punto (i2,j2)

cnt_ok = 0
DO j = 1,nj
DO i = 1,ni

  k = (j-1)*ni + i

! Ciclo sui punti in un introno del punto in esame
  sum = 0.
  sum2 = 0.
  nok = 0

  DO jj = -iradius, iradius, 1
  DO ii = -iradius, iradius, 1
    i2 = i+ii
    j2 = j+jj
    k2 = (j2-1)*ni + i2
    IF (i2<1 .OR. i2>ni .OR. j2<1 .OR. j2>nj .OR. &
        field(k2) == rmis .OR. .NOT. msk_circle(ii,jj)) CYCLE

    nok = nok + 1
    sum = sum + DBLE(field(k2))
    sum2 = sum2 + DBLE(field(k2))**2
  ENDDO
  ENDDO

  IF (nok < nrq .OR. nok == 0) THEN
    field_out(k) = rmis
  ELSE
    IF (oper == "std") THEN
      field_out(k) = REAL(SQRT(sum2/DBLE(nok) - (sum/DBLE(nok))**2))
    ELSE IF (oper == "dlt") THEN
      IF (field(k) == rmis) THEN
        field_out(k) = rmis
      ELSE
        field_out(k) = field(k) - REAL(sum)/REAL(nok)
      ENDIF
    ENDIF
    cnt_ok = cnt_ok + 1
  ENDIF

ENDDO
ENDDO

! 2.4 Scrivo output
IF (ANY(field_out(:)==rmis)) THEN
  psec3(2) = rmis
  ksec1(5) = 192
ENDIF

CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
             field_out,maxdim,kbuffer,maxdim,klen,'C',kret)
IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret

CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

! 2.5 Termino
WRITE (*,'(a,2(a,i8),a,f6.2,a)') oper," calcolata in ",cnt_ok," punti su ", &
  ni*nj," (",100.*REAL(cnt_ok)/REAL(ni*nj),"%)"

STOP

!--------------------------------------------------------------------------
! 3) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,'(a,2i6,a,2i4)') "Errore selezione punti interni: attesi, trovati ", &
  ncircle,nok," punto ",i,j
STOP

9997 CONTINUE
WRITE (*,*) "Scanning mode non gestito! ",ksec2(11)
STOP

END PROGRAM grib_local_std

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help

WRITE (*,*) "Uso: grib_local_std.exe [-h] filein fileout raggio -std/-dlt [-frq F]" 
WRITE (*,*) "  raggio: area entro cui calcolare STD, espressa in passi griglia"
WRITE (*,*) "  F:      frazione minima (0-1) di dati validi per calcolare STD"
WRITE (*,*) "          (nell'intorno di ciscun punto)"

END SUBROUTINE write_help

