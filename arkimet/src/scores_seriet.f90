PROGRAM scores_seriet
!--------------------------------------------------------------------------
! Dati due files in formato seriet (date XLS), calcola alcuni scores tra
! i dati di alcune colonne del primo (osservazioni) e del secondo 
! (previsioni)
!
! Uso: scores_seriet fileoss filepre fileout [filtro] [-h] [-c]
! Legge l'elenco delle colonne da analizzare nel file scores_seriet.inp
!
! Note:
! Scambiando i files di osservazioni e previsioni gli scores non cambiano;
! tuttavia il programma scorre i due files cercando le date presenti nelle
! previsioni, quindi:
! -  Il file con il passo temporale piu' lungo deve essere messo come file
!    previsto
! -  Il primo istante delle oss. non puo' essere successivo al primo istante 
!    delle previ.
! -  Deve esistere un record oss per ogni record previ
!
!                                         Versione 2.2.0, Enrico 06/10/2011
!--------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.
INTEGER, PARAMETER :: mxpar = 500  ! max. n.ro di parametri nei files seriet

REAL, PARAMETER :: ref_lat = 44.53 ! coord. per calcolo angolo solare
REAL, PARAMETER :: ref_lon = 11.30

! Contatori
REAL :: sc(5,mxpar)                ! Bias, MAE, RMS, MaxErr, MinErr
INTEGER :: nok(mxpar),nist1,nist2

! Altre variabili del programma
TYPE (date) :: datao,datap,data1,data2,data_mxe(mxpar),data_mne(mxpar)
REAL :: val_pre(mxpar),val_oss(mxpar),delta,sinalp
INTEGER :: col_oss(mxpar),col_pre(mxpar),flag_dir(mxpar)
INTEGER :: nf_oss,nf_pre,fmt_oss,fmt_pre
INTEGER :: hho,hhp,hh1,hh2,hh_mxe(mxpar),hh_mne(mxpar),scao,scap
INTEGER :: k,ios,npar,pp,idp,kp,nnml
CHARACTER (LEN=80) :: fileoss,filepre,fileout,filtro,chfmt,chrec,chdum
CHARACTER (LEN=40) :: label_oss(mxpar),label_pre(mxpar)
CHARACTER (LEN=10) :: ch10head(3,mxpar)

!##########################################################################
! 1) Preliminari

!--------------------------------------------------------------------------
! 1.1) Parametri da riga comando

filtro = ""
idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "-c") THEN
    CALL scrive_namelist(1)
    STOP
  ELSE IF (chdum(1:2) == "-c") THEN
    READ (chdum(3:),*,IOSTAT=ios) nnml
    IF (ios /= 0) THEN
      CALL write_help
      STOP
    ENDIF
    CALL scrive_namelist(nnml)
    STOP
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      fileoss = chdum
    CASE (2)
      filepre = chdum
    CASE (3)
      fileout = chdum
    CASE (4)
      filtro = chdum
    CASE DEFAULT
      CALL write_help
      STOP
    END SELECT
  ENDIF
ENDDO

IF (filtro == "") THEN
  filtro ="none"
ELSE IF (filtro /= "day" .AND. filtro /= "night") THEN
  WRITE (*,*) "Filtro ",TRIM(filtro)," non gestito, uso tutti i dati"
  filtro ="none"
ELSE
  OPEN (UNIT=90, FILE="filtro.log", STATUS="REPLACE", FORM="FORMATTED")
ENDIF

!--------------------------------------------------------------------------
! 1.3) Parametri da namelist

OPEN (UNIT=40, FILE="scores_seriet.inp", STATUS="OLD", ERR=9999)

npar = 0
DO k = 1,HUGE(0)
  READ (40,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) EXIT
  IF (chrec(1:1) == "!" .OR. chrec == "") CYCLE
  npar = npar + 1
  IF (npar > mxpar) GOTO 9998
  READ (chrec,*,IOSTAT=ios) col_oss(npar),col_pre(npar),flag_dir(npar)
  IF (ios /= 0) GOTO 9997
ENDDO

WRITE (*,'(a,i3)') "Parametri richiesti: ",npar

!--------------------------------------------------------------------------
! 1.4) Analizzo i file di input
CALL inq_file_smr(fileoss,mxpar,fmt_oss,nf_oss)
CALL inq_file_smr(filepre,mxpar,fmt_pre,nf_pre)

!--------------------------------------------------------------------------
! 1.5.1) Osservazioni: apro il file, leggo headers, costruisco labels e 
!        formato dei record dati.
OPEN (UNIT=41, FILE=fileoss, STATUS="OLD")

IF (fmt_oss == 1) THEN
  WRITE (chfmt,'(a,i3,a)') "(17x,",nf_oss,"(1x,a10))"
  READ (41,*)
  READ (41,*)
  READ (41,chfmt) (ch10head(1,k),k=1,nf_oss)
  READ (41,chfmt) (ch10head(2,k),k=1,nf_oss)
  READ (41,*)
  READ (41,chfmt) (ch10head(3,k),k=1,nf_oss)

ELSE IF (fmt_oss == 3) THEN
  WRITE (chfmt,'(a,i3,a)') "(14x,",nf_oss,"(1x,a10))"
  ch10head(1,1:nf_oss) = "Oss"
  ch10head(2,1:nf_oss) = "Surf"
  READ (41,*)
  READ (41,*)
  READ (41,chfmt) (ch10head(3,k),k=1,nf_oss)

ELSE IF (fmt_oss == 4) THEN
  WRITE (chfmt,'(a,i3,a)') "(19x,",nf_oss,"(1x,a10))"
  ch10head(1,1:nf_oss) = "RSD"
  READ (41,chfmt) (ch10head(2,k),k=1,nf_oss)
  READ (41,*)
  READ (41,chfmt) (ch10head(3,k),k=1,nf_oss)

ENDIF

DO k = 1,nf_oss
  label_oss(k) = TRIM(ADJUSTL(ch10head(1,k))) // " " // &
    TRIM(ADJUSTL(ch10head(2,k))) // " " // &
    TRIM(ADJUSTL(ch10head(3,k)))
ENDDO 

!--------------------------------------------------------------------------
! 1.5.2) Previsioni: apro il file, leggo headers, costruisco labels
OPEN (UNIT=42, FILE=filepre, STATUS="OLD")

IF (fmt_pre == 1) THEN
  WRITE (chfmt,'(a,i3,a)') "(17x,",nf_pre,"(1x,a10))"
  READ (42,*)
  READ (42,*)
  READ (42,chfmt) (ch10head(1,k),k=1,nf_pre)
  READ (42,chfmt) (ch10head(2,k),k=1,nf_pre)
  READ (42,*)
  READ (42,chfmt) (ch10head(3,k),k=1,nf_pre)

ELSE IF (fmt_pre == 3) THEN
  WRITE (chfmt,'(a,i3,a)') "(14x,",nf_pre,"(1x,a10))"
  ch10head(1,1:nf_pre) = "Oss"
  ch10head(2,1:nf_pre) = "Surf"
  READ (42,*)
  READ (42,*)
  READ (42,chfmt) (ch10head(3,k),k=1,nf_pre)

ELSE IF (fmt_pre == 4) THEN
  WRITE (chfmt,'(a,i3,a)') "(19x,",nf_pre,"(1x,a10))"
  ch10head(1,1:nf_pre) = "RSD"
  READ (42,chfmt) (ch10head(2,k),k=1,nf_pre)
  READ (42,*)
  READ (42,chfmt) (ch10head(3,k),k=1,nf_pre)

ENDIF

DO k = 1,nf_pre
  label_pre(k) = TRIM(ADJUSTL(ch10head(1,k))) // " " // &
    TRIM(ADJUSTL(ch10head(2,k))) // " " // &
    TRIM(ADJUSTL(ch10head(3,k)))
ENDDO 

!--------------------------------------------------------------------------
! 1.6) Log a schermo dell'analisi files

SELECT CASE (fmt_oss)
CASE (1)
  chfmt = "seriet XLS" 
CASE (2)
  chfmt = "seriet for" 
CASE (3)
  chfmt = "estra_orari" 
CASE (4)
  chfmt = "trasp_temp" 
END SELECT
WRITE (*,'(3a,i3)') "Osservazioni: formato ",TRIM(chfmt),"; campi ",nf_oss

SELECT CASE (fmt_pre)
CASE (1)
  chfmt = "seriet XLS" 
CASE (2)
  chfmt = "seriet for" 
CASE (3)
  chfmt = "estra_orari" 
CASE (4)
  chfmt = "trasp_temp" 
END SELECT
WRITE (*,'(3a,i3)') "Previsioni: formato ",TRIM(chfmt),"; campi ",nf_pre
WRITE (*,'(2a)') "Filtro: ",TRIM(filtro)

!--------------------------------------------------------------------------
! 1.7) Inizializzo i contatori per le statistiche

nok(1:npar) = 0
sc(1:3,1:npar) = 0.
sc(4,1:npar) = -HUGE(0.)
sc(5,1:npar) = HUGE(0.)
data_mxe(1:npar) = date(0,0,0)
data_mne(1:npar) = date(0,0,0)
hh_mxe(1:npar) = 0
hh_mne(1:npar) = 0

!##########################################################################
! 2) Leggo i dati e aggiorno i contatori

nist1 = 0
nist2 = 0

main: DO
! Leggo il prossimo record previsto
  CALL read_rec_smr(42,fmt_pre,nf_pre,datap,hhp,scap,val_pre(1:nf_pre),ios)
  IF (ios /= 0) EXIT main
  hh1 = MOD(hhp+scap,24)
  data1 = datap + (hhp+scap)/24
! print *,"letto pre ",data1,hh1
! print *,val_pre(1:nf_pre)

  DO
!   Trovo il corrispondente record osservato
    CALL read_rec_smr(41,fmt_oss,nf_oss,datao,hho,scao,val_oss(1:nf_oss),ios)
    IF (ios /= 0) EXIT main
    hh2 = MOD(hho+scao,24)
    data2 = datao + (hho+scao)/24
!   print *,"letto oss ",data2,hh2
!   print *,val_oss(1:nf_oss)

    IF (data2 < data1 .OR. (data2 == data1 .AND. hh2 < hh1)) CYCLE
    IF (data2 > data1 .OR. (data2 == data1 .AND. hh2 > hh1)) THEN
      WRITE (*,*) "Errore, nessuna osservazione per l'istante:"
      STOP
    ENDIF
    nist1 = nist1 + 1

!--------------------------------------------------------------------------
!   Eventualmente filtro le date (USER MODIFICATION)

    IF (TRIM(filtro) == "day") THEN
      CALL solar(jul(data1),hh1,ref_lat,ref_lon,sinalp)
      IF (sinalp < 0.) CYCLE main
      WRITE (90,'(a,4i6,f10.3)') "uso: ",data1,hh1,sinalp

    ELSE IF (TRIM(filtro) == "night") THEN
      CALL solar(jul(data1),hh1,ref_lat,ref_lon,sinalp)
      IF (sinalp > 0.) CYCLE main
      WRITE (90,'(a,4i6,f10.3)') "uso: ",data1,hh1,sinalp

    ENDIF
!--------------------------------------------------------------------------

    nist2 = nist2 + 1

!   Incremento i contatori statistici
    DO k = 1,npar
      IF (val_oss(col_oss(k)) == rmis .OR. val_pre(col_pre(k)) == rmis) CYCLE
      nok(k) = nok(k) + 1

      delta = val_pre(col_pre(k)) - val_oss(col_oss(k))
      IF (flag_dir(k) > 0 .AND. delta > 180.) delta = delta - 360.
      IF (flag_dir(k) > 0 .AND. delta < -180.) delta = delta + 360.

      sc(1,k) = sc(1,k) + delta
      sc(2,k) = sc(2,k) + ABS(delta)
      sc(3,k) = sc(3,k) + delta**2

      IF (delta > sc(4,k)) THEN
        sc(4,k) = delta
        data_mxe(k) = data1
        hh_mxe(k) = hh1

!if (k==9) then
!print *, "delta max:",delta
!print *, k,col_pre(k),val_pre(col_pre(k))
!print *, datap,hhp,scap
!print *, k,col_oss(k),val_oss(col_oss(k))
!print *, datao,hho,scao
!read *
!endif

      ENDIF

      IF (delta < sc(5,k)) THEN
        sc(5,k) = delta
        data_mne(k) = data1
        hh_mne(k) = hh1

!if (k==9) then
!print *, "delta min:",delta
!print *, k,col_pre(k),val_pre(col_pre(k))
!print *, datap,hhp,scap
!print *, k,col_oss(k),val_oss(col_oss(k))
!print *, datao,hho,scao
!read *
!endif

      ENDIF

    ENDDO

    EXIT

  ENDDO

ENDDO main

CLOSE (41)
CLOSE (42)
IF (filtro /= "none") CLOSE(90)

WRITE (*,*) "Istanti presenti in entrambi i files : ",nist1
WRITE (*,*) "Istanti rimasti dopo il filtraggio   : ",nist2

!--------------------------------------------------------------------------
! 3) Calcolo scores e scrivo output

! Normalizzo gli scores
WHERE (nok(1:npar) > 0)
  sc(1,1:npar) = sc(1,1:npar) / REAL(nok(1:npar))
  sc(2,1:npar) = sc(2,1:npar) / REAL(nok(1:npar))
  sc(3,1:npar) = SQRT(MAX( sc(3,1:npar) / REAL(nok(1:npar)) ,0. ))
ELSEWHERE
  sc(1,1:npar) = rmis
  sc(2,1:npar) = rmis
  sc(3,1:npar) = rmis
  sc(4,1:npar) = rmis
  sc(5,1:npar) = rmis
ENDWHERE

! Scrivo output: statistiche
OPEN (UNIT=50, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")
!                 123456789012345678901234567890123456789012345678901234567890---12345678901234567890---12345678901234567890
WRITE (50,'(a)') "       Nok      Bias       RMS       MAE    MaxErr    MinErr   Param.previsto         Param. osservato"

DO k = 1,npar
  WRITE (50,'(i10,5f10.3,3x,a)') nok(k),(sc(pp,k),pp=1,5), &
    label_pre(col_pre(k))(1:20) // " - " // label_oss(col_oss(k))(1:20)
ENDDO
CLOSE(50)

! Scrivo output: data degli errori estremi
OPEN (UNIT=51, FILE="date_err_extr.log", STATUS="REPLACE", FORM="FORMATTED")
!                 1234567890--1234567890--1234567890--1234567890---12345678901234567890---12345678901234567890
WRITE (51,'(a)') "    MaxErr  yyyymmddhh      MinErr  yyyymmddhh   Param.previsto         Param. osservato"

DO k = 1,npar
  WRITE (51,'(2(f10.3,2x,i4.4,3i2.2,2x),1x,a)') &
    sc(4,k),data_mxe(k)%yy,data_mxe(k)%mm,data_mxe(k)%dd,hh_mxe(k), &
    sc(5,k),data_mne(k)%yy,data_mne(k)%mm,data_mne(k)%dd,hh_mne(k), &
    label_pre(col_pre(k))(1:20) // " - " // label_oss(col_oss(k))(1:20)
ENDDO
CLOSE(51)

STOP

!--------------------------------------------------------------------------
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo scores_seriet.inp"
STOP

9998 CONTINUE
WRITE (*,*) "Troppi campi in scores_seriet.inp, aumentare parametro mxpar"
RETURN

9997 CONTINUE
WRITE (*,*) "Errore leggendo scores_seriet.inp: record ",k
WRITE (*,'(a)') TRIM(chrec)
STOP

END PROGRAM scores_seriet

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE inq_file_smr(file,mxpar,idfmt,nf)
!-------------------------------------------------------------------------
! Dato un file SMR, ritorna il suo formato e il numero di campi
! formati:
! 0 = sconosciuto
! 1 = seriet per XLS
! 2 = seriet per fortran
! 3 = estra_orari
! 4 = trasp_temp
!-------------------------------------------------------------------------
IMPLICIT NONE

! argomenti della subroutine
CHARACTER (LEN=80), INTENT(IN) :: file
INTEGER, INTENT(IN) :: mxpar
INTEGER, INTENT(OUT) :: idfmt,nf

! Variabili locali
INTEGER :: iun,k,chead,eof,eor,ios,ll
CHARACTER (LEN=mxpar*11+30) :: str
CHARACTER (LEN=10) :: ch10
CHARACTER (LEN=1) :: ch1
LOGICAL :: l1

!-------------------------------------------------------------------------
! Preliminari
CALL get_eof_eor(eof,eor)

idfmt = 0
nf = 0

!-------------------------------------------------------------------------
! Cerco un'unita' libera 
DO k = 10,99
  INQUIRE (UNIT=k, OPENED=l1, IOSTAT=ios)
  IF (.NOT. l1 .AND. ios==0) THEN
    iun = k
    EXIT
  ENDIF
ENDDO
IF (iun == 0) GOTO 9999   ! non ho torvato nessuna unita' libera

!-------------------------------------------------------------------------
! Trovo il formato

OPEN (UNIT=iun, FILE=file, STATUS="OLD", ERR=9998)
READ (iun,*,END=9997,ERR=9997)
READ (iun,*,END=9997,ERR=9997)
READ (iun,'(a)',END=9997,ERR=9997) str

IF (str(1:19) == "aaaa mm gg hh staz.") THEN        ! trasp_temp
  idfmt = 4
  chead = 19

ELSE IF (str(1:13) == "aaaa mm gg hh") THEN         ! estra_orari
  idfmt = 3
  chead = 13

ELSE                                                ! seriet
  READ (iun,*,END=9997,ERR=9997)
  READ (iun,*,END=9997,ERR=9997)
  READ (iun,'(a)',END=9997,ERR=9997) str
  IF (str(1:17) == "gg/mm/aaaa hh sca") THEN 
    idfmt = 1
    chead = 17
  ELSE IF (str(1:17) == "aaaa mm gg hh sca") THEN 
    idfmt = 2
    chead = 17
  ELSE
    RETURN    
  ENDIF

ENDIF

!-------------------------------------------------------------------------
! Conto i campi

ll = LEN(TRIM(str(chead+1:)))
IF (MOD(ll,11) /= 0) GOTO 9995
nf = ll /11

CLOSE (iun)
RETURN

! Vecchia versione (non funzionava piu'...)
!DO k = 1,chead
!  READ (iun,'(a1)',ADVANCE="NO",IOSTAT=ios) ch1
!  IF (ios /= 0) GOTO 9996
!ENDDO
!
!DO k = 1,mxpar
!  READ (iun,'(1x,a10)',ADVANCE="NO",IOSTAT=ios) ch10
!  IF (ios == eor) EXIT
!  IF (ios /= 0) GOTO 9996
!
!print *,k,ch10
!ENDDO
!
!nf = k -1

!-------------------------------------------------------------------------
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore in subroutine inq_file_smr: nessuna unita' libera"
STOP

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file)
STOP

9997 CONTINUE
WRITE (*,*) "Errore leggendo headers ",TRIM(file)
STOP

!9996 CONTINUE
!WRITE (*,*) "Errore contando i campi ",TRIM(file)
!STOP

9995 CONTINUE
WRITE (*,*) "Errore in subroutine inq_file_smr"
WRITE (*,*) " il file ",TRIM(file)," non contiene un numero intero di campi"
WRITE (*,*) ll
STOP

END SUBROUTINE inq_file_smr

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE read_rec_smr(iu,fmt,nf,data,hh,sca,val,ios)
!-------------------------------------------------------------------------
! Legge un record dati da un file SMR gia' aperto
!-------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: iu,fmt,nf
!
TYPE(date), INTENT(OUT) :: data
INTEGER, INTENT(OUT) :: hh,sca,ios
REAL, INTENT(OUT) :: val(nf)
!
CHARACTER(LEN=80) :: chfmt
!-------------------------------------------------------------------------

SELECT CASE (fmt)

CASE (1)                                                  ! seriet xls
  WRITE (chfmt,'(a,i3,a)') "(2(i2,1x),i4,1x,i2,1x,i3,",nf,"(1x,f10.1))"
  READ (iu,chfmt,IOSTAT=ios) data%dd,data%mm,data%yy,hh,sca,val(1:nf)

CASE (2)                                                  ! seriet for

CASE (3)                                                  ! estra_orari
  WRITE (chfmt,'(a,i3,a)') "(i4,3(1x,i2),",nf,"(1x,f10.1))"
  READ (iu,chfmt,IOSTAT=ios) data%yy,data%mm,data%dd,hh,val(1:nf)
  sca = 0

CASE (4)                                                  ! trasp_temp
  WRITE (chfmt,'(a,i3,a)') "(i4,3(1x,i2),6x,",nf,"(1x,f10.1))"
  READ (iu,chfmt,IOSTAT=ios) data%yy,data%mm,data%dd,hh,val(1:nf)
  sca = 0
END SELECT

RETURN
END SUBROUTINE read_rec_smr

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE solar(njul,hh,xlat,xlon,sinalp)
!--------------------------------------------------------------------------
! Da calmet
! Calcola l'angolo solare in un punto per una data ora (GMT)
!--------------------------------------------------------------------------
IMPLICIT NONE

! Argomenti
INTEGER, INTENT(IN) :: njul                    ! giorno giuliano richiesto
INTEGER, INTENT(IN) :: hh                      ! ora GMT richiesta
REAL, INTENT(IN) :: xlat,xlon                  ! coord. punto richiesto

REAL, INTENT(OUT) :: sinalp                    ! angolo solare per ogni ora

! Altre variabili della subroutine
REAL :: radd,xsind,xcosd,rad2d,cos2d,sin2d,em,sigma,sincd,capd,coscd
REAL :: radlat,sinlat,coslat,solha,d,mxlon

!--------------------------------------------------------------------------
! Calcoli preliminari

! La longitudine di calmet ha il segno invertito!!
mxlon = -xlon

d = (float(njul) -1.) * 360./365.242
radd = 0.0174533 * d
xsind = SIN(radd)
xcosd = COS(radd)
rad2d = 2. * radd
sin2d = SIN(rad2d)
cos2d = COS(rad2d)
em = 12. + 0.12357 * xsind - 0.004289 * xcosd + 0.153809 * sin2d + &
     0.060783 * cos2d
sigma = 279.9348 + d + 1.914827 * xsind - 0.079525 * xcosd + 0.019938 * &
        sin2d - 0.00162 * cos2d
sincd = 0.39784989 * SIN(0.0174533 * sigma)
capd = ASIN(sincd)
coscd = COS(capd)

! Calcolo angolo solare
radlat = 0.0174533 * xlat
sinlat = SIN(radlat)
coslat = COS(radlat)

solha = 15. * (hh - em) - mxlon
sinalp = sinlat * sincd + coslat * coscd * &
  COS(0.0174533 * solha)

RETURN
END SUBROUTINE solar

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE get_eof_eor(eof, eor)
!-------------------------------------------------------------------------
! Ritorna i codici di errore macchina-dipendenti corrispondenti alle 
! condizioni di EOF e EOR nella lettura di un file sequenziale formattato
!
! Secondo manuale, questi sono gli unici due casi in cui IOSTAT ritorna
! con un valore negativo. 
! Si noti che EOR riguarda solo non-advancinag READ
!-------------------------------------------------------------------------
IMPLICIT NONE

INTEGER, INTENT(OUT) :: eof,eor

INTEGER :: k, ios, idummy=0, iun=0
LOGICAL :: l1 = .TRUE.


! Cerco un'unita' libera per aprire il file di prova
DO k = 10,99
  INQUIRE (UNIT=k, OPENED=l1, IOSTAT=ios)
  IF (.NOT. l1 .AND. ios==0) THEN
    iun = k
    EXIT
  ENDIF
ENDDO
IF (iun == 0) GOTO 9999   ! non ho torvato nessuna unita' libera
!WRITE (*,*) "uso unita ",iun

! Cerco codice di errore per EOF
OPEN (unit=k, STATUS="SCRATCH", FORM="FORMATTED", ACCESS="SEQUENTIAL", &
  PAD="NO", ERR=9999)
ENDFILE (k)
REWIND (k)
READ (k,*,IOSTAT=eof)
CLOSE(k)

! Cerco codice di errore per EOR
OPEN (unit=k, STATUS="SCRATCH", FORM="FORMATTED", ACCESS="SEQUENTIAL", &
  PAD="NO", ERR=9999)
WRITE (k,'(a1)') "1" 
WRITE (k,'(a1)') "2"
REWIND (k)
READ (k,'(i1)',ADVANCE="NO",ERR=9999) idummy
READ (k,'(i1)',ADVANCE="NO",IOSTAT=eor) idummy
CLOSE(k)

!write (*,*) "eof,eor ",eof,eor
RETURN

! Gestione errori
9999 CONTINUE
WRITE (*,*) "Errore in subroutine get_eof_eor, usero' valori di default"
eof = -1
eor = -2
RETURN

END SUBROUTINE get_eof_eor

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_namelist(nnml)
!
OPEN (UNIT=40, FILE="scores_seriet.inp", STATUS="REPLACE", FORM="FORMATTED")

!                      123451234512345
WRITE (40,'(a,i3,a)') "! oss. pre. dir.?"
DO k = 1,nnml
  WRITE (40,'(3(1x,i3))') k,k,0
ENDDO
WRITE (40,*)
WRITE (40,'(a)') "! Tracciato:"
WRITE (40,'(a)') "! Colonna nel file osservazioni"
WRITE (40,'(a)') "! Colonna nel file previsioni"
WRITE (40,'(a)') "! Flag direzione del vento (0: NO; 1: SI)"

CLOSE (40)

END SUBROUTINE scrive_namelist

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Dati due files in uno dei formati SMR (seriet, estra_orari o"
WRITE (*,*) "  estra_temp), calcola alcuni scores tra i dati di alcune "
WRITE (*,*) "  colonne del primo (oss.) e del secondo (prev.)"
WRITE (*,*)
WRITE (*,*) "Uso: scores_seriet fileoss filepre fileout [filtro] [-h] [-c/-cN]"
WRITE (*,*) "-h: visualizza questo help"
WRITE (*,*) "-c: crea un file scores_seriet.inp di esempio"
WRITE (*,*) "-cN: crea un file scores_seriet.inp di esempio con i campi da 1 a N"
WRITE (*,*) "filtro: seleziona gli istanti da incldere negli scores. Valori gestiti:"
WRITE (*,*) "  day   : istanti in cui il sole e' sopra l'orizzionte"
WRITE (*,*) "  night : istanti in cui il sole e' sotto l'orizzionte"
WRITE (*,*)
WRITE (*,*) "NOTE:"
WRITE (*,*) "Legge da scores_seriet.inp la lista delle colonne da confrontare"
WRITE (*,*) "  nei due files"
WRITE (*,*) "Il file con il passo temporale piu lungo deve essere messo "
WRITE (*,*) "  sempre come file previsto"
WRITE (*,*) "L'angolo solare e' relativo a Bologna, per cambiare punto "
WRITE (*,*) "  modificare nel sorgente"
WRITE (*,*)
 !            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
