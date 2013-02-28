PROGRAM diff_grib
!--------------------------------------------------------------------------
! Programma che legge due files con molti grib e visualizza le differenze
! campo a campo. Pensato principalmente per confrontare due files che 
! dovrebbero essere uguali, ma non sono identici al bit.
!
! L'analisi dei campi in output cerca di tener conto dell'errore di 
! troncamento intrinseco nei GRIB, visualizzando solo le differenze 
! significative.
!
! Note:
! - Ancora da implementare il confronto degli header (controllare le chiavi
!   piu' significative, gestendo GRIB1 e GRIB2; scrivere su files di log
!   le differenze trovate per ciascuna sezione.
!   Per un confronto piu' approfindito degli header, vedi llgrib.exe e 
!   diff_header_gribex.exe (solo GRIB1)
! - Gestione dei campi con dati mancanti implementata ma non testata
! - Versione 1.* rinominata come diff_gribex.f90
!
!                                         Versione 2.0.1, Enrico 27/02/2013
!--------------------------------------------------------------------------

USE grib_api
USE missing_values
IMPLICIT NONE

INTEGER :: ifin,ifout,igin,igout,iret,kg
CHARACTER(LEN=250) :: filea,fileb

REAL, ALLOCATABLE :: fielda(:),fieldb(:)
REAL :: ad,ads,avea,aveb,rms,rms_mx,rms_sig,rms_sig_mx,bias,bias_sig
REAL :: abs_diff,abs_diff_mx,abs_diff_sig,abs_diff_sig_mx
REAL :: abs_diff_sig_norm,abs_diff_sig_norm_mx
REAL :: rva,rvb,stepa,stepb,step,rangea,rangeb
INTEGER :: ifa,ifb,iga,igb
INTEGER :: nia,nja,nib,njb,npa,npb,noka,nokb,nok,kp
INTEGER :: gnova,noma,nocva,gnovb,nomb,nocvb
INTEGER :: ena,bpva,dsfa,bsfa
INTEGER :: enb,bpvb,dsfb,bsfb
CHARACTER (LEN=14) :: str_nok
CHARACTER (LEN=1) :: eq_ksec(4),worse_eq_ksec(4)
LOGICAL :: lnok

!--------------------------------------------------------------------------
! 1) Preliminari

! Parametri da riga comando
CALL getarg(1,filea)
CALL getarg(2,fileb)
IF (TRIM(filea) == "" .OR. TRIM(fileb) == "" .OR. &
  TRIM(filea) == "-h" .OR. TRIM(filea) == "--help") THEN
  CALL scrive_help
  STOP
ENDIF

! Apro i files
CALL grib_open_file(ifa,filea,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifb,fileb,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9998

!--------------------------------------------------------------------------
! 2) Elaborazioni (ciclo sui grib)
!

worse_eq_ksec(:) = "Y"
lnok = .TRUE.
rms_mx = 0.
rms_sig_mx = 0.
abs_diff_mx = 0.
abs_diff_sig_mx = 0.

!                123456---1234567---123456-123456---1234567890-1234567890-1234567890-1234567890-1234567890-(123456)
WRITE (*,'(a)') " ngrib   ksec ==     nok1   nok2         ave1      BiasS       rmsS    absDmxS absDmxS/rg    (nok)"

OPEN (UNIT=90, FILE="diff_grib_sez4.log", STATUS="REPLACE")
!                123456---1234-1234-1234-1234567890-12345678901234---1234-1234-1234-1234567890-12345678901234
WRITE (90,'(a)') "   kg   bpva bsfa dsfa      stepa            rva   bpvb bsfb dsfb      stepb            rvb"
DO kg = 1,HUGE(0)

!--------------------------------------------------------------------------
! 2.1 Preliminari

!  Leggo il prossimo campo da filea e fileb
  CALL grib_new_from_file(ifa,iga,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9997
  
  CALL grib_new_from_file(ifb,igb,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9996

! Inizializzazioni
  eq_ksec(1:4) = "-"

! Leggo edizione numero di punti nei due grib
  CALL grib_get(iga,"editionNumber",ena)
  CALL grib_get(iga,"Ni",nia)
  CALL grib_get(iga,"Nj",nja)
  npa = nia * nja
  CALL grib_get(igb,"editionNumber",enb)
  CALL grib_get(igb,"Ni",nib)
  CALL grib_get(igb,"Nj",njb)
  npb = nib * njb

!--------------------------------------------------------------------------
! 2.2 Confronto gli header

!
! E' un lavoraccio, dipende dall'edizione, ....
!

! Sezione 4 (GRIB1) / sezione 5 (GRIB2)
! step* e' la differenza minima tra due valori codificati nel grib.
! La codifica dei Grib segue l'algortimo Y * 10^D = R + 2^E * X, con:
! Y = valore originale            X = valore scritto nel grib
! D = decimal scale factor        E = binary scale factor
! R = reference value = valore minimo di (Y * 10^D) 

  CALL grib_get(iga,"bitsPerValue",bpva)
  CALL grib_get(iga,"referenceValue",rva)
  CALL grib_get(iga,"decimalScaleFactor",dsfa)
  CALL grib_get(iga,"binaryScaleFactor",bsfa)
  CALL grib_get(igb,"bitsPerValue",bpvb)
  CALL grib_get(igb,"referenceValue",rvb)
  CALL grib_get(igb,"decimalScaleFactor",dsfb)
  CALL grib_get(igb,"binaryScaleFactor",bsfb)
  stepa = 2.**bsfa / 10.**dsfa
  stepb = 2.**bsfb / 10.**dsfb
  step = MIN(stepa,stepb)

  IF (bpva == bpvb .AND. dsfa == dsfb .AND. bsfa == bsfb .AND. &
      ABS(rva-rvb) < 2*step) THEN
    eq_ksec(4) = "Y"
  ELSE
    eq_ksec(4) = "N"
    WRITE (90,'(i6,2(2x,3(1x,i4),1x,e10.3,1x,e14.7))') &
      kg,bpva,bsfa,dsfa,stepa,rva,bpvb,bsfb,dsfb,stepb,rvb
  ENDIF

!--------------------------------------------------------------------------
! 2.3 Confronto i campi
 
  CALL grib_get(iga,"getNumberOfValues",gnova)    ! totale di punti nel grib
  CALL grib_get(iga,"numberOfMissing",noma)       ! n.ro dati mancanti
  CALL grib_get(iga,"numberOfCodedValues",nocva)  ! n.ro dati validi
  CALL grib_get(igb,"getNumberOfValues",gnovb)    ! totale di punti nel grib
  CALL grib_get(igb,"numberOfMissing",nomb)       ! n.ro dati mancanti
  CALL grib_get(igb,"numberOfCodedValues",nocvb)  ! n.ro dati validi

  ALLOCATE (fielda(npa),fieldb(npb))
  IF (nocva == 0) THEN
    fielda(:) = rmiss
  ELSE
    CALL grib_set(iga,"missingValue",rmiss)
    CALL grib_get(iga,"values",fielda)
  ENDIF
  IF (nocvb == 0) THEN
    fieldb(:) = rmiss
  ELSE
    CALL grib_set(igb,"missingValue",rmiss)
    CALL grib_get(igb,"values",fieldb)
  ENDIF

  IF (noma + nocva /= gnova .OR. &
      (nocva /= 0 .AND. nocva /= COUNT(fielda(1:npa) /= rmiss))) GOTO 9995
  IF (nomb + nocvb /= gnovb .OR. &
      (nocvb /= 0 .AND. nocvb /= COUNT(fieldb(1:npb) /= rmiss))) GOTO 9994

! Differenze significative (ie. a meno del troncamento GRIB)

! N.ro punti e valori medi
  noka = nocva
  nokb = nocvb

  IF (noka /= 0) THEN
    avea = SUM(fielda(1:npa), MASK = fielda(1:npa) /= rmiss) / REAL(noka)
  ELSE
    avea = rmiss
  ENDIF
  IF (nokb /= 0) THEN
    aveb = SUM(fieldb(1:npb), MASK = fieldb(1:npb) /= rmiss) / REAL(nokb)
  ELSE
    aveb = rmiss
  ENDIF

  IF (avea /= rmiss .AND. aveb /= rmiss) THEN
    bias = aveb - avea
    bias_sig = REAL(INT(bias/step))*step
  ELSE
    bias = rmiss
    bias_sig = rmiss
  ENDIF

! RMS, MAX(abs_diff), MAX(abs_diff_sig)
  IF (npa == npb) THEN
    nok = 0
    rms = 0.
    rms_sig = 0.
    abs_diff = 0.
    abs_diff_sig = 0.
    DO kp = 1,npa
      IF (fielda(kp) /= rmiss .AND. fieldb(kp) /= rmiss) THEN
        nok = nok + 1
        ad = ABS(fielda(kp)- fieldb(kp))
        ads = REAL(INT(ad/step))*step
        abs_diff = MAX(abs_diff,ad)
        abs_diff_sig = MAX(abs_diff_sig,ads)
        rms = rms + ad*ad
        rms_sig = rms_sig + ads*ads

if (kg==756) write (92,*) kp,fielda(kp),fieldb(kp),step,ad,ads,abs_diff_sig

      ENDIF
    ENDDO

    IF (nok > 0) THEN
      rms = SQRT(rms / REAL(nok))
      rms_sig = SQRT(rms_sig / REAL(nok))
      IF (dsfa == dsfb .AND. bsfa == bsfb) THEN
        IF (abs_diff_sig < EPSILON(0.)) THEN
          abs_diff_sig_norm = 0.
        ELSE
          rangea = MAXVAL(fielda(1:npa),MASK=fielda(kp)/=rmiss) - &
             MINVAL(fielda(1:npa),MASK=fielda(kp)/=rmiss)
          rangeb = MAXVAL(fieldb(1:npb),MASK=fieldb(kp)/=rmiss) - &
             MINVAL(fieldb(1:npb),MASK=fieldb(kp)/=rmiss)
          abs_diff_sig_norm = abs_diff_sig / MAX(rangea,rangeb)
        ENDIF
      ELSE
        abs_diff_sig = rmiss
        abs_diff_sig_norm = rmiss
      ENDIF
    ELSE
      rms = rmiss
      rms_sig = rmiss
      abs_diff = rmiss
      abs_diff_sig = rmiss
      abs_diff_sig_norm = rmiss
    ENDIF

  ELSE
    rms = rmiss
    rms_sig = rmiss
    abs_diff = rmiss
    abs_diff_sig = rmiss
    abs_diff_sig_norm = rmiss

  ENDIF

! Salvo i risultati con piu' differenze
  WHERE (worse_eq_ksec(1:4) /= "-" .AND. eq_ksec(1:4) == "N") 
    worse_eq_ksec(1:4) = "N"
  ENDWHERE
  WHERE (eq_ksec(1:4) == "-") 
    worse_eq_ksec(1:4) = "-"
  ENDWHERE
  IF (noka /= nokb) lnok = .FALSE.
  IF (rms /= rmiss .AND. rms > rms_mx) rms_mx = rms
  IF (rms_sig /= rmiss .AND. rms_sig > rms_sig_mx) rms_sig_mx = rms_sig
  IF (abs_diff /= rmiss .AND. abs_diff > abs_diff_mx) &
    abs_diff_mx = abs_diff
  IF (abs_diff_sig /= rmiss .AND. abs_diff_sig > abs_diff_sig_mx) &
    abs_diff_sig_mx = abs_diff_sig
  IF (abs_diff_sig_norm /= rmiss .AND. &
    abs_diff_sig_norm > abs_diff_sig_norm_mx) &
    abs_diff_sig_norm_mx = abs_diff_sig_norm

! Le visualizzo
  IF (abs_diff_sig_norm == rmiss) abs_diff_sig_norm = -9.99999
  WRITE (*, &
    '(i6,2x,4(1x,a1),2x,2(1x,i6),2x,4(1x,e10.3),1x,f10.5,1x,a1,i6,a1)') &
    kg,eq_ksec(1:4),noka,nokb,avea,bias_sig,rms_sig,abs_diff_sig, &
    abs_diff_sig_norm,"(",nok,")"

! Libero memoria
  DEALLOCATE (fielda,fieldb)

ENDDO

!--------------------------------------------------------------------------
! 3) Output riassuntivo e conclusione

IF (lnok) THEN
  str_nok = "no_differences"
ELSE
  str_nok = "   differences"
ENDIF

WRITE (*,*)
WRITE (*,'(a)') "Caso peggiore:"
WRITE (*,'(6x,2x,4(1x,a1),2x,a14,2x,2(1x,10x),2(1x,e10.3),1x,f10.5)') &
  worse_eq_ksec(1:4),str_nok,rms_sig_mx,abs_diff_sig_mx, &
  abs_diff_sig_norm_mx

CALL grib_close_file(ifa)
CALL grib_close_file(ifb)

WRITE (*,*) "Elaborazioni completate, elaborati ",kg-1," campi"
STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filea)
STOP

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fileb)
STOP

9997 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filea)," grib n.ro " ,kg
STOP

9996 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(fileb)," grib n.ro " ,kg
STOP

9995 CONTINUE
WRITE (*,*) "Errore nelle chiavi relative ai dati mancanti in ",TRIM(filea)
WRITE (*,*) "Dati totali (getNumberOfValues):   ",gnova
WRITE (*,*) "Dati validi (numberOfCodedValues): ",nocva
WRITE (*,*) "Dati mancanti (numberOfMissing):   ",noma
WRITE (*,*) "Dati mancanti (matrice grib):      ",COUNT(fielda(1:npa) /= rmiss)
STOP

9994 CONTINUE
WRITE (*,*) "Errore nelle chiavi relative ai dati mancanti in ",TRIM(fileb)
WRITE (*,*) "Dati totali (getNumberOfValues):   ",gnovb
WRITE (*,*) "Dati validi (numberOfCodedValues): ",nocvb
WRITE (*,*) "Dati mancanti (numberOfMissing):   ",nomb
WRITE (*,*) "Dati mancanti (martice grib):      ",COUNT(fieldb(1:npb) /= rmiss)
STOP

END PROGRAM diff_grib

!==========================================================================
SUBROUTINE scrive_help

!            12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) "Uso: diff_grib.exe file1 file2"
WRITE (*,*) ""
WRITE (*,*) "Programma che cerca le differnze significative tra 2 file grib."
WRITE (*,*) "Per ciascun campo scrive:"
WRITE (*,*) "- numero progressivo;"
WRITE (*,*) "- 4 flag che indicano se gli header coincidono. Per i campi in cui ci sono "
WRITE (*,*) "  differenze, scrive un record nei files diff_grib_sez*.log (INCOMPLETO);"
WRITE (*,*) "- numero di dati validi in ciascuno dei due files;"
WRITE (*,*) "- media del campo nel primo file;"
WRITE (*,*) "- Bias, RMS, max della differenza assoluta, calcolati in modo da ignorare le"
WRITE (*,*) "  differnze dovute al troncamento GRIB;"
WRITE (*,*) "- (max della differenza assoluta) / (range di valori presenti nel GRIB);"
WRITE (*,*) "- numero di dati simultaneamente validi in entrambi i files."

END SUBROUTINE scrive_help
