PROGRAM bufr_csv2temp
!--------------------------------------------------------------------------
! Legge un file csv prodotto da "dbamsg dump -csv", relativo a una stazione
! di radiosondaggio, e lo riscrive nel vecchio formato "estra_temp" (ie.
! un file per ogni radiosondaggio)
!
! Uso: bufr_csv2temp.exe filein [-int] [-geo] [-rdate] [-zsta Z] 
!    [-phpa] [-rh] [-uv] [-tc] [-h]
!
! TODO:
! - Studiare opzione per sfoltire i livelli: in tempi recenti, alcune 
!   stazioni (es: Milano) trasmettono i dati ad alta risoluzione, con 
!   migliaia di livelli anche nella stratosfera.
! - Aggiungere scrittura di un file con l'anagrafica della stazione, in 
!   analogia a quello che fa bufr_csv2orari.f90
!
! Note:
! Programma chiamato da estra_oss.ksh
!
! Possibili problemi non gestiti:
! - Calcolo del geopotenziale in sondaggi con alcuni dati mancanti (per ora 
!   lo calcolo solo se tutti i dati sono mancanti; gestire il caso con il 
!   livello 1000 hPa sottoterra e geop mancante)
!
! Chiavi utili nei BUFR-WMO realtivi ai rsd, scritti csv
!   edition: inizio messaggio
!   B01001 WMO BLOCK NUMBER
!   B01002 WMO STATION NUMBER
!   B05001 LATITUDE
!   B06001 LONGITUDE
!   B07030 HEIGHT OF STATION GROUND ABOVE MSL
!   B04001 YEAR
!   B04002 MONTH
!   B04003 DAY
!   B04004 HOUR
!   B04005 MINUTE
!   B07004 PRESSURE (inizio nuovo livello)
!   B10009 GEOPOTENTIAL HEIGHT
!   B12101 TEMPERATURE
!   B12001 TEMPERATURE (dati vecchi)
!   B12103 DEW-POINT TEMPERATURE 
!   B12003 DEW-POINT TEMPERATURE (dati vecchi)
!   B11001 WIND DIRECTION
!   B11002 WIND SPEED 
!
!                                         Versione 1.0.1, Enrico 29/06/2015
!--------------------------------------------------------------------------

USE missing_values

IMPLICIT NONE

INTEGER, PARAMETER :: maxlev = 10000  ! Max numero di livell in un rsd
INTEGER, PARAMETER :: iu = 20         ! Unita' per lettura filein

REAL :: pp(maxlev),zz(maxlev),tt(maxlev),td(maxlev),ff(maxlev),dd(maxlev)
REAL :: z_sta
INTEGER :: idp,eof,eor,ios,ier,k,kp,p1,cnt_rsd
INTEGER :: id_block,id_sta,yy,mm,dy,hh,mn,nlev
CHARACTER (LEN=200) :: filein,chdum,next_arg,chrec,key
LOGICAL :: lint,lgeo,lrdate,lphpa,lrh,luv,ltc,first,force_zsta

! Flag che indica se nel livello corrente ho trovato dati di tt,td,dd,ff,zz 
LOGICAL :: lfound(5)

!==========================================================================
! 1) Preliminari

! 1.1 Parametri da riga comando
lint = .FALSE.
lgeo = .FALSE.
lrdate = .FALSE.
lphpa = .FALSE.
lrh = .FALSE.
luv = .FALSE.
ltc = .FALSE.
force_zsta = .FALSE.
next_arg = ""
idp = 0
ios = 0

DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (next_arg == "zsta") THEN
    READ (chdum,*,IOSTAT=ios) z_sta
    force_zsta = .TRUE.
    next_arg = ""
  ELSE IF (TRIM(chdum) == "-int") THEN
    lint = .TRUE.
  ELSE IF (TRIM(chdum) == "-geo") THEN
    lgeo = .TRUE.
  ELSE IF (TRIM(chdum) == "-rdate") THEN
    lrdate = .TRUE.
  ELSE IF (TRIM(chdum) == "-zsta") THEN
    next_arg = "zsta"
  ELSE IF (TRIM(chdum) == "-phpa") THEN
    lphpa = .TRUE.
  ELSE IF (TRIM(chdum) == "-rh") THEN
    lrh = .TRUE.
  ELSE IF (TRIM(chdum) == "-uv") THEN
    luv = .TRUE.
  ELSE IF (TRIM(chdum) == "-tc") THEN
    ltc = .TRUE.
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE DEFAULT
      CALL write_help
      STOP
    END SELECT
  ENDIF
ENDDO

IF (idp /= 1 .OR. ios /= 0) THEN
  CALL write_help
  STOP 1
ENDIF

! Altri preliminari
CALL get_eof_eor(eof,eor)
OPEN (UNIT=iu, FILE=filein, STATUS="OLD", FORM="FORMATTED", ERR=9999)

!==========================================================================
! 2) Elaborazioni (ciclo sui rsd presenti in filein)

first = .TRUE.
lfound(:) = .TRUE.
cnt_rsd = 0
DO k = 1,HUGE(0)
  READ (iu,'(a)',IOSTAT=ios) chrec
  
! Fine file: termino
  IF (ios == eof) THEN
    EXIT
  ELSE IF (ios /= 0) THEN
    GOTO 9998
  ENDIF

  p1 = INDEX(chrec,",")
! record senza separatore o chiave senza valore
  IF (p1 == 0 .OR. LEN(TRIM(chrec)) == p1) CYCLE 
  key = chrec(1:p1-1)

! "edition": inizio un nuovo messaggio
  IF (key == "edition") THEN
  
!   Elaboro e scrivo il messaggio appena concluso
    IF (.NOT. first) THEN
      CALL proc_rsd(nlev,pp,zz,tt,td,ff,dd, &
        lint,lgeo,lrdate,lphpa,lrh,luv,ltc, &
        id_block,id_sta,z_sta,yy,mm,dy,hh,mn,ier)
      cnt_rsd = cnt_rsd + 1
    ENDIF
    first = .FALSE.

!   Re-inizializzo le variabili per il prossimo rsd
    id_block = imiss
    id_sta = imiss
    IF (.NOT. force_zsta) z_sta = rmiss
    yy = imiss
    mm = imiss
    dy = imiss
    hh = imiss
    mn = imiss

    pp(:) = rmiss
    zz(:) = rmiss
    tt(:) = rmiss
    td(:) = rmiss
    ff(:) = rmiss
    dd(:) = rmiss
    nlev = 0

! Chiavi relative ad anagrafica stazione e ora del sondaggio
  ELSE IF (key == "B01001") THEN
    READ (chrec(p1+1:),*,IOSTAT=ios) id_block
  ELSE IF (key == "B01002") THEN
    READ (chrec(p1+1:),*,IOSTAT=ios) id_sta
  ELSE IF (key == "B07030" .AND. .NOT. force_zsta) THEN
    READ (chrec(p1+1:),*,IOSTAT=ios) z_sta
  ELSE IF (key == "B04001") THEN
    READ (chrec(p1+1:),*,IOSTAT=ios) yy
  ELSE IF (key == "B04002") THEN
    READ (chrec(p1+1:),*,IOSTAT=ios) mm
  ELSE IF (key == "B04003") THEN
    READ (chrec(p1+1:),*,IOSTAT=ios) dy
  ELSE IF (key == "B04004") THEN
    READ (chrec(p1+1:),*,IOSTAT=ios) hh
  ELSE IF (key == "B04005") THEN
    READ (chrec(p1+1:),*,IOSTAT=ios) mn

! "pressione": inizio un nuovo livello
  ELSE IF (key == "B07004") THEN
    nlev = nlev + 1
    IF (nlev > maxlev) GOTO 9997
    READ (chrec(p1+1:),*,IOSTAT=ios) pp(nlev)
    lfound(:) = .FALSE.

! Chiavi relative alle altre varibili atmosferiche
  ELSE IF (key == "B12101" .OR. key == "B12001") THEN
    READ (chrec(p1+1:),*,IOSTAT=ios) tt(nlev)
    IF (lfound(1)) WRITE (*,*) "Warning: trovata T senza P, record ",k 
    lfound(1) = .TRUE.

  ELSE IF (key == "B12103" .OR. key == "B12003") THEN
    READ (chrec(p1+1:),*,IOSTAT=ios) td(nlev)
    IF (lfound(2)) WRITE (*,*) "Warning: trovata Td senza P, record ",k 
    lfound(2) = .TRUE.

  ELSE IF (key == "B11001") THEN
    READ (chrec(p1+1:),*,IOSTAT=ios) dd(nlev)
    IF (lfound(3)) WRITE (*,*) "Warning: trovata DD senza P, record ",k 
    lfound(3) = .TRUE.

  ELSE IF (key == "B11002") THEN
    READ (chrec(p1+1:),*,IOSTAT=ios) ff(nlev)
    IF (lfound(4)) WRITE (*,*) "Warning: trovata FF senza P, record ",k 
    lfound(4) = .TRUE.

  ELSE IF (key == "B10009") THEN
    READ (chrec(p1+1:),*,IOSTAT=ios) zz(nlev)
    IF (lfound(5)) WRITE (*,*) "Warning: trovata Z senza P, record ",k 
    lfound(5) = .TRUE.

  ENDIF

  IF (ios /= 0) WRITE (*,*) &
    "Warning: errore di lettura al record ",k,":",TRIM(chrec)

ENDDO

! Elaboro e scrivo l'ultimo messaggio
IF (.NOT. first) THEN
  CALL proc_rsd(nlev,pp,zz,tt,td,ff,dd, &
    lint,lgeo,lrdate,lphpa,lrh,luv,ltc, &
    id_block,id_sta,z_sta,yy,mm,dy,hh,mn,ier)
  cnt_rsd = cnt_rsd + 1
ENDIF

! Log a schermo

WRITE (*,*) "Scritti ",cnt_rsd," sondaggi"
STOP 0

!==========================================================================
! 3) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 1

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," record ",k
STOP 2

9997 CONTINUE
WRITE (*,*) "Troppi livelli in un rsd, aumentare parametro maxlev (",maxlev,")"
STOP 3

END PROGRAM bufr_csv2temp

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE proc_rsd(nlev,pp,zz,tt,td,ff,dd, lint,lgeo,lrdate,lphpa,lrh,luv,ltc, &
  id_block,id_sta,z_sta,yy,mm,dy,hh,mn, ier)

!
! Elabora e scrive un radiosondaggio
!
USE datetime_class
USE missing_values

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmiss_out = -9999. ! Valore per dati mancanti in output

! Parametri della subroutine
REAL, INTENT(IN) :: pp(nlev),zz(nlev),tt(nlev),td(nlev),ff(nlev),dd(nlev),z_sta
INTEGER, INTENT(IN) :: id_block,id_sta,yy,mm,dy,hh,mn,nlev
LOGICAL, INTENT(IN) :: lint,lgeo,lrdate,lphpa,lrh,luv,ltc
INTEGER, INTENT(OUT) :: ier

! Variabili inizializzate
CHARACTER (LEN=24) :: nome_sta = ""
CHARACTER (LEN=10) :: label(6) = &
   (/"    Press.","     Temp.","    T.rug.",&
     "    Direz.","    Modulo","     Quota"/)

! Altre variabili locali
TYPE(datetime) :: datat_rsd,datat_out,data_syn
REAL :: u,v
REAL :: out_val(1:nlev,6),dum_val(1:nlev,6),plast,slope,tvmed
REAL :: lnp(nlev),esattd(nlev),esatt(nlev),w(nlev),tv(nlev),rh(nlev)
INTEGER :: yy_syn,mm_syn,dy_syn,hh_syn,kl2,kp,klint,kl1,kl,l1,out_z_sta
CHARACTER (LEN=200) :: fileout
CHARACTER (LEN=10) :: ch10

!==========================================================================
 
!--------------------------------------------------------------------------
! 0) Costruisco il nome per fileout (data/ora del radiosondaggio)

IF (yy==imiss .OR. mm==imiss .OR. dy==imiss .OR. hh==imiss .OR. mn==imiss) GOTO 9999
datat_rsd = datetime_new(YEAR=yy, MONTH=mm, DAY=dy, HOUR=hh, MINUTE=mn)

IF (lrdate) THEN          ! arrotondo all'ora piu' vicina
  IF (mn > 30) THEN
    datat_out = datat_rsd + timedelta_new(HOUR=1)
  ELSE
    datat_out = datat_rsd + timedelta_new(HOUR=1)
  ENDIF

ELSE                      ! arrotondo all'ora sinottica (3h) piu' probabile
  hh_syn = (MOD(hh+2,24) / 3) * 3
  data_syn = datat_rsd + timedelta_new(HOUR=2)
  CALL getval(data_syn, YEAR=yy_syn, MONTH=mm_syn, DAY=dy_syn)
  datat_out = datetime_new(YEAR=yy_syn, MONTH=mm_syn, DAY=dy_syn, HOUR=hh_syn)

ENDIF

CALL getval(datat_out, SIMPLEDATE=ch10) 
WRITE (fileout,'(i2.2,i3.3,3a)') id_block,id_sta,"_",ch10,".dat"

!--------------------------------------------------------------------------
! 1) Controllo il profilo di P (solo dati validi a P decrescente)
!    Riverso i dati nell'array out_val

out_val(1:nlev,1) = pp(1:nlev)
out_val(1:nlev,2) = tt(1:nlev)
out_val(1:nlev,3) = td(1:nlev)
out_val(1:nlev,4) = dd(1:nlev)
out_val(1:nlev,5) = ff(1:nlev)
out_val(1:nlev,6) = zz(1:nlev)

IF (ANY(.NOT. c_e(pp(1:nlev)))) GOTO 9998
IF (ANY(pp(1:nlev) <= 0.)) GOTO 9997
plast = pp(1)
DO kl = 2,nlev
  IF (out_val(kl,1) > plast) THEN
    IF (kl < nlev) WRITE (*,*) &
      "Warning: pressione crescente al livello ",kl," di ",nlev
    out_val(kl:,1:6) = rmiss
    EXIT
  ELSE
    plast = out_val(kl,1)
  ENDIF
ENDDO

!--------------------------------------------------------------------------
! 2) Se richiesto, interpolo T,Td,FF e DD su tutti i livelli
!    l1: ultimo livello con dati validi prima del livello corrente

IF (lint) THEN
  lnp = LOG(out_val(1:nlev,1))
  dum_val(:,:) = out_val(:,:)

  DO kp = 2,5
    kl1 = imiss
    DO kl2 = 1, nlev
      IF (c_e(dum_val(kl2,kp))) THEN
        out_val(kl2,kp) = dum_val(kl2,kp)
        IF (c_e(kl1) .AND. kl2 > kl1+1) THEN
          slope = (dum_val(kl2,kp) - dum_val(kl1,kp)) / (lnp(kl2) - lnp(kl1))
          DO klint = kl1+1, kl2-1
            out_val(klint,kp) = out_val(kl1,kp) + slope * (lnp(klint) - lnp(kl1))
          ENDDO
        ENDIF
        kl1 = kl2
      ENDIF
    ENDDO
  ENDDO

ENDIF

!--------------------------------------------------------------------------
! 3) Se richiesto, calcolo il profilo di Tv e RH
!    Funzioni e unita' di misura della vecchia termolib: 
!    esat in hPa, W in g/kg, Tv in K, RH in %

IF (lgeo .OR. lrh) THEN
  DO kl = 1,nlev
    IF (c_e(out_val(kl,2)) .AND. c_e(out_val(kl,3)) .AND. &
            out_val(kl,2) > 0. .AND. out_val(kl,3) > 0. .AND. &
            out_val(kl,3) <= out_val(kl,2)) THEN
      esatt(kl) = 6.1078 * EXP ((17.2693882*(out_val(kl,2)-273.15))/ &
        (out_val(kl,2) - 273.15+237.3))
      esattd(kl) = 6.1078 * EXP ((17.2693882*(out_val(kl,3)-273.15))/ &
        (out_val(kl,3) - 273.15+237.3))
      rh(kl) = esattd(kl) / esatt(kl) * 100.
      w(kl) = 622. * esattd(kl) / (out_val(kl,1)/100. - esattd(kl))
      tv(kl) = out_val(kl,2) + 0.0006078 * w(kl)
    ELSE
      tv(kl) = rmiss
      rh(kl) = rmiss
    ENDIF
  ENDDO
ENDIF

!--------------------------------------------------------------------------
! 4) Se richiesto, calcolo il geopotenziale sui livelli in cui e'mancante
!    Note:
!    - Se il primo livello e' 1000 hPa, sottoterra e con geop mancante, 
!      suppongo che il secondo livello sia la superifice, e inizio il 
!      calcolo da li'.
!    - Rispetto ai valori riportati nei rsd, questo calcolo produce 
!      differenze dell'ordine di 10m

IF (lgeo) THEN

  IF (.NOT. c_e(out_val(1,6)) .AND. .NOT. c_e(z_sta)) THEN
    WRITE (*,*) ch10, &
      ": calcolo geop impossibile: specificare quota stazione (parametro -zsta)"

  ELSE IF (ALL(c_e(out_val(1:nlev,6)))) THEN
    WRITE (*,*) ch10,": geop gia' presente, non lo calcolo"

  ELSE
    l1 = 2
    IF (.NOT. c_e(out_val(1,6))) THEN
      IF (ABS(out_val(1,1)-100000.) < 1.e-4 .AND. c_e(out_val(2,6))) THEN
        WRITE (*,*) ch10, &
          ": primo livello 1000hPa sottoterra, calcolo geop dal 2o livello"
        l1 = 3
      ELSE
        out_val(1,6) = z_sta
      ENDIF
    ENDIF
    
!deb write (90,*) "lev,P,estattd,w,t,tv,tvmed,delta_geo"
    DO kl = l1,nlev
      IF (c_e(out_val(kl,6))) CYCLE
      IF (.NOT. c_e(out_val(kl-1,6)) .OR. ANY(.NOT. c_e(out_val(kl-1:kl,2:3))) .OR. &
          .NOT. c_e(tv(kl-1)) .OR. .NOT. c_e(tv(kl))) CYCLE
      tvmed = (tv(kl-1) + tv(kl)) / 2.
      out_val(kl,6) = out_val(kl-1,6) + 14.64285*tvmed*2.* &
        ALOG(out_val(kl-1,1)/out_val(kl,1))
!deb write (90,'(i3,f10.1,6f10.5)') &
!deb   kl,out_val(kl,1),esattd(kl),w(kl),out_val(kl,2),tv(kl),tvmed, &
!deb   14.64285*tvmed*2.*ALOG(out_val(kl-1,1)/out_val(kl,1))
    ENDDO

  ENDIF

ENDIF

!--------------------------------------------------------------------------
! 4) Se richiesto, converto unita' di misura

IF (lrh) THEN
  out_val(1:nlev,3) = rh(1:nlev)
  label(3) = "        RH"
ENDIF

IF (luv) THEN
  dum_val(:,4:5) = out_val(:,4:5)
  DO kl = 1,nlev
    IF (c_e(dum_val(kl,4)) .AND. c_e(dum_val(kl,5))) THEN
      out_val(kl,4) = u(dum_val(kl,5),dum_val(kl,4))
      out_val(kl,5) = v(dum_val(kl,5),dum_val(kl,4))
    ELSE
      out_val(kl,4:5) = rmiss
    ENDIF
  ENDDO
  label(4) = "    U-wind"
  label(5) = "    V-wind"
ENDIF

IF (lphpa) THEN
  out_val(1:nlev,1) = out_val(1:nlev,1) / 100.
ENDIF

IF (ltc) THEN
  out_val(1:nlev,2) = out_val(1:nlev,2) - 273.15
  IF (.NOT. lrh) out_val(1:nlev,3) = out_val(1:nlev,3) - 273.15
ENDIF

!--------------------------------------------------------------------------
! 6) Scrivo su fileout

WHERE (out_val(1:nlev,1:6) == rmiss)
  out_val(1:nlev,1:6) = rmiss_out
ENDWHERE
IF (c_e(z_sta)) THEN
  out_z_sta = NINT(z_sta)
ELSE
  out_z_sta = -999
ENDIF

OPEN (UNIT=30, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")
WRITE (30,'(1x,i2.2,i3.3,1x,a24,1x,a8,1x,a2,i2.2,1x,i3.3,1x,i4)') &
  id_block,id_sta,nome_sta,ch10(1:8),ch10(9:10),00,nlev,out_z_sta
WRITE (30,'(6(1x,a10))') label(1:6)
DO kl = 1,nlev
  WRITE (30,'(6(1x,f10.1))') out_val(kl,1:6)
ENDDO
CLOSE (30)

ier = 0
RETURN

!--------------------------------------------------------------------------
! 6) Gestione errori

9999 CONTINUE
WRITE (*,*) "Trovato rsd senza data: skip"
ier = 1
RETURN

9998 CONTINUE
WRITE (*,*) "Trovato rsd con pressioni mancanti: skip"
ier = 2
RETURN

9997 CONTINUE
WRITE (*,*) "Trovato rsd con pressione <= 0: skip"
ier = 3
RETURN

END SUBROUTINE proc_rsd

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

function u(mag,dir)
  IMPLICIT NONE
  REAL, INTENT(IN) :: mag,dir
  REAL u
  REAL, PARAMETER :: rtd = 3.141592654/180.

  if(mag==0)then
     u=0
  else
     u=mag*(-sin(dir*rtd))
  endif

  RETURN
end function u

!--------------------------------------------------------------------------

function v(mag,dir)
  IMPLICIT NONE
  REAL, INTENT(IN) :: mag,dir
  REAL v
  REAL, PARAMETER :: rtd = 3.141592654/180.

  if(mag==0)then
     v=0
  else
     v=mag*(-cos(dir*rtd))
  endif

  RETURN
end function v

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

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) "Uso: bufr_csv2temp.exe filein [-int] [-geo] [-rdate] [-zsta Z]"
WRITE (*,*) "  [-phpa] [-rh] [-uv] [-tc] [-h]"
WRITE (*,*) "Scrive un file per ogni rsd trovato in input, nel vecchio formato estra_temp"
WRITE (*,*)
WRITE (*,*) "filein:  BUFR riscritto csv, prodotto da ""dbamsg dump -csv"""
WRITE (*,*) "-rdate   scrive data/ora riportate nel messaggio (default: ora sinottica)"
WRITE (*,*) "-phpa    scrive la presisone in hPa (default: Pa)"
WRITE (*,*) "-rh      scrive l'umidita' relativa (default: Td)"
WRITE (*,*) "-uv      scrive le compnenti del vento (default: dir e modulo)"
WRITE (*,*) "-tc      scrive le temperature in gradi centigradi (default: K)"
WRITE (*,*) "-h:      visualizza questo help"

WRITE (*,*) "Opzioni utili per dati molto vecchi (gestione incompleta!)"
WRITE (*,*) "-int     interopla tutti i parametri su tutti i livelli"
WRITE (*,*) "-zsta Z  forza la quota della stazione"
WRITE (*,*) "-geo     calcola le quote dei livelli (solo se interamenti mancante; puo' "
WRITE (*,*) "         richiedere -int -zsta; calcolo approssimativo, con errore di +- 10m)"
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
