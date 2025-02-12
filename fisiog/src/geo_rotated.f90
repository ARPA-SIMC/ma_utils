program geo_rotated
!--------------------------------------------------------------------------
! Legge una o piu' coppie di coordinate e le converte GEO <-> Routate
!
!                                         Versione 2.1.0, Enrico 04/06/2018
!--------------------------------------------------------------------------
IMPLICIT NONE

REAL :: xgeo,ygeo,xrot,yrot,tlm0d,tph0d
INTEGER :: conv,cnt_par,kpar,ier,ios,eof,eor
CHARACTER (LEN=100) :: filein,fileout,charg
CHARACTER (LEN=80) :: ch80
CHARACTER (LEN=10) :: gpslon,gpslat
CHARACTER (LEN=2) :: next_arg
LOGICAL :: lgps,lcentre

!--------------------------------------------------------------------------
! 1) Parametri da riga comando

lgps = .FALSE.
conv = 0
tlm0d = 0.
tph0d = 0.
filein = ""
fileout = ""

lcentre = .FALSE.
tlm0d = 10.
tph0d = 57.5

next_arg=""
cnt_par = 0
DO kpar = 1,HUGE(0)
  CALL getarg(kpar,charg)

  IF (TRIM(charg) == "") THEN
    EXIT
  ELSE IF (TRIM(charg) == "-h") THEN
    WRITE (*,*) "Uso: geo_rotated.exe [filein fileout] [-geo2rot/-rot2geo] [-gps]"
    WRITE (*,*) "Converte le coord. di uno o piu' punti da ruotate a geo e viceversa"
    WRITE (*,*) "Senza parametri:   uso interattivo"
    WRITE (*,*) "filein, fileout:   contengono una coppia di coord. (x,y) per ogni record"
    WRITE (*,*) "-geo2rot/-rot2geo: specifico il tipo di conversione"
    WRITE (*,*) "-rc lon lat        specifico il centro di rotazione "
    WRITE (*,*) "                   (vecchio LAMI: 10., 57.5; nuovo COSMO_5M: 10., 43.)"
    WRITE (*,*) "-gps               coordinate geografiche espresse in gg.pp.ss"
    WRITE (*,*) "                   con input da file, formato di lettura (a10,2x,a10)"
    STOP
  ELSE IF (next_arg == "cx") THEN  
    READ (charg,*) tlm0d
    next_arg = "cy"
  ELSE IF (next_arg == "cy") THEN  
    READ (charg,*) tph0d
    next_arg = ""
  ELSE IF (TRIM(charg) == "-rc") THEN  
    next_arg = "cx"
    lcentre = .TRUE.
  ELSE IF (TRIM(charg) == "-gps") THEN  
    lgps = .TRUE.

  ELSE IF (TRIM(charg) == "-geo2rot") THEN  
    conv = 1
  ELSE IF (TRIM(charg) == "-rot2geo") THEN  
    conv = 2
    tlm0d = 10.
    tph0d = 57.5

  ELSE
    cnt_par = cnt_par + 1
    SELECT CASE (cnt_par)
    CASE (1)
      filein = charg
    CASE (2)
      fileout = charg
    END SELECT
  ENDIF
ENDDO

! Tipo di conversione e centro di rotazione non specificati
IF (conv == 0) THEN
  WRITE (*,*) "Seleziona la conversione: geo->rot(1), rot->geo(2)"
  READ (*,*) conv
  IF (conv /= 1 .AND. conv /= 2) STOP 1
  IF (lcentre) THEN
    WRITE (*,*) "coordinate geografiche centro di rotazione: ",tlm0d,tph0d
  ELSE
    WRITE (*,*) "coordinate geografiche centro di rotazione?"
    READ (*,'(a)') ch80
    READ (ch80,*,IOSTAT=ios) tlm0d,tph0d
    IF (ios/=0) STOP 1
  ENDIF
ENDIF

!--------------------------------------------------------------------------
! 2) Elaboro le coordinate richieste

! Input interattivo
IF (filein=="") THEN

  IF (conv==1) THEN
    IF (lgps) THEN
      WRITE (*,*) "Longitudine (X)? (gg.pp.ss)"
      READ (*,'(a)') gpslon
      WRITE (*,*) "Latitudione (Y)? (gg.pp.ss)"
      READ (*,'(a)') gpslat
      CALL gps2gd(gpslon,gpslat,xgeo,ygeo,ier)
    ELSE
      WRITE(*,*)"coordinate geografiche? (lon,lat; gradi.decimali)"
      READ(*,*) xgeo,ygeo
    ENDIF

    CALL TLL(xgeo,ygeo,TLM0D,TPH0D,xrot,yrot)
    WRITE(*,'(a,2f8.2,a)') "coordinate Ruotate centro ",tlm0d,tph0d," (x,y):"
    WRITE(*,*) xrot,yrot

  ELSE IF (conv==2) THEN
    WRITE(*,'(a,2f8.2,a)') "coordinate Ruotate centro ",tlm0d,tph0d," (x,y):"
    READ (*,*) xrot,yrot
    CALL RTLL(xrot,yrot,TLM0D,TPH0D,xgeo,ygeo)
    IF (lgps) THEN
      CALL gd2gps(xgeo,ygeo,gpslon,gpslat,ier)
      WRITE (*,*) "coordinate geografiche (lon,lat):"
      WRITE (*,'(a10,2x,a10)') gpslon,gpslat
    ELSE
      WRITE (*,*) "coordinate geografiche (lon,lat):"
      WRITE (*,*) xgeo,ygeo
    ENDIF

  ENDIF

! Input da file
ELSE
  CALL get_eof_eor(eof,eor)
  OPEN (12, FILE=filein, STATUS="OLD", FORM="FORMATTED")
  OPEN (13, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")

  DO
    IF (conv==1) THEN
      IF (lgps) THEN
        READ (12,'(a10,2x,a10)',IOSTAT=ios) gpslon,gpslat
        IF (ios == eof) EXIT
        IF (ios /= 0) GOTO 9999
        CALL gps2gd(gpslon,gpslat,xgeo,ygeo,ier)
      ELSE
        READ (12,*,IOSTAT=ios) xgeo,ygeo
        IF (ios == eof) EXIT
        IF (ios /= 0) GOTO 9999
      ENDIF
      CALL TLL(xgeo,ygeo,TLM0D,TPH0D,xrot,yrot)
      WRITE (13,'(f8.3,1x,f8.3)') xrot,yrot

    ELSE IF (conv==2) THEN
      READ (12,*,IOSTAT=ios) xrot,yrot
      IF (ios == eof) EXIT
      IF (ios /= 0) GOTO 9999
      CALL RTLL(xrot,yrot,TLM0D,TPH0D,xgeo,ygeo)
      IF (lgps) THEN
        CALL gd2gps(xgeo,ygeo,gpslon,gpslat,ier)
        WRITE (13,'(a,2x,a)') gpslon,gpslat
      ELSE  
        WRITE (13,'(2(f8.3,x))') xgeo,ygeo
      ENDIF

    ENDIF
    WRITE(*,'(a,4f10.3)') "(x,y) geo; (x,y) rot",xgeo,ygeo,xrot,yrot
  ENDDO
ENDIF

STOP

!----------------------------------------------------------------------
9999 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)
STOP

END PROGRAM geo_rotated

!-------------------------------------------------------------------------
      SUBROUTINE TLL(ALMD,APHD,TLM0D,TPH0D,TLMD,TPHD)        
!-------------------------------------------------------------------------
! trasforma le coordinate geografiche ordinarie (ALMD,APHD) in
! coordinte ruotate (TLMD,TPHD). I/O in gradi e decimi.
! TLM0D, TPH0D: lon e lat del centro di rotazione, in gradi e decimi.
! 
      PARAMETER (DTR=3.141592654/180.)

      CTPH0=COS(TPH0D*DTR)
      STPH0=SIN(TPH0D*DTR)

      RELM=(ALMD-TLM0D)*DTR                                             
      SRLM=SIN(RELM)                                                    
      CRLM=COS(RELM)                                                    

      APH=APHD*DTR                                                      
      SPH=SIN(APH)                                                      
      CPH=COS(APH)                                                      

      CC=CPH*CRLM                                                       
      ANUM=CPH*SRLM                                                     
      DENOM=CTPH0*CC+STPH0*SPH                                          

      TLMD=ATAN2(ANUM,DENOM)/DTR
      TPHD=ASIN(CTPH0*SPH-STPH0*CC)/DTR

      RETURN                                                          
      END   

!-------------------------------------------------------------------------
      SUBROUTINE RTLL(TLMD,TPHD,TLM0D,TPH0D,ALMD,APHD)        
!-------------------------------------------------------------------------
! trasforma le coordinate ruotate (TLMD,TPHD) in coordinate geografiche
! ordinarie (ALMD,APHD). I/O in gradi e decimi
! TLM0D, TPH0D: lon e lat del centro di rotazione, in gradi e decimi.
! 
      PARAMETER (DTR=3.141592654/180.)

      CTPH0=COS(TPH0D*DTR)
      STPH0=SIN(TPH0D*DTR)

      STPH=SIN(TPHD*DTR)
      CTPH=COS(TPHD*DTR)
      CTLM=COS(TLMD*DTR)
      STLM=SIN(TLMD*DTR)

      APH=ASIN(STPH0*CTPH*CTLM+CTPH0*STPH)                            
      CPH=COS(APH)                                                    

      ALMD=TLM0D+ASIN(STLM*CTPH/CPH)/DTR                               
      APHD=APH/DTR                                                    

      RETURN                                                          
      END   

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE gps2gd(strx,stry,xx,yy,ier)
!
! Trasfoma le coordinate gradi.primi.secondi in gradi.decimi
! Probabilmente funziona solo per coordinate positive.
! ier=1: coordinate illegali
! ier=2: coordiante negative
! ier=3: parsing error nelle stringhe in input
!
IMPLICIT NONE
!
CHARACTER (LEN=10), INTENT(IN) :: strx,stry  !coord. in gradi.primi.secondi
REAL, INTENT(OUT) :: xx,yy                   !coord. in gradi.decimi
INTEGER, INTENT(OUT) :: ier

INTEGER :: gg,pp,ss,p1,p2
!

p1 = INDEX(strx,".")
p2 = INDEX(strx,".",BACK=.TRUE.)

IF (p1 == 0 .OR. p2 == 0) GOTO 9997
READ (strx(1:p1-1),*,ERR=9999) gg
READ (strx(p1+1:p2-1),*,ERR=9999) pp
READ (strx(p2+1:),*,ERR=9999) ss
IF (gg < -180 .OR. gg > 180 .OR. pp < 0 .OR. pp > 59 .OR. ss < 0 .OR. ss > 59) THEN
  GOTO 9999
ELSE IF (gg < 0) THEN
  GOTO 9998
ENDIF
xx = REAL(gg) + (REAL(pp) + REAL(ss)/60.) / 60.

p1 = INDEX(stry,".")
p2 = INDEX(stry,".",BACK=.TRUE.)
IF (p1 == 0 .OR. p2 == 0) GOTO 9997
READ (stry(1:p1-1),*,ERR=9999) gg
READ (stry(p1+1:p2-1),*,ERR=9999) pp
READ (stry(p2+1:),*,ERR=9999) ss
IF (gg < -90 .OR. gg > 90 .OR. pp < 0 .OR. pp > 59 .OR. ss < 0 .OR. ss > 59) THEN
  GOTO 9999
ELSE IF (gg < 0) THEN
  GOTO 9998
ENDIF
yy = REAL(gg) + (REAL(pp) + REAL(ss)/60.) / 60.

ier = 0
RETURN

9999 CONTINUE
WRITE (*,*) "Subroutine gps2gp: coordinate illegali"
ier = 1
RETURN

9998 CONTINUE
WRITE (*,*) "Subroutine gps2gp: coordinate negative non gestite"
ier = 2
RETURN

9997 CONTINUE
WRITE (*,*) "Subroutine gps2gp: parsing error"
ier = 3
RETURN

END SUBROUTINE gps2gd

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE gd2gps(xx,yy,strx,stry,ier)
!
! Trasfoma le coordinate gradi.decimi in gradi.primi.secondi
! Probabilmente funziona solo per coordinate positive.
! ier=1: coordinate illegali
! ier=2: coordinate negative
!
IMPLICIT NONE
!
REAL, INTENT(IN) :: xx,yy                    !coord. in gradi.decimi
CHARACTER (LEN=10), INTENT(OUT) :: strx,stry !coord. in gradi.primi.secondi
INTEGER, INTENT(OUT) :: ier

REAL :: ppr,ssr
INTEGER :: gg,pp,ss
!
IF (xx < -180. .OR. xx > 180. .OR. yy < 90. .OR. yy > 90.) THEN
  GOTO 9999
ELSE IF (xx < 0. .OR. yy < 0.) THEN
  GOTO 9998
ELSE
  ier = 0
ENDIF

gg = INT(xx)
ppr = (xx - REAL(gg)) * 60.
pp = INT(ppr)
ssr = (ppr - REAL(pp)) * 60.
ss = NINT(ssr)
IF (gg < 100) THEN
  WRITE(strx,'(i2.2,2(a1,i2.2))') gg,".",pp,".",ss
ELSE
  WRITE(strx,'(i3.3,2(a1,i2.2))') gg,".",pp,".",ss
ENDIF

gg = INT(yy)
ppr = (yy - REAL(gg)) * 60.
pp = INT(ppr)
ssr = (ppr - REAL(pp)) * 60.
ss = NINT(ssr)
WRITE(stry,'(i2.2,2(a1,i2.2))') gg,".",pp,".",ss

RETURN

9999 CONTINUE
WRITE (*,*) "Subroutine gd2gps: coordinate illegali"
ier = 1
RETURN

9998 CONTINUE
WRITE (*,*) "Subroutine gd2gps: coordinate negative non gestite"
ier = 2
RETURN

END SUBROUTINE gd2gps

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

