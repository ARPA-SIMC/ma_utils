PROGRAM adjust_ctl
!--------------------------------------------------------------------------
! Programma per modificare un file .ctl in modo che contenga i nomi giusti
! delle variabili usate da SMR.
!
! USO: adjust_ctl.exe filein fileout [filetab] [-lsort] [-lgrp/-lsup] [-tsz N ]
!
! NB: in tutti i casi, grads non funziona se richiedo p.es. 
!   T@ 10 e 40 e U @ 10 e 90
!   
!                                          Versione 6.2   Enrico 14/12/2009
!--------------------------------------------------------------------------
IMPLICIT NONE

INTEGER, PARAMETER :: mxlevs = 60    ! max. n.ro livelli nei GRIB
INTEGER, PARAMETER :: mxvars = 256   ! max. n.ro varibili (tabella e ctl)
REAL, PARAMETER :: rmis = -999.

CHARACTER (LEN=40) :: var_desc(mxvars)
CHARACTER (LEN=8) :: var_id(mxvars)

REAL :: idlevs(mxlevs),xynorm,x1,y1,xstep,ystep
INTEGER :: tab_code(mxvars),var_nlevs(mxvars),var_grb(3,mxvars)
INTEGER :: nlevs,nlevs_g,nvars,nvars_g,nv_tab,tsize
INTEGER :: ios,p1,p2,k,k2,kp,idp,pt,krepl,loper
CHARACTER (LEN=200) :: filein,fileout,filetab,chpar
CHARACTER (LEN=400) :: rec,rec_lev(2),rec_var(mxvars+2),chfmt
CHARACTER (LEN=80) :: chdum,arg(3),ch80
CHARACTER (LEN=40) :: tab_desc(mxvars)
CHARACTER (LEN=32) :: body(mxvars)
CHARACTER (LEN=8) :: tab_id(mxvars),ch8,ch8b
CHARACTER (LEN=3) :: next_arg
LOGICAL :: lsort,adjvar,tforce,xyforce

!==========================================================================
! 1) Preliminari

! 1.1) Parametri da riga comando

loper = 0
lsort=.FALSE.
adjvar=.FALSE.
tforce=.FALSE.
xyforce=.FALSE.
arg(:) = ""
idp = 0
next_arg = ""

DO kp = 1,HUGE(kp)

  CALL getarg(kp,chdum)

  IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "") THEN  
    EXIT
  ELSE IF (next_arg == "tsz") THEN
    READ (chdum,*) tsize
    next_arg = ""
  ELSE IF (next_arg == "xys") THEN
    READ (chdum,*) xynorm
    next_arg = ""
  ELSE IF (TRIM(chdum) == "-lsort") THEN
    lsort = .TRUE.    
  ELSE IF (TRIM(chdum) == "-lsup") THEN
    loper = 1    
  ELSE IF (TRIM(chdum) == "-lgrp") THEN
    loper = 2    
  ELSE IF (TRIM(chdum) == "-tsz") THEN
    tforce = .TRUE.
    next_arg = "tsz"
  ELSE IF (TRIM(chdum) == "-xyscale") THEN
    xyforce = .TRUE.
    next_arg = "xys"
  ELSE   
    idp = idp + 1
    arg(idp) = chdum
  ENDIF

ENDDO

filein = arg(1)
fileout = arg(2)
filetab = arg(3)
IF (TRIM(filetab) /= "") adjvar = .TRUE.

IF (filein == '' .OR. fileout == '') THEN
  CALL write_help
  STOP
ENDIF   

! 1.2) Apro files .ctl
OPEN (UNIT=20,FILE=filein, STATUS="OLD", ACTION="READ", IOSTAT=ios)
IF (ios /= 0) THEN
  CALL write_help
  STOP
ENDIF

OPEN (UNIT=21,FILE=fileout, STATUS="REPLACE", ACTION="WRITE")

!==========================================================================
! 2) Se richiesto, leggo la tabella GRIB

IF (adjvar) THEN

! Apro file
  OPEN (UNIT=22,FILE=filetab, STATUS="OLD", ACTION="READ", & 
    FORM="FORMATTED", IOSTAT=ios)
  IF (ios /= 0) THEN
    CALL write_help
    STOP
  ENDIF


! Leggo tabella GRIB
  nv_tab = 0
  DO
    READ (22,'(a)',IOSTAT=ios) ch80
    IF (ios /= 0) EXIT
    IF (TRIM(ch80) == "" .OR. ch80(1:1) == "!") CYCLE

    nv_tab = nv_tab + 1
    IF (nv_tab > mxvars) THEN
      WRITE (*,*) "errore, troppe variabili in tabella",TRIM(filetab)
      STOP
    ENDIF

    READ (ch80,'(1x,i3,2x,a8,2x,a40)',IOSTAT=ios) tab_code(nv_tab), &
      tab_id(nv_tab),tab_desc(nv_tab)
    IF (ios /= 0) THEN
      WRITE (*,*) "Errore leggendo la tabella ",TRIM(filetab)
      STOP
    ENDIF
 
  ENDDO

ENDIF

!==========================================================================
! 3) Leggo/scrivo i record ordinari (in tutti i casi)

DO
  READ (20,'(a)', IOSTAT=ios) rec
  IF (ios /= 0) THEN
    WRITE (*,*) "Errore, manca la sezione livelli!"
    STOP
  ENDIF

  IF (INDEX(rec,'zdef') /= 0) EXIT   ! sono arrivato alla sez. livelli

! Sostituisco codice valori mancanti
  IF (INDEX(rec,'undef') /= 0 .OR. INDEX(rec,'UNDEF') /= 0 ) THEN
    WRITE (21,'(a,1x,e10.3)') 'undef',rmis   
  
! Se richiesto, forzo tsize
  ELSE IF (tforce .AND. &
           (INDEX(rec,'tdef') /= 0 .OR. INDEX(rec,'TDEF') /= 0) ) THEN
    p1 = INDEX(rec,' ')
    p2 = p1 + INDEX(rec(p1+1:),' ')
    WRITE (21,'(a,1x,i6,1x,a)') 'tdef',tsize,TRIM(rec(p2:))

! Se richiesto, modifico xdef e ydef
  ELSE IF (xyforce .AND. &
           (INDEX(rec,'xdef') /= 0 .OR. INDEX(rec,'XDEF') /= 0) ) THEN
    p2 = INDEX(TRIM(rec),' ',BACK=.TRUE.)
    p1 = INDEX(TRIM(rec(:p2-1)),' ',BACK=.TRUE.)
    READ (rec(p1+1:p2-1),*) x1
    READ (rec(p2+1:),*) xstep
    WRITE (21,'(a,2(1x,f10.5))') rec(:p1),x1*xynorm,xstep*xynorm

  ELSE IF (xyforce .AND. &
           (INDEX(rec,'ydef') /= 0 .OR. INDEX(rec,'YDEF') /= 0) ) THEN
    p2 = INDEX(TRIM(rec),' ',BACK=.TRUE.)
    p1 = INDEX(TRIM(rec(:p2-1)),' ',BACK=.TRUE.)
    READ (rec(p1+1:p2-1),*) y1
    READ (rec(p2+1:),*) ystep
    WRITE (21,'(a,2(1x,f10.5))') rec(:p1),y1*xynorm,ystep*xynorm

! Record da non modificare
  ELSE
    WRITE (21,'(a)') TRIM(rec)

  ENDIF
ENDDO

!==========================================================================
! 4) Elaboro la sezione di livelli e variabili

!--------------------------------------------------------------------------
! 4.1) Leggo e analizzo i records

! sezione livelli
rec_lev(1) = rec

READ ( rec_lev(1)(INDEX(rec_lev(1),'zdef')+4:), *) nlevs
IF (nlevs > 1) THEN
  READ (20,'(a)') rec_lev(2)
  READ (rec_lev(2), *) idlevs(1:nlevs)
ENDIF

! primo record sezione variabili
READ (20,'(a)') rec_var(1)
IF (INDEX(rec_var(1),'vars') == 0) STOP "Errore 1 elaborando liv/var"
READ ( rec_var(1)(INDEX(rec_var(1),'vars')+4:), *) nvars
IF (nvars > mxvars) THEN
  WRITE (*,*) "Errore, troppe varibiali nel file ctl: ",TRIM(filein)
  STOP
ENDIF

! record variabili
DO k = 1, nvars
  READ (20,'(a)') rec_var(k+1)
ENDDO

DO k = 1,nvars
  rec = rec_var(k+1)
  p1 = INDEX(rec,' ')
  p2 = INDEX(rec,'*')
!  var_id(k) = rec(1:p1-1)
  body(k) = rec(p1:p2)
!  var_desc(k) = rec(p2+1:)

  READ (body(k),*,IOSTAT=ios) var_nlevs(k),var_grb(1:3,k)
  IF (ios /= 0) THEN
    WRITE (*,*) "Errore interpretando record variabili :"
    WRITE (*,*) rec
    STOP
  ENDIF
ENDDO

! ultimo record sezione variabili
READ (20,'(a)') rec_var(nvars+2)
IF (INDEX(rec_var(nvars+2),'ENDVARS') == 0) &
  STOP "Errore 2 elaborando liv/var"

!--------------------------------------------------------------------------
! 4.2) Modifico i records

!--------------------------------------------------------------------------
! Se richiesto, scrivo i parametri a 10 metri come livelli in quota
IF (loper == 2) THEN
  CALL group_vars(nvars,mxvars,nlevs,mxlevs,var_nlevs,var_grb,idlevs, &
    nvars_g,nlevs_g)
  IF (nlevs_g > nlevs) &
    WRITE (rec_lev(1),'(a,i2,a)') 'zdef ',nlevs_g,' levels'

ELSE IF (loper == 0 .OR. loper == 1) THEN
  nvars_g = nvars
  nlevs_g = nlevs

ENDIF

!--------------------------------------------------------------------------
! Se richiesto, ordino i livelli in senso decrescente
IF (lsort) CALL sort(idlevs,nlevs_g)
WRITE (chfmt,'(a,i2,a)') '(',nlevs_g,'i6)'
WRITE (rec_lev(2),chfmt) NINT(idlevs(1:nlevs_g))

!--------------------------------------------------------------------------
! Ricostruisco le stringhe identificative e di commento dei parametri.
! Con l'opzione -lsup, devo modificare i nomi delle variabili vicino alla 
! superficie per evitare che abbiano lo stesso nome di quelle in quota

IF (adjvar) THEN
DO k = 1,nvars_g

  pt = 0
  DO k2 = 1,nv_tab
    IF (var_grb(1,k) == tab_code(k2)) THEN
      pt = k2
      EXIT
    ENDIF
  ENDDO

  IF (pt /= 0) THEN           ! variabile contenuta in tabella

    IF (loper == 1) THEN
      CALL append_lev_id(tab_id(pt),var_nlevs(k),var_grb(1:3,k),ch8)
    ELSE
      ch8 = tab_id(pt) 
    ENDIF

    WRITE (rec_var(k+1),'(a8,1x,i2,1x,i3,a1,i3,a1,i6,1x,2a)') &
      ch8,var_nlevs(k),var_grb(1,k),",",var_grb(2,k),",",var_grb(3,k), &
      "*** ",TRIM(tab_desc(pt))

  ELSE                        ! variabile non contenuta in tabella

    WRITE (ch8b,'(a3,i3.3)') "var",var_grb(1,k)
    IF (loper == 1) THEN
      CALL append_lev_id(ch8b,var_nlevs(k),var_grb(1:3,k),ch8)
    ELSE
      ch8 = ch8b
    ENDIF

    WRITE (rec_var(k+1),'(a8,1x,i2,1x,i3,a1,i3,a1,i6,1x,a3,a,i3.3)') &
      ch8,var_nlevs(k),var_grb(1,k),",",var_grb(2,k),",",var_grb(3,k), &
      "***","variabile ",var_grb(1,k)
  ENDIF

ENDDO
ENDIF

! record iniziale e finale del gruppo variabili
WRITE (rec_var(1),'(a,1x,i4)') "vars",nvars_g
rec_var(nvars_g+2) = "ENDVARS"

!--------------------------------------------------------------------------
! 4.3) Scrivo i record modificati

WRITE (21,'(a)') TRIM(rec_lev(1))
IF (nlevs_g > 1) WRITE (21,'(a)') TRIM(rec_lev(2))

DO k=1, nvars_g+2
  WRITE (21,'(a)') TRIM(rec_var(k))
ENDDO

!==========================================================================
! 5) Conclusione

CLOSE(20)
CLOSE(21)

STOP
END PROGRAM adjust_ctl

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE sort(vett,n)
!
! Ordina in senso decrescente i numeri contenuti in vett
!
IMPLICIT NONE

INTEGER, INTENT(IN) :: n
REAL, INTENT(INOUT) :: vett(n)
!
REAL :: vett2(n),mn=0.
INTEGER :: k,k2,mnl,left
!
DO k = 1,n

  left = n-k+1

! Per bachi nel compilatore, non riesco a usare le piu' semplici:
!  mn = MINVAL(vett(1:left))
!  mnl = MINLOC(vett(1:left))

  mn = HUGE(mn)
  mnl = 0
  DO k2 = 1,left
    IF (vett(k2) < mn) THEN
      mn = vett(k2)
      mnl = k2
    ENDIF
  ENDDO
  IF (mnl == 0) STOP "errore 1 sort"

  vett2(k) = mn
  vett(mnl:left-1) = vett(mnl+1:left)

ENDDO

vett = vett2

RETURN
END SUBROUTINE sort

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE group_vars(nvars,mxvars,nlevs,mxlevs,var_nlevs,var_grb,idlevs, &
  nvars_g,nlevs_g)
!
! Elabora il gruppo descrittivo delle variabili: se un parametro e' 
! presente sia in quota sia come variabile superficiale (i.e. a 2 o 10 m), 
! lo scrive una sola volta come variabile in quota, aggiungendo un livello.
!
! NOTA: per il momento non gestisce il caso in cui una variabile e' 
! definita su due livelli superficiali diversi (es. T sup e T 02m) o su
! due insiemi diversi di livelli in quota
!
IMPLICIT NONE

INTEGER,INTENT(IN) :: nvars,mxvars,nlevs,mxlevs
INTEGER,INTENT(INOUT) :: var_nlevs(mxvars),var_grb(3,mxvars)
REAL,INTENT(INOUT) :: idlevs(mxlevs)
INTEGER,INTENT(OUT) :: nlevs_g,nvars_g

! Altre variabili locali
REAL :: idlevs_g(mxlevs),idlev_extra
INTEGER :: var_nlevs_g(mxvars),var_grb_g(3,mxvars)
INTEGER :: k,k2
LOGICAL :: new_rec,new_lev

!--------------------------------------------------------------------------
! Inizio elaborazioni

! Prima variabile: inizializzo variabili locali _g
IF (nvars == 0) RETURN
nvars_g = 1
var_nlevs_g(1) = var_nlevs(1)
var_grb_g(1:3,1) = var_grb(1:3,1)
nlevs_g = nlevs
idlevs_g(1:nlevs) = idlevs(1:nlevs)

! Scorro gli identificativi delle varibili: se incontro una variabile che
! ho gia' trovato e una delle due e' scritta a 2 o 10 m, invece di 
! scriverla di nuovo aggiungo un livello a quella gia' elaborata.
DO k = 2,nvars

  new_rec = .TRUE.
  DO k2 = 1,nvars_g

!   prima sup, adesso in quota
    IF (var_grb(1,k) == var_grb_g(1,k2) .AND. &
        var_nlevs(k) /= 0 .AND. var_nlevs_g(k2) == 0) THEN
      new_rec = .FALSE.
      idlev_extra = var_grb_g(3,k2)
      var_nlevs_g(k2) = var_nlevs(k) + 1
      var_grb_g(1:3,k2) = var_grb(1:3,k)
      EXIT 
        
!   prima in quota, adesso sup
    ELSE IF (var_grb(1,k) == var_grb_g(1,k2) .AND. &
             var_nlevs(k) == 0 .AND. var_nlevs_g(k2) /= 0) THEN
      new_rec = .FALSE.
      idlev_extra = var_grb(3,k)
      var_nlevs_g(k2) = var_nlevs_g(k2) + 1
      var_grb_g(1:3,k2) = var_grb_g(1:3,k2)       ! non modifico!!
      EXIT 
       
    ENDIF
  ENDDO

! non ho raggruppato nessuna variabile, riporto il record cosi' com'e'
  IF (new_rec) THEN     
    nvars_g = nvars_g + 1
    var_nlevs_g(nvars_g) =  var_nlevs(k)
    var_grb_g(1:3,nvars_g) =  var_grb(1:3,k)

! ho raggruppato delle variabili, se ho un livello nuovo lo aggiungo
  ELSE
    new_lev = .TRUE.
    DO k2 = 1, nlevs_g
      IF (idlevs_g(k2) == idlev_extra) THEN
        new_lev = .FALSE.
        EXIT
      ENDIF
    ENDDO        
    IF (new_lev) THEN
      nlevs_g = nlevs_g + 1
      IF (nlevs_g > mxlevs) STOP "Errore, troppi livelli"
      idlevs_g(nlevs_g) = idlev_extra
    ENDIF

  ENDIF

!print *,"loop2, k: ",k
!print *
!print *,new_rec,new_lev,nlevs_g,idlev_extra
!print *,idlevs_g(1:nlevs_g)
!print *
!do k2=1,nvars
!print *,var_nlevs(k2),var_grb(1:3,k2)
!enddo
!print *
!do k2=1,nvars_g
!print *,var_nlevs_g(k2),var_grb_g(1:3,k2)
!enddo
!read *

ENDDO
!
var_nlevs(1:nvars_g) = var_nlevs_g(1:nvars_g)
var_nlevs(nvars_g+1:) = -999
var_grb(1:3,1:nvars_g) = var_grb_g(1:3,1:nvars_g)
var_grb(1:3,nvars_g+1:) = -999
idlevs(1:nlevs_g) = idlevs_g(1:nlevs_g)
idlevs(nlevs_g+1:) = -999
!
RETURN
END SUBROUTINE group_vars

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE append_lev_id(tab_id,nlevs,grb_id,ch8)
!
! Se non raggruppo le variabili in quota e vicino alla superficie 
! (lorg = .T.), questa subr. modifica il nome di una variabile vicino alla 
! superficie in modo che sia sicuramente diversa da quelle in quota.
!
IMPLICIT NONE

CHARACTER (LEN=8), INTENT(IN) :: tab_id
INTEGER, INTENT(IN) :: nlevs,grb_id(3)
CHARACTER (LEN=8), INTENT(OUT) :: ch8

INTEGER :: p1,pp
!--------------------------------------------------------------------------

p1 = LEN(TRIM(tab_id))
ch8 = tab_id

! variabili mulitlivello non modificate
IF (nlevs > 0) THEN
  RETURN

! variabili a 2/10 ... metri
ELSE IF (grb_id(2) == 105 .AND. grb_id(3) < 100) THEN 
  pp = MIN(p1,6) + 1
  WRITE (ch8(pp:pp+1),'(i2.2)') grb_id(3)

! variabili superficiali
ELSE IF (grb_id(2) == 1 .AND. grb_id(3) == 0) THEN 
  pp = MIN(p1,6) + 1
  ch8(pp:pp+1) = "sf"

ENDIF

RETURN

END SUBROUTINE append_lev_id

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a schermo l'help del programma
!
!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) ""
WRITE (*,*) "USO: adjust_ctl.exe filein fileout "
WRITE (*,*) "     [filetab] [-lsort] [-lgrp/-lsup] [-tsz N] [-xyscale S]"
WRITE (*,*) ""
WRITE (*,*) "Modifica i nomi di livelli e varibili in un file .ctl"
WRITE (*,*) ""
WRITE (*,*) "filein : file .ctl originale"
WRITE (*,*) "fileout: file .ctl modificato"
WRITE (*,*) "filetab: file con i nuovi nomi dei parametri (tabella_???.dat)"
WRITE (*,*) ""
WRITE (*,*) "-lsort:  ordina i livelli in senso crescente [Calmet, Chimere]"
WRITE (*,*) "-lgrp:   raggruppa i parametri a 2 e 10 metri con i corrispondenti parametri"
WRITE (*,*) "         in quota [Calmet]"
WRITE (*,*) "-lsup:   non raggruppa i parametri a 2 e 10 metri, ma li scrive con un nome"
WRITE (*,*) "         diverso dalle variabili in quota [LM]"
WRITE (*,*) "-tsz N:  mette a N il numero di istanti nel record tdef, senza fare nessun"
WRITE (*,*) "         controllo (utile per serie storiche con dati mancanti)"
WRITE (*,*) "-xysceale S: moltiplica per il fattore S le scale X e Y (puo risolvere"
WRITE (*,*) "         problemi di griglie incompatibili con dati UTM)"

END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
