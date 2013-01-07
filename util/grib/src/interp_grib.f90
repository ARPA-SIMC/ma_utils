!+ programma per interpolare su un'altra griglia un file com molti grib
! 
Program interp_grib
 
! Description: 
!   Legge un file con molti grib e li riscrive interpolati su un altro 
!   grigliato. E' possibile imporre un grigliato specifico o farlo
!   scegliere al programma (area piu' piccola che contiene l'intero grigliato,
!   con approssimativamente la stessa risoluzione lineare).
!   Gestisce griglie geografiche (regolari o ruotate) e UTM
!   Gestisce antirotazione del vento (anche se la comp. V fosse scritta
!   prima della U)
!
! Method: 
!   Implementati 3 algortitmi di interpolazione: lineare, media, piu' vicino
! 
! Usage:
!   interp_grib file_grib_in file_grib_out
!
! Input files: 
!   - file con i grib originali
!   - file di opzioni, antirot_grib.inp (nella stessa dir)
! 
! Output files: 
!   - file con i grib antiruotati
! 
! Current Code Owner: Enrico Minguzzi
! 
! History: 
! Version   Date         Comment 
! -------   ----         ------- 
! 1.0       19/03/2001   Versione iniziale
! 2.0       24/04/2002   Gestione UTM & comp. del vento non abbinate
!
!                                                Versione 2.5, Enrico 30/10/2009
!-------------------------------------------------------------------------------

! Declarations: 

USE grid_handler
 
IMPLICIT NONE 

! gestione del vento
INTEGER, PARAMETER :: nwind = 3
INTEGER, PARAMETER :: w1(nwind) = (/33,131,165/) !param. associati a comp. u
INTEGER, PARAMETER :: w2(nwind) = (/34,132,166/) !param. associati a comp. v
INTEGER :: wind,pwind
LOGICAL :: towrite,towritew

! dichiarazioni per GRIBEX. La dimensione massima degli array grib (maxdim) 
! e' assegnata nel modulo grid
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: ksec1w(1024),ksec1_swap(1024)
INTEGER :: kbuffer(maxdim), klen, kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

!griglia di output definita dall'utente
REAL :: odx, ody, ox1, oy1, oxrot, oyrot
INTEGER :: onx, ony, outmz
CHARACTER (LEN=1) :: rule
CHARACTER (LEN=3) :: oproj
NAMELIST /outgrid/ oproj,rule,onx,ony,odx,ody,ox1,oy1,oxrot,oyrot,outmz

! altre variabili
TYPE(grid) :: grid_org, grid_orgw, grid_out, grid_outw
TYPE(grid) :: grid_first
INTEGER :: iuin, iuout, ngribin, ngribout, nval
INTEGER :: i,ii,files,ipar,nok
INTEGER :: nscal,nwind_sca,nwind_vec
CHARACTER (LEN=120) :: chpar='',filein='',fileout=''
CHARACTER (LEN=20) :: filepar='interp_grib.inp'
LOGICAL :: user_grid

!- End of header ---------------------------------------------------------------

!-------------------------------------------------------------------------------
! Sezione 1: Gestione dei parametri & inizializzazioni
!
!1.1: parametri da riga comando
!

user_grid = .FALSE.
files = 0

DO ipar = 1,3
  CALL getarg(ipar,chpar)

  IF (trim(chpar) == '-h') THEN
    CALL write_help
    STOP
  ELSE IF (trim(chpar) == '-c') THEN
    CALL crea_esempio_namelist
    STOP
  ELSE IF (trim(chpar) == '-f') THEN
    user_grid = .TRUE.
  ELSE
    IF (files == 0) THEN    
      filein = chpar
      files = 1
    ELSE IF (files == 1) THEN    
      fileout = chpar
      files = 2
    ENDIF
  ENDIF

ENDDO

IF (filein == '' .OR. fileout == '') THEN
  WRITE (*,*) "Errore: parametri mancanti!"
  STOP
ENDIF

!-------------------------------------------------------------------------------
!1.2: se richiesto, legge la griglia di output dal file interp_grib.inp
!

IF (user_grid) THEN
  OPEN (UNIT=30, FILE=filepar, STATUS='OLD', FORM='FORMATTED', ACTION='READ')
  READ (30, outgrid)
  CLOSE (30)
ELSE
  rule = 'L'
  oproj = 'GEO'
ENDIF

!-------------------------------------------------------------------------------
!1.3: apertura files grib
!

CALL PBOPEN (iuin,filein,'R',kret)
IF (kret.ne.0) WRITE(*,*) 'Error pbopen: kret ',kret,' ',filein

CALL PBOPEN(iuout,fileout,'W',kret)
IF (kret.ne.0) WRITE(*,*) 'Error pbopen: kret ',kret,' ',fileout

!-------------------------------------------------------------------------------
!1.4: altre inizializzazioni
!

! Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! Apro files di log
OPEN (UNIT=96, FILE="interp_grib.log1", STATUS="REPLACE",FORM="FORMATTED")
OPEN (UNIT=88, FILE="interp_grib.log2", STATUS="REPLACE",FORM="FORMATTED")
 
!-------------------------------------------------------------------------------
! Sezione 2: Main loop (input grib)
!

ngribin = 0
ngribout = 0
nscal = 0
nwind_sca = 0 
nwind_vec = 0
wind = 0
pwind = 0

grib: DO

!-------------------------------------------------------------------------------
!2.1: lettura & decodifica
!

  CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)
  IF (kret < -1) WRITE(*,*) 'Error pbgrib: kret ',kret,' ',filein

  IF (kret.eq.-1) EXIT grib

  psec3(2) = rmis                     !impongo di mettere i dati mancanti = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'D',kret)
  if (kret.gt.0) print *,'Warning gribex: kret ',kret,' ',filein
  WRITE (88,*) 'gribex decoded, ksec0(1) ',ksec0(1)

  ngribin = ngribin +1

  nval = ksec4(1)
  nok = COUNT(field(1:nval) /= rmis)
  WRITE (96,*)
  WRITE (96,'(a,i4,2i8,e14.7)') "Letto 1 grib:  par, dati, ok, media ", &
    ksec1(6),nval,nok, &
    SUM( field(1:nval), MASK=field(1:nval)/=rmis ) / float(MAX(nok,1))

! Costruisco la variabile di tipo "grigliato" relativa al campo d'ingresso
  CALL build_grid(ksec2,field,grid_org)

  WRITE (88,*) '************************************************************'
  WRITE (88,*) "Input field"
  CALL print_grid(grid_org,88)

 ! Scelgo su quale griglia interpolare
  IF (user_grid) THEN
    grid_out = grid(oproj,onx,ony,odx,ody,ox1,oy1,oxrot,oyrot,outmz,0.)
  ELSE
    CALL find_best_antir(grid_org,grid_out)
  ENDIF

 ! log e segnalo se scrivero' grib definiti su griglie diverse
  IF (ngribin == 1) THEN
    grid_first = grid_out
    WRITE (88,*) '************************************************************'
    WRITE (88,*) "output grid"
    CALL print_grid(grid_out,88)

  ELSE IF (.NOT. (grid_first .samegrid. grid_out) ) THEN
    WRITE (88,*) "Attenzione: non tutti i grib avranno la stessa griglia"
    WRITE (88,*) '************************************************************'
    WRITE (88,*) "output grid (changed)"
    CALL print_grid(grid_out,88)
    grid_first = grid_out

  ENDIF

!-------------------------------------------------------------------------------
! 2.2: gestione del vento e interpolazione
! wind:  descrive l'ultimo grib letto (1: comp. u, 2: comp.v, 0: scalare)
! pwind: descrive il grib memorizzato nelle variabili grid_orgw,ksec1w 
!        (1: comp. u, 2: comp.v, 0: nessun grib memorizzato)
! towrite:  .T. se sono pronto a scrivere l'ultimo campo letto
! towritew: .T. se sono pronto a scrivere il campo memorizzato

! Verifico se l'ultimo grib letto e' scalare o vento
  IF ( ANY( w1 == SPREAD(ksec1(6),1,nwind) )) THEN 
    wind = 1                                           ! ho letto vento u
  ELSE IF ( ANY( w2 == SPREAD(ksec1(6),1,nwind ) )) THEN 
    wind = 2                                           ! ho letto vento v
  ELSE
    wind = 0                                           ! ho letto campo scalare
  ENDIF

! Gestisco i vari casi                             !   memo, last

  IF (wind == 0 .AND. pwind == 0 ) THEN            !   none, scalar

    CALL interp_scalar(grid_org,grid_out,rule)
    nscal = nscal + 1
    towritew = .FALSE.
    towrite = .TRUE.
    pwind = 0

  ELSE IF (wind == 1 .AND. pwind == 0 .OR. &       !   none, wind
           wind == 2 .AND. pwind == 0 ) THEN

    grid_orgw = grid_org
    grid_outw = grid_out
    ksec1w = ksec1
    towritew = .FALSE.
    towrite = .FALSE.
    pwind = wind

  ELSE IF (wind == 0 .AND. pwind /= 0 ) THEN       !   wind, scalar

    CALL interp_scalar(grid_orgw,grid_outw,rule)
    CALL interp_scalar(grid_org,grid_out,rule)
    nscal = nscal + 1
    nwind_sca = nwind_sca + 1
    towritew = .TRUE.
    towrite = .TRUE.
    pwind = 0

  ELSE IF (wind == 2 .AND. pwind == 1 .AND. &
    ALL(ksec1(1:5) == ksec1w(1:5)) .AND. ALL(ksec1(7:) == ksec1w(7:)) .AND. &
    (grid_org .samegrid. grid_orgw) ) THEN         ! u-wind, v-wind (compat.)

    CALL interp_wind(grid_orgw,grid_org,grid_outw,grid_out,rule)
    nwind_vec = nwind_vec + 2
    towritew = .TRUE.
    towrite = .TRUE.
    pwind = 0

  ELSE IF (wind == 2 .AND. pwind == 1 .AND. &
    ALL(ksec1(1:5) == ksec1w(1:5)) .AND. ALL(ksec1(7:) == ksec1w(7:)) .AND. &
    (grid_org .samegrid. grid_orgw) ) THEN         ! u-wind, v-wind (compat.)
     
    CALL interp_wind(grid_org,grid_orgw,grid_out,grid_outw,rule)
    nwind_vec = nwind_vec + 2
    towritew = .TRUE.
    towrite = .TRUE.
    pwind = 0

  ELSE IF ((wind == 1 .AND. pwind == 1) .OR. &     !    wind,   wind (incompat)
    (wind == 2 .AND. pwind == 2) .OR. &
    (wind == 2 .AND. pwind == 1 .AND. &
      ( ANY(ksec1(1:5)/=ksec1w(1:5)) .OR. ANY(ksec1(7:)/=ksec1w(7:)) .OR. &
        .NOT. (grid_org .samegrid. grid_orgw) )) .OR. &
    (wind == 1 .AND. pwind == 2 .AND. &
      ( ANY(ksec1(1:5)/=ksec1w(1:5)) .OR. ANY(ksec1(7:)/=ksec1w(7:)) .OR. &
        .NOT. (grid_org .samegrid. grid_orgw) )) ) THEN

!   interpolo e scrivo grib salvato, salvo ultimo grib letto
    CALL interp_scalar(grid_orgw,grid_outw,rule)
    grid_out = grid_outw
    grid_orgw = grid_org
    ksec1_swap(:) = ksec1(:)
    ksec1(:) = ksec1w(:)
    ksec1w(:) = ksec1_swap(:)

    nwind_sca = nwind_sca + 1
    towritew = .FALSE.
    towrite = .TRUE.
    pwind = wind

  ENDIF

!-------------------------------------------------------------------------------
! 2.3: Se necessario, codifica & scrittura campo memorizzato
!
  IF (towritew) THEN

    WRITE (88,*) '************************************************************'
    WRITE (88,*) "Output field (memo)"
    CALL print_grid(grid_outw,88)

!   costruisco ksec2
    CALL expand_grid(grid_outw,ksec2,field)
    ksec4(1) =  ksec2(2)*ksec2(3)

!   aggiusto l'header del grib
    CALL modif_grib_header(ksec1w,ksec3,psec3,field)

    nval = ksec4(1)
    nok = COUNT(field(1:nval)/=rmis)
    WRITE (96,'(a,i4,2i8,e14.7)') "Scrivo 1 grib: par, dati, ok, media ", &
      ksec1w(6),nval,nok, &
      SUM( field(1:nval), MASK=field(1:nval)/=rmis ) / float(MAX(nok,1))

    CALL GRIBEX (ksec0,ksec1w,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) print *,'Warning gribex: kret ',kret,' ',fileout

    CALL PBWRITE(iuout,kbuffer,ksec0(1),kret)

    IF (kret <= 0) THEN 
      WRITE(*,*) 'Error pbwrite, kret ',kret, ' ',fileout
    ELSE
      ngribout = ngribout + 1
      WRITE(*,*) 'Scritto un grib: ',ngribout
    ENDIF

  ENDIF

!-------------------------------------------------------------------------------
! 2.4: Se necessario, codifica & scrittura ultimo campo letto
!
  IF (towrite) THEN

    WRITE (88,*) '************************************************************'
    WRITE (88,*) "Output field (latest read)"
    CALL print_grid(grid_out,88)

!   costruisco ksec2
    CALL expand_grid(grid_out,ksec2,field)
    ksec4(1) =  ksec2(2)*ksec2(3)

!   aggiusto l'header del grib
    CALL modif_grib_header(ksec1,ksec3,psec3,field)

    nval = ksec4(1)
    nok = COUNT(field(1:nval)/=rmis)

    WRITE (96,'(a,i4,2i8,e14.7)') "Scrivo 1 grib: par, dati, ok, media ", &
      ksec1(6),nval,nok, &
      SUM( field(1:nval), MASK=field(1:nval)/=rmis ) / float(MAX(nok,1))

    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'C',kret)
    WRITE (88,*) 'gribex encoded, ksec0(1) ',ksec0(1)
    WRITE (88,*)

    IF (kret > 0) print *,'Warning gribex: kret ',kret,' ',fileout
  
    CALL PBWRITE(iuout,kbuffer,ksec0(1),kret)

    IF (kret <= 0) THEN 
      WRITE(*,*) 'Error pbwrite, kret ',kret, ' ',fileout
    ELSE
      ngribout = ngribout + 1
      WRITE(*,*) 'Scritto un grib: ',ngribout
    ENDIF

    WRITE (96,*)

  ENDIF

ENDDO grib

!-------------------------------------------------------------------------------
! 2.5: Se l'ultimo grib del file era una componente del vento isolata, e' 
!      rimasta memorizzata ma non e' stata scritta ne' interpolata...

IF (pwind /= 0) THEN

! interpolo il campo
  CALL interp_scalar(grid_orgw,grid_outw,rule)

  WRITE (88,*) '************************************************************'
  WRITE (88,*) "Output field (memo - last grib in file)"
  CALL print_grid(grid_outw,88)

! costruisco ksec2
  CALL expand_grid(grid_outw,ksec2,field)
  ksec4(1) =  ksec2(2)*ksec2(3)

! aggiusto l'header del grib
  CALL modif_grib_header(ksec1w,ksec3,psec3,field)

  nval = ksec4(1)
  nok = COUNT(field(1:nval)/=rmis)

  WRITE (96,'(a,i4,2i8,e14.7)') "Scrivo 1 grib: par, dati, ok, media ", &
    ksec1w(6),nval,nok, &
    SUM( field(1:nval), MASK=field(1:nval)/=rmis ) / float(MAX(nok,1))

  CALL GRIBEX (ksec0,ksec1w,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) print *,'Warning gribex: kret ',kret,' ',fileout

  CALL PBWRITE(iuout,kbuffer,ksec0(1),kret)

  IF (kret <= 0) THEN 
    WRITE(*,*) 'Error pbwrite, kret ',kret, ' ',fileout
  ELSE
    ngribout = ngribout + 1
    WRITE(*,*) 'Scritto un grib: ',ngribout
  ENDIF

  pwind = 0
  nwind_sca = nwind_sca + 1

ENDIF

!-------------------------------------------------------------------------------
! Sezione 3: conclusione
!

CALL PBCLOSE(iuin,kret)
if (kret.ne.0) WRITE(*,*) 'Error pbclose: kret ',kret,filein

CALL PBCLOSE(iuout,kret)
if (kret.ne.0) WRITE(*,*) 'Error pbclose: kret ',kret,fileout

WRITE (*,*) "Input file esaurito, programma terminato"
WRITE (*,*) "grib: letti ",ngribin,", scritti ",ngribout," di cui:"
WRITE (*,*) " - scalari           : ",nscal 
WRITE (*,*) " - vento int. scalare: ",nwind_sca 
WRITE (*,*) " - vento int. vettore: ",nwind_vec

STOP
END PROGRAM interp_grib

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE modif_grib_header(ksec1,ksec3,psec3,field)
!
! Subroutinte che modifica l'intestazione del grib (tranne sezione 2) per
! tappare problemi vari. Chiamata prima della scrittura di ciscun GRIB
!
USE grid_handler

!INTEGER, INTENT(INOUT) :: ksec0(2),ksec1(1024),ksec3(2),ksec4(512)
!REAL, INTENT(INOUT) :: psec2(512),psec3(2)
INTEGER, INTENT(INOUT) :: ksec1(1024),ksec3(2)
REAL, INTENT(INOUT) :: psec3(2)
REAL, INTENT(INOUT) :: field(maxdim)
!-------------------------------------------------------------------------------

! 1) Se ci sono dati mancanti, forzo scrittura sez. 2 e 3
IF ( ANY(field == rmis) ) THEN  
  ksec1(5) = 192
  ksec3(1) = 0
  psec3(2) = rmis
ENDIF

! 2) Tappo il fatto che le pioggia di LM e' scritta come accumulazione di un 
!    numero di prodotti pari a 0, il che fa arrabbiare la gribex!!
IF (ksec1(18) == 4) ksec1(19) = ksec1(17) - ksec1(16)

! per visualizzare Calmet con GRADS: scrivo il livelli come altezza
! SLM invece che dalla superficie come sarebbe giusto
!IF (ksec1(1)==200 .AND. ksec1(2)==200 .AND. ksec1(3)==201 .AND. &
!    ksec1(7)==105) ksec1(7) = 103

RETURN

END SUBROUTINE modif_grib_header

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a schermo l'help del programma
!
WRITE (*,*) ""
WRITE (*,*) "                 ***** INTERP_GRIB  ***** "
WRITE (*,*) "Programma che legge un file contente molti grib su griglia "
WRITE (*,*) "regolare o ruotata e li riscrive interpolati su un'altra griglia"
WRITE (*,*)
WRITE (*,*) "USO:"
WRITE (*,*) "interp_grib [-h] [-c] [-f] filein fileout"
WRITE (*,*) ""
WRITE (*,*) "filein  : file da cui leggere i grib"
WRITE (*,*) "fileout : file su cui scrivere i grib"
WRITE (*,*) " -h     : visualizza questo help"
WRITE (*,*) " -c     : crea una namelist d'esempio"
WRITE (*,*) " -f     : forza il programma a interpolare sulla griglia definita"
WRITE (*,*) "          dall'utente nel file interp_grib.inp" 
WRITE (*,*) "          se questo flag non viene specificato, il programma" 
WRITE (*,*) "          interpola sulla griglia non ruotata piu' simile alla"
WRITE (*,*) "          griglia originaria"
WRITE (*,*) ""

END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE crea_esempio_namelist
!
! crea  un file interp_grib.inp di esempio
!
OPEN (UNIT=60, FILE="interp_grib.inp", STATUS="replace", FORM="formatted")

WRITE (60,'(a)') "&outgrid"
WRITE (60,'(a)') " oproj='GEO',"
WRITE (60,'(a)') " rule='L',"
WRITE (60,'(a)') " onx=0,"
WRITE (60,'(a)') " ony=0,"
WRITE (60,'(a)') " odx=0.,"
WRITE (60,'(a)') " ody=0.,"
WRITE (60,'(a)') " ox1=0.,"
WRITE (60,'(a)') " oy1=0.,"
WRITE (60,'(a)') " oxrot=0.,"
WRITE (60,'(a)') " oyrot=0.,"
WRITE (60,'(a)') " outmz=32,"
WRITE (60,'(a)') "/"
WRITE (60,'(a)') ""
WRITE (60,'(a)') ""
!                 12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (60,'(a)') "**** DOCUMENTAZIONE ELEMENTI DELLA NAMELIST *****"
WRITE (60,'(a)') ""
WRITE (60,'(a)') "oproj    : proiezione della griglia richiesta"
WRITE (60,'(a)') "             'GEO' (regolare o ruotata); 'UTM' "
WRITE (60,'(a)') "rule     : tipo di interpolazione"
WRITE (60,'(a)') "         :   'N'=p.to + vicino; 'L'=bilineare; 'M'=media tra i punti della box"
WRITE (60,'(a)') "         :     (se ci sono punti mancanti, metto il box mancante)"
WRITE (60,'(a)') "         :   'A'=media tra i punti della box (uso i punti con dati validi)"
WRITE (60,'(a)') ""
WRITE (60,'(a)') "onx, ony : n.ro di punti nelle direzioni x e y"
WRITE (60,'(a)') "odx, ody : passo di griglia in x e y"
WRITE (60,'(a)') "             gradi e decimi (proj='GEO'); km (proj='UTM')"
WRITE (60,'(a)') "ox1, oy1 : coord. x e y del primo punto, in gradi e decimi (proj='GEO') o"
WRITE (60,'(a)') "             km (proj='UTM'). Indicare sempre le coor. del punto griglia SW,"
WRITE (60,'(a)') "             non quelle dell'angolo SW della cella"
WRITE (60,'(a)') ""
WRITE (60,'(a)') "oxrot, oyrot: coord. centro di rotazione, gradi e decimi (solo proj='GEO')"
WRITE (60,'(a)') "outmz       : fuso UTM della griglia richiesta           (solo proj='UTM')"
WRITE (60,'(a)') ""

CLOSE (60)

END SUBROUTINE crea_esempio_namelist
