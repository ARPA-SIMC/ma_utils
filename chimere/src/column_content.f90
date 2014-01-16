PROGRAM column_content
!--------------------------------------------------------------------------
! Legge da due file le concentrazioni di una specie chimica e le quote dei 
! model levels livelli relative a un run Chimere; scrive un file con il 
! contenuto colonnare totale della specie chimica
!
! Uso: column_content.exe fileconc filez fileout
!
! Note:
! - fileconc filez fileout in formato grib (ed. 1 oppure 2)
! - fileconc deve contenere i dati di un'unica specie chimica, un ug/m3
! - i due files di input devono contenere campi corrispondenti (stessa area,
!   livello, reftime e timerange)
! - i due files di input devono essere ordinati per reftime, timerange, 
!   level (con level crescente; quote livelli Chimere relative al top dello
!   strato)
! - il programma elabora ogni istante in modo indipendente
!
! Todo:
! - scrittura in output su livello giusto (adesso: quello dell'ultimo campo 
!   di ogni istante)
! ? gestione dati mancanti 
! ? estendere al calcoalo del contenuto colonnare di una variabile "acqua"
!   di un modello meteo: 
!   * gestire model layers/ pressure levels, considerando che le quote
!     sarebbero riferite al centro del livello e non al top
!   * gestire quote livelli fissi nel tempo (Cosmo; reftime/trange di conc
!     e z risulterebbero diverse)
!
!                                                 V1.0.0, Enrico 14/01/2014
!--------------------------------------------------------------------------

USE grib_api
USE datetime_class
USE missing_values
USE grib2_utilities
IMPLICIT NONE

!--------------------------------------------------------------------------
! 0) Dichiarazioni

INTEGER, PARAMETER :: par_z(3) = (/201,200,8/)
INTEGER, PARAMETER :: maxlev = 8

REAL, ALLOCATABLE :: conc(:,:),zz(:,:),ww(:,:),cout(:)
TYPE (datetime) :: datahc,datahc_dum
INTEGER :: par_chem(3),par_dum(3),lev_dum(3),scad(4),scad_dum(4)
INTEGER :: idlev(maxlev),nlevs,nlevs_min,nlevs_max,idlev_dum
INTEGER :: ifc=0,ifz=0,ifo=0,igc=0,igz=0,igo=0,igc_last=0
INTEGER :: kt,kg,kl,iret,clret(6),cnt_out,ni,nj,np
CHARACTER (LEN=200) :: fileconc,filez,fileout

!--------------------------------------------------------------------------
! 1) Preliminari

! Parametri da riga comandi
CALL getarg(1,fileconc)
CALL getarg(2,filez)
CALL getarg(3,fileout)

IF (TRIM(fileconc) == "" .OR. TRIM(filez) == "" .OR. TRIM(fileout) == "" .OR. &
    TRIM(fileconc) == "-h") THEN
  CALL scrive_help
  STOP 1
ENDIF

! Apro i files
CALL grib_open_file(ifc,fileconc,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifz,filez,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9998
CALL grib_open_file(ifo,fileout,"w",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9997

!--------------------------------------------------------------------------
! 2) Legggo/scrivo (ciclo sugli istanti)

datahc = datetime_miss
scad(:) = imiss
kt = 1
cnt_out = 0
nlevs_max = 0
nlevs_min = HUGE(0)

DO kg = 1,HUGE(0)

! 2.1 Leggo il prossimo campo da ciascuno dei files in input
  CALL grib_new_from_file(ifc,igc,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9996

  CALL grib_new_from_file(ifz,igz,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9995
  
! 2.2 Controllo parametri e allineamento dei files
  CALL get_grib1_header(igc, REFTIME=datahc_dum, PAR=par_dum, LEV=lev_dum, &
    SCAD=scad_dum, IRET=iret)
  IF (iret /= 0) GOTO 9994
  IF (kg == 1) THEN
    par_chem(:) = par_dum(:)
  ELSE
    IF (ANY(par_dum(:) /= par_chem(:))) GOTO 9993
  ENDIF

  CALL get_grib1_header(igz, PAR=par_dum, IRET=iret)
  IF (iret /= 0) GOTO 9992
  IF (ANY(par_dum(2:3) /= par_z(2:3))) GOTO 9991

  CALL check_consistency (igc,igz, &
    .TRUE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.FALSE.,clret,iret)
  IF (iret /= 0) GOTO 9990

  IF (lev_dum(1) /= 109) GOTO 9989
  IF (lev_dum(2) > maxlev) GOTO 9988
  idlev_dum = lev_dum(2)

! 2.3 Controllo se e' iniziato un nuovo istante
  IF (kg==1 .OR. datahc/=datahc_dum .OR. ANY(scad(:)/=scad_dum(:))) THEN

!   Se ho letto il primo campo, alloco gli array di lavoro
    IF (kg == 1) THEN
      CALL grib_get(igc,"Ni",ni)
      CALL grib_get(igc,"Nj",nj)
      np = ni * nj
      ALLOCATE (conc(np,maxlev),zz(np,maxlev),ww(np,maxlev),cout(np))

!   Altrimenti scrivo i dati dell'istante precedente
    ELSE  

      ww(1:np,1) = zz(1:np,1)
      DO kl = 2,nlevs
        ww(1:np,kl) = (zz(1:np,kl) - zz(1:np,kl-1))
      ENDDO
      cout(1:np) = SUM(ww(1:np,1:nlevs) * conc(1:np,1:nlevs), DIM=2)

      CALL grib_clone(igc,igo)
      CALL grib_set(igo,"values",cout(1:np))
      CALL grib_write(igo,ifo,iret)
      CALL grib_release(igo)

      cnt_out = cnt_out + 1
      nlevs_min = MIN(nlevs,nlevs_min)
      nlevs_max = MAX(nlevs,nlevs_max)

      kt = kt + 1
    ENDIF
  
!   (Re)inizializzo gli array di lavoro
    datahc = datahc_dum
    scad(:) = scad_dum(:)
    nlevs = 0
    idlev(:) = imiss
    conc(:,:) = rmiss
    zz(:,:) = rmiss

  ENDIF

! 2.4 Salvo gli ultimi dati letti
  nlevs = nlevs + 1
  idlev(nlevs) = lev_dum(2)
  IF (nlevs > 1) THEN
    IF (idlev(nlevs) <= idlev(nlevs-1)) GOTO 9987
  ENDIF
  CALL grib_get(igc,"values",conc(1:np,nlevs))
  CALL grib_get(igz,"values",zz(1:np,nlevs))
  igc_last = igc

ENDDO

!--------------------------------------------------------------------------
! 3) Scrivo i dati all'ultimo istante e concludo

! Calcolo il contenuto colonnare
ww(1:np,1) = zz(1:np,1)
DO kl = 2,nlevs
  ww(1:np,kl) = (zz(1:np,kl) - zz(1:np,kl-1))
ENDDO
cout(1:np) = SUM(ww(1:np,1:nlevs) * conc(1:np,1:nlevs), DIM=2)

! Lo scrivo su fileout
CALL grib_clone(igc_last,igo)
CALL grib_set(igo,"values",cout(1:np))
CALL grib_write(igo,ifo,iret)
CALL grib_release(igo)

! Aggiorno i contatori statistici
cnt_out = cnt_out + 1
nlevs_min = MIN(nlevs,nlevs_min)
nlevs_max = MAX(nlevs,nlevs_max)

! Log a schermo finale
WRITE (*,*) "Esecuzione terminata"
WRITE (*,*) "Campi totali in input (data/scad/liv): ",kg-1
WRITE (*,*) "Istanti elaborati (data/scad)          ",kt
WRITE (*,*) "Campi scritti in output:               ",cnt_out
WRITE (*,'(2(a,i3))') "N.ro di livelli per istante (min-max): ",nlevs_min,"/",nlevs_max

! Libero memeoria
CALL grib_release(igc)
CALL grib_release(igz)
CALL grib_release(igo)
CALL grib_release(igc_last)
CALL grib_close_file(ifc)
CALL grib_close_file(ifz)
CALL grib_close_file(ifo)
DEALLOCATE (conc,zz,ww,cout)

STOP

!==========================================================================
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fileconc)
STOP 1

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filez)
STOP 1

9997 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fileout)
STOP 1

9996 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(fileconc)
STOP 2

9995 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filez)
STOP 2

9994 CONTINUE
WRITE (*,*) "Errore get_grib1_header: file ",TRIM(fileconc),"campo ",kg," istante ",kt," iret ",iret
STOP 3

9993 CONTINUE
WRITE (*,*) "Specie chimica inattesa in  ",TRIM(fileconc)," campo ",kg," istante ",kt
WRITE (*,'(4(a,i3.3))') "Attesa ",par_chem(2),"/",par_chem(3), &
  ", trovata ",par_dum(2),"/",par_dum(3)
STOP 3

9992 CONTINUE
WRITE (*,*) "Errore get_grib1_header: file ",TRIM(filez)," istante ",kt," iret ",iret
STOP 3

9991 CONTINUE
WRITE (*,*) "Parametro inatteso in ",TRIM(filez),"campo ",kg," istante ",kt
WRITE (*,'(4(a,i3.3))') "Attesa ",par_chem(2),"/",par_chem(3), &
  ", trovata ",par_dum(2),"/",par_dum(3)
STOP 3

9990 CONTINUE
WRITE (*,*) "Campi disallineati in ",TRIM(fileconc)," e ",TRIM(filez),"campo ",kg," istante ",kt
IF (clret(1) /= 0) WRITE (*,*) "Griglie diverse"
IF (clret(2) /= 0) WRITE (*,*) "Reftime / Timerange diversi"
IF (clret(4) /= 0) WRITE (*,*) "Livelli diversi"
STOP 3

9989 CONTINUE
WRITE (*,*) "Trovato livello di tipo non gestivo: ",lev_dum(:)
STOP 3

9988 CONTINUE
WRITE (*,*) "Troppi livelli in input, aumentare parametero ",maxlev
STOP 3

9987 CONTINUE
WRITE (*,*) "Livelli in input non crescenti "
WRITE (*,*) idlev(1:nlevs)
STOP 3


END PROGRAM column_content

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Scrive a schermo l'help del programma
!
IMPLICIT NONE

!                12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*)
WRITE (*,'(a)') "Uso: column_content.exe fileconc filez fileout"
WRITE (*,'(a)') "Legge da file grib le conc. di una specie chimica e le quote dei livelli"
WRITE (*,'(a)') "scrive un file con il contenuto colonnare totale della specie chimica" 
RETURN
END SUBROUTINE scrive_help
