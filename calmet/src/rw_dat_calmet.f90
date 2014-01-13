PROGRAM rw_dat_calmet
!--------------------------------------------------------------------------
! Programma per leggere l'output di calmet (calmet.dat) e riscriverlo
! cambiando le date. Gestisce le versioni di Calmet 5.0 e 5.2
! Versioni < 3.0 come read_dat_calmet.f90
!
!                                         Versione 3.1.1, Enrico 13/01/2014
!--------------------------------------------------------------------------

USE date_handler
IMPLICIT NONE       

!--------------------------------------------------------------------------
! 0) Dichiarazioni
        
! 0.1) Parametri che dimensionano al massimo gli array 
!      compatibilmente con il file params.met di calmet 

INTEGER, PARAMETER :: mxnx=150,mxny=150,mxnz=12
INTEGER, PARAMETER :: mxss=150,mxus=150,mxps=1,mxows=1
INTEGER, PARAMETER :: mxnzp1=mxnz+1
INTEGER, PARAMETER :: npmx = 15

!       MXNX    - Maximum number of X grid cells
!       MXNY    - Maximum number of Y grid cells
!       MXNZ    - Maximum number of layers
!       MXNZP1  - Maximum number of face cells
!       MXSS    - Maximum number of surface meteorological stations
!       MXUS    - Maximum number of upper air stations
!       MXPS    - Maximum number of precipitation stations
!       MXOWS   - Maximum number of overwater stations


! 0.2) Dichiarazioni per lettura headers
REAL :: zfacem(mxnzp1),xssta(mxss),yssta(mxss),xusta(mxus),yusta(mxus), &
        xPsta(mxps),ypsta(mxps), &
        zo(mxnx,mxny),elev(mxnx,mxny),xlai(mxnx,mxny)

REAL :: dgrid,xorigr,yorigr
REAL :: xlat0,xlon0,conec,xlat1,xlat2,rlat0,rlon0

INTEGER :: ilandu(mxnx,mxny),nears(mxnx,mxny)
INTEGER :: ibyr,ibmo,ibdy,ibhr,ibtz,irlg,irtype
INTEGER :: nx,ny,nz,iutmzn,iwfcod,nssta
INTEGER :: nusta,npsta,nowsta,nlu,iwat1,iwat2

CHARACTER (LEN=80) title(3)
CHARACTER (LEN=8)  ver,level,clab1,clab2,clab3,clab4,clab5,clab6, & 
                   clab7,clab8,clab9,clab10,clab11,clab12

LOGICAL :: lcalgrd,llconf
 
! 0.3) Dichiarazioni per lettura data records
REAL :: u(mxnx,mxny,mxnz),v(mxnx,mxny,mxnz),w(mxnx,mxny,mxnz)
REAL ::  ztemp(mxnx,mxny,mxnz)      
REAL :: ustar(mxnx,mxny),zi(mxnx,mxny),el(mxnx,mxny)
REAL :: wstar(mxnx,mxny),rmm(mxnx,mxny)
REAL :: tempk(mxss),rho(mxss),qsw(mxss)

INTEGER :: ipgt(mxnx,mxny)
INTEGER :: irh(mxss),ipcode(mxss)
INTEGER :: ndathr
        
CHARACTER (LEN=8) :: clabu,clabv,clabw,clabt,clabsc,clabus,clabzi
CHARACTER (LEN=8) :: clabl,clabws,clabrmm,clabtk,clabd,clabq,clabrh
CHARACTER (LEN=8) :: clabpc 
  
! 0.5) Altre variabili del programma
TYPE (date) :: data_in,data_out,data_in1,data_out1,data_req
INTEGER, PARAMETER :: iuin=30, iuout=31
INTEGER :: hh_in,hh_in1,hh_out,hh_out1,hh_req,hht,ndathr_out
INTEGER :: ios,idum,iyy,ijj,ih,j,i,k
REAL :: rver
CHARACTER (LEN=200) :: filein,fileout,charg

!=====================================================================
! 0) Preliminari

! Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)
CALL getarg(3,charg)
READ (charg,'(i4,3i2)',IOSTAT=ios) &
  data_out1%yy,data_out1%mm,data_out1%dd,hh_out1

IF (filein == "-h" .OR. filein == "" .OR. fileout == "" .OR. ios /= 0) THEN
  WRITE (*,*) "Uso: rw_dat_calmet.exe filein fileout data1_new (yyyymmddhh)"
  WRITE (*,*) "Legge un file calmet.dat e lo riscrive attribuendo al primo istante la data data1_new"
  STOP
ENDIF

!=====================================================================
! 1) Apro files, leggo e scrivo gli headers

!--------------------------------------------------------------------------
! 1.1 Apro files
OPEN (UNIT=iuin,FILE=filein,FORM='UNFORMATTED',STATUS='OLD',ERR=9999)
OPEN (UNIT=iuout,FILE=fileout,FORM='UNFORMATTED',STATUS='REPLACE')

!--------------------------------------------------------------------------
! 1.2 Leggo headers

! Lettura header record 1: run title
READ (iuin) title

! Lettura header record 2: run and grid information
READ (iuin) ver,level,ibyr,ibmo,ibdy,ibhr,ibtz,irlg,irtype, &
  nx,ny,nz,dgrid,xorigr,yorigr,iutmzn,iwfcod,nssta, &
  nusta,npsta,nowsta,nlu,iwat1,iwat2,lcalgrd 

READ (ver,*) rver ! numero della versione (reale)

! Lettura header record 3 (-): Lambert Conformal parameters
IF (rver >= 5.2) THEN
  READ (iuin) xlat0,xlon0,llconf,conec,xlat1,xlat2,rlat0,rlon0
ENDIF

! Lettura header record 4 (3): vertical cell face height
READ (iuin) clab1,idum,(zfacem(j),j=1,nz+1) 

! Lettura header record 5-6 (4-5): surface station coordinates
IF (nssta >= 1) THEN
  READ (iuin) clab2,idum,(xssta(j),j=1,nssta) 
  READ (iuin) clab3,idum,(yssta(j),j=1,nssta) 
ENDIF

! Lettura header record 7-8 (6-7): upper air station coordinates
IF (nusta >= 1) THEN
  READ (iuin) clab4,idum,(xusta(j),j=1,nusta) 
  READ (iuin) clab5,idum,(yusta(j),j=1,nusta) 
ENDIF

! Lettura header record 9-10 (8-9): precipitation station coordinates
IF (npsta >= 1) THEN
  READ (iuin) clab6,idum,(xpsta(j),j=1,npsta) 
  READ (iuin) clab7,idum,(ypsta(j),j=1,npsta) 
ENDIF

! Lettura header record 11 (10): surface roughness lenghts
READ (iuin) clab8,idum,((zo(i,j),i=1,nx),j=1,ny)

! Lettura header record 12 (11): land use categories
READ (iuin) clab9,idum,((ilandu(i,j),i=1,nx),j=1,ny)

! Lettura header record 13 (12): terrain elevations
READ (iuin) clab10,idum,((elev(i,j),i=1,nx),j=1,ny)

! Lettura header record 14 (13): leaf area indexes
READ (iuin) clab11,idum,((xlai(i,j),i=1,nx),j=1,ny)

! Lettura header record 15 (14): nearest surface station to each grid point
READ (iuin) clab12,idum,((nears(i,j),i=1,nx),j=1,ny)

WRITE (*,'(a,f4.1,a,i4)') &
  "rw_dat_calmet: vers. calmet ",rver," numero istanti ",irlg

!--------------------------------------------------------------------------
! 1.3 Scrivo headers

! Header record 1: run title
WRITE (iuout) title

! Header record 2: run and grid information
WRITE (iuout) ver,level,data_out1%yy,data_out1%mm,data_out1%dd,ibhr, &
  ibtz,irlg,irtype, &
  nx,ny,nz,dgrid,xorigr,yorigr,iutmzn,iwfcod,nssta, &
  nusta,npsta,nowsta,nlu,iwat1,iwat2,lcalgrd 

! Header record 3 (-): Lambert Conformal parameters
IF (rver >= 5.2) THEN
  WRITE (iuout) xlat0,xlon0,llconf,conec,xlat1,xlat2,rlat0,rlon0
ENDIF

! Header record 4 (3): vertical cell face height
WRITE (iuout) clab1,idum,(zfacem(j),j=1,nz+1) 

! Header record 5-6 (4-5): surface station coordinates
IF (nssta >= 1) THEN
  WRITE (iuout) clab2,idum,(xssta(j),j=1,nssta) 
  WRITE (iuout) clab3,idum,(yssta(j),j=1,nssta) 
ENDIF

! Header record 7-8 (6-7): upper air station coordinates
IF (nusta >= 1) THEN
  WRITE (iuout) clab4,idum,(xusta(j),j=1,nusta) 
  WRITE (iuout) clab5,idum,(yusta(j),j=1,nusta) 
ENDIF

! Header record 9-10 (8-9): precipitation station coordinates
IF (npsta >= 1) THEN
  WRITE (iuout) clab6,idum,(xpsta(j),j=1,npsta) 
  WRITE (iuout) clab7,idum,(ypsta(j),j=1,npsta) 
ENDIF

! Header record 11 (10): surface roughness lenghts
WRITE (iuout) clab8,idum,((zo(i,j),i=1,nx),j=1,ny)

! Header record 12 (11): land use categories
WRITE (iuout) clab9,idum,((ilandu(i,j),i=1,nx),j=1,ny)

! Header record 13 (12): terrain elevations
WRITE (iuout) clab10,idum,((elev(i,j),i=1,nx),j=1,ny)

! Header record 14 (13): leaf area indexes
WRITE (iuout) clab11,idum,((xlai(i,j),i=1,nx),j=1,ny)

! Header record 15 (14): nearest surface station to each grid point
WRITE (iuout) clab12,idum,((nears(i,j),i=1,nx),j=1,ny)

WRITE (*,'(2(a,i4.4,3i2.2))') &
  "Header: data originale ",ibyr,ibmo,ibdy,ibhr, &
  " data nuova ",data_out1%yy,data_out1%mm,data_out1%dd,ibhr

!=====================================================================
! 2) Leggo e scrivo data records

DO ih=1,irlg

!---------------------------------------------------------------------
! 2.1) Leggo i dati

! Wind: U,V,W components 
  DO k=1,nz
    READ (iuin) clabu,ndathr,((u(i,j,k),i=1,nx),j=1,ny)       
    READ (iuin) clabv,ndathr,((v(i,j,k),i=1,nx),j=1,ny)       
    IF (lcalgrd) READ (iuin) clabw,ndathr,((w(i,j,k),i=1,nx),j=1,ny)  
  ENDDO
              
! 3-D temperature field 
  IF (lcalgrd .AND. irtype == 1) THEN
    DO k=1,nz
      READ (iuin) clabt,ndathr,((ztemp(i,j,k),i=1,nx),j=1,ny)       
    ENDDO
  ENDIF

! 2-D meteorological fields 
  IF (irtype == 1) THEN 
    READ (iuin) clabsc,ndathr,((ipgt(i,j),i=1,nx),j=1,ny)
    READ (iuin) clabus,ndathr,((ustar(i,j),i=1,nx),j=1,ny)
    READ (iuin) clabzi,ndathr,((zi(i,j),i=1,nx),j=1,ny)
    READ (iuin) clabl,ndathr,((el(i,j),i=1,nx),j=1,ny)
    READ (iuin) clabws,ndathr,((wstar(i,j),i=1,nx),j=1,ny)

    IF (npsta.gt.0) READ (iuin) clabrmm,ndathr, &
       ((rmm(i,j),i=1,nx),j=1,ny)           ! RMM non usato per calgrid

  ENDIF
 
! 1-D meteorological fields 
  IF (irtype.eq.1) THEN 
    READ (iuin) clabtk,ndathr,(tempk(i),i=1,nssta)
    READ (iuin) clabd,ndathr,(rho(i),i=1,nssta)
    READ (iuin) clabq,ndathr,(qsw(i),i=1,nssta)
    READ (iuin) clabrh,ndathr,(irh(i),i=1,nssta)
    IF (npsta.gt.0) READ (iuin) clabpc,ndathr,(ipcode(i),i=1,nssta)
  ENDIF
                   
!--------------------------------------------------------------------------
! 2.2) Controlli, calcolo nuova data

! Parsing data corrente
  iyy=int(ndathr/100000)   
  IF (rver < 5.2 .AND. iyy <= 50) THEN    ! versioni < 5.2: anno in 2 cifre
    iyy = 2000 + iyy
  ELSE IF (rver < 5.2 .AND. iyy <= 50) THEN
    iyy = 1900 + iyy
  ENDIF  
  ijj=int((ndathr-iyy*100000)/100)
  hh_in = ndathr-iyy*100000-ijj*100
  data_in = greg(ijj,iyy)

! Controlli
  IF (ih == 1) THEN
    data_in1 = data_in
    hh_in1 = hh_in
  ELSE
    hht = hh_in1+ih-1
    hh_req = MOD(hht,24)
    data_req = data_in1 + hht/24
    IF (data_req /= data_in .OR. hh_req /= hh_in) GOTO 9998
  ENDIF

! Calcolo nuova data
  hht = hh_out1+ih-1
  hh_out = MOD(hht,24)
  data_out = data_out1 + hht/24
  IF (rver < 5.2) THEN  
    ndathr_out = MOD(data_out%yy,100)*100000 + jul(data_out)*100 + hh_out
  ELSE
    ndathr_out = data_out%yy*100000 + jul(data_out)*100 + hh_out
  ENDIF

  WRITE (*,'(a,i4,2(a,i4.4,3i2.2))') "Istante ",ih, &
    " data originale ",data_in%yy,data_in%mm,data_in%dd,hh_in, &
    " data nuova ",data_out%yy,data_out%mm,data_out%dd,hh_out

!--------------------------------------------------------------------------
! 2.3) Scrivo i dati

! Wind: U,V,W components 
  DO k=1,nz
    WRITE (iuout) clabu,ndathr_out,((u(i,j,k),i=1,nx),j=1,ny)       
    WRITE (iuout) clabv,ndathr_out,((v(i,j,k),i=1,nx),j=1,ny)       
    IF (lcalgrd) WRITE (iuout) clabw,ndathr_out,((w(i,j,k),i=1,nx),j=1,ny)  
  ENDDO
              
! 3-D temperature field 
  IF (lcalgrd .AND. irtype == 1) THEN
    DO k=1,nz
      WRITE (iuout) clabt,ndathr_out,((ztemp(i,j,k),i=1,nx),j=1,ny)
    ENDDO
  ENDIF

! 2-D meteorological fields 
  IF (irtype == 1) THEN 
    WRITE (iuout) clabsc,ndathr_out,((ipgt(i,j),i=1,nx),j=1,ny)
    WRITE (iuout) clabus,ndathr_out,((ustar(i,j),i=1,nx),j=1,ny)
    WRITE (iuout) clabzi,ndathr_out,((zi(i,j),i=1,nx),j=1,ny)
    WRITE (iuout) clabl,ndathr_out,((el(i,j),i=1,nx),j=1,ny)
    WRITE (iuout) clabws,ndathr_out,((wstar(i,j),i=1,nx),j=1,ny)
    IF (npsta.gt.0) WRITE (iuout) clabrmm,ndathr_out, &
       ((rmm(i,j),i=1,nx),j=1,ny)           ! RMM non usato per calgrid
  ENDIF
 
! 1-D meteorological fields 
  IF (irtype.eq.1) THEN 
    WRITE (iuout) clabtk,ndathr_out,(tempk(i),i=1,nssta)
    WRITE (iuout) clabd,ndathr_out,(rho(i),i=1,nssta)
    WRITE (iuout) clabq,ndathr_out,(qsw(i),i=1,nssta)
    WRITE (iuout) clabrh,ndathr_out,(irh(i),i=1,nssta)
    IF (npsta.gt.0) WRITE (iuout) clabpc,ndathr_out,(ipcode(i),i=1,nssta)
  ENDIF

ENDDO
STOP

!---------------------------------------------------------------------
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,'(a,2(a,i4.4,3i2.2))') "Data disallineata in input; ", &
  "attesa: ",data_req%yy,data_req%mm,data_req%dd,hh_req, &
  "trovata :",data_in%yy,data_in%mm,data_in%dd,hh_in
STOP


END PROGRAM rw_dat_calmet
