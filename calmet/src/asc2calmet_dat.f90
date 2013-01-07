PROGRAM asc2calmet_dat
!---------------------------------------------------------------------
! Programma per leggere l'output di calmet in formato ASCII (calmet.asc) e
! riscriverlo unformatted (calmet.dat). 
! Programma complementare a calmet_dat2asc
! Legge le versioni di Calmet 5.0 e 5.2, e riscrive nella versione 
! specificata dal parametro out_ver
!
! Uso: asc2calmet_dat2 filein fileout [-h]
!
! Note: se calmet.asc fosse stato scritto usando fomrati particolari 
!   incompatibili con la lettura *: mettere T la variabile "form" e 
!   modificare i formati di lettura (sez. 3.1)
!
!                                           Versione 2.1, Enrico 19/11/2007
!--------------------------------------------------------------------------

IMPLICIT NONE       

!--------------------------------------------------------------------------
! 0) Dichiarazioni
        
! 0.1) Parametri che dimensionano al massimo gli array, contenuti nel file
!      params.met di calmet 

INTEGER, PARAMETER :: mxnx=150,mxny=150,mxnz=12
INTEGER, PARAMETER :: mxss=99,mxus=99,mxps=1,mxows=1
INTEGER, PARAMETER :: mxnzp1=mxnz+1

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
REAL :: ztemp(mxnx,mxny,mxnz)      
REAL :: ustar(mxnx,mxny),zi(mxnx,mxny),el(mxnx,mxny)
REAL :: wstar(mxnx,mxny),rmm(mxnx,mxny)
REAL :: tempk(mxss),rho(mxss),qsw(mxss)

INTEGER :: ipgt(mxnx,mxny)
INTEGER :: irh(mxss),ipcode(mxss)
INTEGER :: ndathr
        
CHARACTER (LEN=8) :: clabu,clabv,clabw,clabt,clabsc,clabus,clabzi
CHARACTER (LEN=8) :: clabl,clabws,clabrmm,clabtk,clabd,clabq,clabrh
CHARACTER (LEN=8) :: clabpc 
  
! 0.4) Altre variabili del programma
REAL :: out_ver,rver
INTEGER :: ios,idum(12),j,i,ih,k,k2,yy
CHARACTER (LEN=80) :: filein,fileout,chver
CHARACTER (LEN=9) :: ch9
LOGICAL :: form

!--------------------------------------------------------------------------
! 1) Preliminari

form =.FALSE.         ! F: input in fomato *; T: input in formato diverso
out_ver = 5.0         ! default per fomato di output

! Parametri da riga comandi
CALL getarg(1,filein)
CALL getarg(2,fileout)
CALL getarg(3,chver)
READ (chver,*,IOSTAT=ios) out_ver

IF (filein == "-h" .OR. filein == "" .OR. fileout == "" .OR. &
    ios /= 0 .OR. (out_ver /= 5.0 .AND. out_ver /= 5.2)) THEN
  WRITE (*,*) " Uso: calmet_dat2asc filein fileout [versione output] [-h]"
  STOP
ENDIF

! Apro i files
OPEN (UNIT=30, FILE=filein, FORM='FORMATTED', STATUS='OLD',ERR=9999)
OPEN (UNIT=31, FILE=fileout, FORM='UNFORMATTED', STATUS='REPLACE')


!--------------------------------------------------------------------------
! 2) Leggo/scrivo: Header

!--------------------------------------------------------------------------
! 2.1 Lettura

! Lettura header record 1: run title
READ (30,'(a)') title(1)
READ (30,'(a)') title(2)
READ (30,'(a)') title(3)

! Lettura header record 2: run and grid information
READ (30,'(a)') ver
READ (30,'(a)') level
READ (30,*) ibyr,ibmo,ibdy,ibhr,ibtz,irlg,irtype, &
  nx,ny,nz,dgrid,xorigr,yorigr,iutmzn,iwfcod,nssta, &
  nusta,npsta,nowsta,nlu,iwat1,iwat2,lcalgrd 
READ (ver,*) rver         ! numero della versione (reale)

! Lettura header record 3 (-): Lambert Conformal parameters
IF (rver >= 5.2) THEN
  READ (30,*) xlat0,xlon0,llconf,conec,xlat1,xlat2,rlat0,rlon0
ENDIF

! Lettura header record 4 (3): vertical cell face height
READ (30,'(a)') clab1
READ (30,*) idum(1),(zfacem(j),j=1,nz+1) 

! Lettura header record 5-6 (4-5): surface station coordinates
IF (nssta >= 1) THEN
  READ (30,'(a)') clab2
  READ (30,*) idum(2),(xssta(j),j=1,nssta) 
  READ (30,'(a)') clab3
  READ (30,*) idum(3),(yssta(j),j=1,nssta) 
ENDIF

! Lettura header record 7-8 (6-7): upper air station coordinates
IF (nusta >= 1) THEN
  READ (30,'(a)') clab4
  READ (30,*) idum(4),(xusta(j),j=1,nusta) 
  READ (30,'(a)') clab5
  READ (30,*) idum(5),(yusta(j),j=1,nusta) 
ENDIF

! Lettura header record 9-10 (8-9): precipitation station coordinates
IF (npsta >= 1) THEN
  READ (30,'(a)') clab6
  READ (30,*) idum(6),(xpsta(j),j=1,npsta) 
  READ (30,'(a)') clab7
  READ (30,*) idum(7),(ypsta(j),j=1,npsta) 
ENDIF

! Lettura header record 11 (10): surface roughness lenghts
READ (30,'(a)') clab8
READ (30,*) idum(8),((zo(i,j),i=1,nx),j=1,ny)

! Lettura header record 12 (11): land use categories
READ (30,'(a)') clab9
READ (30,*) idum(9),((ilandu(i,j),i=1,nx),j=1,ny)

! Lettura header record 13 (12): terrain elevations
READ (30,'(a)') clab10
READ (30,*) idum(10),((elev(i,j),i=1,nx),j=1,ny)

! Lettura header record 14 (13): leaf area indexes
READ (30,'(a)') clab11
READ (30,*) idum(11),((xlai(i,j),i=1,nx),j=1,ny)

! Lettura header record 15 (14): nearest surface station to each grid point
READ (30,'(a)') clab12
READ (30,*) idum(12),((nears(i,j),i=1,nx),j=1,ny)

!--------------------------------------------------------------------------
! 2.2 Versione 5.0: anno in 2 cifre
!     Versione 5.2 (o superiori): anno in 4 cifre
IF (out_ver == 5.0) THEN
  ver = "5.0"
  IF (rver >= 5.2) ibyr = MOD(ibyr,100)

ELSE IF (out_ver >= 5.2) THEN
  ver = "5.2"
  IF (rver == 5.0) THEN
    IF (ibyr <  50) ibyr = 2000 + ibyr
    IF (ibyr >= 50) ibyr = 1900 + ibyr
  ENDIF
ENDIF

!--------------------------------------------------------------------------
! 2.3 Scrittura

! Scrittura header record 1: run title
WRITE (31) title

! Scrittura header record 2: run and grid information
WRITE (31) ver,level,ibyr,ibmo,ibdy,ibhr,ibtz,irlg,irtype, &
  nx,ny,nz,dgrid,xorigr,yorigr,iutmzn,iwfcod,nssta, &
  nusta,npsta,nowsta,nlu,iwat1,iwat2,lcalgrd 

! Scrittura header record 3 (-): Lambert Conformal parameters
IF (out_ver >= 5.2) THEN
  WRITE (31) xlat0,xlon0,llconf,conec,xlat1,xlat2,rlat0,rlon0
ENDIF

! Scrittura header record 4 (3): vertical cell face height
WRITE (31) clab1,idum(1),(zfacem(j),j=1,nz+1) 

! Scrittura header record 5-6 (4-5): surface station coordinates
IF (nssta >= 1) THEN
  WRITE (31) clab2,idum(2),(xssta(j),j=1,nssta) 
  WRITE (31) clab3,idum(3),(yssta(j),j=1,nssta) 
ENDIF

! Scrittura header record 7-8 (6-7): upper air station coordinates
IF (nusta >= 1) THEN
  WRITE (31) clab4,idum(4),(xusta(j),j=1,nusta) 
  WRITE (31) clab5,idum(5),(yusta(j),j=1,nusta) 
ENDIF

! Scrittura header record 9-10 (8-9): precipitation station coordinates
IF (npsta >= 1) THEN
  WRITE (31) clab6,idum(6),(xpsta(j),j=1,npsta) 
  WRITE (31) clab7,idum(7),(ypsta(j),j=1,npsta) 
ENDIF

! Scrittura 11 (10): surface roughness lenghts
WRITE (31) clab8,idum(8),((zo(i,j),i=1,nx),j=1,ny)

! Scrittura 12 (11): land use categories
WRITE (31) clab9,idum(9),((ilandu(i,j),i=1,nx),j=1,ny)

! Scrittura header record 13 (12): terrain elevations
WRITE (31) clab10,idum(10),((elev(i,j),i=1,nx),j=1,ny)

! Scrittura header record 14 (13): leaf area indexes
WRITE (31) clab11,idum(11),((xlai(i,j),i=1,nx),j=1,ny)

! Scrittura header record 15 (14): nearest surface station to each g.p.
WRITE (31) clab12,idum(12),((nears(i,j),i=1,nx),j=1,ny)


!--------------------------------------------------------------------------
! 3) Leggo/scrivo: Data records (ciclo sulle scadenze)

DO ih = 1, irlg

!--------------------------------------------------------------------------
! 3.1 Lettura

! Wind: U,V,W components 
  DO k=1,nz
    READ (30,'(a)') clabu
    READ (30,*) ndathr
    IF (form) THEN
      READ (30,*) ((u(i,j,k),i=1,nx),j=1,ny)       
    ELSE
      READ (30,*) ((u(i,j,k),i=1,nx),j=1,ny)       
    ENDIF

    READ (30,'(a)') clabv
    READ (30,*) ndathr
    IF (form) THEN
      READ (30,*) ((v(i,j,k),i=1,nx),j=1,ny)       
    ELSE
      READ (30,*) ((v(i,j,k),i=1,nx),j=1,ny)       
    ENDIF

    IF (lcalgrd) THEN
      READ (30,'(a)') clabw
      READ (30,*) ndathr
      IF (form) THEN
        READ (30,*) ((w(i,j,k),i=1,nx),j=1,ny)  
      ELSE
        READ (30,*) ((w(i,j,k),i=1,nx),j=1,ny)  
      ENDIF
    ENDIF
  ENDDO
              
! 3-D temperature field 
  IF (lcalgrd .AND. irtype == 1) THEN
    DO k=1,nz
      READ (30,'(a)') clabt
      READ (30,*) ndathr
      IF (form) THEN
        READ (30,*) ((ztemp(i,j,k),i=1,nx),j=1,ny)       
      ELSE
        READ (30,*) ((ztemp(i,j,k),i=1,nx),j=1,ny)       
      ENDIF
    ENDDO
  ENDIF

! 2-D meteorological fields 
  IF (irtype == 1) THEN 
    READ (30,'(a)') clabsc
    READ (30,*) ndathr,((ipgt(i,j),i=1,nx),j=1,ny)
    READ (30,'(a)') clabus
    READ (30,*) ndathr,((ustar(i,j),i=1,nx),j=1,ny)
    READ (30,'(a)') clabzi
    READ (30,*) ndathr,((zi(i,j),i=1,nx),j=1,ny)
    READ (30,'(a)') clabl
    READ (30,*) ndathr,((el(i,j),i=1,nx),j=1,ny)
    READ (30,'(a)') clabws
    READ (30,*) ndathr,((wstar(i,j),i=1,nx),j=1,ny)

    IF (npsta.gt.0) THEN           ! RMM non usato per calgrid
      READ (30,'(a)') clabrmm
      READ (30,*) ndathr,((rmm(i,j),i=1,nx),j=1,ny)
    ENDIF
  ENDIF
 
! 1-D meteorological fields 
  IF (irtype.eq.1) THEN 
    READ (30,'(a)') clabtk
    READ (30,*) ndathr,(tempk(i),i=1,nssta)
    READ (30,'(a)') clabd
    READ (30,*) ndathr,(rho(i),i=1,nssta)
    READ (30,'(a)') clabq
    READ (30,*) ndathr,(qsw(i),i=1,nssta)
    READ (30,'(a)') clabrh
    READ (30,*) ndathr,(irh(i),i=1,nssta)
    IF (npsta.gt.0) THEN
      READ (30,'(a)') clabpc
      READ (30,*) ndathr,(ipcode(i),i=1,nssta)
    ENDIF
  ENDIF
                   
  WRITE (*,*) "Letta scadenza ",ih

!--------------------------------------------------------------------------
! 3.2 Versione 5.0: anno in 2 cifre
!     Versione 5.2 (o superiori): anno in 4 cifre
IF (out_ver == 5.0) THEN
  IF (rver >= 5.2) THEN
    WRITE (ch9,'(i9.9)') ndathr
    READ  (ch9,'(2x,i7.7)') ndathr
  ENDIF

ELSE IF (out_ver >= 5.2) THEN
  IF (rver == 5.0) THEN
    yy = ndathr / 100000
    IF (yy <  50) ndathr = ndathr + 20000000
    IF (yy >= 50) ndathr = ndathr + 19000000
  ENDIF
ENDIF

!--------------------------------------------------------------------------
! 3.3 Scrittura

! Wind: U,V,W components 
  DO k=1,nz
    WRITE (31) clabu,ndathr,((u(i,j,k),i=1,nx),j=1,ny)       
    WRITE (31) clabv,ndathr,((v(i,j,k),i=1,nx),j=1,ny)       
    IF (lcalgrd) WRITE (31) clabw,ndathr,((w(i,j,k),i=1,nx),j=1,ny)  
  ENDDO
              
! 3-D temperature field 
  IF (lcalgrd .AND. irtype == 1) THEN
    DO k=1,nz
      WRITE (31) clabt,ndathr,((ztemp(i,j,k),i=1,nx),j=1,ny)
    ENDDO
  ENDIF

! 2-D meteorological fields 
  IF (irtype == 1) THEN 
    WRITE (31) clabsc,ndathr,((ipgt(i,j),i=1,nx),j=1,ny)
    WRITE (31) clabus,ndathr,((ustar(i,j),i=1,nx),j=1,ny)
    WRITE (31) clabzi,ndathr,((zi(i,j),i=1,nx),j=1,ny)
    WRITE (31) clabl,ndathr,((el(i,j),i=1,nx),j=1,ny)
    WRITE (31) clabws,ndathr,((wstar(i,j),i=1,nx),j=1,ny)
    IF (npsta.gt.0) WRITE (31) clabrmm,ndathr, &
       ((rmm(i,j),i=1,nx),j=1,ny)           ! RMM non usato per calgrid
  ENDIF
 
! 1-D meteorological fields 
  IF (irtype.eq.1) THEN 
    WRITE (31) clabtk,ndathr,(tempk(i),i=1,nssta)
    WRITE (31) clabd,ndathr,(rho(i),i=1,nssta)
    WRITE (31) clabq,ndathr,(qsw(i),i=1,nssta)
    WRITE (31) clabrh,ndathr,(irh(i),i=1,nssta)
    IF (npsta.gt.0) WRITE (31) clabpc,ndathr,(ipcode(i),i=1,nssta)
  ENDIF
                   
  WRITE (*,*) "Scritta scadenza ",ih

ENDDO

!---------------------------------------------------------------------
! 4) Conclusione

CLOSE(30)
CLOSE(31)
STOP

!---------------------------------------------------------------------
! 5) Gestione errori I/O

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

END PROGRAM asc2calmet_dat
