PROGRAM calmet_dat2asc
!--------------------------------------------------------------------------
! Programma per leggere l'output di calmet (calmet.dat) e riscriverlo 
! ASCII. 
! Legge le versioni di Calmet 5.0 e 5.2, e riscrive nello stesso formato
!
! Uso: calmet_dat2asc filein fileout [-h]
!
! Note: fissando a T la variabile "form", e' possibile scrivere il file di 
!   output con un formato diverso da *. 
!   La scelta dei formati e' fatta per ottimizzare le dimensioni del file 
!   Ascii g-zippato, minimizzando la perdita di infomrazione. 
!   Con questi formati, calmet.asc.gz occupa circa 8M; scrivere in formato
!   compatto anche ai campi 2d farebbe risparmiare meno del 10%
!
!                                           Versione 3.1, Enrico 19/11/2007
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
  
! 0.4) Altre variabili del programma
REAL :: rver
INTEGER :: ios,idum(12),j,i,ih,k,k2
CHARACTER (LEN=80) :: filein,fileout
CHARACTER (LEN=9) :: ch9
LOGICAL :: form

!--------------------------------------------------------------------------
! 1) Preliminari

! User modification
form =.TRUE.         ! F: output in fomato *; T: output in formato compatto

! Parametri da riga comandi
CALL getarg(1,filein)
CALL getarg(2,fileout)

IF (filein == "-h" .OR. filein == "" .OR. fileout == "") THEN
  WRITE (*,*) " Uso: calmet_dat2asc filein fileout [-h]"
  STOP
ENDIF

! Apro i files
OPEN (UNIT=30, FILE=filein, FORM='UNFORMATTED', STATUS='OLD',ERR=9999)
OPEN (UNIT=31, FILE=fileout, FORM='FORMATTED', STATUS='REPLACE')

!--------------------------------------------------------------------------
! 2) Leggo/scrivo: Header

!--------------------------------------------------------------------------
! 2.1 Lettura

! Lettura header record 1: run title
READ (30) title

! Lettura header record 2: run and grid information
READ (30) ver,level,ibyr,ibmo,ibdy,ibhr,ibtz,irlg,irtype, &
  nx,ny,nz,dgrid,xorigr,yorigr,iutmzn,iwfcod,nssta, &
  nusta,npsta,nowsta,nlu,iwat1,iwat2,lcalgrd 

READ (ver,*) rver ! numero della versione (reale)
WRITE (*,'(a,f4.1)') "Versione calmet : ",rver

! Lettura header record 3 (-): Lambert Conformal parameters
IF (rver >= 5.2) THEN
  READ (30) xlat0,xlon0,llconf,conec,xlat1,xlat2,rlat0,rlon0
ENDIF

! Lettura header record 4 (3): vertical cell face height
READ (30) clab1,idum(1),(zfacem(j),j=1,nz+1) 

! Lettura header record 5-6 (4-5): surface station coordinates
IF (nssta >= 1) THEN
  READ (30) clab2,idum(2),(xssta(j),j=1,nssta) 
  READ (30) clab3,idum(3),(yssta(j),j=1,nssta) 
ENDIF

! Lettura header record 7-8 (6-7): upper air station coordinates
IF (nusta >= 1) THEN
  READ (30) clab4,idum(4),(xusta(j),j=1,nusta) 
  READ (30) clab5,idum(5),(yusta(j),j=1,nusta) 
ENDIF

! Lettura header record 9-10 (8-9): precipitation station coordinates
IF (npsta >= 1) THEN
  READ (30) clab6,idum(6),(xpsta(j),j=1,npsta) 
  READ (30) clab7,idum(7),(ypsta(j),j=1,npsta) 
ENDIF

! Lettura header record 11 (10): surface roughness lenghts
READ (30) clab8,idum(8),((zo(i,j),i=1,nx),j=1,ny)

! Lettura header record 12 (11): land use categories
READ (30) clab9,idum(9),((ilandu(i,j),i=1,nx),j=1,ny)

! Lettura header record 13 (12): terrain elevations
READ (30) clab10,idum(10),((elev(i,j),i=1,nx),j=1,ny)

! Lettura header record 14 (13): leaf area indexes
READ (30) clab11,idum(11),((xlai(i,j),i=1,nx),j=1,ny)

! Lettura header record 15 (14): nearest surface station to each grid point
READ (30) clab12,idum(12),((nears(i,j),i=1,nx),j=1,ny)

!--------------------------------------------------------------------------
! 2.2 Scrittura

! Scrittura header record 1: run title
WRITE (31,'(a)') title(1)
WRITE (31,'(a)') title(2)
WRITE (31,'(a)') title(3)

! Scrittura header record 2: run and grid information
WRITE (31,'(a)') ver
WRITE (31,'(a)') level
WRITE (31,*) ibyr,ibmo,ibdy,ibhr,ibtz,irlg,irtype, &
  nx,ny,nz,dgrid,xorigr,yorigr,iutmzn,iwfcod,nssta, &
  nusta,npsta,nowsta,nlu,iwat1,iwat2,lcalgrd 

! Scrittura header record 3 (-): Lambert Conformal parameters
IF (rver >= 5.2) THEN
  WRITE (31,*) xlat0,xlon0,llconf,conec,xlat1,xlat2,rlat0,rlon0
ENDIF

! Scrittura header record 4 (3): vertical cell face height
WRITE (31,'(a)') clab1
WRITE (31,*) idum(1),(zfacem(j),j=1,nz+1) 

! Scrittura header record 5-6 (4-5): surface station coordinates
IF (nssta >= 1) THEN
  WRITE (31,'(a)') clab2
  WRITE (31,*) idum(2),(xssta(j),j=1,nssta) 
  WRITE (31,'(a)') clab3
  WRITE (31,*) idum(3),(yssta(j),j=1,nssta) 
ENDIF

! Scrittura header record 7-8 (6-7): upper air station coordinates
IF (nusta >= 1) THEN
  WRITE (31,'(a)') clab4
  WRITE (31,*) idum(4),(xusta(j),j=1,nusta) 
  WRITE (31,'(a)') clab5
  WRITE (31,*) idum(5),(yusta(j),j=1,nusta) 
ENDIF

! Scrittura header record 9-10 (8-9): precipitation station coordinates
IF (npsta >= 1) THEN
  WRITE (31,'(a)') clab6
  WRITE (31,*) idum(6),(xpsta(j),j=1,npsta) 
  WRITE (31,'(a)') clab7
  WRITE (31,*) idum(7),(ypsta(j),j=1,npsta) 
ENDIF

! Scrittura header record 11 (10): surface roughness lenghts
WRITE (31,'(a)') clab8
WRITE (31,*) idum(8),((zo(i,j),i=1,nx),j=1,ny)

! Scrittura header record 12 (11): land use categories
WRITE (31,'(a)') clab9
WRITE (31,*) idum(9),((ilandu(i,j),i=1,nx),j=1,ny)

! Scrittura header record 13 (12): terrain elevations
WRITE (31,'(a)') clab10
WRITE (31,*) idum(10),((elev(i,j),i=1,nx),j=1,ny)

! Scrittura header record 14 (13): leaf area indexes
WRITE (31,'(a)') clab11
WRITE (31,*) idum(11),((xlai(i,j),i=1,nx),j=1,ny)

! Scrittura header record 15 (14): nearest surface station to each g.p.
WRITE (31,'(a)') clab12
WRITE (31,*) idum(12),((nears(i,j),i=1,nx),j=1,ny)


!--------------------------------------------------------------------------
! 3) Leggo/scrivo: Data records (ciclo sulle scadenze)

DO ih = 1, irlg

!--------------------------------------------------------------------------
! 3.1 Lettura

! Wind: U,V,W components 
  DO k=1,nz
    READ (30) clabu,ndathr,((u(i,j,k),i=1,nx),j=1,ny)       
    READ (30) clabv,ndathr,((v(i,j,k),i=1,nx),j=1,ny)       
    IF (lcalgrd) READ (30) clabw,ndathr,((w(i,j,k),i=1,nx),j=1,ny)  
  ENDDO
              
! 3-D temperature field 
  IF (lcalgrd .AND. irtype == 1) THEN
    DO k=1,nz
      READ (30) clabt,ndathr,((ztemp(i,j,k),i=1,nx),j=1,ny)       
    ENDDO
  ENDIF

! 2-D meteorological fields 
  IF (irtype == 1) THEN 
    READ (30) clabsc,ndathr,((ipgt(i,j),i=1,nx),j=1,ny)
    READ (30) clabus,ndathr,((ustar(i,j),i=1,nx),j=1,ny)
    READ (30) clabzi,ndathr,((zi(i,j),i=1,nx),j=1,ny)
    READ (30) clabl,ndathr,((el(i,j),i=1,nx),j=1,ny)
    READ (30) clabws,ndathr,((wstar(i,j),i=1,nx),j=1,ny)
    IF (npsta.gt.0) READ (30) clabrmm,ndathr, &
       ((rmm(i,j),i=1,nx),j=1,ny)           ! RMM non usato per calgrid
  ENDIF
 
! 1-D meteorological fields 
  IF (irtype.eq.1) THEN 
    READ (30) clabtk,ndathr,(tempk(i),i=1,nssta)
    READ (30) clabd,ndathr,(rho(i),i=1,nssta)
    READ (30) clabq,ndathr,(qsw(i),i=1,nssta)
    READ (30) clabrh,ndathr,(irh(i),i=1,nssta)
    IF (npsta.gt.0) READ (30) clabpc,ndathr,(ipcode(i),i=1,nssta)
  ENDIF
                   
  WRITE (*,*) "Letta scadenza ",ih

!--------------------------------------------------------------------------
! 3.3 Scrittura

! Wind: U,V,W components 
  DO k=1,nz
    WRITE (31,'(a)') clabu
    WRITE (31,*) ndathr
    IF (form) THEN
      WRITE (31,'(10f8.2)') ((u(i,j,k),i=1,nx),j=1,ny)       
    ELSE
      WRITE (31,*) ((u(i,j,k),i=1,nx),j=1,ny)       
    ENDIF

    WRITE (31,'(a)') clabv
    WRITE (31,*) ndathr
    IF (form) THEN
      WRITE (31,'(10f8.2)') ((v(i,j,k),i=1,nx),j=1,ny)       
    ELSE
      WRITE (31,*) ((v(i,j,k),i=1,nx),j=1,ny)       
    ENDIF

    IF (lcalgrd) THEN
      WRITE (31,'(a)') clabw
      WRITE (31,*) ndathr
      IF (form) THEN
        WRITE (31,'(10f10.4)') ((w(i,j,k),i=1,nx),j=1,ny)  
      ELSE
        WRITE (31,*) ((w(i,j,k),i=1,nx),j=1,ny)  
      ENDIF
    ENDIF
  ENDDO
              
! 3-D temperature field 
  IF (lcalgrd .AND. irtype == 1) THEN
    DO k=1,nz
      WRITE (31,'(a)') clabt
      WRITE (31,*) ndathr
      IF (form) THEN
        WRITE (31,'(10f8.2)') ((ztemp(i,j,k),i=1,nx),j=1,ny)
      ELSE
        WRITE (31,*) ((ztemp(i,j,k),i=1,nx),j=1,ny)
      ENDIF
    ENDDO
  ENDIF

! 2-D meteorological fields 
  IF (irtype == 1) THEN 
    WRITE (31,'(a)') clabsc
    WRITE (31,*) ndathr,((ipgt(i,j),i=1,nx),j=1,ny)
    WRITE (31,'(a)') clabus
    WRITE (31,*) ndathr,((ustar(i,j),i=1,nx),j=1,ny)
    WRITE (31,'(a)') clabzi
    WRITE (31,*) ndathr,((zi(i,j),i=1,nx),j=1,ny)
    WRITE (31,'(a)') clabl
    WRITE (31,*) ndathr,((el(i,j),i=1,nx),j=1,ny)
    WRITE (31,'(a)') clabws
    WRITE (31,*) ndathr,((wstar(i,j),i=1,nx),j=1,ny)
    IF (npsta.gt.0) THEN            ! RMM non usato per calgrid
      WRITE (31,'(a)') clabrmm
      WRITE (31,*) ndathr,((rmm(i,j),i=1,nx),j=1,ny)
    ENDIF
  ENDIF
 
! 1-D meteorological fields 
  IF (irtype.eq.1) THEN 
    WRITE (31,'(a)') clabtk
    WRITE (31,*) ndathr,(tempk(i),i=1,nssta)
    WRITE (31,'(a)') clabd
    WRITE (31,*) ndathr,(rho(i),i=1,nssta)
    WRITE (31,'(a)') clabq
    WRITE (31,*) ndathr,(qsw(i),i=1,nssta)
    WRITE (31,'(a)') clabrh
    WRITE (31,*) ndathr,(irh(i),i=1,nssta)
    IF (npsta.gt.0) THEN
      WRITE (31,'(a)') clabpc
      WRITE (31,*) ndathr,(ipcode(i),i=1,nssta)
    ENDIF
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

END PROGRAM calmet_dat2asc
