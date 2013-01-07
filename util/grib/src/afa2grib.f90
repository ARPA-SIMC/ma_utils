PROGRAM afa2grb
!--------------------------------------------------------------------------
! Uso: afa2grib.exe filein fileout
!
!                                Versione 1.0.1, Enrico & Johnny 11/09/2012
!--------------------------------------------------------------------------
  IMPLICIT NONE
  INTEGER, PARAMETER   :: MAXGR=10000000
  REAL, PARAMETER :: epsilon=1E-3
  REAL , ALLOCATABLE   :: FIELD(:,:)           ! Field
  REAL     :: RLON1,RLAT1,  &                  ! Coordinates bottom left
       RLON2,RLAT2,  &                         ! Coordinates top right
       DLON,DLAT,    &                         ! Increments of coordinates
       CRLON,CRLAT                             ! Coordinates of the center of rotation
  CHARACTER(LEN=100)  :: grb_file,&            ! Output file
       afa_file                                ! Input file
  INTEGER ::  IUNIT, NPLON, NPLAT, NVAR, NTAB, &
       DATE(5), SCAD(4), LEV(3), VAR(3), SPECIF(3)
  INTEGER  KBUFFER(MAXGR), KRET, IGN, KWORD, KER, i, k, j
  INTEGER  ISEC0(2), ISEC1(36), ISEC2(384), &
       ISEC3(2), ISEC4(42)
  REAL     ZSEC3(2), ZSEC2(96), PRLON, PRLAT, R_MIS

  CALL GETARG(1,afa_file)
  CALL GETARG(2,grb_file)
  OPEN (11,file=TRIM(afa_file),err=901)
  CALL GRSVCK(0)
  CALL PBOPEN(iunit,grb_file,'W',KRET)

  DO k=1,HUGE(k)
     ! READ afa FILE: HEADER
     READ (11,*,end=801)
     read (11,*,err=902)date
     read (11,*,err=902)scad
     read (11,*,err=902)lev
     read (11,*,err=902)var
     read (11,*,err=902)
     read (11,*,err=902)rlat1,rlat2,dlon,nplat
     read (11,*,err=902)rlon1,rlon2,dlat,nplon
     read (11,*,err=902)specif,r_mis

     !  DEFINE GRIB SECTION 1
     isec1(1)    =var(2)                 ! centro di emissione
     isec1(2)    =var(1)                 ! versione tabella 2
     isec1(3)    =255                    
     isec1(4)    =255                    
     isec1(5)    =192                    ! segnala la presenza delle sezioni 2 e 3           
     isec1(6)    =var(3)                 ! parametro
     isec1(7:9)  =lev(1:3)               ! livelli
     isec1(10)   =mod((date(1)-1),100)+1 ! anno a 2 cifre
     isec1(11:14)=date(2:5)              ! data,ora
     isec1(15:18)=scad(1:4)              ! scadenza
     isec1(19)   =0                      
     isec1(20)   =0                      
     isec1(21)   =(date(1)-1)/100+1      ! secolo
     isec1(22:)   =0                      

     ! READ afa FILE: DATA
     ALLOCATE (field(nplon,nplat))
     read (11,*,err=902)((field(i,j),i=1,nplon),j=1,nplat)

     !  DEFINE GRIB SECTION 2
     ISEC2(1)=0
     ISEC2(2)=NPLON
     ISEC2(3)=NPLAT
     ISEC2(4)=NINT(RLAT1)
     ISEC2(5)=NINT(RLON1)
     ISEC2(6)=128
     ISEC2(7)=NINT(RLAT2)
     ISEC2(8)=NINT(RLON2)
     ISEC2(9)=NINT(DLON)
     ISEC2(10)=NINT(DLAT)
     ISEC2(11)=64
     ISEC2(12:)=0
     ZSEC2(:)=0.

     !  DEFINE GRIB SECTION 3
     ISEC3(:)=0
     ZSEC3(:)=r_mis

     !  DEFINE OTHER GRIB SECTIONS
     ISEC4(1)=NPLON*NPLAT
     ISEC4(2)=24
     ISEC4(3:)=0

     !  WRITE THE GRIB
     WHERE(abs(field-r_mis)<epsilon)
        field=r_mis
     END WHERE
     CALL GRIBEX (ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,  &
          FIELD,NPLON*NPLAT,KBUFFER,MAXGR,KWORD,'C',KER)
     CALL PBWRITE(iunit,KBUFFER,ISEC0(1),KRET)

     DEALLOCATE (field)
  END DO

801 CALL PBCLOSE(iunit,KRET)

  STOP

  !  ERROR MANAGEMENT
901 PRINT *,"Error opening ",afa_file
  STOP
902 PRINT *,"Error reading ",afa_file
  STOP

END PROGRAM afa2grb
