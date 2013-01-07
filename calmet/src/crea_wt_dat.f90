PROGRAM crea_wt_dat
!--------------------------------------------------------------------------
! Programma di appoggio alla catena Calmet.
! Costruisce il file wt_PROGETTO.dat, richiesto dai run di calmet che usano
! i campi LAM.
! 
! Legge da crea_wt_dat.inp nome progetto, dimensioni della griglia Calmet 
! (nx,ny,nz), quota dei livelli (uv), pesi da assegnare a ciascun livello.
!
! Compilazione:
! f90 ordinario
!
! Uso:
! crea_wt_dat.exe [-c] [-h]
!
! Note:
! Per il momento, i pesi dipendono solo dal livello
! 
!                                                   V1.1, Enrico 10/10/2003
!--------------------------------------------------------------------------

IMPLICIT NONE

REAL, ALLOCATABLE :: quo(:),wt(:)
INTEGER :: nx,ny,nz
INTEGER :: ll,klev,i,j
CHARACTER (LEN=80) :: chpar,chfmt,str,fileout
CHARACTER (LEN=40) :: progetto

!--------------------------------------------------------------------------
! 1: Parametri da riga comandi

CALL getarg(1,chpar)

IF (TRIM(chpar) == "-c") THEN
  CALL scrive_esempio
  STOP
ELSE IF (TRIM(chpar) == "-h") THEN
  WRITE (*,'(a)') "Uso: crea_wt_dat.exe [-c] [-h]"
  WRITE (*,'(a)') "  -c: crea un file crea_wt_dat.inp di esempio"
  WRITE (*,'(a)') "  -h: visualizza questo help"
  WRITE (*,*)
  STOP
ENDIF

!--------------------------------------------------------------------------
! 2: Leggo opzioni da crea_wt.inp

OPEN (UNIT=30, FILE="crea_wt_dat.inp", STATUS="OLD", ACTION="READ", &
  ERR=9999)

READ (30,'(a)',ERR=9998) str
ll = LEN(TRIM(str))
IF (INDEX(str,"!") > 0) ll = MIN(ll,INDEX(str,"!"))
IF (INDEX(str," ") > 0) ll = MIN(ll,INDEX(str," "))
IF (ll > 0) THEN
  progetto = str(1:ll)
ELSE
  GOTO 9998
ENDIF

READ (30,*,ERR=9998) nx
READ (30,*,ERR=9998) ny
READ (30,*,ERR=9998) nz
READ (30,*,ERR=9998)

ALLOCATE (quo(nz),wt(nz))
WRITE (chfmt,'(a,i3,a)') "(6x,",nx,"(1x,f3.2))"

DO klev = 1,nz
  READ (30,*,ERR=9998) quo(klev),wt(klev)
ENDDO

CLOSE(30)

!--------------------------------------------------------------------------
! 3: Scrivo wt_PROGETTO.dat

WRITE (fileout,'(3a)') "wt_",TRIM(progetto),".dat"
OPEN (UNIT=40, FILE=fileout, STATUS="REPLACE", ACTION="WRITE")

WRITE (40,*)
WRITE (40,*)
WRITE (40,*)
WRITE (40,'(15x,2f8.1,2i5,f8.3)') -999.,-999.,nx,ny,-999.
WRITE (40,'(15x,2f8.1,2i5,f8.3)') -999.,-999.,-999,-999,-999.
WRITE (40,*)
WRITE (40,*)

DO klev = 1,nz
  WRITE (40,'(a12,f12.4)') "Height(m) = ",quo(klev)
  WRITE (40,*)
  DO j = 1,ny  
    WRITE (40,chfmt) (wt(klev),i=1,nx)
  ENDDO  
  WRITE (40,*)
ENDDO  

CLOSE(40)

STOP

!--------------------------------------------------------------------------
! 4: Gestione errori I/O

9999 CONTINUE
WRITE (*,*) "Errore aprendo crea_wt_dat.inp"
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo crea_wt_dat.inp"
STOP



END PROGRAM crea_wt_dat

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_esempio
!
! Scrive un file crea_wt.inp di esempio
!
IMPLICIT NONE

OPEN (UNIT=20, FILE="crea_wt_dat.inp", STATUS="REPLACE",FORM="FORMATTED")

!                  1234567890123456789012345678901234567890123456789012345678901234567890
WRITE (20,'(a)')  "tmp        ! nome del progetto"
WRITE (20,'(a)')  "60         ! NX (area di calmet)"
WRITE (20,'(a)')  "60         ! NY (area di calmet)"
WRITE (20,'(a)')  "1          ! NZ (numero di livelli uv)"
WRITE (20,'(2a)') "! Quota (m) e peso attribuito alle osser", &
                  "vazioni (0-1); NZ record"
WRITE (20,'(a)')  "10.    0."

WRITE (20,'(a)')  "Esempi di pesi:"
WRITE (20,'(a)')  "1) CESI (city-delta)"
WRITE (20,'(a)')  "z    10   33   63  105  180  315  525  825 1350 2250 3350"
WRITE (20,'(a)')  "wt 0.50 0.45 0.38 0.31 0.21 0.11 0.04 0.01 0.00 0.00 0.00"
WRITE (20,*)
WRITE (20,'(a)')  "2) Livelli SMR, formula CESI"
WRITE (20,'(a)')  "z    10   40   90  160  250  400  625  875 1500 2500"
WRITE (20,'(a)')  "wt 0.50 0.43 0.34 0.20 0.16 0.08 0.03 0.01 0.00 0.00"


CLOSE(20)
RETURN

END SUBROUTINE scrive_esempio
