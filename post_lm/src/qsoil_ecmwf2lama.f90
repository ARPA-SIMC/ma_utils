PROGRAM qsoil_ecmwf2lama
!-------------------------------------------------------------------------------
! Programma per interpolare l'umidita del terreno ECMWF in modo che possa essere
! importata nell'archivio LAMA.
! Operazioni compiute:
! - ribaltamento griglia ECMWF
! - interpolazione spaziale su griglia LAMA
! - interplazione temporale (da 6h a 1h)
! - normalizzazione
!
! NOTE:
! Programma molto specifico per input ogni 6 ore senza buchi, potrebbe essere
! generalizzato in modo da gestire interp. temporale di dati a intervalli 
! variabili.
!
!                                                Versione 1.0, Enrico 07/05/2007
!-------------------------------------------------------------------------------

USE grid_handler
USE date_handler
 
IMPLICIT NONE 

! Parametri costanti
REAL, PARAMETER :: fnorm = 100.        ! fattore moltiplicativo ECM -> LAMA

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim),klen,kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

INTEGER :: ksec1_lama(1024),ksec2_lama(1024),ksec3_lama(2),ksec4_lama(512)
INTEGER :: ksec1_out(1024)
REAL    :: psec2_lama(512),psec3_lama(2)
REAL    :: field_t1(maxdim),field_t2(maxdim),field_out(maxdim)

! Altre variabili
TYPE(date) :: data1,data2,datac
TYPE(grid) :: qs_in,qs_out,qs_lama
INTEGER :: iuin,iuout,iulama,kg,hh1,hh2,hhc,hht,cntw,np_lama,hdt
CHARACTER (LEN=80) :: filein,fileout,filelama

!-------------------------------------------------------------------------------
! 0) Preliminari

! Parametri da riga comandi
CALL getarg(1,filein)
CALL getarg(2,fileout)
CALL getarg(3,filelama)

IF (TRIM(filein)=='-h' .OR. TRIM(filein)=="" .OR.TRIM(fileout)=="" .OR. &
    TRIM(filelama)=="") THEN
  WRITE (*,*) "Uso: qsoil_ecmwf2lama.exe filein fileout filelama"
  STOP
ENDIF  

! Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

!--------------------------------------------------------------------------
! 1) Leggo grib di esempio (per avere sez. 1 e 2 coerenti con l'archivio)

CALL PBOPEN (iulama,filelama,'R',kret)
IF (kret.ne.0) GOTO 9999
CALL PBGRIB(iulama,kbuffer,maxdim*4,klen,kret)
CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
             field,maxdim,kbuffer,maxdim,klen,'D',kret)

ksec1_lama(:) = ksec1(:)
ksec2_lama(:) = ksec2(:)
ksec3_lama(:) = ksec3(:)
ksec4_lama(:) = ksec4(:)
psec2_lama(:) = psec2(:)
psec3_lama(:) = psec3(:)

! Costruisco la variabile di tipo "grigliato" di esempio
CALL build_grid(ksec2_lama,field,qs_lama)
np_lama = qs_lama%nx * qs_lama%ny

CALL PBCLOSE (iulama,kret)

!--------------------------------------------------------------------------
! 2) Leggo e interopolo (ciclo prinicipale)

CALL PBOPEN (iuin,filein,'R',kret)
IF (kret.ne.0) GOTO 9998
CALL PBOPEN (iuout,fileout,'W',kret)

cntw = 0
DO kg = 1,HUGE(0)
  
! 2.1 Leggo un grib 
  CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)
  IF (kret == -1) THEN
    EXIT
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! 2.2 data e scadenza del GRIB letto. 
  data2%yy = ksec1(10) + 100*(ksec1(21)-1)
  data2%mm = ksec1(11)    
  data2%dd = ksec1(12)    
  hh2 = ksec1(13)

! Verifico che sia 6 ore dopo l'ora precedente
  IF (kg > 1) THEN
    hdt = (data2 - data1) * 24 + hh2 - hh1
    IF (hdt /= 6) GOTO 9997
  ENDIF

! Verifico scadenza analisi
  IF (ANY(ksec1(15:18)/=(/1,0,0,0/))) GOTO 9996

! 2.3 Interpolo su griglia LAMA
  CALL build_grid(ksec2,field,qs_in)
  qs_out = qs_lama
  CALL interp_scalar(qs_in,qs_out,"L")
  field_t2(1:np_lama) = qs_out%field(1:np_lama)

! 2.4 Se non e' il 1o istante, interpolo nel tempo i campi orari relativi 
!     alle 6 ore precedenti e li scrivo
  IF (kg > 1) THEN
    DO hdt = 1,5
      hht = hh1 + hdt
      hhc = MOD(hht,24)
      datac = data1 + hht/24

      ksec1_out(:) = ksec1_lama(:)
      ksec1_out(10) = MOD((datac%yy-1),100) + 1
      ksec1_out(11) = datac%mm
      ksec1_out(12) = datac%dd
      ksec1_out(13) = hhc
      ksec1_out(21) = (datac%yy-1)/100 + 1
      field_out(1:np_lama) = fnorm * &
        (REAL(6-hdt)/6. * field_t1(1:np_lama) + &
         REAL(hdt)/6. * field_t2(1:np_lama))
    
      CALL GRIBEX (ksec0,ksec1_out,ksec2_lama,psec2_lama,ksec3_lama, &
        psec3_lama,ksec4_lama,field_out,maxdim,kbuffer,maxdim,klen,'C',kret)
      IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
      
      CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
      IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
      cntw = cntw + 1

    ENDDO
  ENDIF

! 2.5 Scrivo il campo all'istante corrente
  ksec1_out(:) = ksec1_lama(:)
  ksec1_out(10) = MOD((data2%yy-1),100) + 1
  ksec1_out(11) = data2%mm
  ksec1_out(12) = data2%dd
  ksec1_out(13) = hh2
  ksec1_out(21) = (data2%yy-1)/100 + 1
  field_out(1:np_lama) = field_t2(1:np_lama) * fnorm

  CALL GRIBEX (ksec0,ksec1_out,ksec2_lama,psec2_lama,ksec3_lama, &
    psec3_lama,ksec4_lama,field_out,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
  cntw = cntw + 1

! 2.6 Salvo i dati per interpolazione istanti successivi
  data1 = data2
  hh1 = hh2
  field_t1(1:np_lama) = field_t2(1:np_lama)

ENDDO

!--------------------------------------------------------------------------
! 3) Conclusione

CALL PBCLOSE (iuin,kret)
CALL PBCLOSE (iuout,kret)

WRITE (*,*) "Input file terminato; grib letti, scritti: ",kg-1,cntw

STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filelama)
STOP

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9997 CONTINUE
WRITE (*,*) "Errore sequenza date ECMWF, dt /= 6 ore", hdt
WRITE (*,*) "datah corrente:   ",data1,hh1
WRITE (*,*) "datah precedente: ",data2,hh2
STOP

9996 CONTINUE
WRITE (*,*) "Scadenza illegale in input",ksec1(15:18),"al grib",kg
STOP

END PROGRAM qsoil_ecmwf2lama
