PROGRAM calc_ipgt_inv
!--------------------------------------------------------------------------
! Programma che stima la classe di stabilita' a partire dagli output 
! LM/LAMA
! Per le ore diurne, usa Bowen 1983 (vento e radiazione solare)
! Per le ore notturne, usa Reuter 1970 modificato (vento e radiazione IR)
!
! Note:
! Legge e scrive da files grib diversi per ciascun parametro.
! Tutti i grib devono essere definiti sulla stessa area.
! Data e scadenza nei files di input devono corrispondere; i dati di 
!   radiazione visibile e IR devono riferirsi alla media sull'ora 
!   precedente (in analogia con la logica seguita da Calmet)
!
!                                           Versione 2.1, Enrico 15/09/2007
!--------------------------------------------------------------------------

IMPLICIT NONE

INTEGER, PARAMETER :: klog = 11288  ! punto per log (S.P.Cap., area LAMAZ)
REAL, PARAMETER :: rmis = -1.e20    ! valore per dati mancanti

! Dichiarazioni per GRIBEX.
INTEGER, PARAMETER :: maxdim = 300000  ! dimensione massima dei GRIB
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: ksec1_sav(1024),ksec2_sav(1024)
INTEGER :: kbuffer(maxdim), klen, kret
REAL :: psec2(512),psec3(2)
REAL :: field(maxdim)

! Nomi files input/output
CHARACTER (LEN=13), PARAMETER :: file_in(5) = (/ &
  "10_u.grb     ","10_v.grb     ", & 
  "srf_sosbs.grb","srf_thsbs.grb", &
  "srf_alb.grb  "/)
CHARACTER (LEN=80), PARAMETER :: file_out = "srf_ipgt.grb"

! Tabella per calcolo IPGT diurna (econdo Bowen 1983)
INTEGER, PARAMETER :: ncrd = 4
INTEGER, PARAMETER :: ncfd = 5
REAL, PARAMETER :: class_rad_day(0:ncrd) = &
  (/HUGE(0.),925.,675.,175.,-HUGE(0.)/)
REAL, PARAMETER :: class_ff_day(0:ncfd) = &
  (/-HUGE(0.),2.,3.,5.,6.,HUGE(0.)/)
INTEGER, PARAMETER :: ipgt_day(ncrd,ncfd) = RESHAPE((/ &
  1,1,2,4, &
  1,2,3,4, &
  2,2,3,4, &
  3,3,4,4, &
  3,4,4,4  &
  /),(/ncrd,ncfd/))  

! Tabella per calcolo IPGT notturna (secondo Reuter 1970)
INTEGER, PARAMETER :: ncrn = 3
INTEGER, PARAMETER :: ncfn = 6
REAL, PARAMETER :: class_rad_nig(0:ncrn) = &
  (/-HUGE(0.),-70.,-6.5,HUGE(0.)/)
REAL, PARAMETER :: class_ff_nig(0:ncfn) = &
  (/0.,2.,3.,4.,5.,7.,HUGE(0.)/)
INTEGER, PARAMETER :: ipgt_nig(ncrn,ncfn) = RESHAPE((/ &
  6,6,4, &
  6,5,4, &
  5,4,4, &
  5,4,4, &
  5,4,4, &
  4,4,4  &
  /),(/ncrn,ncfn/))  

! Altre variabili del programma
REAL, ALLOCATABLE :: par_in(:,:),sw_down(:),ff(:)
INTEGER, ALLOCATABLE :: ipgt(:),cl_rad(:),cl_ff(:)
LOGICAL, ALLOCATABLE :: day_time(:)

REAL :: tlog(50)
INTEGER :: np,nlevs,iu_in(5),iu_out
INTEGER :: kfin,kp,k,kc,kl,kist,ios
CHARACTER (LEN=80) :: chdum,next_arg
CHARACTER (LEN=1) :: inp

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
CALL getarg(1,chdum)
IF (chdum /= "") THEN
  CALL write_help
  STOP
ENDIF

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
DO kfin = 1,5
  CALL PBOPEN (iu_in(kfin),file_in(kfin),'R',kret)
  IF (kret /= 0) GOTO 9999
ENDDO

CALL PBOPEN (iu_out,file_out,'W',kret)
OPEN (UNIT = 95, FILE="calc_ipgt.log", STATUS="REPLACE", FORM="FORMATTED")

!--------------------------------------------------------------------------
! 2) Lettura - Scrittura (cicli su: istanti, files di input, livelli in input)

ist: DO kist = 1,HUGE(kist)

! Se non e' il primo istante, resetto le variabili di calcolo
  IF (kist > 1) THEN
    ipgt(:) = 0
    par_in(:,:) = rmis
    ff(:) = rmis
    sw_down(:) = rmis
  ENDIF

  DO kfin = 1,5

!--------------------------------------------------------------------------
! 2.1) Lettura e controlli

! 2.1.1 Leggo e decodifico i grib di input
    CALL PBGRIB(iu_in(kfin),kbuffer,maxdim*4,klen,kret)
    IF (kret == -1 .AND. kfin == 1) THEN 
      EXIT ist
    ELSE IF (kret == -1) THEN
      GOTO 9998
    ELSE IF (kret < -1) THEN
      WRITE(*,*) "Error pbgrib: kret ",kret
      STOP
    ENDIF
 
    psec3(2) = rmis                           ! forzo dati mancanti = rmis
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'D',kret)
    IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! 2.1.2 Se e' il primo grib: alloco e inizializzo le variabili, salvo sezione 2,
!       capisco se sto elaborando analisi (LAMA) o previsioni (LM)
    IF (kfin == 1 .AND. kist == 1) THEN
      ksec2_sav(:) = ksec2(:)
      np = ksec4(1)
 
      ALLOCATE (par_in(5,np),ipgt(np))
      ALLOCATE (ff(np),cl_rad(1:np),cl_ff(1:np))
      ALLOCATE (sw_down(np),day_time(np))
 
      par_in(:,:) = rmis
      ipgt(:) = 0
      ff(:) = rmis
      sw_down(:) = rmis

      IF (ksec1(18) == 0) THEN
        WRITE (*,*) "Elaboro previsioni LM"
        inp = "F"
      ELSE IF (ksec1(18) == 13) THEN
        WRITE (*,*) "Elaboro analisi LAMA"
        inp = "A"
      ELSE
        GOTO 9996
      ENDIF
 
    ENDIF
   
! 2.1.3 Controlli su data, scadenza, livello e area
!   IF (ANY(ksec2(:) /= ksec2_sav(:)) .OR. ksec4(1) /= np) GOTO 9997
    IF (ANY(ksec2((/1,2,3,4,5,7,8,11,13,14/)) /= &
            ksec2_sav((/1,2,3,4,5,7,8,11,13,14/))) .OR. &
        ksec4(1) /= np) GOTO 9997
 
    SELECT CASE (kfin)
    CASE (1)                                         ! U
      ksec1_sav(:) = ksec1(:)
      IF (inp == "F" .AND. ANY(ksec1(15:18)/=(/1,ksec1(16),0,0/))) GOTO 9995
      IF (inp == "A" .AND. ANY(ksec1(15:18)/=(/1,0,0,13/))) GOTO 9995

    CASE(2)                                          ! V
      IF (ANY(ksec1(7:18) /= ksec1_sav(7:18)) .OR. &
        ksec1(21) /= ksec1_sav(21)) GOTO 9994

    CASE(3,4)                                        ! Radiazioni
      IF (ANY(ksec1(7:9) /= (/1,0,0/))) GOTO 9994
      IF (inp == "A" .AND. &
          (ANY(ksec1(10:16)/=ksec1_sav(10:16)) .OR. ksec1(17)/=1 .OR. &
           ksec1(18)/=ksec1_sav(18) .OR. ksec1(21)/=ksec1_sav(21)) ) GOTO 9994
      IF (inp == "F" .AND. &
          (ANY(ksec1(10:15)/=ksec1_sav(10:15)) .OR. &
           (ksec1(16)/=ksec1_sav(16)-1 .AND. ksec1(16)/=(ksec1_sav(16)+255)) .OR. &
           ksec1(17)/=ksec1_sav(16) .OR. &
           ksec1(18)/=3 .OR. ksec1(21)/=ksec1_sav(21)) ) GOTO 9994

    CASE(5)                                          ! Albedo
      IF (ANY(ksec1(7:9) /= (/1,0,0/))) GOTO 9994
      IF (ANY(ksec1(10:18) /= ksec1_sav(10:18)) .OR. &
        ksec1(21) /= ksec1_sav(21)) GOTO 9994

    END SELECT
   
! 2.1.4 Controlli sul parametro
    SELECT CASE (kfin)
    CASE(1)                                          ! U
      IF (ksec1(1)/=2 .OR. ksec1(6)/=33) GOTO 9993
    CASE(2)                                          ! V
      IF (ksec1(1)/=2 .OR. ksec1(6)/=34) GOTO 9993
    CASE(3)                                          ! Rad. Vis.
      IF (ksec1(1)/=2 .OR. ksec1(6)/=111) GOTO 9993
    CASE(4)                                          ! Rad. IR
      IF (ksec1(1)/=2 .OR. ksec1(6)/=112) GOTO 9993
    CASE(5)                                          ! Albedo
      IF (ksec1(1)/=2 .OR. ksec1(6)/=84) GOTO 9993
    END SELECT      
   
! 2.1.5 Salvo il campo in par_in
    par_in(kfin,1:np) = field(1:np)

  ENDDO                                         ! files di input

! 2.1.6  Scrivo log
  WRITE (95,*)
  WRITE (95,'(a,5i3,a,4i3)') "Elaboro istante ",ksec1_sav(10:14), &
    " scad ",ksec1_sav(15:18)
  WRITE (95,'(a,5(2x,f8.3))') "Input: U,V,SWB,LWB,ALB: ",par_in(1:5,klog)

! 2.1.7 Se ci sono dati mancanti, passo all'istante successivo
  IF (ANY(par_in(1:5,1:np) == rmis)) THEN
    WRITE (*,'(a,5i3,a,4i3,a)') "Istante: ",ksec1_sav(10:14), &
      " scad ",ksec1_sav(15:18),"   skippo per dati mancanti"
    WRITE (95,*) "Ci sono dati mancanti, passo all'istante successivo"
    CYCLE
  ENDIF

!--------------------------------------------------------------------------
! 2.2) Calcoli

! 2.2.1 Calcolo velocita' vento
  ff(1:np) = SQRT(par_in(1,1:np)*par_in(1,1:np)+par_in(2,1:np)*par_in(2,1:np))

! 2.2.2 Calcolo SW_down
  WHERE (par_in(5,1:np) >= 0. .AND. par_in(5,1:np) < 100.)
    sw_down(1:np) = par_in(3,1:np) / (1. - 0.01 * par_in(5,1:np))
  ELSEWHERE
    sw_down(1:np) = 0.
  ENDWHERE

! 2.2.3 Calcolo ore diurne
  WHERE (sw_down(1:np) > 5.)
    day_time(1:np) = .TRUE.
  ELSEWHERE
    day_time(1:np) = .FALSE.
  ENDWHERE

! 2.2.4 Calcolo classe di vento (diurna e notturna)
  cl_ff(:) = 0
  DO kc = 1,ncfd
    WHERE (ff(1:np) > class_ff_day(kc-1) .AND. &
           ff(1:np) <= class_ff_day(kc) .AND. day_time(1:np))
      cl_ff(1:np) = kc
    ENDWHERE
  ENDDO
  DO kc = 1,ncfn
    WHERE (ff(1:np) > class_ff_nig(kc-1) .AND. &
           ff(1:np) <= class_ff_nig(kc) .AND. .NOT.day_time(1:np))
      cl_ff(1:np) = kc
    ENDWHERE
  ENDDO

! 2.2.5 Calcolo classe di radiazione (diurna e notturna)
  cl_rad(:) = 0
  DO kc = 1,ncrd
    WHERE (sw_down(1:np) >= class_rad_day(kc) .AND. &
           sw_down(1:np) < class_rad_day(kc-1) .AND. day_time(1:np))
      cl_rad(1:np) = kc
    ENDWHERE
  ENDDO
  DO kc = 1,ncrn
    WHERE (par_in(4,1:np) > class_rad_nig(kc-1) .AND. &
           par_in(4,1:np) <= class_rad_nig(kc) .AND. .NOT.day_time(1:np))
      cl_rad(1:np) = kc
    ENDWHERE
  ENDDO

! 2.2.6 Calcolo classe di stabilita' diurna (Bowen 1983)
  DO k = 1,np
    IF (day_time(k)) THEN
      ipgt(k) = ipgt_day(cl_rad(k),cl_ff(k))
    ELSE
      ipgt(k) = ipgt_nig(cl_rad(k),cl_ff(k))
    ENDIF
  ENDDO

! 2.2.7 Log e controlli su calcolo IPGT
  WRITE (95,'(a,2(2x,f8.3),2x,l2,2x,3i2,2x,3i2)') &
    "Derivati: ff,sw_down,day,cl_rad,cl_ff ", &
    ff(klog),sw_down(klog),day_time(klog),cl_rad(klog),cl_ff(klog)
  WRITE (95,'(a,i3)') "Output IPGT: ",ipgt(klog)

  IF (ANY(cl_rad(:)==0) .OR. ANY(cl_ff(:)==0) .OR. ANY(ipgt(:)==0)) GOTO 9992

!--------------------------------------------------------------------------
! 2.3) Scrittura output

  ksec1(1) = 2                                      ! tabella
  ksec1(6) = 202                                    ! parametro
  ksec1(7:9) = (/1,0,0/)                            ! livello
  ksec1(10:18) = ksec1_sav(10:18)                   ! data e scadenza
  ksec2(:) = ksec2_sav(:)                           ! griglia
  ksec4(2) = 10                                     ! nbit

  field(1:np) = ipgt(1:np)
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iu_out,kbuffer,ksec0(1),kret)

  WRITE (*,'(a,5i3,a,4i3,a)') "Istante: ",ksec1_sav(10:14), &
    " scad ",ksec1_sav(15:18),"   elaborato"

ENDDO ist
STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE(*,*) "Errore aprendo ",TRIM(file_in(kfin))," kret ",kret
STOP

9998 CONTINUE
WRITE(*,*) "Il file ",TRIM(file_in(kfin))," contiene meno istanti"
STOP

9997 CONTINUE
WRITE(*,*) "Area diversa in ",TRIM(file_in(kfin))," istante ",kist
IF (ksec4(1) /= np) WRITE (*,'(a,i10,a,i10)') &
  "ksec4(1) attesto ",np," trovato ",ksec4(1)
DO k = 1,1024
  IF (ksec2_sav(k) /= ksec2(k)) WRITE (*,'(a,i4,2(a,i10))') &
    "ksec2(",k,") attesto ",ksec2_sav(k)," trovato ",ksec2(k)
ENDDO
STOP

9996 CONTINUE
WRITE(*,'(2a,4i4)') "Scadenza non gestita in ",TRIM(file_in(kfin)), &
  ksec1(15:18)
STOP

9995 CONTINUE
WRITE(*,'(2a,4i4,a,i6)') "Scadenza inattesa in ",TRIM(file_in(kfin)), &
  ksec1(15:18)," istante ",kist
STOP

9994 CONTINUE
WRITE(*,'(3a,i5)') "Livello/data/scadenza disallineata in ", &
  TRIM(file_in(kfin))," istante ",kist
WRITE(*,'(a,3i4,4x,6i3,4x,4i3,2x,2a)') "Primo campo: ", &
  ksec1_sav(7:9),ksec1_sav(10:14),ksec1_sav(21),ksec1_sav(15:18),&
  " file: ",TRIM(file_in(1))
WRITE(*,'(a,3i4,4x,6i3,4x,4i3,2x,2a)') "Trovato:     ", &
  ksec1(7:9),ksec1(10:14),ksec1(21),ksec1(15:18), &
  " file: ",TRIM(file_in(kfin))
IF (kfin == 6) WRITE (*,'(3a,i3,a)') "*** Verificare che il file ", &
  TRIM(file_in(kfin))," contenga esattamente ",nlevs," livelli ***"
STOP

9993 CONTINUE
WRITE(*,'(3a,3i4)') "Parametro errato in ",TRIM(file_in(kfin)), &
  " file,tab,var ",kfin,ksec1(1),ksec1(6)
STOP

9992 CONTINUE
WRITE (*,*) "Errore attribuzione classi, istante ",kist
DO k = 1,np
  IF (cl_rad(k)==0 .OR. cl_ff(k)==0 .OR. ipgt(k)==0) &
    WRITE (96,'(a,i6,a,l2,2x,3i5)') "Punto ",k," day,cl_rad,cl_ff,ipgt ", &
    day_time(k),cl_rad(k),cl_ff(k),ipgt(k)
ENDDO

STOP

END PROGRAM calc_ipgt_inv

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: calc_ipgt_inv.exe [-h]"
WRITE (*,*) "Calcola la classe di stabilita' a prtire dai dati LM"
WRITE (*,*) "Scrive il file srf_ipgt.grb"
WRITE (*,*)
WRITE (*,*) "Nomi files di input:"
WRITE (*,*) "1) vento u:       10_u.grb      (istantaneo)"
WRITE (*,*) "2) vento v:       10_v.grb      (istantaneo)"
WRITE (*,*) "3) rad.visibile:  srf_sosbs.grb (media ora precedente)"
WRITE (*,*) "4) rad.IR:        srf_thsbs.grb (media ora precedente)"
WRITE (*,*) "5) albedo:        srf_alb.grb   (istantaneo)"
WRITE (*,*)
RETURN

END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
