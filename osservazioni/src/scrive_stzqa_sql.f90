!________________________________________________________________________
!
! scrive.stzqa.sql.f90
!
! Programma che scrive la query per fare un'anagrafica sintetica e
! temporanea, di appoggio per la catena estra_qaria
!
! 31/10/2006 bug fixed: elimina bachi nella scrittura della query
!
!                                                   V1.5, Johnny 31/10/2006
!________________________________________________________________________
!
PROGRAM sstzqa
!--------------------------------------------------------------------------
! 0) Parametri, dichiarazioni...

USE date_handler
IMPLICIT NONE

! 0.1) Parametri
INTEGER, PARAMETER :: nparam=9

! 0.2) Dichiarazioni
INTEGER :: id_param(nparam),kparam,k,nstaz,nprov
INTEGER, ALLOCATABLE :: id_staz(:)
CHARACTER(LEN=5) :: nome_param(nparam),ch5
CHARACTER(LEN=100) :: fmt1,fmt2
CHARACTER(LEN=2), ALLOCATABLE :: id_prov(:)
TYPE(DATE) :: date_start,date_end

! 0.3) Inizializzazioni
DATA id_param  /      7,      5,      8,     38,      9,      1,     10,     20,    111/, &
     nome_param/"O3   ","PM10 ","NO2  ","NO   ","NOX  ","SO2  ","CO   ","BZ   ","PM2.5"/

!--------------------------------------------------------------------------
! 1) Lettura opzioni
OPEN(11,file="estra_qaria.inp",err=901)
READ(11,'(i4,2i2)',err=902)date_start%yy,date_start%mm,date_start%dd
READ(11,'(i4,2i2)',err=902)date_end%yy,date_end%mm,date_end%dd
READ(11,'(a)',err=902)ch5

DO k=1,nparam
   IF(TRIM(ch5)==TRIM(nome_param(k)))THEN
      kparam=k
      EXIT
   END IF
END DO

READ(11,*,err=902)nstaz
IF(nstaz>0)THEN
   ALLOCATE(id_staz(nstaz))
   DO k=1,nstaz
      READ(11,*,err=902)id_staz(k)
   END DO
END IF

READ(11,*,err=902)nprov
IF(nprov>0)THEN
   ALLOCATE(id_prov(nprov))
   DO k=1,nprov
      READ(11,'(a)',err=902)id_prov(k)
   END DO
END IF

!--------------------------------------------------------------------------
! 2) Scrittura query
OPEN(12,file="stzqa.sql",err=903)
WRITE(12,'(a)',err=904)   "set lines 400"
WRITE(12,'(a)',err=904)   "set pages 10000"
WRITE(12,'(a)',err=904)   "col ID_CONFIG_SENSORE heading ""IDCNFSNS"" null -99   format 99999999"
WRITE(12,'(a)',err=904)   "col ID_STAZIONE       heading ""IDSTAZ""   null -99   format 99999999"
WRITE(12,'(a)',err=904)   "col ID_PARAMETRO      heading ""PAR""      null -99   format 999"
WRITE(12,'(a)',err=904)   "col PARAMETRO         heading ""NOMEPAR""  null ??    format A50"
WRITE(12,'(a)',err=904)   "col UM_VIS_DATI       heading ""UMIS""     null ??    format A10"
WRITE(12,'(a)',err=904)   "col TEMPO_MEDIAZIONE  heading ""INTV""     null -99   format 9999"
WRITE(12,'(a)',err=904)   "col PROVINCIA         heading ""PR""       null ??    format A2"
WRITE(12,'(a)',err=904)   "col COMUNE            heading ""COMUNE""   null ??    format A50"
WRITE(12,'(a)',err=904)   "col NOME_STAZIONE     heading ""NOMESTAZ"" null ??    format A50"
WRITE(12,'(a)',err=904)   "col LON               heading ""LON""      null -99   format 99.99999"
WRITE(12,'(a)',err=904)   "col LAT               heading ""LAT""      null -99   format 99.99999"
WRITE(12,'(a)',err=904)   "col ALT               heading ""ALT""      null -9999 format 9999"
WRITE(12,'(a)',err=904)   "spool stzqa.tmp;"
WRITE(12,'(a)',err=904)   "select "
WRITE(12,'(a)',err=904)   "ID_CONFIG_SENSORE,"
WRITE(12,'(a)',err=904)   "ID_STAZIONE,"
WRITE(12,'(a)',err=904)   "ID_PARAMETRO,"
WRITE(12,'(a)',err=904)   "PARAMETRO,"
WRITE(12,'(a)',err=904)   "UM_VIS_DATI,"
WRITE(12,'(a)',err=904)   "TEMPO_MEDIAZIONE,"
WRITE(12,'(a)',err=904)   "PROVINCIA,"
WRITE(12,'(a)',err=904)   "COMUNE,"
WRITE(12,'(a)',err=904)   "NOME_STAZIONE,"
WRITE(12,'(a)',err=904)   "LON,"
WRITE(12,'(a)',err=904)   "LAT,"
WRITE(12,'(a)',err=904)   "ALT"
WRITE(12,'(a)',err=904)   "from ANG_CONFIG_SENSORI"
WRITE(12,'(a,i12)',err=904) "where ID_PARAMETRO =",id_param(kparam)
IF(nstaz>0)THEN
   IF(nstaz>11)THEN
      IF(MOD(nstaz-1,10)==0)THEN
         WRITE(fmt1,'(a,i5,a)')"(a,/,",(nstaz-1)/10,"(10(i12,','),/),i12,a)"
      ELSE
         WRITE(fmt1,'(a,i5,a,i5,a)')"(a,/,",(nstaz-1)/10,"(10(i12,','),/),",MOD(nstaz-1,10),"(i12,','),i12,a)"
      END IF
   ELSE IF(nstaz>1)THEN
      WRITE(fmt1,'(a,i5,a)')"(a,/,",nstaz-1,"(i12,','),i12,a)"
   ELSE
      fmt1="(a,i12,a)"
   END IF
   WRITE(12,fmt1,err=904)  " and ID_STAZIONE in(",(id_staz(k),k=1,nstaz),")"
END IF
IF(nprov>0)THEN
   IF(nprov>1)THEN
      WRITE(fmt2,'(a,i5,a)')"(a,",nprov-1,"(a,','),2a)"
   ELSE
      fmt2="(3a)"
   END IF
   WRITE(12,fmt2,err=904)  " and PROVINCIA in(",("'"//id_prov(k)//"'",k=1,nprov),")"
END IF
WRITE(12,'(a)',err=904)   ";"
WRITE(12,'(a)',err=904)   "spool off;"

!--------------------------------------------------------------------------
! 3) Gestione errori
STOP
901 print *,"Errore aprendo estra_qaria.inp"
STOP
902 print *,"Errore leggendo estra_qaria.inp"
STOP
903 print *,"Errore aprendo stzqa.sql"
STOP
904 print *,"Errore scrivendo stzqa.sql"
STOP
END PROGRAM sstzqa


