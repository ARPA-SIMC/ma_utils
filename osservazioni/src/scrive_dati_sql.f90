!________________________________________________________________________
!
! scrive.dati.sql.f90
!
! Programma per l'estrazione dei dati dal DB di QA (scrive la query)
! Appartiene alla catena estra_qaria
!
! 27/03/2006 bug fixed: forzato a pescare "quasi mai" dalla tabella di
!                       emergenza
! 18/10/2006 bug fixed: evita di scrivere un record troppo lungo
!                       (= incompatibile con Oracle) nella lista dei 
!                       sensori da estrarre
! 17/11/2006 bug fixed: scrittura di n stazioni, se n multiplo di 10
! 31/08/2007 non pesca piu' dalla tabella di emergenza perche' non e'
!            piu' necessario; DATA_DB come campo di selezione (piu'
!            veloce di DATA_F)
! 22/03/2010 riattivata la tabella dei dati recenti, con nome QA_DATI_ALTFLUX
!            in modalita' aut-aut (pesca solo da una tabella, default QA_DATI)
! 31/01/2013 QA_DATI_ALTFLUX non esiste piu' nel nuovo DB, ma questo non cambia
!            la struttura del codice
! 31/01/2013 gestisce le condizioni sulla data nella query in maniera
!            indipendente dalla lingua del DB
!
!                                                 V2.1, Johnny 31/01/2013
!________________________________________________________________________
!
PROGRAM sds
  !--------------------------------------------------------------------------
  ! 0) Parametri, dichiarazioni...

  USE date_handler
  IMPLICIT NONE

  ! 0.1) Parametri

  ! 0.2) Dichiarazioni
  INTEGER :: i,j,k,l,n,nconfsens,nskip, &
       opt_stat
  INTEGER, ALLOCATABLE :: id_confsens(:)
  CHARACTER :: ch4*4,ch8*8,ch10*10,inq*8,tab*20
  CHARACTER(LEN=100) :: fmt,line
  TYPE(DATE) :: date_start,date_end

  ! 0.3) Inizializzazioni

  !--------------------------------------------------------------------------
  ! 1) Lettura opzioni
  OPEN(11,file="estra_qaria.inp",err=901)
  READ(11,'(i4,2i2)',err=902)date_start%yy,date_start%mm,date_start%dd
  READ(11,'(i4,2i2)',err=902)date_end%yy,date_end%mm,date_end%dd
  date_end=date_end+3
  date_start=date_start-3
  READ(11,'(a)',err=902)inq
  READ(11,*,err=902)n
  DO k=1,n
     READ(11,*,err=902)
  END DO
  READ(11,*,err=902)n
  DO k=1,n
     READ(11,*,err=902)
  END DO
  READ(11,*,err=902)j
  READ(11,*,err=902)opt_stat

  !--------------------------------------------------------------------------
  ! 2) Lettura della lista di codici di configurazione dei sensori
  !    (ID_CONFIG_SENSORE)
  OPEN(13,file="stzqa.tmp",err=905)
  DO k=1,huge(k)
     READ(13,'(a)',err=906)ch4
     IF(ch4=="----")THEN
        nskip=k
        EXIT
     END IF
  END DO
  DO k=1,huge(k)
     READ(13,'(a)',err=906)line
     IF(trim(line)=="")THEN
        nconfsens=k-1
        EXIT
     END IF
  END DO
  CLOSE(13)
  OPEN(13,file="stzqa.tmp",err=905)
  ALLOCATE(id_confsens(nconfsens))
  DO k=1,nskip
     READ(13,*)
  END DO
  DO k=1,nconfsens
     READ(13,'(a)')line
     READ(line,*)id_confsens(k)
  END DO


  !--------------------------------------------------------------------------
  ! 3) Scrittura query

  ! 3.1) selezione tabella dati
  CALL getarg(1,tab)
  IF(tab=="") tab="QA_DATI"

  ! 3.2) scrittura query

  OPEN(12,file="dati.sql",err=903)
  WRITE(12,'(a)',err=904) "set linesize 100"
  WRITE(12,'(a)',err=904) "set pagesize 0"
  WRITE(12,'(a)',err=904) "set null -999.0"
  WRITE(12,'(a)',err=904) "col F1 format 99 null 0"
  WRITE(12,'(a)',err=904) "col F2 format 99 null 0"
  WRITE(12,'(a)',err=904) "col F3 format 99 null 0"
  WRITE(12,'(a)',err=904) "col F4 format 99 null 0"
  WRITE(12,'(a)',err=904) "col F5 format 99 null 0"
  WRITE(12,'(a)',err=904) "col F6 format 99 null 0"
  WRITE(12,'(a)',err=904) "col F7 format 99 null 0"
  WRITE(12,'(a)',err=904) "col F8 format 99 null 0"
  WRITE(12,'(a)',err=904) "col F9 format 99 null 0"
  WRITE(12,'(a)',err=904) "spool dati.tmp;"
  WRITE(12,'(a)',err=904) "select ID_CONFIG_SENSORE,"
  WRITE(12,'(a)',err=904) "       to_char(DATA_I,'yyyymmddhh24') as DATA_I, " 
  WRITE(12,'(a)',err=904) "       to_char(DATA_F,'yyyymmddhh24') as DATA_F, " 
  WRITE(12,'(a)',err=904) "       VAL_VIS,                                  " 
  WRITE(12,'(a)',err=904) "       F1,F2,F3,F4,F5,F6,F7,F8,F9                " 
  WRITE(12,'(2a)',err=904)"from ",TRIM(tab)

  IF(nconfsens>11)THEN
      IF(MOD(nconfsens-1,10)==0)THEN
         WRITE(fmt,'(a,i15,a)')"(a,/,",(nconfsens-1)/10,"(10(i15,','),/),i15,a)"
      ELSE
         WRITE(fmt,'(a,i15,a,i15,a)')"(a,/,",(nconfsens-1)/10,"(10(i15,','),/),",MOD(nconfsens-1,10),"(i15,','),i15,a)"
      END IF
  ELSE IF(nconfsens>1)THEN
     WRITE(fmt,'(a,i15,a)')"(a,/,",nconfsens-1,"(i15,','),i15,a)"
  ELSE
     fmt="(a,i15,a)"
  END IF
  WRITE(12,fmt,err=904)   "where ID_CONFIG_SENSORE in(",(id_confsens(k),k=1,nconfsens),")"
  WRITE(12,'(a,i4.4,i2.2,i2.2,a,i4.4,i2.2,i2.2,a)',err=904)"and to_char(DATA_DB,'yyyymmdd') between '", &
   date_start%yy,date_start%mm,date_start%dd, &
   "' and '",date_end%yy,date_end%mm,date_end%dd,"';"
  WRITE(12,'(a)',err=904) "spool off;"

  !--------------------------------------------------------------------------
  ! 4) Gestione errori
  STOP
901 print *,"Errore aprendo estra_qaria.inp"
  STOP
902 print *,"Errore leggendo estra_qaria.inp"
  STOP
903 print *,"Errore aprendo dati.sql"
  STOP
904 print *,"Errore scrivendo dati.sql"
  STOP
905 print *,"Errore aprendo anagr_stzqa.tmp"
  STOP                        
906 print *,"Errore leggendo anagr_stzqa.tmp"
  STOP
END PROGRAM sds


