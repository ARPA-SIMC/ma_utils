!________________________________________________________________________
!
! ordina.dati.tmp.f90
!
! Programma per riorganizzare i dati estratti dal DB di QA
! Appartiene alla catena estra_qaria
!
! 19/10/2006: risolto bug per estrazione dati non invalidati
!
!                                          V1.5, Johnny&Enrico 05/09/2013
!________________________________________________________________________
!
PROGRAM ordina
  !--------------------------------------------------------------------------
  ! 0) Parametri, dichiarazioni...

  USE date_handler
  IMPLICIT NONE

  ! 0.1) Parametri

  INTEGER, PARAMETER :: &
       nparam_lst=9,  &
       nflags_man=5,  &
       nflags_auto=4, &
       nflags=nflags_man+nflags_auto, &
       missflag=0,    &
       nstats=3,      &
       noptval=4,     &
       nunmis=1
  REAL, PARAMETER :: missdata(2)=(/-9999.0,-999.0/)

  ! 0.2) Dichiarazioni

  INTEGER :: param_lst(nparam_lst), &
       opt_unmis,opt_stat,opt_valid, &
       nstaz_req,nprov_req, &
       nstaz_anag,nsens, &
       i,j,k,l, &
       kparam,kstaz,ksens,kday,krow,kflag,irec,  &
       nskip, &
       ndays,hour_db1,hour_db2,  &
       nstep, &
       hstart
  REAL :: opt_percval,buono
  LOGICAL :: new, unknown
  CHARACTER (LEN=4) :: ch4,codparam_gds(nparam_lst),codstat(nstats),intv_gds(nstats)
  CHARACTER (LEN=5) :: ch5,codparam_lst(nparam_lst)
  CHARACTER (LEN=8) :: ch8
  CHARACTER (LEN=10) :: strdate2,strdate1,nomeunmis(nunmis)
  CHARACTER (LEN=20) :: nomestat(nstats),tab
  CHARACTER (LEN=30) :: form,fileout,nomeoptval(0:noptval-1)
  CHARACTER (LEN=100) :: line
       
  INTEGER, ALLOCATABLE :: staz_req(:),prov_req(:), &
       idconfsens(:),staz(:),param(:),intv_db(:),alt(:), &
       flag(:,:,:,:),valid(:,:,:),stazsens(:),sensstaz(:)
  REAL, ALLOCATABLE :: lon(:),lat(:), &
       hmed_db(:,:,:),hmed_out(:,:,:),dmed_db(:,:),dmed_out(:,:),dmax_out(:,:)

  CHARACTER (LEN=2), ALLOCATABLE :: prov(:)
  CHARACTER (LEN=10), ALLOCATABLE :: unmis(:)
  CHARACTER (LEN=50), ALLOCATABLE :: nomeparam(:),comune(:),nomestaz(:)
  TYPE(DATE) :: date_start,date_end,date_db1,date_db2,date_out

  ! 0.3) Inizializzazioni

  DATA param_lst   /      7,      5,      8,     38,      9,      1,     10,     20,    111/, &
       codparam_lst/"O3   ","PM10 ","NO2  ","NO   ","NOX  ","SO2  ","CO   ","BZ   ","PM2.5"/, &
       codparam_gds/"o3  " ,"pm10" ,"no2 " ,"no  " ,"nox " ,"so2 " ,"co  " ,"bz  " ,"pm25" /
  DATA nomestat/"med.orarie","med.giorn.","max.giorn."/, &
       codstat /"hrav"      ,"dyav"      ,"dymx"      /, &
       intv_gds/"1hr"       ,"1dy"       ,"1dy"       /
  DATA nomeunmis/"ug/m3"/
  DATA nomeoptval/"tiene tutto","esclude invalidati","tiene validati","tiene validati da operatore"/

  !--------------------------------------------------------------------------
  ! 1) Lettura files d'appoggio

  ! 1.1) Lettura file opzioni

  OPEN(11,file="estra_qaria.inp",err=901)
  READ(11,'(i4,2i2)',err=902)date_start%yy,date_start%mm,date_start%dd
  READ(11,'(i4,2i2)',err=902)date_end%yy,date_end%mm,date_end%dd
  READ(11,'(a)',err=902)ch5

  unknown = .TRUE.
  DO k=1,nparam_lst
     IF(TRIM(ch5)==TRIM(codparam_lst(k)))THEN
        kparam=k
        unknown = .FALSE.
        EXIT
     END IF
  END DO
  IF(unknown)THEN
     PRINT *, "Non gestisco l'inquinante: ",ch5
     STOP
  END IF

  READ(11,*,err=902)nstaz_req
  IF(nstaz_req>0)THEN
     ALLOCATE(staz_req(nstaz_req))
     DO k=1,nstaz_req
        READ(11,*,err=902)staz_req(k)
     END DO
  END IF

  READ(11,*,err=902)nprov_req
  IF(nprov_req>0)THEN
     ALLOCATE(prov_req(nprov_req))
     DO k=1,nprov_req
        READ(11,'(a)',err=902)prov_req(k)
     END DO
  END IF

  READ(11,*,err=902)opt_unmis
  READ(11,*,err=902)opt_stat
  READ(11,*,err=902)opt_valid
  READ(11,*,err=902)opt_percval

  ! 1.2) Lettura dell'anagrafica temporanea d'appoggio

  OPEN(12,file="stzqa.tmp",err=903)
  DO k=1,huge(k)
     READ(12,'(a)',err=904)ch4
     IF(ch4=="----")THEN
        nskip=k
        EXIT
     END IF
  END DO
  DO k=1,huge(k)
     READ(12,'(a)',err=904)line
     IF(trim(line)=="")THEN
        nsens=k-1
        EXIT
     END IF
  END DO
  REWIND(12)
  ALLOCATE(idconfsens(nsens),staz(nsens),param(nsens), &
       intv_db(nsens),lon(nsens),lat(nsens),alt(nsens),  &
       nomeparam(nsens),unmis(nsens),prov(nsens),&
       comune(nsens),nomestaz(nsens),stazsens(nsens))
  DO k=1,nskip
     READ(12,*)
  END DO
  DO k=1,nsens
     READ(12,'(2(i9,1x),i4,1x,a50,1x,a10,1x,i5,x,a2,1x,2(a50,1x),2(f9.5,1x),i5)') &
          idconfsens(k),staz(k),param(k),nomeparam(k), &
          unmis(k),intv_db(k),prov(k),comune(k),nomestaz(k),&
          lon(k),lat(k),alt(k)
  END DO

  ! 1.3) Controllo della query dati.sql

  OPEN(16,file="dati.sql",err=907)
  tab=""
  DO i=1,HUGE(i)
     READ(16,'(a)'),line
     IF(line(1:4)=="from")THEN
        tab=line(6:)
        EXIT
     END IF
  END DO
  IF(tab=="")THEN
     WRITE(*,*)"non trovo la tabella in dati.sql"
     STOP
  END IF


  !--------------------------------------------------------------------------
  ! 2) Lettura dati

  ! 2.1) Calcola dimensioni vettori, alloca, inizializza

  ndays=date_end-date_start+1
  nstaz_anag=0
  DO k=1,nsens
     new=.TRUE.
     DO l=1,k-1
        IF(staz(k)==staz(l))THEN
           new=.FALSE.
           stazsens(k)=stazsens(l)
           EXIT
        END IF
     END DO
     IF(new)THEN
        nstaz_anag=nstaz_anag+1
        stazsens(k)=nstaz_anag
     END IF
  END DO
  ALLOCATE(hmed_db(nsens,ndays,0:23),dmed_db(nsens,ndays), &
       flag(nsens,ndays,0:23,nflags), &
       hmed_out(nstaz_anag,ndays,0:23), &
       dmed_out(nstaz_anag,ndays),dmax_out(nstaz_anag,ndays))
  hmed_db =missdata(1)
  dmed_db =missdata(1)
  flag    =missflag
  hmed_out=missdata(1)
  dmed_out=missdata(1)
  dmax_out=missdata(1)

  ! (stazsens e sensstaz definiscono le corrispondenze tra stazioni e sensori)
  ALLOCATE(sensstaz(nstaz_anag))
  DO k=1,nstaz_anag
     DO l=1,nsens
        IF(stazsens(l)==k)THEN
           sensstaz(k)=l
           EXIT
        END IF
     END DO
  END DO

  ! 2.2) Lettura file dati

  OPEN(13,file="dati.tmp",err=905)
!!$  DO k=1,huge(k)
!!$     READ(13,'(a)',err=906)ch4
!!$     IF(ch4=="----")EXIT
!!$  END DO
  DO krow=1,huge(krow)
     READ(13,'(a)',err=906)line
     IF(line=="")EXIT

     ! lettura data-ora, riconoscimento sensore
     READ(line,*)i,strdate1,strdate2
     READ(strdate1,'(i4.4,3i2.2)')date_db1%yy,date_db1%mm,date_db1%dd,hour_db1
     READ(strdate2,'(i4.4,3i2.2)')date_db2%yy,date_db2%mm,date_db2%dd,hour_db2
     ksens=0
     DO k=1,nsens
        IF(i==idconfsens(k))THEN
           ksens=k
           EXIT
        END IF
     END DO

     ! lettura dati a cadenza oraria
     IF(ksens>0)THEN
        IF(intv_db(ksens)==60)THEN
           kday=date_db1-date_start+1
           IF(ksens/=0 .AND. kday>=1 .AND. kday<=ndays)THEN
              READ(line,*)i,i,i,hmed_db(ksens,kday,hour_db1), &
                   (flag(ksens,kday,hour_db1,kflag),kflag=1,nflags)
           END IF

           ! lettura dati a cadenza giornaliera
        ELSE IF(intv_db(ksens)==1440 .AND. hour_db1==0)THEN
           kday=date_db1-date_start+1
           IF(ksens/=0 .AND. kday>=1 .AND. kday<=ndays)THEN
              READ(line,*)i,i,i,dmed_db(ksens,kday), &
                   (flag(ksens,kday,hour_db1,kflag),kflag=1,nflags)
           END IF

           ! lettura dati a cadenza ne' oraria ne' giornaliera (intermedia)
        ELSE IF(intv_db(ksens)<1440 .AND. intv_db(ksens)>60)THEN
           kday=date_db1-date_start+1
           IF(ksens/=0 .AND. kday>=1 .AND. kday<=ndays)THEN
              READ(line,*)i,i,i,hmed_db(ksens,kday,hour_db1), &
                   (flag(ksens,kday,hour_db1,kflag),kflag=1,nflags)
           END IF
        END IF
     END IF
  END DO


  !--------------------------------------------------------------------------
  ! 3) Elaborazione dati

  ! 3.1) Controllo qualita' (flags)

  ALLOCATE(valid(nsens,ndays,0:23))
  valid   =0

  ! tiene tutti i dati
  IF(opt_valid==0)THEN
     valid=1

     ! esclude i dati invalidati
  ELSE IF(opt_valid==1)THEN
     DO kflag=nflags,1,-1
        WHERE(flag(:,:,:,kflag)<0 .OR. (valid(:,:,:)==0 .AND. flag(:,:,:,kflag)==0))
           valid(:,:,:)=0
        ELSEWHERE
           valid(:,:,:)=1
        END WHERE
     END DO

     ! tiene solo i dati validati
  ELSE IF(opt_valid==2)THEN
     DO kflag=nflags,1,-1
        WHERE(flag(:,:,:,kflag)>0 .OR.  (valid(:,:,:)==1 .AND. flag(:,:,:,kflag)==0))
           valid(:,:,:)=1
        ELSEWHERE
           valid(:,:,:)=0
        END WHERE
     END DO

     ! tiene solo i dati validati da operatore
  ELSE IF(opt_valid==3)THEN
     DO kflag=nflags_man,1,-1
        WHERE(flag(:,:,:,kflag)>0 .OR. (valid(:,:,:)==1 .AND. flag(:,:,:,kflag)==0))
           valid(:,:,:)=1
        ELSEWHERE
           valid(:,:,:)=0
        END WHERE
     END DO
  END IF

  ! pone zero la matrice valid anche dove il dato del DB e' mancante
  ! (era gia' zero dove il dato non ha i requisiti di validazione richiesti)
  DO ksens=1,nsens
     IF(intv_db(ksens)>=60 .AND. intv_db(ksens)<1440)THEN
        WHERE(hmed_db(ksens,:,:)==missdata(1) .OR. hmed_db(ksens,:,:)==missdata(2))
           valid(ksens,:,:)=0
        END WHERE

     ELSE IF(intv_db(ksens)==1440)THEN
        WHERE(dmed_db(ksens,:)==missdata(1) .OR. dmed_db(ksens,:)==missdata(2))
           valid(ksens,:,0)=0
        END WHERE
     END IF
  END DO

  ! 3.2)  Costruzione della matrice dei dati di output,
  !       pescando dal sensore attivo e corrispondente
  !       alla stazione in ciascun istante
  
  hmed_out=missdata(1) 
  dmed_out=missdata(1) 
  DO ksens=1,nsens
     kstaz=stazsens(ksens)
     WHERE(valid(ksens,:,:)==1)
        hmed_out(kstaz,:,:)=hmed_db(ksens,:,:)
     END WHERE
     WHERE(valid(ksens,:,0)==1)
        dmed_out(kstaz,:)=dmed_db(ksens,:)
     END WHERE
  END DO

  ! 3.3) Calcolo medie e massimi giornalieri

  IF(opt_stat==2)THEN
     DO ksens=1,nsens
        kstaz=stazsens(ksens)
        WHERE(dmed_out(kstaz,:) == missdata(1) .AND.&
             SUM(valid(ksens,:,:)*intv_db(ksens), DIM=2) >= 60*24/100*opt_percval .AND.&
             SUM(valid(ksens,:,:)*intv_db(ksens), DIM=2) > 0.)
           dmed_out(kstaz,:)= &
                SUM(valid(ksens,:,:)*hmed_db(ksens,:,:)*intv_db(ksens), DIM=2) /&
                SUM(valid(ksens,:,:)*intv_db(ksens), DIM=2)
        ENDWHERE
     END DO

  ELSE IF(opt_stat==3)THEN
     DO ksens=1,nsens
        kstaz=stazsens(ksens)
        WHERE(dmax_out(kstaz,:) == missdata(1) .AND.&
             SUM(valid(ksens,:,:)*intv_db(ksens), DIM=2) >= 60*24/100*opt_percval .AND.&
             SUM(valid(ksens,:,:)*intv_db(ksens), DIM=2) > 0.)
           dmax_out(kstaz,:)= &
                MAXVAL(valid(ksens,:,:)*hmed_db(ksens,:,:), DIM=2)
        ENDWHERE
     END DO
  END IF

  ! 3.4) Statistiche intero intervallo

  IF(opt_stat==1)THEN
     nstep=ndays*24
  ELSE IF(opt_stat==2 .OR. opt_stat==3)THEN
     nstep=ndays
  END IF

  !--------------------------------------------------------------------------
  ! 4) Scrittura dati

  ! 4.1) File ASCII (formato estra_orari)
  DO kstaz=1,nstaz_anag
     ksens=sensstaz(kstaz)
     IF((opt_stat==1 .AND. COUNT(hmed_out(kstaz,:,:)/=missdata(1))>0) .OR.  &
          (opt_stat==2 .AND. COUNT(dmed_out(kstaz,:)/=missdata(1))>0) .OR.  &
          (opt_stat==3 .AND. COUNT(dmax_out(kstaz,:)/=missdata(1))>0))THEN
        WRITE(fileout,'(a,"_",i9.9,".asc")')TRIM(codparam_lst(kparam)),staz(ksens)
        WRITE(*,*)"Scrivo il file ",fileout
        OPEN(14,file=fileout)

        ! intestazione
        WRITE(14,'(a,1x,a," [",a,"] staz.",i2,1x,a)') &
             TRIM(nomestat(opt_stat)),             &
             TRIM(codparam_lst(kparam)),           &
             TRIM(nomeunmis(opt_unmis)),           &
             staz(ksens),                          &
             TRIM(nomestaz(ksens))
        WRITE(14,*)

        ! nome colonne
        WRITE(form,'(a,i2,a)')"(a,",11-LEN(TRIM(codparam_lst(kparam))),"x,a)"
        IF(opt_stat==1)THEN
           WRITE(14,form)"aaaa mm gg hh",TRIM(codparam_lst(kparam))
        ELSEIF(opt_stat==2 .OR. opt_stat==3)THEN
           WRITE(14,form)"aaaa mm gg",TRIM(codparam_lst(kparam))
        END IF

        ! dati
        DO kday=1,ndays
           date_out=date_start+kday-1
           IF(opt_stat==1)THEN
              DO i=0,22
                 WRITE(14,'(i4.4,x,3(i2.2,x),f10.1)')          &
                      date_out%yy,date_out%mm,date_out%dd,i+1, &   ! passa da ora di inizio a ora di fine misura
                      hmed_out(kstaz,kday,i)
              END DO
              date_out=date_out+1                               ! la misura iniziata alle 23 finisce alle 00 del giorno seguente
              WRITE(14,'(i4.4,x,3(i2.2,x),f10.1)')          &
                   date_out%yy,date_out%mm,date_out%dd,0, &   ! passa da ora di inizio a ora di fine misura
                   hmed_out(kstaz,kday,i)
           ELSEIF(opt_stat==2)THEN
              WRITE(14,'(i4.4,x,2(i2.2,x),f10.1)')           &
                   date_out%yy,date_out%mm,date_out%dd,      &
                   dmed_out(kstaz,kday)
           ELSEIF(opt_stat==3)THEN
              WRITE(14,'(i4.4,x,2(i2.2,x),f10.1)')           &
                   date_out%yy,date_out%mm,date_out%dd,      &
                   dmax_out(kstaz,kday)
           END IF
        END DO

        CLOSE(14)
     END IF
  END DO

  ! 4.3) File GrADS su punto: binario

  DO kstaz=1,nstaz_anag
     ksens=sensstaz(kstaz)
     IF((opt_stat==1 .AND. COUNT(hmed_out(kstaz,:,:)/=missdata(1))>0) .OR.  &
          (opt_stat==2 .AND. COUNT(dmed_out(kstaz,:)/=missdata(1))>0) .OR.  &
          (opt_stat==3 .AND. COUNT(dmax_out(kstaz,:)/=missdata(1))>0))THEN
        WRITE(fileout,'(a,"_",i9.9,".bin")')TRIM(codparam_lst(kparam)),staz(ksens)
        WRITE(*,*)"Scrivo il file ",TRIM(fileout)
        OPEN(14,file=trim(fileout),form="unformatted",ACCESS='DIRECT', RECL=4)
        irec=0
        DO kday=1,ndays

           ! medie orarie
           IF(opt_stat==1)THEN
              DO i=0,23
                 irec=irec+1
                 WRITE(14,rec=irec)hmed_out(kstaz,kday,i)
              END DO

              ! medie giornaliere
           ELSE IF(opt_stat==2)THEN
              irec=irec+1
              WRITE(14,rec=irec)dmed_out(kstaz,kday)

              ! massimi giornalieri
           ELSE IF(opt_stat==3)THEN
              irec=irec+1
              WRITE(14,rec=irec)dmax_out(kstaz,kday)
           END IF

        END DO
        CLOSE(14)
     END IF
  END DO

  ! 4.4) File GrADS su punto: ctl

  ! medie orarie
  IF(opt_stat==1)THEN
     hstart=1
  ! medie e massimi giornalieri
  ELSE
     hstart=0
  END IF
  DO kstaz=1,nstaz_anag
     ksens=sensstaz(kstaz)
     IF((opt_stat==1 .AND. COUNT(hmed_out(kstaz,:,:)/=missdata(1))>0) .OR.  &
          (opt_stat==2 .AND. COUNT(dmed_out(kstaz,:)/=missdata(1))>0) .OR.  &
          (opt_stat==3 .AND. COUNT(dmax_out(kstaz,:)/=missdata(1))>0))THEN
        WRITE(fileout,'(a,"_",i9.9)')TRIM(codparam_lst(kparam)),staz(ksens)
        OPEN(14,file=TRIM(fileout)//".ctl")
        WRITE(*,*)"Scrivo il file ",TRIM(fileout),".ctl"
        WRITE(14,'(3a)')        "dset    ^",TRIM(fileout),".bin"
        WRITE(14,'(a,f7.1)')    "undef   ",missdata(1)
        WRITE(14,'(a,3(x,a),i10,x,a)')       &
             "title   ",                     &
             TRIM(nomestat(opt_stat)),       &
             TRIM(codparam_lst(kparam)),     &
             "staz.",                        &
             staz(ksens),                    &
             TRIM(nomestaz(ksens))
        WRITE(14,'(a,f8.5,a)')       "xdef    1 linear ",lon(ksens)," 1.0"   
        WRITE(14,'(a,f8.5,a)')       "ydef    1 linear ",lat(ksens)," 1.0"
        WRITE(14,'(a)')               "zdef    1 linear  0 1"
        WRITE(14,'(a,i12,a,i2.2,2a,1x,a)') "tdef    ",nstep," linear ",hstart,"z", &
          grads_date(date_start),TRIM(intv_gds(opt_stat))
        WRITE(14,'(a)')               "vars    1"
        WRITE(14,'(a," 0 99",2(x,a)," [",a,"] staz.",i10,x,a)')    &
             TRIM(codparam_gds(kparam)),                           &
             TRIM(nomestat(opt_stat)),                             &
             TRIM(codparam_lst(kparam)),                           &
             TRIM(nomeunmis(opt_unmis)),                           &
             staz(ksens),                                          &                           
             TRIM(nomestaz(ksens))
        WRITE(14,'(a)')      "endvars"
        CLOSE(14)
     END IF
  END DO

  ! 4.5) File GrADS stazioni: binario

  fileout=TRIM(codparam_lst(kparam))//"_"//TRIM(codstat(opt_stat))//".bin"
  OPEN(14,file=TRIM(fileout),form="unformatted")
  WRITE(*,*)"Scrivo il file ",TRIM(fileout)
  DO kday=1,ndays
     ! medie orarie
     IF(opt_stat==1)THEN
        DO i=0,23
           DO kstaz=1,nstaz_anag
              ksens=sensstaz(kstaz)
              WRITE(ch8,'(i8.8)')staz(ksens)
              WRITE(14)ch8,lat(ksens),lon(ksens),0.0,1,1
              WRITE(14)hmed_out(kstaz,kday,i)
           ENDDO
           WRITE(14)ch8,lat(nsens),lon(nsens),0.0,0,1
        END DO

        ! medie giornaliere
     ELSE IF(opt_stat==2)THEN
        DO kstaz=1,nstaz_anag
           ksens=sensstaz(kstaz)
           WRITE(ch8,'(i8.8)')staz(ksens)
           WRITE(14)ch8,lat(ksens),lon(ksens),0.0,1,1
           WRITE(14)dmed_out(kstaz,kday)
        ENDDO
        WRITE(14)ch8,lat(nsens),lon(nsens),0.0,0,1

        ! massimi giornalieri
     ELSE IF(opt_stat==3)THEN
        DO kstaz=1,nstaz_anag
           ksens=sensstaz(kstaz)
           WRITE(ch8,'(i8.8)')staz(ksens)
           WRITE(14)ch8,lat(ksens),lon(ksens),0.0,1,1
           WRITE(14)dmax_out(kstaz,kday)
        ENDDO
        WRITE(14)ch8,lat(nsens),lon(nsens),0.0,0,1
     END IF

  ENDDO
  CLOSE(14)

  ! 4.6) File GrADS stazioni: ctl

  fileout=TRIM(codparam_lst(kparam))//"_"//TRIM(codstat(opt_stat))
  OPEN(14,file=TRIM(fileout)//".ctl")
  WRITE(*,*)"Scrivo il file ",TRIM(fileout),".ctl"
  WRITE(14,'(3a)')        "dset    ^",TRIM(fileout),".bin"
  WRITE(14,'(a)')         "dtype   station"
  WRITE(14,'(a)')         "options sequential"
  WRITE(14,'(3a)')        "stnmap  ^",TRIM(fileout),".map"
  WRITE(14,'(a,f7.1)')    "undef   ",missdata(1)
  WRITE(14,'(2a,x,a)')    "title   ",TRIM(nomestat(opt_stat)),TRIM(codparam_lst(kparam))
  WRITE(14,'(a,i12,2a,x,a)')"tdef    ",nstep," linear 00z",grads_date(date_start),TRIM(intv_gds(opt_stat))
  WRITE(14,'(a)')         "vars    1"
  WRITE(14,'(a," 0 99",2(x,a)," [",a,"]")')     &
       TRIM(codparam_gds(kparam)),           &
       TRIM(nomestat(opt_stat)),             &
       TRIM(codparam_lst(kparam)),           &
       TRIM(nomeunmis(opt_unmis))
  WRITE(14,'(a)')      "endvars"
  CLOSE(14)

  ! scrittura di un file di appoggio
  ! che contiene il nome del .ctl
  ! (serve per costruire il .map)
  OPEN(15,file="_nomectl.tmp")
  WRITE(15,'(a)')TRIM(fileout)//".ctl"
  CLOSE(15)

  !--------------------------------------------------------------------------
  ! 5) Scrittura logs

  ! 5.1) Scrittura log controllo qualita'

  ! 5.2) Scrittura statistiche intervallo

  !--------------------------------------------------------------------------
  ! 6) Gestione errori e avvisi

  STOP
901 print *, "Errore aprendo estra_qaria.inp"
  STOP
902 print *,"Errore leggendo estra_qaria.inp"
  STOP
903 print *, "Errore aprendo stzqa.tmp"
  STOP                         
904 print *,"Errore leggendo stzqa.tmp"
  STOP
905 print *, "Errore aprendo dati.tmp"
  STOP                         
906 print *,"Errore leggendo dati.tmp"
  STOP
907 print *, "Errore aprendo dati.sql"
  STOP                         
908 print *,"Errore leggendo dati.sql"
  STOP
END PROGRAM ordina
