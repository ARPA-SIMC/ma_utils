! legge i dati meteo delle stazioni
! estratti con estra_orari
! o i dati di un modello
! estratti su un punto con seriet
! e scrive per grads i file stations
! e i file grid su ogni punto 

! NB dopo aver girato il programma
!    occorre lanciare l'utility
!    stnmap che costruisce il file .map:
!    stnmap -i filein.ctl               

! autore:   Giovanni Bonafe'
! versione inziale: settembre 2005 
! Modifiche lettura headers aprile 2009 (Enrico)
! debug conteggio variabili se presente il vento aprile 2010 (Johnny)
!
! Versione 3.0.0, Giovanni & Enrico 07/09/2012

! Prossime modifiche da fare:
! - gestione livelli modello

program orari2grads

  ! dichiarazioni e parametri
  use date_handler
INTEGER :: eof,eor
  character(len=100):: &
       file_ana_met
  character &
       filelst*100, &
       filein*100, &
       fileout*100, &
       filectl*100, &
       ch22*22,ch50*50, &
       ch13*13, &
       ch3*3, &
       ch2*2, &
       ch5*5, &
       line*1000, &
       header*1000, &
       header_old*1000, &
       ch10*10, &
       inp_data*3, &
       chpar*100
  character,allocatable:: &
       codrete(:)*2, &
       codstaz(:)*5, &
       codmodl(:)*4, &
       codpunt(:)*3, &
       id_staz(:)*8, &
       nomevar(:)*10
  real,allocatable:: &
       val(:,:,:), &
       lat(:), &
       lon(:)
  real,parameter:: &
       missing=-9999., &
       pi=3.14159
  integer &
       flag_vento,pos_ff,pos_dd, &
       p1,p2
  logical :: &
       ok
  type(date):: date_start,date_old

  ! inizializzazioni
  data flag_vento,pos_ff,pos_dd/0,0,0/

  ! Variabili d'ambiente
  CHARACTER (LEN=30) HOME_BONAFE
  CALL GETENV('HOME_BONAFE',HOME_BONAFE)
  file_ana_met=TRIM(HOME_BONAFE)//"/osservazioni/dat/db_anagrafica.dat"

  ! trovo codice eof
  CALL get_eof_eor(eof,eor)

  ! Parametri da riga comandi

  filelst="_"
  inp_data="hhr"
  DO kpar = 1,HUGE(kpar)
     CALL getarg(kpar,chpar)
     SELECT CASE (TRIM(chpar))
     CASE ("")
        EXIT
     CASE ("-h")
        call scrive_help(file_ana_met)
        STOP
     CASE ("-o")
        inp_data = "hhr"
     CASE ("-s")
        inp_data = "ser"
     CASE DEFAULT
        filelst = TRIM(chpar)
     END SELECT
  ENDDO
  IF (filelst=="_") THEN
     call scrive_help(file_ana_met)
     STOP
  END IF

  ! legge dalla lista di files
  open(1,file=trim(filelst)//".lst")
  do i=1,huge(i)
     read(1,*,end=20,err=20)ch2
  enddo
20 nstaz=i-1
  rewind(1)

  allocate(id_staz(nstaz),lat(nstaz),lon(nstaz))
  if(inp_data=="hhr")then
     allocate(codrete(nstaz),codstaz(nstaz))
  elseif(inp_data=="ser")then
     allocate(codmodl(nstaz),codpunt(nstaz))
  end if

  ! apre files
  open(5,file=trim(file_ana_met))
  do istaz=1,nstaz
     read(1,*)filein
     open(4,file=trim(filein))

     ! formato estra_orari: legge codice stazione (da header dati)
     if(inp_data=="hhr")then
        read(4,'(a)')ch22
        read(ch22,'(5x,i2,10x,i5)',IOSTAT=ios) iirete,iistaz
        if (ios == 0) then
          write (codrete(istaz),'(i2.2)') iirete
          write (codstaz(istaz),'(i5.5)') iistaz
          write (id_staz(istaz),'(i1,i2.2,i5.5)') 0,iirete,iistaz
          ok = .true.
        else
          codrete(istaz) = "xx"
          codstaz(istaz) = "xxxxx"
          id_staz(istaz) = ch22(1:8)
          do k = 1,8
            if(id_staz(istaz)(k:k)==" ") id_staz(istaz)(k:k)="_"
          enddo
          ok = .false.
        end if
     end if

     ! formato estra_orari: legge lat e lon (da anagrafica)
     if(inp_data=="hhr")then
        if (ok) then
          read(5,*)
          read(5,*)
          read(5,*)
          ok = .FALSE.
          do i=1,huge(i)
             read(5,'(2x,a,2x,a,9x,f7.3,x,f7.3)',iostat=ios,err=901)ch2,ch5,alon,alat
             if (ios == eof) exit
             if (ios /= 0) cycle 
             do j=1,5
                if(ch5(j:j)==" ")then
                   ch5(j:j)="0"
                end if
             enddo
             do j=1,2
                if(ch2(j:j)==" ")then
                   ch2(j:j)="0"
                end if
             enddo
             if(ch2==codrete(istaz).and. &
                  ch5==codstaz(istaz))then
                lat(istaz)=alat
                lon(istaz)=alon
                rewind(5)
                ok = .TRUE.
                exit
             endif
          enddo
        endif

        if (ok) then
          write (*,'(5a,2f9.3)') "Elaboro stazione ", &
            codstaz(istaz)," rete ",codrete(istaz)," coordinate ",lat(istaz),lon(istaz)
        else
          write (*,'(3a)') "Stazione non trovata in anagrafica, ", &
            "uso coordinate (0,0) e idstaz: ",id_staz(istaz)
          lat(istaz)=0.
          lon(istaz)=0.
        endif
      
     end if
 
     !formato seriet: legge lat, lon e costruisce codice punto
     if(inp_data=="ser")then
        read(4,'(a)')ch50
        p1 = INDEX(ch50,":")
        p2 = INDEX(ch50,"!")
        ier = 0
        if (p1 > 0 .AND. p2 > p1) then
          ch22=adjustl(ch50(p1+1:p2-1))
          read(ch22,*,iostat=ier)lat(istaz),lon(istaz)
          codpunt(istaz)=ch50(p2+1:p2+3)
        endif
        if (p1 <= 0 .OR. p2 <= p1 .OR. ier /= 0) then
          write (*,*) "Errore interpetando il 1o record di ",trim(filein)
          write (*,*) "Impossibile leggere coordinate e label del punto richiesto, metto 0."
          lat(istaz) = 0.
          lat(istaz) = 0.
          codpunt(istaz) = ch50(1:3)
        endif
        read(4,*)
        read(4,'(a)')ch50
        p1 = INDEX(ch50,">")
        ch22=adjustl(ch50(p1+1:))
        codmodl(istaz)=trim(ch22)
        id_staz(istaz)=codmodl(istaz)//codpunt(istaz)
        do k = 1,8
          if(id_staz(istaz)(k:k)==" ") id_staz(istaz)(k:k)="_"
        enddo
        read(4,*)
     end if

     ! legge nomi variabili
     read(4,*)
     read(4,'(a)')header
     if(istaz>1.and.header/=header_old)then
        write(*,*)" ATTENZIONE i file della lista devono  "
        write(*,*)"            avere le stesse variabili,  "
        write(*,*)"            controlla ",filein
        stop
     else
        header_old=header
     endif
     if(istaz==1)then
        if(inp_data=="hhr")then
           nvar=(len(trim(header))-13)/11
        elseif(inp_data=="ser")then
           nvar=(len(trim(header))-17)/11
        end if

        allocate(nomevar(nvar))
        do i=1,nvar
           if(inp_data=="hhr")then
              ch10=adjustl(header(14+11*(i-1):13+11*(i)))
           elseif(inp_data=="ser")then
              ch10=adjustl(header(18+11*(i-1):17+11*(i)))
           end if
           nomevar(i)=""
           k=0
           do j=1,10
              if(ch10(j:j)/=" " .AND. ch10(j:j)/="_".AND. &
                      ch10(j:j)/="." .AND. ch10(j:j)/="-")then
                 k=k+1
                 nomevar(i)(k:k)=ch10(j:j)
              end if
           enddo
           if(nomevar(i)=="FFist")then
              pos_ff=i
              flag_vento=flag_vento+1
           endif
           if(nomevar(i)=="DDist")then
              pos_dd=i
              flag_vento=flag_vento+1
           endif
        enddo
        if(flag_vento==1) flag_vento=0

        ! aggiunge i nomi delle 
        ! componenti del vento
        if(flag_vento==2 .AND. inp_data=="hhr")then
           deallocate(nomevar)
           nvar=nvar+2
           allocate(nomevar(nvar))
           do i=1,nvar-2
              ch10=adjustl(header(14+11*(i-1):13+11*(i)))
              nomevar(i)=""
              k=0
              do j=1,10
                 if(ch10(j:j)/=" " .AND. ch10(j:j)/="_".AND. &
                      ch10(j:j)/="." .AND. ch10(j:j)/="-")then
                    k=k+1
                    nomevar(i)(k:k)=ch10(j:j)
                 end if
              enddo
           enddo
           nomevar(nvar-1)="Uist"
           nomevar(nvar)="Vist"
        endif
     endif
     write(*,'(1000a)')"variabili: ", &
          (trim(nomevar(i)),", ",i=1,nvar-1),trim(nomevar(nvar))

     ! legge data e ora iniziale
     if(inp_data=="hhr")then
        read(4,'(i4,x,i2,x,i2,x,i2)')date_start%yy,date_start%mm,date_start%dd,iora
     elseif(inp_data=="ser")then
        read(4,'(i2,x,i2,x,i4,x,i2)')date_start%dd,date_start%mm,date_start%yy,iora
     end if
     if(istaz>1.and.date_start/=date_old)then
        write(*,*)" ATTENZIONE i file della lista devono"
        write(*,*)"            iniziare nello stesso istante,  "
        write(*,*)"            controlla ",filein
        stop
     else
        date_old=date_start
     endif
     write(*,'(a,i2.2,a,i2.2,a,i4.4,a,i2.2)') &
          "istante iniziale: ",date_start%dd,"/",date_start%mm,"/",date_start%yy," ore ",iora

     ! legge dati
     do i=1,huge(i)
        read(4,*,end=10,err=10)ch13
     enddo
10   nstep=i
     if(istaz>1.and.nstep/=nstep_old)then
        write(*,*)" ATTENZIONE i file della lista devono"
        write(*,*)"            avere lo stesso numero di step, "
        write(*,*)"            controlla ",filein
        stop
     else
        nstep_old=nstep
     endif
     write(*,*)"numero di ore: ",nstep
     write(*,*)"numero di variabili: ",nvar
     if(istaz==1)then
        allocate(val(nstep,nvar,nstaz))
     endif
     rewind(4)
     read(4,*)
     read(4,*)
     read(4,*)
     if(inp_data=="ser")then
        read(4,*)
        read(4,*)
        read(4,*)
     end if
     do istep=1,nstep
        read(4,'(a)')line
        if(inp_data=="hhr")then
           read(line(14:),*)(val(istep,ivar,istaz),ivar=1,nvar-flag_vento)
        elseif(inp_data=="ser")then
           read(line(18:),*)(val(istep,ivar,istaz),ivar=1,nvar-flag_vento)
        end if

        ! calcola le componenti del vento
        if(flag_vento==2)then
           if(val(istep,pos_ff,istaz)==missing.or. &
                val(istep,pos_dd,istaz)==missing.or. &
                val(istep,pos_dd,istaz)>360.or. &
                val(istep,pos_ff,istaz)<0)then
              val(istep,nvar-1,istaz)=missing
              val(istep,nvar,istaz)=missing
           else
              angle=val(istep,pos_dd,istaz)*pi/180.
              val(istep,nvar-1,istaz)= &
                   -val(istep,pos_ff,istaz)*sin(angle)
              val(istep,nvar,istaz)= &
                   -val(istep,pos_ff,istaz)*cos(angle)
           endif
        endif
     enddo
     close(4)
  enddo




  ! scrive file di dati station
  fileout=trim(filelst)//".bin"
  open(2,file=trim(fileout),form="unformatted")
  write(*,'(2a)')"scrivo file ",trim(fileout)
  do istep=1,nstep
     do istaz=1,nstaz
        write(2)id_staz(istaz),lat(istaz),lon(istaz),0.0,1,1
        write(2)(val(istep,ivar,istaz),ivar=1,nvar)
     enddo
     write(2)id_staz(istaz),lat(istaz),lon(istaz),0.0,0,1
  enddo
  close(2)

  ! scrive files di dati grid
  do istaz=1,nstaz
     irec=0
     fileout=trim(id_staz(istaz))//".bin"
     open(2,file=trim(fileout),form="unformatted",ACCESS='DIRECT', RECL=4)
     write(*,'(2a)')"scrivo file ",trim(fileout)
     do istep=1,nstep
        do ivar=1,nvar
           irec=irec+1
           write(2,rec=irec)val(istep,ivar,istaz)
        enddo
     enddo
     close(2)
  enddo

  ! scrive file .ctl station
  filectl=trim(filelst)//".ctl"
  open(3,file=trim(filectl))
  write(*,'(2a)')"scrivo file ",trim(filectl)
  write(3,'(3a)')"dset   ^",trim(filelst),".bin"
  write(3,'(a)') "dtype  station"
  write(3,'(a)') "options sequential"
  write(3,'(3a)')"stnmap ^",trim(filelst),".map"
  write(3,'(a)') "undef  -9999.0"
  write(3,'(2a)')"title  ",trim(filelst)
  write(3,'(a,i6,a,i2.2,3a)') &
       "tdef   ",nstep," linear ",iora,"z",grads_date(date_start)," 1hr"
  write(3,'(a,i4)')"vars   ",nvar
  do ivar=1,nvar
     write(3,'(3a)')trim(nomevar(ivar)),"   0 99 ", &
          trim(nomevar(ivar))
  enddo
  write(3,'(a)') "endvars"
  close(3)

  ! scrive files .ctl grid
  do istaz=1,nstaz
     filectl=trim(id_staz(istaz))//".ctl"
     open(3,file=trim(filectl))
     write(*,'(2a)')"scrivo file ",trim(filectl)
     write(3,'(3a)')"dset   ^",trim(id_staz(istaz)),".bin"
     write(3,'(a)') "undef  -9999.0"
     WRITE(3,'(a,f8.5,a)')  "xdef        1 linear ",lon(istaz)," 1.0"   
     WRITE(3,'(a,f8.5,a)')  "ydef        1 linear ",lat(istaz)," 1.0"
     WRITE(3,'(a)')         "zdef        1 linear  0 1"
     if(inp_data=="hhr")then
        write(3,'(4a)')"title  rete: ",codrete(istaz),", stazione: ",codstaz(istaz)
     elseif(inp_data=="ser")then
        write(3,'(4a)')"title  modello: ",codmodl(istaz),", punto: ",codpunt(istaz)
     end if
     write(3,'(a,i6,a,i2.2,3a)') &
          "tdef   ",nstep," linear ",iora,"z",grads_date(date_start)," 1hr"
     write(3,'(a,i4)')"vars   ",nvar
     do ivar=1,nvar
        write(3,'(3a)')trim(nomevar(ivar)),"   0 99 ", &
             trim(nomevar(ivar))
     enddo
     write(3,'(a)') "endvars"
     close(3)
  enddo

  ! avviso
  write(*,*)
  write(*,'(a)') "NB adesso occorre lanciare l'utility "        
  write(*,'(3a)')"   stnmap, che costruisce il file ",trim(filelst),".map:"
  write(*,*)
  write(*,'(3a)')"   stnmap -i ",trim(filelst),".ctl     "          
  write(*,*)

  ! errori
  stop
901 print *,"error reading",trim(file_ana_met)
  stop

end program orari2grads


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help(file_ana_met)
!
! Visualizza a schermo l'help del programma
!
IMPLICIT NONE
  character(len=100):: file_ana_met

  write(*,*)""
  write(*,*)"orari2grads.exe [-h] [-o/-s] list"
  write(*,*)""
  write(*,*)"list    : lista files di input (senza estensione .lst)"
  write(*,*)"-o      : input in formato estra_orari (default)"
  write(*,*)"-s      : input in formato trasp_seriet (gestisce livelli)"
  write(*,*)"-h      : visualizza questo help"
  write(*,*)""
  write(*,*)"Converte files di dati orari da ASCII a GrADS."
  write(*,*)"Se si desidera l'output di tipo station occorre"
  write(*,*)"lanciare l'utility stnmap che costruisce "
  write(*,*)"il file .map:"
  write(*,*)"   stnmap -i filein.ctl               "
  write(*,*)"Il programma cerca i nomi delle stazioni meteo"
  write(*,*)"in ",trim(file_ana_met)
  write(*,*)""

RETURN

END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE get_eof_eor(eof, eor)
!-------------------------------------------------------------------------
! Ritorna i codici di errore macchina-dipendenti corrispondenti alle 
! condizioni di EOF e EOR nella lettura di un file sequenziale formattato
!
! Secondo manuale, questi sono gli unici due casi in cui IOSTAT ritorna
! con un valore negativo. 
! Si noti che EOR riguarda solo non-advancinag READ
!-------------------------------------------------------------------------
IMPLICIT NONE

INTEGER, INTENT(OUT) :: eof,eor

INTEGER :: k, ios, idummy=0, iun=0
LOGICAL :: l1 = .TRUE.


! Cerco un'unita' libera per aprire il file di prova
DO k = 10,99
  INQUIRE (UNIT=k, OPENED=l1, IOSTAT=ios)
  IF (.NOT. l1 .AND. ios==0) THEN
    iun = k
    EXIT
  ENDIF
ENDDO
IF (iun == 0) GOTO 9999   ! non ho torvato nessuna unita' libera
!WRITE (*,*) "uso unita ",iun

! Cerco codice di errore per EOF
OPEN (unit=k, STATUS="SCRATCH", FORM="FORMATTED", ACCESS="SEQUENTIAL", &
  PAD="NO", ERR=9999)
ENDFILE (k)
REWIND (k)
READ (k,*,IOSTAT=eof)
CLOSE(k)

! Cerco codice di errore per EOR
OPEN (unit=k, STATUS="SCRATCH", FORM="FORMATTED", ACCESS="SEQUENTIAL", &
  PAD="NO", ERR=9999)
WRITE (k,'(a1)') "1" 
WRITE (k,'(a1)') "2"
REWIND (k)
READ (k,'(i1)',ADVANCE="NO",ERR=9999) idummy
READ (k,'(i1)',ADVANCE="NO",IOSTAT=eor) idummy
CLOSE(k)

!write (*,*) "eof,eor ",eof,eor
RETURN

! Gestione errori
9999 CONTINUE
WRITE (*,*) "Errore in subroutine get_eof_eor, usero' valori di default"
eof = -1
eor = -2
RETURN

END SUBROUTINE get_eof_eor

