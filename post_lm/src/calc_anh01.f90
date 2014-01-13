PROGRAM calc_anh01
!--------------------------------------------------------------------------
! Legge un file con 12 scadenze orari cumulate, le prime 11 nell'ora 
! precedente e l'ultima in 12; 
! scrive cumulata nell'ora precedente per l'ultimo istante. 
! programma scritto per recuperare id dati si sf e cp dell'archivio LAMAZ
!--------------------------------------------------------------------------

USE grib_api
IMPLICIT NONE

INTEGER, PARAMETER :: np = 169*181
INTEGER :: ifin=0,ifout=0,igin=0,igout=0,iret,kg,out_neg,data,time
CHARACTER(LEN=200) :: filein,fileout
REAL :: field_in(np,12),field_out(np),out_ave

!--------------------------------------------------------------------------
! Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)
IF (TRIM(filein) == "" .OR. TRIM(fileout) == "" .OR. &
  TRIM(filein) == "-h" .OR. TRIM(filein) == "--help") THEN
  WRITE (*,*) "Uso: calc_anh01.exe filein fileout"
  STOP
ENDIF

! Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifout,fileout,"w")

! Leggo i dati in input
DO kg = 1,12
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998
  CALL grib_get(igin,"values",field_in(:,kg))
ENDDO

! Calcolo la cumulata nell'ora precedente all'ultimo istante
field_out(:) = field_in(:,12) - SUM(field_in(:,1:11), DIM=2)

CALL grib_get (igin,"dataDate",data)
CALL grib_get (igin,"dataTime",time)
out_neg = COUNT(field_out(:) < -1.e-3)
out_ave = SUM(field_out(:)) / REAL(np)
WRITE (*,'(i8,1x,i4.4,a,i5,a,f7.3)') data,time," neg ",out_neg," ave ",out_ave

! Scrivo output
CALL grib_clone(igin,igout)
CALL grib_set(igout,"values",field_out)
CALL grib_set(igout,"P2",1)
CALL grib_write (igout,ifout)

STOP

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib n.ro " ,kg
STOP

END PROGRAM calc_anh01
