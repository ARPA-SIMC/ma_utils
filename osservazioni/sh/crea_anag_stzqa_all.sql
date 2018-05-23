-- ************************************************************************
-- Script per costruire un'anagrafica estesa delle stazioni di 
-- qualita' dell'aria
-- Lanciare come:
-- /usr/bin/sqlplus AA_USER/USER_AA@RMQAC_NEWARIA
-- sta crea_anag_stzqa_all.sql
--
--                                                  V1, Johnny 10/10/2005
-- ************************************************************************

-- Formato del file di output
set lines 170
set pages 10000

col ID_STAZIONE    heading "CODE"         format 9999999999 null 'unknown'
col NOME_STAZIONE  heading "NAME"         format A30	    null 'unknown'
col COMUNE  	   heading "MUNICIPALITY" format A30	    null 'unknown'
col PR                                    format A2         null '??'
col LAT            heading "LAT"          format 99.99999   null -99.99999
col LON            heading "LON"          format 99.99999   null -99.99999
col ALT            heading "ELEV"         format 9999       null -9999 
col CLA                                   format 000	    null 0
col FIRSTDAY                                                null -999
col LASTDAY                                                 null -999
col O3                                    format 00	    null 9
col PM                                    format 00	    null 9
col N2                                    format 00	    null 9
col NO                                    format 00	    null 9
col NX                                    format 00	    null 9
col S2                                    format 00	    null 9
col CO                                    format 00	    null 9
col BZ                                    format 00         null 9  
col P2                                    format 00         null 9  
col MOB                                   format 00         null 9  


-- Estrazione records
spool anagr_stzqa_all.tmp;

SELECT
  ID_STAZIONE   , 
  max(SUBSTR(NOME_STAZIONE,1,30)) as NOME_STAZIONE,      
  max(SUBSTR(COMUNE       ,1,30)) as COMUNE,  
  max(PROVINCIA                    ) as PR,  
  max(LAT)           as LAT,
  max(LON)           as LON,
  max(ALT)           as ALT,
  greatest(
      min(decode(ID_COD_TP_EMISS, 1,20,0)),
      min(decode(ID_COD_TP_EMISS, 2,10,0)),
      min(decode(ID_COD_TP_EMISS, 3,20,0)),
      min(decode(ID_COD_TP_EMISS, 4,20,0)),
      min(decode(ID_COD_TP_EMISS, 7,10,0)),
      min(decode(ID_COD_TP_EMISS,10,30,0)),
      min(decode(ID_COD_TP_EMISS,11,30,0)),
      min(decode(ID_CLASSE_MIN,   1,30,0))) +
  greatest(
      min(decode(ID_ZONA, 1, 1,0)),
      min(decode(ID_ZONA, 2, 2,0)),
      min(decode(ID_ZONA, 3, 3,0))) as CLA,
   min(to_char((DATA_INIZIO),'yyyymmdd')) as FIRSTDAY,
   min(decode(DATA_FINE,null,'-999',to_char((DATA_FINE  ),'yyyymmdd'))) as LASTDAY,
   max(decode(ID_PARAMETRO,  7,1,0)) as O3,
   max(decode(ID_PARAMETRO,  5,1,0)) as PM,
   max(decode(ID_PARAMETRO,  8,1,0)) as N2,
   max(decode(ID_PARAMETRO, 38,1,0)) as NO,
   max(decode(ID_PARAMETRO,  9,1,0)) as NX,
   max(decode(ID_PARAMETRO,  1,1,0)) as S2,
   max(decode(ID_PARAMETRO, 10,1,0)) as CO,
   max(decode(ID_PARAMETRO, 20,1,0)) as BZ,
   max(decode(ID_PARAMETRO,111,1,0)) as P2,
   max(staz.FLG_MOBILE) as MOB
FROM
 ANG_CONFIG_SENSORI staz
GROUP BY
 ID_STAZIONE
ORDER BY
 ID_STAZIONE
;

spool off;
