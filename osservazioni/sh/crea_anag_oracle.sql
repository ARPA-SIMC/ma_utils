-- ************************************************************************
-- Script per costruire l'anagrafica delle stationi superificali presenti
-- nel db ORACLE
-- ************************************************************************

-- Formato del file di output
set lines 90
set pages 50000

col GSTA_IDENTNR   heading "NET"   format 999
col CODICE_UTENTE  heading "USRID" format 99999
col IDENTNR        heading "DB_ID" format 99999
col X_LONG_CENT    heading "LONG"  format 999.999
col Y_LAT_CENT     heading "LAT"   format 99.999
col Z_QUOTA_SLM    heading "QUOTA" format 9999
col NOME           heading "NOME"  format A40

-- Estrazione records
spool ../dat/anag_oracle.dat

select msm.GSTA_IDENTNR, msm.CODICE_UTENTE, msm.IDENTNR, 
       mpm.X_LONG_CENT, mpm.Y_LAT_CENT, substr(mpm.Z_QUOTA_SLM,1,5) as "Z_QUOTA_SLM", msm.NOME
  from MET_STAZIONI_MISURA msm, MET_PUNTI_MISURA mpm
  where msm.PMIS_IDENTNR = mpm.IDENTNR
  order by GSTA_IDENTNR, CODICE_UTENTE;

spool off;






