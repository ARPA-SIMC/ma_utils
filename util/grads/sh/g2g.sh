#!/bin/bash
#--------------------------------------------------------------------------
# Script per importare in GRADS un GRIB generico
#
#                                         Versione 1.0.0, Enrico 31/01/2022
#--------------------------------------------------------------------------
#set -x

# Set environment
if [ -z $MA_UTILS_SVN ] ; then
  g1_to_ctl=/usr/libexec/ma_utils/grib2ctl.pl
  g2_to_ctl=/usr/libexec/ma_utils/g2ctl
else
  g1_to_ctl=${MA_UTILS_SVN}/util/grads/sh/grib2ctl.pl
  g2_to_ctl=${MA_UTILS_SVN}/util/grads/sh/g2ctl
fi

# Command line
batch="N"
if [ $# -lt 2 ] ; then
  echo "Use: g2g.sh file ana/for [-b]"
  exit 1
elif [ $# -eq 2 ] ; then
  filein=$1
  type=$2
elif [ $# -eq 3 ] ; then
  filein=$1
  type=$2
  opt=$3
  if [ $opt = "-b" ] ; then
    batch="Y"
  fi
else
  echo "Use: g2g.sh file ana/for [-b]"
  exit 1
fi

# Process command line parameters
bname=$(basename $filein)
rname=${bname%.*}

if [ $type = "for" ] ; then
  g2ctl_opt="-verf"
else
  g2ctl_opt=""
fi
  
# Get edition number
en=$(grib_get -w count=1 -p editionNumber $filein)
echo "GRIB edition "$en

# Create .ctl file
if [ $en = 1 ] ; then
  perl $g1_to_ctl $filein $g2ctl_opt > ${rname}.ctl
elif [ $en = 2 ] ; then
  perl $g2_to_ctl $filein $g2ctl_opt > ${rname}.ctl
fi

# Creat gribmap file
gribmap -i ${rname}.ctl

# Run GRADS (interactively)
if [ $batch = "N" ] ; then
  grads -cl 'open '${rname}'.ctl'
fi

