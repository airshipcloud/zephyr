#!/usr/bin/env bash

##########################################################################################
# Apply config vars to *.source files
#
# USAGE:
# $ script/apply_config.sh [file1] [file2] [...]
#
# EXAMPLE:
# $ script/apply_config.sh dev joe
# applies config/base, config/dev, and config/joe to vars found in *.source files
#
# config/base is always sourced first followed by any files passed in on the command line
#
##########################################################################################

cd -- "${0%/*}"
# set -x
set -e

args=`echo $@`
arr=( "base" ${args} )

cd ..

# Use existing config if no params were passed in
if [ -z "${args}" ] && [ -e "config/applied_config" ]; then
  arr=( $(cat config/applied_config) )
else
  rm -f config/applied_config
  for i in ${arr[@]}; do
    echo ${i} >> config/applied_config
  done
fi

echo "========================================="
echo "Applying config/"
for file in ${arr[@]}; do
  echo "  ${file}"
  source ./config/${file}
done
echo ''

apply_config_to=( cloudstore/db cloudstore/rel/files cloudstore/test router/rel/files apps/auth )

for d in ${apply_config_to[@]}; do
  if [ -d ${d} ]; then
    # sf -> source file
    for sf in `find ./${d} -name *.source`; do
      tf=${sf%.*} # target file
      echo ${tf}
      cp ${sf} ${tf}
      > ${tf}
IFS="
"
      for line in $(< ${sf}); do
        z=`echo ${line} | sed 's_\"_\\\"_g'`
        z=`echo ${z} | sed 's_\\\$_CF-CONFIG-DOLLAR_g'`
        x=`echo ${z} | sed -n "s_@@\([^@]*\)@@_$\{\1\}_g p"`
        if [ -n "${x}" ]; then
          y=`eval echo \"${x}\"`
          y=`echo ${y} | sed 's_CF-CONFIG-DOLLAR_\\\$_g'`
          echo ${y} >> ${tf}
        else
          echo ${line} >> ${tf}
        fi
      done # < ${sf}
    done
  fi
done

echo ""
echo "DONE applying config"
echo "========================================="
echo ""
