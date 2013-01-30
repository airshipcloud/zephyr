#!/usr/bin/env bash

cd -- "${0%/*}"
# set -x
set -e

rm -f config/applied_config

args=`echo $@`
arr=( "base" ${args} )

echo "Applying ${args}"

for i in ${arr[@]}
do
    source ./config/${i}
    echo ${i} >> config/applied_config
done

# set; exit 0

apply_config_to=( cloudstore/db cloudstore/rel/files cloudstore/test )

for d in ${apply_config_to[@]}
do
    if [ -d ${d} ]
    then
        # sf -> source file
        for sf in `find ./${d} -name *.source`
        do
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
                if [ -n "${x}" ]
                then
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
