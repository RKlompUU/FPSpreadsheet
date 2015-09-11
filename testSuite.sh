#!/bin/bash

. BashStd.sh

./make.sh

if [ $# = "0" ]; then
    testCases=(pLambda)
else
    testCases=$1
fi

for inF in ./in/*.w; do
    echo "Processing $inF.."


    for testCase in "${testCases[@]}"; do
        outF=`echo $inF | sed 's/\/in\//\/out\//'`
        outF=`echo $outF | sed "s/\.w/\.w${testCase}/"`

        cmpF=`echo $outF | sed 's/\.w/\.wSaved_/'`

        echo "Test case: $testCase"

        ./bin/Main $testCase < $inF 2&> $outF || confirm 'Runtime crash! Do you want to continue testing?' || exit

        if [ -f $cmpF ]; then
            diff=`cmp $outF $cmpF 2>&1 | wc -l`
            if [ "$diff" != "0" ]; then
                echo "Output differs! Prompting kdiff3.."

                kdiff3 $outF $cmpF

                while true; do
                    read -p "Do you want to overwrite the previous output with the current? " yn
                    case $yn in
                        [Yy]* ) cp -v $outF $cmpF; break;;
                        [Nn]* ) break;;
                        * ) echo "Please answer yes/no/y/n"
                    esac
                done
            fi
        else
            cp -v $outF $cmpF
        fi
    done

    echo ""
done
