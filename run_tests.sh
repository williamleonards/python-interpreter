a=0
for f in tests/*; 
do 
    out="`./opython $f`"
    if ((${#out} \> 0))
    then
        echo "'$out' in $f"
        a=1
    fi
done 
if (($a \< 1))
then
    echo "All python tests pass"
fi
if (($a \> 0))
then
    echo "Some python tests fail"
fi