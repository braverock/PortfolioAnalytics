# #! /bin/bash
# Automate file conversion in bash
if [ "$1" == "" ]; then
  inputdir="../results"
  destdir="../docs/symposium-slides-2013-figure"
else
  inputdir=$1
  destdir=$2
fi
cd $inputdir
n=0;
echo " ";
echo "Converting:"
echo " ";
for file in *.pdf
  do 
    pngfile=${file%%.pdf}.png
    echo "$((++n)): $file to $destdir/$pngfile"
    convert -density 300 $file -quality 100 -sharpen 0x1.0 $destdir/$pngfile 
done
echo " ";
echo "$((n)) files were converted from PDF to PNG format."
