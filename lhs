# produce literate haskell code from latex source code   
#
cat < $1'.tex' | sed -f /home/unarmedlad/Documents/Haskell/lhsfilter.sed > $1'.lhs'
echo "Literate Haskell code written to file $1.lhs"
