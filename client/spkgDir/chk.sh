echo "Enter password for bombardier server:"
stty -echo
read password
stty echo
echo $password | /cygdrive/c/python25/python.exe _chk.py -p $*
