. x509functions
[ -z "$1" ] && echo "Forgot first argument: fname.lname of user" && exit
[ -z "$2" ] && echo "Forgot second argument: name of the vpn" && exit
x509 request -email $1@ge.com -name $1 2&> /dev/null
x509 sign -csr $1.csr -ca $2-CA.crt  2&> /dev/null
x509 key_password -key $1.key 
rm -f /tmp/$1-$2-outside.zip /tmp/$1-$2-inside.zip 2&> /dev/null
x509 buildopenvpnzip -ca $2-CA.crt -crt $1 -server `cat outside-address` 2&> /dev/null
mv /tmp/$1-$2.zip archive/$1-$2-outside.zip
x509 buildopenvpnzip -ca $2-CA.crt -crt $1 -server `cat inside-address` 2&> /dev/null
mv /tmp/$1-$2.zip archive/$1-$2-inside.zip
rm $1*
echo
echo Created: $1-$2-outside.zip and $1-$2-inside.zip
