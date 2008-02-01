. ../x509functions
[ -z "$1" ] && echo "Forgot first argument: fname.lname of user" && exit
x509 request -email $1@ge.com -name $1 2&> /dev/null
x509 sign -csr $1.csr -ca cgvpn-CA.crt  2&> /dev/null
x509 key_password -key $1.key 
rm -f /tmp/$1-salem-outside.zip /tmp/$1-salem-inside.zip 2&> /dev/null
x509 buildopenvpnzip -ca cgvpn-CA.crt -crt $1 -server 12.22.34.213 2&> /dev/null
mv /tmp/$1-cgvpn.zip $1-salem-outside.zip
x509 buildopenvpnzip -ca cgvpn-CA.crt -crt $1 -server 3.137.195.208 2&> /dev/null
mv /tmp/$1-cgvpn.zip $1-salem-inside.zip
echo
echo Created: $1-salem-outside.zip and $1-salem-inside.zip
