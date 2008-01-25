#! /bin/bash
. /usr/share/davud/x509functions
while read safe safepw name user group password email notes
  do
  [[ "$safe" =~  "^#" ]] && continue
  [ -z "$safe" ] && continue

  if [ ! -f "${safe}.dat" ]  
      then
      { echo $safepw
	  echo $safepw
      } |   pwsafe -f "${safe}.dat" --createdb
  fi
  echo created ${safe}.dat >&2
  echo $safepw |pwsafe -f "${safe}.dat" --delete $name 
  {
      [ "$group" = "." ] && group=""
      echo $safepw
      echo $name
      echo $group
      echo $user
      echo $password
      echo $password
      echo $notes
      echo n
  } | pwsafe -a -f "${safe}.dat"




done 

 