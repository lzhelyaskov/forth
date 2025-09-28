: test-fio
    s" testfile.txt" r/o file-open
    0= if
        ." file opened" cr
    else
        ." could not open" cr
        exit
    then
    dup 
    6000 6 ( fd fd addr 6 )
    rot    ( fd addr 6 fd )
    file-read drop drop
    file-close
    6000 6 tell
;

test-fio