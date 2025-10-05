: source ( -- c-addr u )
    in-buf +1
    in-buf c@
;

: included ( addr n -- )
    source-id @ >r \ save source id

    file-open ( fd error ) if
        ." failed to open file"
        drop \ drop fd
        r> source-id ! \ restore source id
        exit \ abort" failed to open file"
    then

    source-id ! \ store file descriptor as source-id

    begin
        in-buf 1+ 128 source-id read-line (  c-addr u1 fileid -- u2 flag ior )
        if  \ io error
            drop \ flag
            drop \ u2
            source-id file-close
            r> source-id !
            exit
        then
    while \ while flag > 0
        dup       \ ( u2 u2 )
        in-buf c! \ set string len
        0 >in !   \ reset index

        begin
            dup  \ ( u2 u2)
            >in @ <  \ *>in < len
        while
            interpret
        repeat
        drop \ u2
    repeat



    source-id file-close \ close file
    r> source-id ! \ restore source id
;