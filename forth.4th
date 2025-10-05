: '\n' 10 ;
: bl   32 ; \ bl (BLank) is a standard FORTH word for space.

\ cr prints a carriage return
: cr '\n' emit ;

\ space prints a space
: space bl emit ;

\ literal takes whatever is on the stack and compiles LIT whatever
: literal immediate
	' lit ,		\ compile LIT
	,		    \ compile the literal itself (from the stack)
;

: ':' [ char : ] literal ;
: ';' [ char ; ] literal ;
: '(' [ char ( ] literal ;
: ')' [ char ) ] literal ;
: '"' [ char " ] literal ;
: 'A' [ char A ] literal ;
: '0' [ char 0 ] literal ;
: '-' [ char - ] literal ;
: '.' [ char . ] literal ;

\ '[compile] word' compiles 'word' if it would otherwise be immediate.
: [compile] immediate
    word find >cfa ,
;

\ recurse makes a recursive call to the current word that is being compiled.
: recurse immediate
    latest @ >cfa ,
;

\ if {true-part} then foo
\   compiles to [ 0branch offset true-part foo ]
\   where offset is the offset of 'foo'
\ if {true-part} else {false-part} then foo
\   compiles to [ 0branch offset-false true-part branch offset-foo false-part foo ]
\   where offset-false is the offset of false-part and offset-foo is the offset of foo 

: if immediate
    ' 0branch , here @ 0 ,
;

: then immediate
    dup here @ swap - swap !
;

: else immediate
    ' branch , here @ 0 , swap dup here @ swap - swap !
;

\ same as if but condition is negated
: unless immediate
    ' not , [compile] if
;

\ loops
\ begin loop-body condition until ( do { loop-body } while (condition); )

: begin immediate
    here @
;

: until immediate
    ' 0branch , here @ - ,
;

\ begin loop-body again ( loop { loop-body } )

: again immediate
    ' branch , here @ - ,
;

\ begin condition while loop-body repeat ( while (condition) { loop-body } )

: while immediate
    ' 0branch , here @ 0 ,
;

: repeat immediate
    ' branch , swap here @ - , dup here @ swap - swap !
;

\ forth '( ... )' comments
: ( immediate
    1                   \ depth counter
    begin
        key             \ read next char
        dup '(' = if    \ open paran?
            drop        \ drop it
            1+          \ increase depth counter
        else
            ')' = if    \ close paran?
                1-      \ decrease depth counter
            then
        then
        dup 0=          \ is depth counter = 0?
    until
    drop                \ drop depth counter
;

\ prints n spaces
: spaces ( n -- )
    begin
        dup 0>
    while
        space
        1-
    repeat
    drop
;

\: nip ( a b -- b ) swap drop ;
\: tuck ( a b -- b a b ) swap over ;

: pick ( xn..x0 n -- xn..x0 xn )
    1+      \ skip n on the stack
    1+
    4 *     \ multiply by cell size
    dsp@ +  \ add to stack pointer
    @       \ fetch
;

: decimal ( -- ) 10 base ! ;
: hex ( -- ) 16 base ! ;

: depth ( -- n ) 
    s0 dsp@ -
    4-
;

\ print unsigned integer
: U. ( u -- )
    base @ /mod
    ?dup if
        recurse
    then

    dup 10 < if
        '0'
    else
        10 -
        'A'
    then
    +
    emit
;

\ print stack contents
: .s ( -- )
    dsp@
    begin
        dup s0 <
    while
        4+
        dup @ U.
        space
    repeat
    drop
;

\ width in characters of an unsigned integer
: uwidth ( u -- width )
    base @ /
    ?dup if
        recurse 1+
    else
        1
    then
;

\ prints an unsigned number, padded to a certain width
: U.R ( u width -- )
    swap dup uwidth rot swap - spaces U.
;

\ prints a signed number, padded to a certain width.
: .R ( n width -- )
    swap
    dup 0< if
        negate
        1
        swap
        rot
        1-
    else
        0 swap rot
    then
    swap
    dup
    uwidth
    rot
    swap -
    spaces
    swap

    if
        '-' emit
    then
    U.
;

\ prints an integer followed by space
: . ( n -- ) 0 .R space ;

\ fetch and print an integer
: ? ( addr -- ) @ . ;

\ c a b within returns 1 (true) if a <= c and c < b
: within ( c a b -- f )
    over - >r - r> u<
;

\ takes address and aligns it to the next 4 bytes
: aligned ( addr -- addr )
    3 + 3 invert and \ ( (addr + 3) & ~3)
;

\ aligns the HERE pointer
: align ( -- )
    here @ aligned here !
;

\ 
: cells ( n -- n ) 4 * ;

\ allot allocates n bytes of memory
: allot ( n -- addr )
    here @ swap
    here +!
;

\ c, appends (compiles) a byte to the current compiled word.
: c,
	here @ c!	( store the character in the compiled image )
	1 here +!	( increment HERE pointer by 1 byte )
;

\ s" string" is used in FORTH to define strings.
: s" immediate ( -- addr len )
    state @ if ( compiling? )
        ' litstring ,
        here @
        0 ,
        begin 
            key
            dup '"' <>
        while
            c,
        repeat
        drop
        dup
        here @ swap -
        4-
        swap !
        align
    else
        here @
        begin
            key
            dup '"' <>
        while
            over c!
            1+
        repeat
        drop
        here @ -
        here @
        swap
    then
;

\ ." is the print string operator in FORTH.  Example: ." Something to print"
: ." immediate ( -- )
    state @ if ( compiling? )
        [compile] s"
        ' tell ,
    else
        begin
            key
            dup '"' = if
                drop
                exit
            then
            emit
        again
    then
;

\ defines a constant
: const ( n -- )
    word
    create
    DOCOL ,
    ' lit ,
    ,
    ' exit ,
;

\ defines a variable
: var 
    1 cells allot
    word create
    DOCOL ,
    ' lit ,
    ,
    ' exit ,
;

\ forget xxx removes xxx word and all words defined after it from dictionary
: forget
	word find
	dup @ latest !
	here !
;

\ print words name e.g. latest @ id.
: id. ( addr -- )
    4+
    dup c@ ( flags and length byte )
    F_LENMASK and

    begin
        dup 0> ( length > 0 ? )
    while
        swap 1+
        dup c@
        emit
        swap 1-
    repeat
    2drop
;

\ returns true if word is flagged as hidden
: ?hidden ( addr -- f )
    4+
    c@
    F_HIDDEN and
;

\ returns true if word is flagged as immediate
: ?immediate ( addr -- f )
    4+
    c@
    F_IMMED and
;

\ prints all words defined in the dictionary
\ does not print hidden words
: words ( -- )
    latest @
    begin
        ?dup
    while
        dup ?hidden not if
            dup id.
            cr \ space
        then
        @
    repeat
;

\ dump memory content
\ e.g latest @ 128 dumps
: dump ( addr len -- )
    base @ -rot \ save current base
    hex

    begin
        ?dup
    while
        over 8 U.R space

        2dup
        1- 15 and 1+
        begin
            ?dup
        while
            swap
            dup c@
            2 .R space
            1+ swap 1-
        repeat
        drop

        2dup 1- 15 and 1+
        begin
            ?dup
        while
            swap
            dup c@
            dup 32 128 within if
                emit
            else
                drop '.' emit
            then
            1+ swap 1-
        repeat
        drop
        cr

        dup 1- 15 and 1+
        tuck
        -
        >r + r>
    repeat
    drop

    base ! \ restore saved base
;

\ ( case ... endcase is forths switch statement

\     ( some value on the stack )
\ 		CASE
\             test1 OF ... ENDOF
\             test2 OF ... ENDOF
\             testn OF ... ENDOF
\             ... ( default case )
\ 		ENDCASE

\ compiles to:

\ 	CASE				(push 0 on the immediate-mode parameter stack)
\ 	test1 OF ... ENDOF		test1 OVER = IF DROP ... ELSE
\ 	test2 OF ... ENDOF		test2 OVER = IF DROP ... ELSE
\ 	testn OF ... ENDOF		testn OVER = IF DROP ... ELSE
\ 	... ( default case )		...
\ 	ENDCASE				DROP THEN [THEN [THEN ...]]
\ )

: case immediate 0 ; \ marks bottom of the stack
: of immediate
    ' over ,
    ' = ,
    [compile] if
    ' drop ,
;
: endof immediate
    [compile] else 
;
: endcase immediate
    ' drop ,
    begin 
        ?dup
    while 
        [compile] then
    repeat
;
(
: test_case
    case
        1 of ." one" endof
        2 of ." two" endof
        ." something else"
    endcase
;
)
\ opposite of >cfa
\ transforms code field address into word address
\ returns 0 if nothing found
: cfa> ( addr -- addr | 0)
    latest @
    begin
        ?dup
    while
        2dup swap
        < if
            nip
            exit
        then
        @
    repeat
    drop
    0
;

\ see xxx
\ decompiles a forth word xxx
: see ( -- )
    word find
    here @
    latest @
    \ ." before repeat " .s cr
    \ find the word right after xxx
    \ this way we know where xxx ends
    begin
        2 pick
        over
        <>
    while
        nip
        dup @
    repeat
    \ ." after repeat " .s cr

    drop swap
    ':' emit space dup id. space \ : NAME [IMMEDIATE]
    dup ?immediate if 
        ." immediate " 
    then

    \ ." after name " .s cr

    >dfa
    begin
        2dup >
    while
        dup @
        case
            \ is it lit ?
            ' lit of
                4+ dup @ .
            endof
            \ is it litstring ?
            ' litstring of
                [ char s ] literal emit '"' emit space \ print s"<space>
                4+ dup @
                swap 4+ swap
                2dup tell
                '"' emit space
                + aligned
                4-
            endof
            \ is it 0branch ?
            ' 0branch of
                ." 0branch ( "
                4+ dup @ .
                ." ) "
            endof
            \ is it branch ?
            ' branch of
                ." branch ( "
                4+ dup @ .
                ." ) "
            endof
            \ is it ' (tick) ?
            ' ' of
                [ char ' ] literal emit space
                4+ dup @
                cfa> id. space
            endof
            \ is it exit ?
            ' exit of
                2dup 4+ <> if
                    ." exit "
                then
            endof
            \ default:
            dup cfa> id. space
        endcase
        4+
    repeat
    ';' emit cr
    2drop
;

\ create a word with no name
: :noname
    0 0 create
    here @
    DOCOL ,
    ]
;

: ['] immediate
    ' lit ,
;

\ exceptions

: exception-marker
    rdrop
    0
;

: catch ( xt -- exn? )
    dsp@ 4+ >r \ save parameter stack pointer
    ' exception-marker 4+
    >r
    execute
;

: throw ( n -- )
    ?dup if
        rsp@
        begin
            dup r0 4- <
        while
            dup @
            ' exception-marker 4+ = if
                4+
                rsp!
                dup dup dup
                r>
                4-
                swap over
                !
                dsp! exit
            then
            4+
        repeat

        drop

        case
            0 1- of
                ." aborted" cr
            endof
            ." uncaught throw "
            dup . cr
        endcase
        quit
    then
;

\ interrupts execution, drops both stacks
: abort ( -- )
    0 1- throw
;

: abort" ( f -- )
    if 
        

    then
;

\ prints a stack trace by walking up return stack
: stack-trace
    rsp@
    begin
        dup r0 4- <
    while
        dup @
        case
            ' exception-marker 4+ of
                ." catch ( dsp="
                4+ dup @ U.
                ." ) "
            endof

            dup
            cfa>
            ?dup if
                2dup
                id.
                [ char + ] literal emit
                swap >dfa 4+ - .
            then
        endcase
        4+
    repeat
    drop
    cr
;

: is-whitespace ( c -- f )
    33 < \ c < ' ' 
;

: is-not-whitespace
    is-whitespace 0=
;

\ : skip-whitespace ( addr n -- addr n )

\     begin
\         2dup ( addr n addr n )
\         0>
\         swap ( addr n n addr )
\         c@ is-whitespace and ( addr n f ) 
\     while
\         1- \ decrement n
\         swap 1+ \ increment addr
\         swap
\     repeat
\ ;

: xt-skip ( addr n xt -- addr n )
    >r \ save xt in return stack
    begin
        2dup ( addr n addr n )
        0>
        swap ( addr n n addr )
        c@ r@ execute and ( addr n f ) 
    while
        1- \ decrement n
        swap 1+ \ increment addr
        swap
    repeat
    r> drop \ drop xt
;

: skip-whitespace ( addr n -- addr n )
    ['] is-whitespace xt-skip
;

: chars ( n -- n )
;

: /string ( addr len n -- addr len2 )
    dup >r
    - ( addr len2 )
    swap ( len2 addr )
    r> + swap
;

: source ( -- c-addr u )
    in-buf @ 1+
    in-buf c@
;

: parse-name ( "name" -- addr n )
    source  ( addr n )
    >in @   ( addr n i )
    /string ( addr n)
    ['] is-whitespace xt-skip over  >r  ( addr n :r addr )
    ['] is-not-whitespace xt-skip       ( end-addr restlen : r start-addr )
    2dup    ( end-addr restlen end-addr restlen :r start-addr )
    1 min + 
    source drop >in !
    drop r> tuck -
;