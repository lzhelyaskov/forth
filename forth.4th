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

: spaces ( n -- ) \ prints n spaces
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

: U. ( u -- ) \ print unsigned number
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