\ Define some character constants
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
: recurse
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
        dup \ 0>
    while
        space
        1-
    repeat
    drop
;

: nip ( a b -- b ) swap drop ;

\ we do not have 'over' yet
\: tuck ( a b -- b a b ) swap over ;
: pick ( xn..x0 n -- xn..x0 xn )
    1+      \ skip n on the stack
    4 *     \ multiply by cell size
    dsp@ +  \ add to stack pointer
    @       \ fetch
;

: decimal ( -- ) 10 base ! ;
: hex ( -- ) 16 base ! ;

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