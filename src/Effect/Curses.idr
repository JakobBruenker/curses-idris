module Effect.Curses

import Effects
import public UI.Curses

%access abstract

||| This is the resource for the `CURSES` effect before it has done
||| anything.
data Pre : Type where
  MkPre : Pre

||| When the `CURSES` effect has this resource, the curses functions can be
||| called. Note that a `CURSES` effect should only be `run` if the type
||| signature does not contain the `Active` resource.
data Active : (colorEnabled : Bool) -> Type where
  MkActive : Active colorEnabled

||| This is the resource the `CURSES` effect has after curses has been
||| terminated
data Post : Type where
  MkPost : Post

instance Default Pre where
  default = MkPre

data Curses : Effect where
  
  Start : GetChMode ->  { Pre      ==> Active False } Curses ()
  End   :               { Active c ==> Post         } Curses ()

  StartColor :  { Active False ==> {success} Active $ if success
                                                        then True
                                                        else False
                } Curses Bool

  MovePrevCh    :                   { Active c } Curses Bool
  MoveNextCh    :                   { Active c } Curses Bool
  Move          : Int -> Int ->     { Active c } Curses Bool
  SetGetChMode  : GetChMode ->      { Active c } Curses ()
  ScrSize       :                   { Active c } Curses (Int, Int)
  Refresh       :                   { Active c } Curses ()
  Keypad        : Bool ->           { Active c } Curses ()
  GetYX         :                   { Active c } Curses (Int, Int)
  GetMaxYX      :                   { Active c } Curses (Int, Int)
  GetStr        : Bool -> Bool ->   { Active c } Curses String
  GetCh         :                   { Active c } Curses (Maybe Char)
  ForceCh       :                   { Active c } Curses Char
  Echo          : Bool ->           { Active c } Curses ()
  CursSet       : CursorState ->    { Active c } Curses (Maybe CursorState)
  ClrToEol      :                   { Active c } Curses ()
  Clear         :                   { Active c } Curses ()
  AddStr        : String ->         { Active c } Curses ()
  AddNStr       : String -> Nat ->  { Active c } Curses ()
  AddCh         : Char ->           { Active c } Curses ()

  SetAttr         : List Attr -> { Active False } Curses ()
  SetAttrAndColor : List Attr -> Maybe ColorPair ->
                      { Active True } Curses ()

instance Handler Curses IO where
  handle _ (Start gcm) k = startCurses gcm  $> k () MkActive
  handle _ End         k = endWin           $> k () MkPost

  handle _ StartColor k = do success <- hasColors
                             startColor
                             k success MkActive


  handle m MovePrevCh k = movePrevCh >>= \success => k success m
  handle m MoveNextCh k = moveNextCh >>= \success => k success m
  handle m (Move y x) k = move y x   >>= \success => k success m

  handle m (SetGetChMode gcm)  k = setGetChMode gcm $>              k ()      m
  handle m ScrSize             k = scrSize          >>= \dims   =>  k dims    m
  handle m Refresh             k = refresh          $>              k ()      m
  handle m (Keypad bf)         k = keypad   bf      $>              k ()      m
  handle m GetYX               k = getYX            >>= \coords =>  k coords  m
  handle m GetMaxYX            k = getMaxYX         >>= \maxYX  =>  k maxYX   m
  handle m (GetStr ue se)      k = getStr   ue  se  >>= \str    =>  k str     m
  handle m GetCh               k = getCh            >>= \mc     =>  k mc      m
  handle m ForceCh             k = forceCh          >>= \c      =>  k c       m
  handle m (Echo bf)           k = echo bf          $>              k ()      m
  handle m (CursSet cs)        k = cursSet  cs      >>= \mcs    =>  k mcs     m
  handle m ClrToEol            k = clrToEol         $>              k ()      m
  handle m Clear               k = clear            $>              k ()      m
  handle m (AddStr str)        k = addStr   str     $>              k ()      m
  handle m (AddNStr str n)     k = addNStr  str n   $>              k ()      m
  handle m (AddCh c)           k = addCh    c       $>              k ()      m

  handle m (SetAttr attrs)             k =
    setAttrAndColor attrs Nothing $> k () m
  handle m (SetAttrAndColor attrs col) k =
    setAttrAndColor attrs col     $> k () m

||| The curses effect.
CURSES : Type -> EFFECT
CURSES res = MkEff res Curses

||| Use this function to start curses. Note that `end` should *always* be called
||| before the program terminates.
||| @getChMode the mode that getCh will use
start : (getChMode : GetChMode) ->
        { [CURSES Pre] ==> [CURSES $ Active False] } Eff ()
start gcm = call $ Start gcm

||| Terminates curses. This should *always* be called before the program
||| terminates.
end : { [CURSES $ Active c] ==> [CURSES Post] } Eff ()
end = call Effect.Curses.End

||| Tries to initialize colors. The return value specifies if this was
||| successful. You can only use colors if you have called this function
||| and it returned `True`.
startColor : { [CURSES $ Active False] ==>
               [CURSES $ Active (if result
                                   then True
                                   else False)] } Eff Bool
startColor = call StartColor

||| Moves the cursor to the previous position, if possible. Returns `True` if
||| cursor was moved, `False` otherwise.
movePrevCh : { [CURSES $ Active c] } Eff Bool
movePrevCh = call MovePrevCh

||| Advances the cursor to the next position, if possible. Returns `True` if
||| cursor was moved, `False` otherwise.
moveNextCh : { [CURSES $ Active c] } Eff Bool
moveNextCh = call MoveNextCh

||| Moves the cursor to the specified coordinates.
||| Returns `True` if the cursor was moved, `False` otherwise.
||| @y the line to which the cursor will move (counting starts at 0)
||| @x the column to which the cursor will move (counting starts at 0)
move : (y : Int) -> (x : Int) -> { [CURSES $ Active c] } Eff Bool
move y x = call $ Move y x

||| Sets the mode `getCh` will operate in.
setGetChMode : GetChMode -> { [CURSES $ Active c] } Eff ()
setGetChMode gcm = call $ SetGetChMode gcm

||| Returns the size of the standard window.
|||
||| The format is `(lines, columns)`
scrSize : { [CURSES $ Active c] } Eff (Int, Int)
scrSize = call ScrSize

||| Refreshes the standard window. It is unlikely that a program needs to
||| manually call this function.
refresh : { [CURSES $ Active c] } Eff ()
refresh = call Refresh

||| Enables or disables the reading of function keys, arrow keys, and so on.
||| This is turned on by default.
||| @bf if `True`, enables reading, otherwise, disables it.
keypad : (bf : Bool) -> { [CURSES $ Active c] } Eff ()
keypad bf = call $ Keypad bf

||| Returns the current cursor position.
|||
||| The format is `(line, column)`.
getYX : { [CURSES $ Active c] } Eff (Int, Int)
getYX = call GetYX

||| Returns the highest line and column the cursor can be at.
|||
||| The format is `(line, column)`.
getMaxYX : { [CURSES $ Active c] } Eff (Int, Int)
getMaxYX = call GetMaxYX

||| Returns a `String` the user enters. This function is affected by whether or
||| not the `GetChMode` is a "raw" or a "linebuf" mode.
||| @useEcho  if `True`, the user will see the `String` they enter
||| @setEcho  if `True`, echo will be on after the `String` has been returned,
|||             otherwise, echo will be off
getStr : (useEcho : Bool) -> (setEcho : Bool) -> 
         { [CURSES $ Active c] } Eff String
getStr ue se = call $ GetStr ue se

||| Returns `Just` a character, or `Nothing` if the `GetChMode` is `Wait k` and
||| the time runs out. See `GetChMode` for more information.
getCh : { [CURSES $ Active c] } Eff $ Maybe Char
getCh = call GetCh

||| Returns a character once the user presses a key. This function is affected
||| by whether or not the `GetChMode` is a "raw" or a "linebuf" mode.
forceCh : { [CURSES $ Active c] } Eff Char
forceCh = call ForceCh

||| `echo` determines whether a character will be printed when the user presses
||| a key
||| @bf whether or not the echo will be on
echo : (bf : Bool) -> { [CURSES $ Active c] } Eff ()
echo bf = call $ Echo bf

||| Sets the cursor state. Returns `Nothing`, if the specified cursor state
||| cannot be set, and `Just` the previous cursor state otherwise.
||| @cs the cursor state that will be set
cursSet : (cs : CursorState) -> { [CURSES $ Active c] } Eff $ Maybe CursorState
cursSet cs = call $ CursSet cs

||| Clears the screen to the end of the line
clrToEol : { [CURSES $ Active c] } Eff ()
clrToEol = call ClrToEol

||| Clears the screen
clear : { [CURSES $ Active c] } Eff ()
clear = call Clear

||| Prints a `String` to the standard screen at current cursor position.
||| The cursor will be advanced after printing.
||| @s the string that will be printed
addStr : (s : String) -> { [CURSES $ Active c] } Eff ()
addStr str = call $ AddStr str

||| Prints a `String` to the standard screen at current cursor position.
||| The cursor will be advanced after printing.
||| @s        the string that will be printed 
||| @maxChars specifies the maximum number of characters that will be printed
addNStr : (s : String) -> (maxChars : Nat) -> { [CURSES $ Active c] } Eff ()
addNStr str k = call $ AddNStr str k

||| Prints a character to the standard screen at current cursor position.
||| The cursor will be advanced after printing.
||| @c the character that will be printed
addCh : (c : Char) -> { [CURSES $ Active col] } Eff ()
addCh c = call $ AddCh c

||| Sets all given attributes.
||| @attrs  the attributes that will be set
setAttr : (attrs : List Attr) -> { [CURSES $ Active False] } Eff ()
setAttr attrs = call $ SetAttr attrs

||| Sets all given attributes and the specified colors.
||| @attrs  the attributes that will be set
||| @colors if this is `Nothing`, the default colors will be used, which are
|||           usually white on black; if this is not `Nothing`, the specified
|||           Pair will be initialized and used - note that if characters on the
|||           screen use the pair with index *i*, and the pair with index *i* is
|||           then reinitialized with different colors, the characters that are
|||           already on the screen will change their color
setAttrAndColor : (attrs : List Attr) -> (colors : Maybe ColorPair) ->
                    { [CURSES $ Active True] } Eff ()
setAttrAndColor attrs col = call $ SetAttrAndColor attrs col
