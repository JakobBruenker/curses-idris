module Effect.Curses

import Effects
import public UI.Curses

data Active : (colorEnabled : Bool) -> Type where
  MkActive : Active colorEnabled

data Inactive : (colorEnabled : Bool) -> Type where
  MkInactive : Inactive colorEnabled

data Curses : Effect where
  
  Start : GetChMode ->  { Inactive c ==> Active   c } Curses ()
  End   :               { Active   c ==> Inactive c } Curses ()

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
  handle _ End         k = endWin           $> k () MkInactive

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


CURSES : Type -> EFFECT
CURSES res = MkEff res Curses

start : GetChMode -> { [CURSES $ Inactive c] ==> [CURSES $ Active c] } Eff ()
start gcm = call $ Start gcm

end : { [CURSES $ Active c] ==> [CURSES $ Inactive c] } Eff ()
end = call Effect.Curses.End

startColor : { [CURSES $ Active False] ==>
               [CURSES $ Active (if result
                                   then True
                                   else False)] } Eff Bool
startColor = call StartColor

movePrevCh : { [CURSES $ Active c] } Eff Bool
movePrevCh = call MovePrevCh

moveNextCh : { [CURSES $ Active c] } Eff Bool
moveNextCh = call MoveNextCh

move : Int -> Int -> { [CURSES $ Active c] } Eff Bool
move y x = call $ Move y x

setGetChMode : GetChMode -> { [CURSES $ Active c] } Eff ()
setGetChMode gcm = call $ SetGetChMode gcm

scrSize : { [CURSES $ Active c] } Eff (Int, Int)
scrSize = call ScrSize

refresh : { [CURSES $ Active c] } Eff ()
refresh = call Refresh

keypad : Bool -> { [CURSES $ Active c] } Eff ()
keypad bf = call $ Keypad bf

getYX : { [CURSES $ Active c] } Eff (Int, Int)
getYX = call GetYX

getMaxYX : { [CURSES $ Active c] } Eff (Int, Int)
getMaxYX = call GetMaxYX

getStr : Bool -> Bool -> { [CURSES $ Active c] } Eff String
getStr ue se = call $ GetStr ue se

getCh : { [CURSES $ Active c] } Eff $ Maybe Char
getCh = call GetCh

forceCh : { [CURSES $ Active c] } Eff Char
forceCh = call ForceCh

echo : Bool -> { [CURSES $ Active c] } Eff ()
echo bf = call $ Echo bf

cursSet : CursorState -> { [CURSES $ Active c] } Eff $ Maybe CursorState
cursSet cs = call $ CursSet cs

clrToEol : { [CURSES $ Active c] } Eff ()
clrToEol = call ClrToEol

clear : { [CURSES $ Active c] } Eff ()
clear = call Clear

addStr : String -> { [CURSES $ Active c] } Eff ()
addStr str = call $ AddStr str

addNStr : String -> Nat -> { [CURSES $ Active c] } Eff ()
addNStr str k = call $ AddNStr str k

addCh : Char -> { [CURSES $ Active c] } Eff ()
addCh c = call $ AddCh c

setAttr : List Attr -> { [CURSES $ Active False] } Eff ()
setAttr attrs = call $ SetAttr attrs

setAttrAndColor : List Attr -> Maybe ColorPair ->
                    { [CURSES $ Active True] } Eff ()
setAttrAndColor attrs col = call $ SetAttrAndColor attrs col
