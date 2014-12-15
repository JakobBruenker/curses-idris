module Curses

%include C "curses.h"
%include C "cursesrun.h"
%link C "cursesrun.o"
%lib C "curses"

%access private

||| A window, also known as screen.
|||
||| This is used to interface with certain C functions.
data Window = MkWindow Ptr

||| The modes that `getCh` can use to get characters.
public
data GetChMode : Type where
  ||| `getCh` waits for the user to press a key, but doesn't affect interrupt
  ||| and control flow characters.
  WaitForever           : GetChMode
  ||| `getCh` waits for the user to press a key, allows the program to determine
  ||| what happens when interrupt or control flow characters are encountered.
  WaitForeverRaw        : GetChMode
  ||| `getCh` waits for a certain amount of time before returning Nothing.
  ||| Doesn't affect interrupt and control flow characters.
  ||| @k tenths of a second `getCh` will wait
  Wait                  : (k : Nat) -> GetChMode
  ||| `getCh` waits for the user to press a key, but the program doesn't
  ||| receive any information about which keys have been pressed until a
  ||| newline character is encountered. Doesn't affect interrupt and control
  ||| flow characters.
  WaitForeverLinebuf    : GetChMode
  ||| `getCh` waits for the user to press a key, but the program doesn't
  ||| receive any information about which keys have been pressed until a newline
  ||| character is encountered. Allows the program to determine what happens
  ||| when interrupt or control flow characters are encountered.
  WaitForeverLinebufRaw : GetChMode

||| These are the color that are defined once curses is initialized.
|||
||| Note that some terminals may display these colors differently.
public
data Color = Black
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | White

||| This is used to give a foreground and a background color to `init_pair`.
data ColorPair : Type where
  ||| Use this constructor to make a ColorPair.
  ||| @i index of the ColorPair
  ||| @foreground foreground color
  ||| @background background color
  MkColorPair : (i : Nat) -> (foreground : Color) -> (background : Color) ->
    ColorPair

||| These are the attributes that can be used to print text in various ways.
|||
||| Note that some terminals may not support all of these attributes.
public
data Attr : Type where
  ||| Prints text normally.
  Normal      : Attr
  ||| Highlights the printed text.
  Standout    : Attr
  ||| Puts a line under printed text.
  Underline   : Attr
  ||| Reverses the foreground and background colors.
  Reverse     : Attr
  ||| Prints blinking text.
  Blink       : Attr
  ||| Prints half bright text.
  Dim         : Attr
  ||| Prints bold text.
  Bold        : Attr
  ||| Prints with an alternate character set.
  AltCharSet  : Attr
  ||| Prints out invisible text.
  Invis       : Attr
  ||| Protects printed text.
  Protect     : Attr

namespace Cursor
  ||| These are the available cursor states.
  public
  data CursorState = Invisible
                   | Normal
                   | VeryVisible

||| These are special characters, which can be converted into regular
||| Characters with the function `specialChar`.
public
data SpecialChar = Escape
                 | Backspace
                 | Up
                 | Down
                 | Left
                 | Right
                 | F1
                 | F2
                 | F3
                 | F4
                 | F5
                 | F6
                 | F7
                 | F8
                 | F9
                 | F10
                 | F11
                 | F12
                 | Insert
                 | Delete
                 | Home
                 | End
                 | PageUp
                 | PageDown
                 | Tab
                 | Enter

||| Converts a `SpecialChar` into a regular `Char`.
public
specialChar : SpecialChar -> Char
specialChar Escape    = '\ESC'
specialChar Backspace = '\263' 
specialChar Up        = '\259'
specialChar Down      = '\258'
specialChar Left      = '\260'
specialChar Right     = '\261'
specialChar F1        = '\410'
specialChar F2        = '\266'
specialChar F3        = '\267'
specialChar F4        = '\268'
specialChar F5        = '\269'
specialChar F6        = '\270'
specialChar F7        = '\271'
specialChar F8        = '\272'
specialChar F9        = '\273'
specialChar F10       = '\274'
specialChar F11       = '\275'
specialChar F12       = '\276'
specialChar Insert    = '\331'
specialChar Delete    = '\330'
specialChar Home      = '\262'
specialChar End       = '\360'
specialChar PageUp    = '\339'
specialChar PageDown  = '\338'
specialChar Tab       = '\t'
specialChar Enter     = '\n'

intToCursorState : Int -> Maybe CursorState
intToCursorState 0 = Just Invisible
intToCursorState 1 = Just Normal
intToCursorState 2 = Just VeryVisible
intToCursorState _ = Nothing

cursorStateToInt : CursorState -> Int
cursorStateToInt Invisible   = 0
cursorStateToInt Normal      = 1
cursorStateToInt VeryVisible = 2

colorToInt : Color -> Int
colorToInt Black   = 0
colorToInt Red     = 1
colorToInt Green   = 2
colorToInt Yellow  = 3
colorToInt Blue    = 4
colorToInt Magenta = 5
colorToInt Cyan    = 6
colorToInt White   = 7

attrToInt : Attr -> Int
attrToInt Normal     = 0
attrToInt Standout   = pow 2 16
attrToInt Underline  = pow 2 17
attrToInt Reverse    = pow 2 18
attrToInt Blink      = pow 2 19
attrToInt Dim        = pow 2 20
attrToInt Bold       = pow 2 21
attrToInt AltCharSet = pow 2 22
attrToInt Invis      = pow 2 23
attrToInt Protect    = pow 2 24

cBool : Bool -> Int
cBool True  = 1
cBool False = 0

idrBool : Int -> Bool
idrBool = (/= 0)

||| Returns the standard window.
stdScr : IO Window
stdScr = map MkWindow $ mkForeign (FFun "stdScr" [] FPtr)

noDelay : Bool -> IO ()
noDelay bf = do
  (MkWindow scr) <- stdScr
  mkForeign (FFun "nodelay" [FPtr, FInt] FUnit) scr (cBool bf)

halfDelay : Int -> IO ()
halfDelay delay = mkForeign (FFun "halfdelay" [FInt] FUnit) delay

||| Enables or disables the reading of function keys, arrow keys, and so
||| on.
||| @bf  if `True`, enables reading, if `False`, disables it
abstract
keypad : (bf : Bool) -> IO ()
keypad bf = do
  (MkWindow scr) <- stdScr
  mkForeign (FFun "keypad" [FPtr, FInt] FUnit) scr (cBool bf)

meta : Bool -> IO ()
meta bf = do
  (MkWindow scr) <- stdScr
  mkForeign (FFun "meta" [FPtr, FInt] FUnit) scr (cBool bf)

leaveOk : Bool -> IO Int
leaveOk bf = do
  (MkWindow scr) <- stdScr
  mkForeign (FFun "leaveok" [FPtr, FInt] FInt) scr (cBool bf)

nl : Bool -> IO ()
nl True  = mkForeign (FFun "nl"   [] FUnit)
nl False = mkForeign (FFun "nonl" [] FUnit)

||| `echo` determines whether a character will be printed when the user presses
||| a key
abstract
echo : Bool -> IO ()
echo False = mkForeign (FFun "noecho" [] FUnit)
echo True  = mkForeign (FFun "echo"   [] FUnit)

cBreak : Bool -> IO ()
cBreak True  = mkForeign (FFun "cbreak"   [] FUnit)
cBreak False = mkForeign (FFun "nocbreak" [] FUnit)

raw : Bool -> IO ()
raw True  = mkForeign (FFun "raw"   [] FUnit)
raw False = mkForeign (FFun "noraw" [] FUnit)

startColor : IO ()
startColor = mkForeign (FFun "start_color" [] FUnit)

initScr : IO Window
initScr = map MkWindow $ mkForeign (FFun "initscr" [] FPtr)

endWin : IO ()
endWin = mkForeign (FFun "endwin" [] FUnit)

||| Returns the size of the standard window.
|||
||| The format is `(lines, columns)`.
abstract
scrSize : IO (Int, Int)
scrSize = [| MkPair (mkForeign (FFun "getLines" [] FInt))
                    (mkForeign (FFun "getCols"  [] FInt)) |]

||| Refreshes the screen. It is unlikely that a program needs to manually
||| call this function.
abstract
refresh : IO ()
refresh = mkForeign (FFun "refresh" [] FUnit)

||| Initializes a color pair.
initPair : ColorPair -> IO ()
initPair (MkColorPair natIndex colorFG colorBG) =
  mkForeign (FFun "init_pair" [FInt, FInt, FInt] FUnit) ind fg bg 
  where
    ind : Int
    ind = toIntNat natIndex
    fg  : Int
    fg  = colorToInt colorFG
    bg  : Int
    bg  = colorToInt colorBG

setAttr : Int -> IO ()
setAttr a = mkForeign (FFun "attrset" [FInt] FUnit) a

||| Prints a String to the standard screen at current cursor position.
||| The cursor will be advanced after printing.
||| @s        the string that will be printed
||| @maxChars specifies the maximum number of characters that will be printed
abstract
addNStr : (s : String) -> (maxChars : Nat) -> IO ()
addNStr s n = mkForeign (FFun "addnstr" [FString, FInt] FUnit) s (toIntNat n)

||| Prints a String to the standard screen at current cursor position.
||| The cursor will be advanced after printing.
||| @s the string that will be printed
abstract
addStr : (s: String) -> IO ()
addStr s = mkForeign (FFun "addstr" [FString] FUnit) s

||| Prints a character to the standard screen at current cursor position.
||| The cursor will be advanced after printing.
||| @c the character that will be printed
abstract
addCh : (c : Char) -> IO ()
addCh c = mkForeign (FFun "addch" [FChar] FUnit) c

||| Clears the screen to the end of the line.
abstract
clrToEol : IO ()
clrToEol = mkForeign (FFun "clrtoeol" [] FUnit)

||| Moves the cursor to the specified coordinates.
||| @y the line to which the cursor will move (counting starts at 0)
||| @x the column to which the cursor will move (counting starts at 0)
abstract
move : (y : Int) -> (x : Int) -> IO ()
move y x = mkForeign (FFun "move" [FInt, FInt] FUnit) y x

curs_set : Int -> IO Int
curs_set x = mkForeign (FFun "curs_set" [FInt] FInt) x

||| Sets the cursor state. Returns `Nothing`, if the specified cursor state
||| cannot be set, and `Just` the previous cursor state otherwise.
||| @cs the cursor state that will be set
abstract
cursSet : (cs : CursorState) -> IO $ Maybe CursorState
cursSet Invisible = leaveOk True  $> curs_set 0 >>= return . intToCursorState
cursSet cs = leaveOk False $> curs_set (cursorStateToInt cs) >>=
  return . intToCursorState

||| Returns the current cursor position.
||| The format is `(line, column)`.
abstract
getYX : IO (Int, Int)
getYX = do
  (MkWindow scr) <- stdScr
  [| MkPair (mkForeign (FFun "getY" [FPtr] FInt) scr)
            (mkForeign (FFun "getX" [FPtr] FInt) scr) |]

||| Returns the highest line and column the cursor can be at.
||| The format is `(line, column)`.
abstract
getMaxYX : IO (Int, Int)
getMaxYX = scrSize >>= \(row, col) => return (row - 1, col - 1)

getch : IO Int
getch = mkForeign (FFun "getch" [] FInt)

||| Clears the screen.
abstract
clear : IO ()
clear = mkForeign (FFun "clear" [] FUnit)

||| Sets all given attributes and the specified colors.
||| @attrs  the attributes that will be set
||| @colors if this is not `Nothing`, the first color of the pair will be set
|||           as foreground color and the second color of the pair will be set
|||           as background color
abstract
setAttrAndColor : (attrs : List Attr) -> (colors : Maybe (Color, Color)) ->
  IO ()
setAttrAndColor as c = do when (isJust c) $ initPair (colorPair c)
                          setAttr $ combineAttr (256 * cBool (isJust c)) as
  where
    combineAttr : Int -> List Attr -> Int
    combineAttr col = foldr prim__orInt col . map attrToInt
    colorPair : Maybe (Color, Color) -> ColorPair
    colorPair (Just (fg, bg)) = MkColorPair 1 fg bg

||| Advances the cursor to the next position, if possible.
abstract
moveNextCh : IO ()
moveNextCh = do
  (maxY, maxX) <- getMaxYX
  (y, x) <- getYX
  let (newY, newX) = if x >= maxX
    then if y >= maxY then (maxY, maxX) else (y + 1, 0)
    else (y, x + 1)
  move newY newX

||| Advances the cursor to the previous position, if possible.
abstract
movePrevCh : IO ()
movePrevCh = do
  (maxY, maxX) <- getMaxYX
  (y, x) <- getYX
  let (newY, newX) = if x <= 0
    then if y <= 0 then (0, 0) else (y - 1, maxX)
    else (y, x - 1)
  move newY newX

||| Returns `Just` a character, or `Nothing` if the `GetChMode` is `Wait k` and 
||| the time runs out. See `GetChMode` for more information.
abstract
getCh : IO $ Maybe Char
getCh = do
  refresh
  v <- getch
  case v of
    (-1) => return Nothing
    c    => return . return $ chr c

||| Returns a character once the user presses a key. This function is affected
||| by whether or not the `GetChMode` is a "raw" mode.
abstract
forceCh : IO Char
forceCh = do
  refresh
  v <- getch
  case v of
    (-1) => forceCh
    c    => return $ chr c

||| Returns a `String` the user enters. This function is affected by whether or
||| not the `GetChMode` is a "raw" mode.
||| @useEcho  if `True`, the user will see the String they enter
||| @setEcho  if `True`, echo will be on after the String has been returned,
|||             otherwise, echo will be off
abstract
getStr : (useEcho : Bool) -> (setEcho : Bool) -> IO String
getStr useEcho setEcho = do
    echo useEcho
    (y, x) <- getYX
    (maxY, maxX) <- scrSize
    str <- map reverse $ getRawStr y x ""
    echo setEcho
    return str
  where
    getRawStr : Int -> Int -> String -> IO String
    getRawStr initY initX str = do
      (preY, preX) <- getYX
      c <- forceCh
      (y, x) <- getYX
      case c of
        specialChar Enter     => move preY preX $> return str
        specialChar Backspace => if y <= initY && x < initX
          then move preY preX $> getRawStr initY initX str
          else do if (preY, preX) == (y, x)
                    then movePrevCh $> addStr " " $> movePrevCh
                    else addStr " " $> move y x
                  getRawStr initY initX $ strTail str
        char                  => getRawStr initY initX $ strCons char str

||| Sets the mode `getCh` will operate in.
abstract
setGetChMode : GetChMode -> IO ()
setGetChMode WaitForever           = raw False $> cBreak True  $> noDelay False
setGetChMode WaitForeverRaw        = raw True                  $> noDelay False
setGetChMode (Wait Z)              = raw False $> cBreak True  $> noDelay True
setGetChMode (Wait (S k))          = halfDelay (toIntNat k)    $> noDelay False
setGetChMode WaitForeverLinebuf    = raw False $> cBreak False $> noDelay False
setGetChMode WaitForeverLinebufRaw = raw True  $> cBreak False $> noDelay False

||| Use this function to run curses.
||| @enableColors whether colors should be enabled or not (note that some
|||                 terminals may not support colors)
||| @getChMode    the mode that `getCh` will use
||| @actions      the IO action that curses will run , `runCurses` will return
|||                 the result of these actions
abstract
runCurses : (enableColors : Bool) -> (getChMode : GetChMode) ->
  (action : IO a) -> IO a
runCurses enableColors getChMode actions = do
  initScr
  when enableColors startColor
  nl True
  echo False
  keypad True
  meta True
  leaveOk False
  setGetChMode getChMode
  result <- actions
  endWin
  return result
