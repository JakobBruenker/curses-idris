-- Prints out some things, tries using color, and asks the user something.
-- compile with `idris -p effects -p curses -o testE testE.idr

module Main

import Effects
import Effect.Curses

test : { [CURSES Pre] ==> [CURSES Post] } Eff ()
test = do start WaitForever
          addStr "Welcome to testE\nPress any key to continue"
          forceCh
          True <- startColor | False => noColor
          setAttrAndColor [Bold, Underline] $ Just (MkColorPair 1 Red Blue)
          clear
          (lin, _) <- scrSize
          move (lin `div` 2) 0
          addStr "This is colorful text with some attributes."
          forceCh
          setAttrAndColor [] Nothing
          loop
          end

   where noColor : { [CURSES $ Active False] ==> [CURSES Post] } Eff ()
         noColor = do addStr "\nYour terminal doesn't support color!"
                      addStr "\nThe program will terminate now!"
                      end
         loop : { [CURSES $ Active True] } Eff ()
         loop = do clear
                   addStr "Please write \"quit\" and press enter: "
                   when (!(getStr True False) /= "quit") loop

main : IO ()
main = run test
