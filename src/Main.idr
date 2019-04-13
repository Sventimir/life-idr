module Main

import Control.Monad.State
import Data.SortedSet as S
import Graphics.SDL
import Life


record Game where
    constructor MkGame
    canvas : SDLSurface
    winSize : (Int, Int)
    anchor : Cell -- The cell in upper-right corner of the window
    cellSize : Int
    life : Life

initial : SDLSurface -> Game
initial canvas = MkGame canvas (500, 500) (0, 0) 12 Life.initial

data Zoom = In | Out


zoom : Zoom -> (cellSize : Int) -> Int
zoom Out cellSize = if cellSize > 3 then cellSize - 3 else cellSize
zoom In cellSize = if cellSize < 21 then cellSize + 3 else cellSize


ofWinCoords : (cellSize : Int) -> Cell -> Maybe Cell
ofWinCoords cellSize c =
    if mapCell (flip mod cellSize) c == (0, 0) then
        Nothing
    else
        Just $ mapCell (flip div cellSize) c

drawCell : Cell -> StateT Game IO ()
drawCell (x, y) = do
    c <- gets canvas
    size <- gets cellSize
    (leftmostX, upmostY) <- gets anchor
    lift $ filledRect c
        ((x - leftmostX) * size + 1)
        ((y - upmostY) * size + 1)
        (size - 2)
        (size - 2)
        255 255 255 1

renderState : StateT Game IO ()
renderState = do
    l <- gets life
    c <- gets canvas
    (wx, wy) <- gets winSize
    lift $ filledRect c 0 0 wx wy 0 0 0 1
    sequence_ . map drawCell $ S.toList l
    lift $ flipBuffers c

switchCell : Cell -> StateT Game IO ()
switchCell coords = do
    l <- gets life
    size <- gets cellSize
    (leftmostX, upmostY) <- gets anchor
    case ofWinCoords size coords of
        Nothing => pure ()
        Just (x, y) =>
            let cell = (x + leftmostX, y + upmostY) in
            if contains cell l then
                modify $ record { life = delete cell l }
            else
                modify $ record { life = insert cell l }


eventLoop : () -> StateT Game IO ()
eventLoop () = do
    renderState
    event <- lift pollEvent
    case event of
        Just (KeyUp KeyEsc) => pure ()
        Just (KeyUp KeyUpArrow) => (modify $ record { anchor $= mapRight pred }) >>= eventLoop
        Just (KeyUp KeyDownArrow) => (modify $ record { anchor $= mapRight succ }) >>= eventLoop
        Just (KeyUp KeyLeftArrow) => (modify $ record { anchor $= mapLeft pred }) >>= eventLoop
        Just (KeyUp KeyRightArrow) => (modify $ record { anchor $= mapLeft succ }) >>= eventLoop
        Just (KeyUp (KeyAny 'n')) => (modify $ record { life $= stepForward }) >>= eventLoop
        Just (KeyUp (KeyAny 'i')) => (modify $ record { cellSize $= zoom In }) >>= eventLoop
        Just (KeyUp (KeyAny 'o')) => (modify $ record { cellSize $= zoom Out }) >>= eventLoop
        Just (KeyUp (KeyAny 'c')) => (modify $ record { life = initial }) >>= eventLoop
        Just (MouseButtonUp Left x y) => switchCell (x, y) >>= eventLoop
        _ => eventLoop ()

main : IO ()
main = do
    canvas <- startSDL 500 500
    case canvas of
        Nothing => pure ()
        Just c => (runStateT (eventLoop ()) (initial c)) >>= \_ => pure ()
    endSDL
