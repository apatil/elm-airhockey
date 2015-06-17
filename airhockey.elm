 -- See this document for more information on making Pong:
-- http://elm-lang.org/blog/pong
import Color exposing (rgb, white, black)
import Graphics.Collage exposing (collage, oval, rect, filled, toForm, move)
import Graphics.Element exposing (container, middle, Element, spacer, leftAligned)
import BoxesAndBubblesBodies as BBB
import BoxesAndBubbles as BB
import BoxesAndBubblesEngine as BBE
import Keyboard
import Text
import Time exposing (Time, inSeconds, fps)
import Window
import Mouse
import Maybe
import Debug
import Maybe
import Signal exposing ((<~), (~))


-- Config

(gameWidth,gameHeight) = (600.0,400.0)
goalHeight = 100.0
paddleSize = 30.0
puckSize = 20.0
-- Make this huge to prevent the puck from going through the bounds.
boundsWidth = 10000.0
maxVelocity = 20.0

-- Model

(halfWidth, halfHeight) = (gameWidth / 2, gameHeight / 2)
halfGoal = goalHeight / 2
inf = 1/0

-- Default restitution coefficient
e0 = 0.8

type State = Play | Pause
type Side = Left | Right | Neither

type alias Vec2 = (Float, Float)

type alias Player =
  -- Record the last two timesteps of the player's paddle for
  -- easy computation of velocity.
  { paddle: {current: BBB.Body {}, last: BBB.Body {}}, score: Int }

type alias Game =
  { state : State
  , puck : BBB.Body {}
  , player1 : Player
  , player2 : Player
  }

type alias Input =
  { space : Bool
  , pos1 : Vec2
  , pos2 : Vec2
  , dt : Time
  }

getPaddle pos v = BB.bubble paddleSize inf e0 pos v
defaultPaddle side =
  case side of
    Left -> getPaddle (-gameWidth / 4,0) (0,0)
    Right -> getPaddle (gameWidth/4,0) (0,0)
newPlayer : Side -> Player
newPlayer side =
  {paddle={current=defaultPaddle side, last=defaultPaddle side}, score=0}
newPlayer1 = newPlayer Left
newPlayer2 = newPlayer Right

boundBox = {velocity=(0,0), inverseMass=0,restitution=e0, pos=(0,0), shape=BBB.Box (0,0)}
bounds = [
    -- top
    { boundBox | pos <- (0, (gameHeight + boundsWidth*2)/2), shape <- BBB.Box (gameWidth + boundsWidth, boundsWidth) },
    -- bottom
    { boundBox | pos <- (0, -(gameHeight + boundsWidth*2)/2), shape <- BBB.Box (gameWidth + boundsWidth, boundsWidth) }
  ]
  -- left
  ++ [
    -- Above goal
    { boundBox | pos <- (-(gameWidth + boundsWidth*2)/2, (gameHeight + goalHeight) / 4), shape <- BBB.Box (boundsWidth, (gameHeight - goalHeight) / 4) },
    -- Below goal
    { boundBox | pos <- (-(gameWidth + boundsWidth*2)/2, -(goalHeight + gameHeight) / 4), shape <- BBB.Box (boundsWidth, (gameHeight - goalHeight) / 4) }
  ]
  -- right
  ++ [
    -- Above goal
    { boundBox | pos <- ((gameWidth + boundsWidth*2)/2, (gameHeight + goalHeight) / 4), shape <- BBB.Box (boundsWidth, (gameHeight - goalHeight) / 4) },
    -- Below goal
    { boundBox | pos <- ((gameWidth + boundsWidth*2)/2, -(goalHeight + gameHeight) / 4), shape <- BBB.Box (boundsWidth, (gameHeight - goalHeight) / 4) }
  ]

defaultPuck = BB.bubble puckSize 10 e0 (0,0) (0,0)
defaultGame : Game
defaultGame =
  { state = Pause
  , puck =  defaultPuck
  , player1 = newPlayer Left
  , player2 = newPlayer Right
  }

inGoal puck side =
  case side of
    Left -> (fst puck.pos) < -halfWidth
    Right -> (fst puck.pos) > halfWidth

update : Input -> Game -> Game
update {space,pos1,pos2,dt} ({state,puck,player1,player2} as game) =
  let
    score1 =
      if inGoal puck Right then 1 else 0

    score2 =
      if inGoal puck Left then 1 else 0

    newState =
      if
        | space -> Play
        --| score1 /= score2 -> Pause
        | otherwise -> state

    newPlayer1 = case state of
      Pause -> player1
      _ -> updatePlayer dt Right pos1 player1

    newPlayer2 = case state of
      Pause -> player2
      _ -> updatePlayer dt Left pos2 player2

    blarg = Debug.watch "hi" {score1=score1, score2=score2, puck=puck}

    newPuck =
      --puck
      if
        | score1 /= score2 -> defaultPuck
        | state == Pause -> puck
        | otherwise -> updatePuck dt puck ([newPlayer1.paddle.current, newPlayer2.paddle.current] ++ bounds)
  in
    {
      state = newState,
      puck = newPuck,
      player1 = {newPlayer1 | score <- player1.score + score1},
      player2 = {newPlayer2 | score <- player2.score + score2}
    }

scale2 s (x,y) = (s * x, s * y)
diff2 (x,y) (z,w) = (x-z, y-w)

capVelocity maxV (vx, vy) =
  let 
    absV2 = (vx * vx) + (vy * vy)
    maxV2 = maxV * maxV
  in
    if 
      | maxV2 > absV2 -> (vx, vy)
      | otherwise -> let scale = sqrt (maxV2 / absV2)
        in
          (vx * scale, vy * scale)


updatePuck : Time -> BBB.Body {} -> List (BBB.Body {}) -> BBB.Body {}
updatePuck dt puck bodies =
  case List.head <| BBE.collideWith puck bodies [] of
    Just newPuck -> 
      BBE.update (0,0) (0,0) {newPuck | velocity <- capVelocity maxVelocity newPuck.velocity}
    -- This should never happen, but let's be thorough.
    Nothing -> defaultPuck

clampInGame : Side -> Float -> (Float, Float) -> (Float, Float)
clampInGame side size pos =
  -- Make sure the paddle doesn't penetrate the walls or go out of bounds.
  let
    x = case side of
      Left -> clamp (-gameWidth / 2 + size) -size (fst pos)
      Right -> clamp size (gameWidth / 2 - size) (fst pos)
      Neither -> clamp (-gameWidth / 2 + size) (gameWidth / 2 - size) (fst pos)
    y = clamp (-gameHeight / 2 + size) (gameHeight / 2 - size) (snd pos) 
  in
    (x, y)
    --pos

updatePlayer : Time -> Side -> (Float, Float) -> Player -> Player
updatePlayer dt side pos player =
  let
    current = player.paddle.current
    last = player.paddle.last
    currentPos = clampInGame side paddleSize pos
    paddle = {
      last=current,
      current={ current |
        pos <- currentPos,
        velocity <- scale2 (1 / dt) <| diff2 current.pos last.pos
      }
    }
  in
  { player | paddle <- paddle }

-- View

view : (Int,Int) -> Game -> Element
view (w,h) {state,puck,player1,player2} =
  let
    scores =
      txt (Text.height 50) (toString player1.score ++ "  " ++ toString player2.score)
  in
    container w h middle <|
    collage (floor gameWidth) (floor gameHeight)
      [ rect gameWidth gameHeight
          |> filled rinkBlue
        , rect 5 gameHeight 
          |> filled textBlue
        , rect 5 goalHeight
          |> filled goalRed
          |> move (2.5 - gameWidth/2, 0)
        , rect 5 goalHeight
          |> filled goalRed
          |> move (gameWidth/2 - 2.5, 0)
        , viewBB puck puckYellow
        , viewBB player1.paddle.current white
        , viewBB player2.paddle.current white
        , toForm scores
            |> move (0, gameHeight/2 - 40)
        , toForm (if state == Play then spacer 1 1 else txt identity msg)
            |> move (0, 40 - gameHeight/2)
        ]

rinkBlue =
  rgb 15 185 215

textBlue =
  rgb 10 95 205

goalRed =
  rgb 240 10 95

puckYellow =
  rgb 250 250 200

txt f string =
  Text.fromString string
    |> Text.color textBlue
    |> Text.monospace
    |> f
    |> leftAligned

msg = "SPACE to start, WS and &uarr;&darr; to move"

viewBB obj color =
  let shape = case obj.shape of
    BBB.Box (w, h) -> rect (w * 2) (h * 2)
    BBB.Bubble r -> oval (r * 2) (r * 2)
  in
    shape
      |> filled color
      |> move obj.pos

-- SIGNALS

input : Signal Input
input =
  Signal.sampleOn dt <|
    Signal.map4 Input
      Keyboard.space
      (mouseToPos <~ Mouse.position ~ Window.dimensions)
      (Signal.constant (-gameWidth/4, 0.0))
      dt

gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input

dt =
  Signal.map inSeconds (fps 45)

mouseToPos (x,y) (w,h) =
  (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)

-- FIXME: Mouse position is at (0,0) in upper left corner and like (1400, 800)
-- in lower right. How to relate to intended position of paddle?

-- FIXME: The x and y coordinate of the moving player's paddle are the same right now.

-- FIXME: Players should not be able to move before space bar is pressed.

-- FIXME: The non-moving player starts way off center.

main =
  Signal.map2 view Window.dimensions gameState
