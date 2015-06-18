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
import Debug
import Random
import Signal as S exposing ((<~), (~))

-- Config

newGame : Game
newGame = resetBodies Right {
    state = Pause
  , leftPlayer = {side=Left, control=Automatic goalie, score=0}
  , rightPlayer = {side=Right, control=Manual, score=0}
  , width = 1200.0
  , height = 800.0
  , goalHeight = 100.0
  , paddleSize = 30.0
  , puckSize = 20.0
  , puckDensity = 10.0
  , maxPuckVelocity = 1000.0
  , maxAutoPlayerVelocity = 200.0
  , e0 = 0.8
  , bodies = {
      leftPlayer = placeholderBody,
      rightPlayer = placeholderBody,
      puck = placeholderBody
    }
  }


-- Computer player strategies (none very good so far)

wall : Strategy
wall dt game side player opponent puck =
  let (x,y) = puck.pos
  in
    case side of
      Left -> (-game.width / 3, y)
      Right -> (game.width / 3, y)
      _ -> (0, y)

follower : Strategy
follower dt game side player opponent puck =
  puck.pos

goalie: Strategy
goalie dt game side player opponent puck =
  let
    rTarg = 200.0
    goalCtr = (-game.width/2, 0)
    (x,y) = diff2 puck.pos goalCtr
    scale = sqrt <| (rTarg * rTarg /(x*x + y*y))
  in
    axpy2 scale (x,y) goalCtr


-- Air hockey type defs

type State = Play | Pause
type Side = Left | Right | Neither

type alias Vec2 = (Float, Float)

type alias Player =
  -- Record the last two timesteps of the player's paddle for
  -- easy computation of velocity.
  {score: Int, control: Control, side: Side }

type alias Game =
  { state : State
  , leftPlayer : Player
  , rightPlayer : Player
  , width : Float
  , height : Float
  , goalHeight : Float
  , paddleSize : Float
  , puckSize : Float
  , puckDensity: Float
  , maxPuckVelocity : Float
  , maxAutoPlayerVelocity : Float
  , e0 : Float
  , bodies : {
      leftPlayer : BBB.Body {},
      rightPlayer : BBB.Body {},
      puck : BBB.Body {}
    }
  }

type alias Input =
  {
    space : Bool
  , pos : Vec2
  , dt : Time
  }

type alias Strategy = Float -> Game -> Side -> BBB.Body {} -> BBB.Body {} -> BBB.Body {} -> (Float, Float)

type Control = Automatic Strategy | Manual


-- Model

placeholderBody = BB.bubble 0 0 0 (0,0) (0,0)

defaultPaddle : Side -> Game -> (Float, Float)
defaultPaddle side game =
  case side of
    Left -> (-game.width / 3, 0)
    Right ->(game.width / 3, 0)

defaultPuck : Side -> Game -> (Float, Float)
defaultPuck side game =
  case side of
    Left -> (-game.width / 6, 0)
    Right -> (game.width / 6, 0)

resetBodies : Side -> Game -> Game
resetBodies puckSide game =
  let
    leftPlayerPos = defaultPaddle game.leftPlayer.side game
    rightPlayerPos = defaultPaddle game.rightPlayer.side game
    puckPos = defaultPuck puckSide game
    bodies = {
      leftPlayer=BB.bubble game.paddleSize inf game.e0 leftPlayerPos (0,0),
      rightPlayer=BB.bubble game.paddleSize inf game.e0 rightPlayerPos (0,0),
      puck=BB.bubble game.puckSize game.puckDensity game.e0 puckPos (0,0)
    }
  in
    {game | bodies <- bodies}

-- Non-configurable constants
inf = 1/0
boundsWidth = 10000.0

scale2 s (x,y) = (s * x, s * y)
diff2 (x,y) (z,w) = (x-z, y-w)
add2 (x,y) (z,w) = (x+z, y+w)
axpy2 : Float -> (Float, Float) -> (Float, Float) -> (Float, Float)
axpy2 a (x,y) (z,w) = (a*x + z, a*y + w)

boundBox = {velocity=(0,0), inverseMass=0,restitution=newGame.e0, pos=(0,0), shape=BBB.Box (0,0)}
bounds : List (BBB.Body {})
bounds = [
    -- top
    { boundBox | pos <- (0, (newGame.height + boundsWidth*2)/2), shape <- BBB.Box (newGame.width + boundsWidth, boundsWidth) },
    -- bottom
    { boundBox | pos <- (0, -(newGame.height + boundsWidth*2)/2), shape <- BBB.Box (newGame.width + boundsWidth, boundsWidth) }
  ]
  -- left
  ++ [
    -- Above goal
    { boundBox | pos <- (-(newGame.width + boundsWidth*2)/2, (newGame.height + newGame.goalHeight) / 4), shape <- BBB.Box (boundsWidth, (newGame.height - newGame.goalHeight) / 4) },
    -- Below goal
    { boundBox | pos <- (-(newGame.width + boundsWidth*2)/2, -(newGame.goalHeight + newGame.height) / 4), shape <- BBB.Box (boundsWidth, (newGame.height - newGame.goalHeight) / 4) }
  ]
  -- right
  ++ [
    -- Above goal
    { boundBox | pos <- ((newGame.width + boundsWidth*2)/2, (newGame.height + newGame.goalHeight) / 4), shape <- BBB.Box (boundsWidth, (newGame.height - newGame.goalHeight) / 4) },
    -- Below goal
    { boundBox | pos <- ((newGame.width + boundsWidth*2)/2, -(newGame.goalHeight + newGame.height) / 4), shape <- BBB.Box (boundsWidth, (newGame.height - newGame.goalHeight) / 4) }
  ]

inGoal : Game -> Side
inGoal game =
  if
    | (fst game.bodies.puck.pos) < -game.width / 2 -> Left
    | (fst game.bodies.puck.pos) > game.width / 2 -> Right
    | otherwise -> Neither

incrementScore : Player -> Player
incrementScore player = {player | score <- player.score + 1}

capVelocity : Float -> BBB.Body {} -> BBB.Body {}
capVelocity maxV body =
  -- Make sure neither players nor puck leave the rink or exceed their speed limits.
  let
    (vx, vy) = body.velocity
    absV2 = (vx * vx) + (vy * vy)
    maxV2 = maxV * maxV
  in
    if
      | maxV2 > absV2 -> body
      | otherwise -> let
          scale = sqrt (maxV2 / absV2)
        in
          {body | velocity <- (vx * scale, vy * scale)}

clampInGame : Side -> Game -> BBB.Body {} -> BBB.Body {}
clampInGame side game body =
  -- Make sure an object doesn't penetrate the walls or go out of bounds.
  case body.shape of
    BBB.Bubble size -> let
        x = case side of
          Left -> clamp (-game.width / 2 + size) -size (fst body.pos)
          Right -> clamp size (game.width / 2 - size) (fst body.pos)
          Neither -> clamp (-game.width / 2 + size) (game.width / 2 - size) (fst body.pos)
        y = clamp (-game.height / 2 + size) (game.height / 2 - size) (snd body.pos)
      in
        {body | pos <- (x, y)}
    -- This case is not supported right now.
    BBB.Box (w,h) -> body

updatePuck : Input -> Game -> BBB.Body {}
updatePuck input game =
  case List.head <| BBE.collideWith game.bodies.puck ([game.bodies.leftPlayer, game.bodies.rightPlayer] ++ bounds) [] of
    Just newPuck -> let
        capped = capVelocity game.maxPuckVelocity newPuck
        newPos = axpy2 input.dt capped.velocity capped.pos
        updated = {capped | pos <- newPos, velocity <- capped.velocity}
      in
        updated
    -- This should never happen, but let's be thorough.
    Nothing -> game.bodies.puck

updatePlayer : Input -> Side -> Game -> BBB.Body {}
updatePlayer input side game =
  let
    bodies = game.bodies
    (lastPlayer, opponent, control) = case side of
      Left -> (bodies.leftPlayer, bodies.rightPlayer, game.leftPlayer.control)
      Right -> (bodies.rightPlayer, bodies.rightPlayer, game.rightPlayer.control)
  in
    case control of
       Manual -> clampInGame side game <| updatePlayerPosition lastPlayer input.dt input.pos
       Automatic strategy ->
         let
           attemptedPos = strategy input.dt game side lastPlayer opponent bodies.puck
           player = updatePlayerPosition lastPlayer input.dt attemptedPos
           capped = capVelocity game.maxAutoPlayerVelocity player
           newPos = axpy2 input.dt capped.velocity lastPlayer.pos
         in
           clampInGame side game  {player | pos <- newPos, velocity <- capped.velocity}

updatePlayerPosition : BBB.Body {} -> Float ->  (Float, Float) -> BBB.Body {}
updatePlayerPosition lastPlayer dt pos =
  {lastPlayer | pos <- pos, velocity <- scale2 (1 / dt) <| diff2 pos lastPlayer.pos}

updateBodies : Input -> Game -> Game
updateBodies input game =
  let
    newLP = updatePlayer input Left game
    newRP = updatePlayer input Right game
    newPuck = updatePuck input game
  in
    {game | bodies <- {puck=newPuck, leftPlayer=newLP, rightPlayer=newRP}}

update : Input -> Game -> Game
update input game =
  case inGoal game of
    Left -> resetBodies Left {game | rightPlayer <- incrementScore game.rightPlayer}
    Right -> resetBodies Right {game | leftPlayer <- incrementScore game.leftPlayer}
    otherwise ->
      case game.state of
        Pause -> if input.space then {game | state <- Play} else game
        Play -> updateBodies input game

-- View

view : (Int,Int) -> Game -> Element
view (w,h) {state,width,height,goalHeight,leftPlayer,rightPlayer,bodies} =
  let
    scores =
      txt (Text.height 50) (toString leftPlayer.score ++ "  " ++ toString rightPlayer.score)
  in
    container w h middle <|
    collage (floor width) (floor height)
      [ rect width height
          |> filled rinkBlue
        , rect 5 height
          |> filled textBlue
        , rect 5 goalHeight
          |> filled goalRed
          |> move (2.5 - width/2, 0)
        , rect 5 goalHeight
          |> filled goalRed
          |> move (width/2 - 2.5, 0)
        , viewBB bodies.puck puckYellow
        , viewBB bodies.leftPlayer white
        , viewBB bodies.rightPlayer white
        , toForm scores
            |> move (0, height/2 - 40)
        , toForm (if state == Play then spacer 1 1 else txt identity msg)
            |> move (0, 40 - height/2)
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

msg = "SPACE to start, move paddle with mouse"

viewBB obj color =
  let shape = case obj.shape of
    BBB.Box (w, h) -> rect (w * 2) (h * 2)
    BBB.Bubble r -> oval (r * 2) (r * 2)
  in
    shape
      |> filled color
      |> move obj.pos

-- SIGNALS

dt =
  Signal.map inSeconds (fps 45)

input : Signal Input
input =
  Signal.sampleOn dt <|
    Signal.map3 Input
      Keyboard.space
      (mouseToPos <~ Mouse.position ~ Window.dimensions)
      dt

gameState : Signal Game
gameState =
  Signal.foldp update newGame input


mouseToPos (x,y) (w,h) =
  (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)

main =
  Signal.map2 view Window.dimensions gameState
