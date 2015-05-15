import AABB
import AABB (..)
import Vec2
import Vec2 (..)
import Text (asText)
import Text
import Graphics.Collage (..)
import Graphics.Collage
import Graphics.Element (..)
import Color (..)
import Window
import Signal (..)
import Time
import Keyboard
import List
import Array

type alias Sprite =
    { box : AABB
    , velocity : Vec2
    , angle : Float
    , rotateSpeed : Float
    , moveAngle : Float
    , speed : Float
    , image : String
    , isPlayer : Bool
    , isBullet : Bool
    , isAsteroid : Bool
    }

type alias State =
    { sprites : List Sprite , winText : Element, cooldown : Int }

startingSprites : List Sprite
startingSprites =
    [
    { box = {position = (-200, -200), size = (20, 20) }
    , velocity = (0, 0)
    , angle = 0
    , rotateSpeed = 2
    , moveAngle = 0
    , speed = 0
    , image = "img/ship.png"
    , isPlayer = True
    , isBullet = False
    , isAsteroid = False
    }
    ] ++
    List.map (\i -> addAsteroid (25, 25) (get (i+3) randArray, get (i+4) randArray) i) [0..3]

pseudo : number -> number -> number -> number
pseudo i seed mag =
          ( (sin (seed * i^2))^2 / 3
          + (sin (seed * i^5))^2 / 3
          + (sin (seed * i^7))^2 / 3) * mag

randList : List number
randList = List.map (\i -> pseudo i 4812 300) [1..1000]

randArray : Array.Array number
randArray = Array.fromList randList

cooldown : number
cooldown = 50

get index array = case (Array.get index array) of
    Just val -> val

addAsteroid : Vec2 -> Vec2 -> number -> Sprite
addAsteroid size pos i =
    let randAngle = get i randArray
        randMoveAngle = get (i+1) randArray
        randSpeed = get (i+2) randArray
    in
    { box = {position = pos, size = size }
    , velocity = (0, 0)
    , angle = randAngle
    , rotateSpeed = 2
    , moveAngle = randMoveAngle
    , speed = randSpeed
    , image = "img/asteroid.png"
    , isPlayer = False
    , isBullet = False
    , isAsteroid = True
    }

addBullet : Sprite -> Sprite
addBullet player =
    let (x, y) = player.box.position
        angle = player.angle
    in
       { box = {position = (x, y), size = (1, 5) }
       , velocity = (0, 0)
       , angle = angle
       , rotateSpeed = 0
       , moveAngle = angle
       , speed = 1000
       , image = "img/bullet.png"
       , isPlayer = False
       , isBullet = True
       , isAsteroid = False
       }

acceleration : number
acceleration = 2

maxSpeed : number
maxSpeed = 4

update : Input -> State -> State
update input state =
    let spritesArray = Array.fromList state.sprites
        player = get 0 spritesArray
        sprites' = List.map (\x -> updateSprite input x state) state.sprites ++ (if input.space && (state.cooldown <= 0) && (fst player.box.size) > 0 then [addBullet player] else List.foldr (++) [] (List.map (\x -> if collide state.sprites x && (fst x.box.size) > 15 then [addAsteroid (fst x.box.size / 1.5, snd x.box.size / 1.5) x.box.position (clamp (round (fst x.box.position)) 10 90)] else []) state.sprites))
    in
    { state |
        sprites <- sprites'
        , cooldown <- if input.space && state.cooldown <= 0 then cooldown else state.cooldown - 1
        , winText <- if (fst player.box.size) == 0 then whiteText "You lose" else if not (contains sprites' (\x -> x.isAsteroid && not ((fst x.box.size) == 0))) then whiteText "You win" else whiteText ""
    }

updateSprite : Input -> Sprite -> State -> Sprite
updateSprite input sprite state =
    let dt = input.dt / 1000
        (w, h) = Vec2.map toFloat input.dimensions
        arrows = if sprite.isPlayer then (toFloat input.arrows.x, toFloat input.arrows.y) else (1, 0)
        velocity = (sprite.speed * (cos (degrees sprite.moveAngle)), sprite.speed * (sin (degrees sprite.moveAngle)))
        (x, y) = sprite.box.position
        translateBox = if not sprite.isBullet then wrap sprite w h else sprite.box
        sprite' =
            { sprite
            | box <- {
                position = (AABB.translate (dt *> (sprite.velocity)) translateBox).position
                , size = if collide state.sprites sprite then if sprite.isAsteroid && (fst sprite.box.size) > 15 then (fst sprite.box.size / 1.5, snd sprite.box.size / 1.5) else (0, 0) else sprite.box.size
                }
            , velocity <- sprite.velocity <+> velocity
            , angle <- sprite.angle - (fst arrows * sprite.rotateSpeed)
            , speed <- if (snd arrows) == 1 then clamp 0 maxSpeed 2 else clamp 0 maxSpeed -2
            , moveAngle <- if (snd arrows) == 1 then sprite.angle else sprite.moveAngle
            }
    in sprite'

wrap : Sprite -> Float -> Float -> AABB
wrap sprite w h = 
    let (x, y) = sprite.box.position
    in
    {position = ((if x > w/2 then -w/2 else if x < -w/2 then w/2 else x), (if y > h/2 then -h/2 else if y < -h/2 then h/2 else y)), size = sprite.box.size}

collide : List Sprite -> Sprite -> Bool
collide sprites sprite =
    contains sprites (\x -> AABB.intersects sprite.box x.box && not (x.box == sprite.box) && not (sprite.isBullet && x.isPlayer) && not (sprite.isAsteroid && x.isAsteroid) && not (sprite.isPlayer && x.isBullet))

contains : List a -> (a -> Bool) -> Bool
contains list f = List.length (fst (List.partition (f) list)) > 0

view (w, h) state =
    let sprites = state.sprites
    in
    color black <| collage w h <|
    List.map (\x -> AABB.toForm x.image x.angle x.box) sprites ++ [Graphics.Collage.toForm state.winText]

type alias Input = { dt : Time.Time, arrows : { x : Int, y : Int }, space : Bool, dimensions : (Int, Int)}

framerate = Time.fps 60

space : Signal Bool
space = foldp (\x y -> if x then True else False) False Keyboard.space

input : Signal Input
input = Input <~ framerate ~ Keyboard.arrows ~ space ~ Window.dimensions

state : State
state = State startingSprites (whiteText "") 0

whiteText : String -> Element
whiteText text = Text.leftAligned <| Text.color white <| Text.fromString text

main = view <~ Window.dimensions ~ foldp update state (sampleOn framerate input)
