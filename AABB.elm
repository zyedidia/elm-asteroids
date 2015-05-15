module AABB where

import Vec2 (..)
import Graphics.Collage (..)
import Graphics.Collage
import Graphics.Element (..)

type alias AABB = { position: Vec2, size: Vec2 }

toForm : String -> number -> AABB -> Form
toForm img angle box = 
    let (w, h) = 2 *> box.size
    in move box.position (rotate (degrees (angle - 90)) (Graphics.Collage.toForm (image w h img)))

translate : Vec2 -> AABB -> AABB
translate amount box = { box | position <- box.position <+> amount }

intersects : AABB -> AABB -> Bool
intersects box1 box2 =
    let (x1, y1) = box1.position
        (w1, h1) = box1.size
        (x2, y2) = box2.position
        (w2, h2) = box2.size
        r1 = x1 + w1
        l1 = x1 - w1
        t1 = y1 + h1
        b1 = y1 - h1
        r2 = x2 + w2
        l2 = x2 - w2
        t2 = y2 + h2
        b2 = y2 - h2
    in
    if w1 == 0 || w2 == 0 then False else (r1 > l2 && r2 > l1) && (t1 > b2 && t2 > b1)

corners : AABB -> (Vec2, Vec2)
corners box = (box.position <-> box.size, box.position <+> box.size)
