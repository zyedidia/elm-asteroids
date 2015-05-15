module Vec2 where

infixl 4 <+>
infixl 4 <->
infixl 5 <*>
infixl 5 </>
infixl 6 *>

type alias Vec2 = (Float, Float)

(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)
(x1, y1) <-> (x2, y2) = (x1 - x2, y1 - y2)
(x1, y1) <*> (x2, y2) = (x1 * x2, y1 * y2)
(x1, y1) </> (x2, y2) = (x1 / x2, y1 / y2)

f *> (x, y) = (f * x, f * y)

normalize (x, y) =
    let length = sqrt(x^2 + y^2)
    in if length == 0 then (0, 0) else 
          (x / length, y / length)

map f (x, y) = (f x, f y)
