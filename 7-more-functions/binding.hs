bindExp :: Integer -> String
bindExp x =
  let y = 5 in
  let z = y + x in
    "the integer was: "
    ++ show x ++ " and y was: "
    ++ show y ++ " and z was: " ++ show z