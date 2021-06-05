import CodeWorld

botCircle, midCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
midCircle c = colored c (solidCircle 1)
topCircle c = colored c (translated 0 2.5 (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

data TrafficLightPhase = Red | Amber | Green | RedAmber

trafficLightState :: Color -> Color -> Color -> Picture
trafficLightState botColor midColor topColor = botCircle botColor & midCircle midColor & topCircle topColor & frame

trafficLight :: TrafficLightPhase -> Picture
trafficLight Green  = trafficLightState green black black
trafficLight Amber  = trafficLightState black yellow black
trafficLight Red = trafficLightState black black red
trafficLight RedAmber = trafficLightState black yellow red


trafficController :: Int -> Picture
trafficController t
  | t >= 0 && t <= 2 = trafficLight Red
  | t == 3           = trafficLight RedAmber
  | t >= 4 && t <= 6 = trafficLight Green
  | otherwise        = trafficLight Amber

trafficLightAnimation :: Double -> Picture
trafficLightAnimation t = trafficController (round t `mod` 8)

main :: IO ()
main = animationOf trafficLightAnimation
