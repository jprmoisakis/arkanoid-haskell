module Main where
    import Graphics.UI.Fungen
  
    data GameAttribute = Score Int
    type PongObject = GameObject ()
    type PongAction a = IOGame GameAttribute () () () a
    
    main :: IO ()
    main = do 
      let winConfig = ((0, 0), (250, 250), "A brief example!")
          bmpList = [("tex.bmp",Nothing)]
          gameMap = textureMap 0 50 50 250.0 250.0
          bar = objectGroup "barGroup"  [createBar]
          ball = objectGroup "ballGroup" [createBall]
          target = objectGroup "targetGroup" createTarget
          initScore = Score 0
          input = [(SpecialKey KeyLeft, StillDown, movePlayerToLeft), (SpecialKey KeyRight, StillDown, movePlayerToRight)]
      funInit winConfig gameMap [bar, ball, target] () initScore input gameCycle (Timer 40) bmpList
  
    createBall :: PongObject
    createBall = let ballPic = Basic (Circle 3.0 0.0 1.0 0.0 Filled)
                  in object "ball" ballPic False (125, 125) (-5, 5) ()
  
    createBar :: PongObject
    createBar = let barBound = [(-25, -6), (25, -6), (25, 6), (-25, 6)]
                    barPic = Basic (Polyg barBound 1.0 1.0 1.0 Filled)
                    in object "bar" barPic False (125, 30) (0,0) ()

    createTarget :: [PongObject]
    createTarget = 
        let barBound = [(-50,-6),(50,-6),(50,6),(-50,6)]
            barPic   = Basic (Polyg barBound 8.0 5.0 1.0 Filled)
        in [(object "target" barPic False (100,100) (0,0) () ),
            (object "target" barPic False (150,150) (0,0) () )]
        
    
    movePlayerToRight :: Modifiers -> Position -> PongAction()
    movePlayerToRight _ _ = do
      obj <- findObject "bar" "barGroup"
      (pX, pY) <- getObjectPosition obj
      (sX, _) <- getObjectSize obj
      if (pX + (sX / 2) + 5 <= 250)
        then (setObjectPosition ((pX + 5), pY) obj)
        else (setObjectPosition ((250 - (sX / 2)), pY) obj)
    
    movePlayerToLeft :: Modifiers -> Position -> PongAction ()
    movePlayerToLeft _ _ = do
      obj <- findObject "bar" "barGroup"
      (pX, pY) <- getObjectPosition obj
      (sX, _) <- getObjectSize obj
      if (pX - (sX / 2) - 5 >= 0)
        then (setObjectPosition ((pX - 5), pY) obj)
        else (setObjectPosition ((sX / 2), pY) obj)
  
    
    gameCycle :: PongAction ()
    gameCycle = do
      (Score n) <- getGameAttribute
      printOnScreen (show n) TimesRoman24 (0,0) 1.0 1.0 1.0
      ball <- findObject "ball" "ballGroup"
      bar <- findObject "bar" "barGroup"
      target <- findObject "target" "targetGroup"
      colTop <- objectTopMapCollision ball
      colRight <- objectRightMapCollision ball
      colLeft <- objectLeftMapCollision ball
      colBottom <- objectBottomMapCollision ball
      colBallBar <- objectsCollision ball bar

      when colTop (reverseYSpeed ball)
      when colRight (reverseXSpeed ball)
      when colLeft (reverseXSpeed ball)
      when colBallBar (reverseYSpeed ball)
      when colBottom (funExit)
