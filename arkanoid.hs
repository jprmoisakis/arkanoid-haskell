module Main where
    import Graphics.UI.Fungen
    import Control.Concurrent
    import Control.Concurrent.MVar

    data GameAttribute = Score Int | Ball (Int, Int)
    type PongObject = GameObject ()
    type PongAction a = IOGame GameAttribute () () () a

    addToScore :: MVar GameAttribute -> Int -> IO ()
    addToScore currScore toAdd = do (Score c) <- takeMVar currScore
                                    putMVar currScore (Score(c + 10))

    i = newMVar (Score 0)

    main :: IO ()
    main = do 
      let winConfig = ((0, 0), (460, 460), "Arkanoid")
          bmpList = []
          gameMap = colorMap 0 0 0 460.0 460.0
          bar = objectGroup "barGroup"  [createBar]
          ball = objectGroup "ballGroup" [createBall]
          target = objectGroup "targetGroup" createTarget
          initScore = Score 0
          input = [(SpecialKey KeyLeft, StillDown, movePlayerToLeft), (SpecialKey KeyRight, StillDown, movePlayerToRight)]
      funInit winConfig gameMap [bar, ball, target] () initScore input gameCycle (Timer 40) bmpList
  
    createBall :: PongObject
    createBall = let ballPic = Basic (Circle 3.0 0.0 1.0 0.0 Filled)
                  in object "ball" ballPic False (250, 250) (4, -4) ()
  
    createBar :: PongObject
    createBar = let barBound = [(-25, -6), (25, -6), (25, 6), (-25, 6)]
                    barPic = Basic (Polyg barBound 1.0 1.0 1.0 Filled)
                    in object "bar" barPic False (250, 30) (0,0) ()
    
    -- addScore :: Int -> Int -> IO ()
    -- addScore init adder = forkIO(do setGameAttribute (Score (init + adder)))
  
    createTarget :: [PongObject]
    createTarget = 
        let barBound = [(-30,-6),(30,-6),(30,6),(-30,6)]
            barPic   = Basic (Polyg barBound 1.0 1.0 0.0 Filled)
        in [(object "target1" barPic False (90,300) (0,0) () ),
            (object "target2" barPic False (160,300) (0,0) () ),
            (object "target3" barPic False (230,300) (0,0) () ),
            (object "target4" barPic False (300,300) (0,0) () ),
            (object "target5" barPic False (370,300) (0,0) () ),
            (object "target6" barPic False (90,350) (0,0) () ),
            (object "target7" barPic False (160,350) (0,0) () ),
            (object "target8" barPic False (230,350) (0,0) () ),
            (object "target9" barPic False (300,350) (0,0) () ),
            (object "target10" barPic False (370,350) (0,0) () ),
            (object "target11" barPic False (90,400) (0,0) () ),
            (object "target12" barPic False (160,400) (0,0) () ),
            (object "target13" barPic False (230,400) (0,0) () ),
            (object "target14" barPic False (300,400) (0,0) () ),
            (object "target15" barPic False (370,400) (0,0) () )]
        
    
    movePlayerToRight :: Modifiers -> Position -> PongAction()
    movePlayerToRight _ _ = do
      obj <- findObject "bar" "barGroup"
      (pX, pY) <- getObjectPosition obj
      (sX, _) <- getObjectSize obj
      if (pX + (sX / 2) + 5 <= 460)
        then (setObjectPosition ((pX + 5), pY) obj)
        else (setObjectPosition ((460 - (sX / 2)), pY) obj)
    
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
      (x, y) <- getObjectSpeed ball
      printOnScreen (show x) TimesRoman24 (350, 0) 1.0 1.0 1.0
      printOnScreen (show y) TimesRoman24 (410, 0) 1.0 1.0 1.0
      bar <- findObject "bar" "barGroup"
      target1 <- findObject "target1" "targetGroup"
      target2 <- findObject "target2" "targetGroup"
      target3 <- findObject "target3" "targetGroup"
      target4 <- findObject "target4" "targetGroup"
      target5 <- findObject "target5" "targetGroup"
      target6 <- findObject "target6" "targetGroup"
      target7 <- findObject "target7" "targetGroup"
      target8 <- findObject "target8" "targetGroup"
      target9 <- findObject "target9" "targetGroup"
      target10 <- findObject "target10" "targetGroup"
      target11 <- findObject "target11" "targetGroup"
      target12 <- findObject "target12" "targetGroup"
      target13 <- findObject "target13" "targetGroup"
      target14 <- findObject "target14" "targetGroup"
      target15 <- findObject "target15" "targetGroup"
      colTop <- objectTopMapCollision ball
      colRight <- objectRightMapCollision ball
      colLeft <- objectLeftMapCollision ball
      colBottom <- objectBottomMapCollision ball
      colBallBar <- objectsCollision ball bar

      colBallTarget1 <- objectsCollision target1 ball
      colBallTarget2 <- objectsCollision target2 ball
      colBallTarget3 <- objectsCollision target3 ball
      colBallTarget4 <- objectsCollision target4 ball
      colBallTarget5 <- objectsCollision target5 ball
      colBallTarget6 <- objectsCollision target6 ball
      colBallTarget7 <- objectsCollision target7 ball
      colBallTarget8 <- objectsCollision target8 ball
      colBallTarget9 <- objectsCollision target9 ball
      colBallTarget10 <- objectsCollision target10 ball
      colBallTarget11 <- objectsCollision target11 ball
      colBallTarget12 <- objectsCollision target12 ball
      colBallTarget13 <- objectsCollision target13 ball
      colBallTarget14 <- objectsCollision target14 ball
      colBallTarget15 <- objectsCollision target15 ball
      
      when colBallTarget1 (reverseYSpeed ball)
      when colBallTarget1 (setObjectPosition (1000, 1000) target1)
      when colBallTarget1 (setGameAttribute (Score (n + 10)))
      when colBallTarget2 (reverseYSpeed ball)
      when colBallTarget2 (setObjectPosition (1000, 1000) target2)
      when colBallTarget2 (setGameAttribute (Score (n + 10)))
      when colBallTarget3 (reverseYSpeed ball)
      when colBallTarget3 (setObjectPosition (1000, 1000) target3)
      when colBallTarget3 (setGameAttribute (Score (n + 10)))
      when colBallTarget4 (reverseYSpeed ball)
      when colBallTarget4 (setObjectPosition (1000, 1000) target4)
      when colBallTarget4 (setGameAttribute (Score (n + 10)))
      when colBallTarget5 (reverseYSpeed ball)
      when colBallTarget5 (setObjectPosition (1000, 1000) target5)
      when colBallTarget5 (setGameAttribute (Score (n + 10)))
      when colBallTarget6 (reverseYSpeed ball)
      when colBallTarget6 (setObjectPosition (1000, 1000) target6)
      when colBallTarget6 (setGameAttribute (Score (n + 10)))
      when colBallTarget7 (reverseYSpeed ball)
      when colBallTarget7 (setObjectPosition (1000, 1000) target7)
      when colBallTarget7 (setGameAttribute (Score (n + 10)))
      when colBallTarget8 (reverseYSpeed ball)
      when colBallTarget8 (setObjectPosition (1000, 1000) target8)
      when colBallTarget8 (setGameAttribute (Score (n + 10)))
      when colBallTarget9 (reverseYSpeed ball)
      when colBallTarget9 (setObjectPosition (1000, 1000) target9)
      when colBallTarget9 (setGameAttribute (Score (n + 10)))
      when colBallTarget10 (reverseYSpeed ball)
      when colBallTarget10 (setObjectPosition (1000, 1000) target10)
      when colBallTarget10 (setGameAttribute (Score (n + 10)))
      when colBallTarget11 (reverseYSpeed ball)
      when colBallTarget11 (setObjectPosition (1000, 1000) target11)
      when colBallTarget11 (setGameAttribute (Score (n + 10)))
      when colBallTarget12 (reverseYSpeed ball)
      when colBallTarget12 (setObjectPosition (1000, 1000) target12)
      when colBallTarget12 (setGameAttribute (Score (n + 10)))
      when colBallTarget13 (reverseYSpeed ball)
      when colBallTarget13 (setObjectPosition (1000, 1000) target13)
      when colBallTarget13 (setGameAttribute (Score (n + 10)))
      when colBallTarget14 (reverseYSpeed ball)
      when colBallTarget14 (setObjectPosition (1000, 1000) target14)
      when colBallTarget14 (setGameAttribute (Score (n + 10)))
      when colBallTarget15 (reverseYSpeed ball)
      when colBallTarget15 (setObjectPosition (1000, 1000) target15)
      when colBallTarget15 (setGameAttribute (Score (n + 10)))

      when colBallBar (setObjectSpeed ((abs x) + 1, y - 1) ball)

      when colTop (reverseYSpeed ball)
      when colRight (reverseXSpeed ball)
      when colLeft (reverseXSpeed ball)
      when colBallBar (reverseYSpeed ball)
      when colBottom (funExit)
