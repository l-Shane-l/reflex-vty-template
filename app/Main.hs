{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

import Control.Monad
import Control.Monad.Fix
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Reflex.Vty

main :: IO ()
main = mainWidget $ withCtrlC $ initManager_ $ do
    -- Main layout
    row $ do
        grout flex blank -- Left margin
        grout (fixed 60) $ col $ do
            -- Title bar with custom style
            grout (fixed 3) $
                localTheme (const $ constant titleStyle) $
                    boxStatic def $
                        text "FFmpeg Screen Recorder"

            -- Status section
            grout (fixed 3) $ boxStatic roundedBoxStyle $ col $ do
                grout (fixed 1) $ text "Status: Ready"
                grout (fixed 1) $ text "Last Recording: None"

            -- Main controls with timer logic
            (recordClick, stopClick, resetClick) <- grout (fixed 6) $ boxStatic doubleBoxStyle $ col $ do
                grout (fixed 1) $ text "Controls"
                grout (fixed 3) $ row $ do
                    let buttonCfg = def{_buttonConfig_focusStyle = pure doubleBoxStyle}

                    recordClick <- tile flex $ do
                        buttonClick <- textButtonStatic buttonCfg "⏺ Record"
                        keyPress <- keyCombos $ Set.fromList [(V.KEnter, []), (V.KChar ' ', [])]
                        pure $ leftmost [void buttonClick, void keyPress]

                    stopClick <- tile flex $ do
                        buttonClick <- textButtonStatic buttonCfg "⏹ Stop"
                        keyPress <- keyCombos $ Set.fromList [(V.KEnter, []), (V.KChar ' ', [])]
                        pure $ leftmost [void buttonClick, void keyPress]

                    resetClick <- tile flex $ do
                        buttonClick <- textButtonStatic buttonCfg "⟲ Reset"
                        keyPress <- keyCombos $ Set.fromList [(V.KEnter, []), (V.KChar ' ', [])]
                        pure $ leftmost [void buttonClick, void keyPress]

                    pure (recordClick, stopClick, resetClick)

            -- Timer logic using button events
            isRunning <- toggle False $ leftmost [recordClick, stopClick]

            -- Get tick events for when we're running
            tick <- tickLossyFromPostBuildTime 1

            -- Track accumulated time
            accumTime <-
                foldDyn ($) 0 $
                    mergeWith
                        (.)
                        [ const 0 <$ resetClick -- Reset to 0
                        , (+ 1) <$ gate (current isRunning) tick -- Only add time when running
                        ]

            -- Timer display
            grout (fixed 6) $ boxStatic singleBoxStyle $ col $ do
                grout (fixed 1) $ text "Timer"
                grout (fixed 2) $ displayTimer accumTime

            -- Recent recordings list
            _ <- grout (fixed 10) $ boxStatic singleBoxStyle $ col $ do
                grout (fixed 1) $ text "Recent Recordings"
                tile flex $ scrollableText def $ pure "No recordings yet..."

            -- Settings
            grout (fixed 5) $ boxStatic roundedBoxStyle $ col $ do
                grout (fixed 1) $ text "Settings"
                grout (fixed 1) $ row $ do
                    grout (fixed 15) $ text "Output Format:"
                    tile flex $ text "MP4"
                grout (fixed 1) $ row $ do
                    grout (fixed 15) $ text "Save Location:"
                    tile flex $ text "~/Videos/"

            -- Help text
            tile (fixed 2) $
                boxStatic def $
                    text "Tab: Navigate | Enter/Space: Select | Ctrl+C: Exit"

        grout flex blank -- Right margin
  where
    titleStyle =
        V.Attr
            { V.attrStyle = V.SetTo V.standout
            , V.attrForeColor = V.SetTo V.green
            , V.attrBackColor = V.SetTo V.black
            , V.attrURL = V.Default
            }

withCtrlC :: (Monad m, HasInput t m, Reflex t) => m () -> m (Event t ())
withCtrlC f = do
    inp <- input
    f
    return $ fforMaybe inp $ \case
        V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
        _ -> Nothing

displayTimer ::
    (Reflex t, MonadHold t m, MonadFix m, HasImageWriter t m, HasDisplayRegion t m, HasTheme t m) =>
    Dynamic t Double ->
    m ()
displayTimer accumTime = do
    let formatTime seconds =
            let total = floor seconds :: Integer
                hours = total `div` 3600
                mins = (total `mod` 3600) `div` 60
                secs = total `mod` 60
             in T.pack $ show hours <> ":" <> padZero mins <> ":" <> padZero secs
        padZero n = if n < 10 then "0" <> show n else show n

    text $ formatTime <$> current accumTime
