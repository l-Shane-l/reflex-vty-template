{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

import Control.Monad
import Control.Monad.Fix
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Reflex.Network
import Reflex.Vty

main :: IO ()
main = mainWidget $ withCtrlC $ initManager_ $ do
    -- Get display dimensions for responsive layout
    dw <- displayWidth
    dh <- displayHeight

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

            -- Main controls
            grout (fixed 6) $ boxStatic doubleBoxStyle $ col $ do
                grout (fixed 1) $ text "Controls"
                grout (fixed 3) $ row $ do
                    recordBtn <- tile flex $ fancyBtn "⏺" "Record" (pure True)
                    stopBtn <- tile flex $ fancyBtn "⏹" "Stop" (pure False)
                    pure ()

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
    -- Enhanced button with icon
    fancyBtn icon label enabledDyn = do
        let cfg = def{_buttonConfig_focusStyle = pure doubleBoxStyle}
        buttonClick <- textButtonStatic cfg (icon <> " " <> label)
        keyPress <-
            keyCombos $
                Set.fromList
                    [ (V.KEnter, [])
                    , (V.KChar ' ', [])
                    ]
        pure $ gate enabledDyn $ leftmost [void buttonClick, void keyPress]

    -- Custom styles
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
