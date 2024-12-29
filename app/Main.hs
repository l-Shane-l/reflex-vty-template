{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

import Control.Applicative (liftA2)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import qualified Data.Foldable
import Data.Functor
import Data.List (sortBy)
import Data.Ord (Down (Down), comparing)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import GHC.IO.Handle.FD (withFile)
import GHC.IO.Handle.Text (hPutStrLn)
import GHC.IO.IOMode
import qualified Graphics.Vty as V
import Reflex
import Reflex.FSNotify
import Reflex.Vty
import System.Directory (doesDirectoryExist, getHomeDirectory, getModificationTime, listDirectory)
import qualified System.FSNotify as FS
import System.FilePath ((</>))
import System.Process

data AppSettings where
    AppSettings :: {appVideoDirectory :: [Char]} -> AppSettings

-- Create default settings
defaultSettings :: IO AppSettings
defaultSettings = do
    return $
        AppSettings
            { appVideoDirectory = "Videos"
            }

data UIState = UIState
    { currentSelection :: Maybe FilePath -- The currently selected file
    }

-- Helper functions for recording
-- ffmpeg -f x11grab -i :0.0 output.mp4 >/dev/null 2>&1 & echo $! > /tmp/screencast.pid
startRecording :: [Char] -> IO ProcessHandle
startRecording videosDir = do
    time <- getCurrentTime
    let filename = formatTime defaultTimeLocale "%Y%m%d_%H%M%S.mp4" time
        outputPath = "/home/shane/" </> videosDir </> filename
        logPath = "./ffmpeg_" <> filename <> ".log"

    -- Open our log file handle - we use AppendMode to keep adding to the log
    withFile logPath AppendMode $ \logHandle -> do
        -- Write our initial log entry
        hPutStrLn logHandle "Starting recording..."
        hPutStrLn logHandle $ "Output path: " <> outputPath

        -- Start ffmpeg process using our log handle
        (_, _, _, ph) <-
            createProcess
                (proc "ffmpeg" ["-f", "x11grab", "-i", ":0", outputPath])
                    { std_out = UseHandle logHandle
                    , std_err = UseHandle logHandle
                    }

        -- Return the process handle
        return ph

stopRecording :: ProcessHandle -> IO ()
stopRecording = terminateProcess

-- ffmpeg -i output.mp4 -vf "fps=15,scale=800:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" output.gif
convertToGif :: FilePath -> IO ()
convertToGif inputPath = do
    let outputPath = inputPath <> ".gif"
        logPath = "./ffmpeg.log"
    withFile logPath AppendMode $ \logHandle -> do
        -- Write our initial log entry
        hPutStrLn logHandle "Starting recording..."
        hPutStrLn logHandle $ "Output path: " <> outputPath
        -- duration <- getVideoDuration inputPath
        -- filteredDuration <- filterVideoDuration
        (_, _, _, _) <-
            createProcess
                (proc "ffmpeg" ["-i", inputPath, "-vf", "fps=15,scale=800:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse", outputPath])
                    { std_out = UseHandle logHandle
                    , std_err = UseHandle logHandle
                    }
        return ()

-- ffmpeg -ss 5 -i output.mp4 -c copy -t $(ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 output.mp4 | awk '{print $1-10}') trim.mp4
trimVideo :: FilePath -> IO ()
trimVideo inputPath = do
    let outputPath = inputPath <> ".trim.mp4"
        logPath = "./ffmpeg.log"
    withFile logPath AppendMode $ \logHandle -> do
        -- Write our initial log entry
        hPutStrLn logHandle "Starting recording..."
        hPutStrLn logHandle $ "Output path: " <> outputPath
        -- need to move ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 /home/shane/Videos/20241228_232222.mp4 | awk '{print $1-10}' to its own process
        (_, _, _, _) <-
            createProcess
                (proc "ffmpeg" ["-ss", "5", "-i", inputPath, "-c", "copy", "-t", "1.245", outputPath])
                    { std_out = UseHandle logHandle
                    , std_err = UseHandle logHandle
                    }
        return ()

getVideoDuration :: FilePath -> Float
getVideoDuration inputPath = 11.245000

-- filterVideoDuration :: Float -> IO ()
-- filterVideoDuration = do
--     let logPath = "./ffmpeg.log"
--     withFile logPath AppendMode $ \logHandle -> do
--         (_, _, _, _) <-
--             createProcess
--                 (proc "echo" ["11.245000" "|" "awk" "{print $1-10}"])
--                     { std_out = UseHandle logHandle
--                     , std_err = UseHandle logHandle
--                     }
--         return ()

main :: IO ()
main = do
    settings <- defaultSettings
    let greenAttr = V.defAttr `V.withForeColor` V.green

    mainWidget $ withCtrlC $ initManager_ $ do
        let videosDir = appVideoDirectory settings
        tabNavigation

        -- Main layout
        row $ do
            grout flex blank -- Left margin
            grout (stretch 60) $ col $ do
                -- Title bar with custom style
                grout (fixed 3) $
                    localTheme (const $ constant titleStyle) $
                        boxTitle (constant def) (constant "FFmpeg Screen Recorder") $
                            grout (fixed 1) $
                                row $ do
                                    grout flex blank
                                    grout (stretch 6) $ text "A simple screen recorder using FFmpeg"
                                    grout flex blank
                -- richText (RichTextConfig $ constant greenAttr) (constant "Status: Ready")

                -- Main controls with timer logic
                (startE, stopE, gifE, trimEv) <- grout (fixed 6) $ boxTitle (constant doubleBoxStyle) (constant "Controls") $ col $ do
                    grout (stretch 3) $ row $ do
                        let buttonCfg = def{_buttonConfig_focusStyle = pure singleBoxStyle}

                        rec isRecording <- holdDyn False toggleE
                            toggleE <- tile flex $ do
                                let buttonText = current $ ffor isRecording $ \recording ->
                                        if recording then "⏹️" <> "Stop" else "▶️" <> "Record"
                                clickR <- textButton buttonCfg buttonText
                                pure $ not <$> tag (current isRecording) clickR

                            let startE = ffilter id $ updated isRecording
                                stopE = ffilter not $ updated isRecording
                        rec isConverting <- holdDyn False convertE
                            convertE <- tile flex $ do
                                let buttonText = current $ ffor isConverting $ \converting ->
                                        if converting then "⌛ Running" else "🪄 Create Gif"
                                clickG <- textButton buttonCfg buttonText
                                pure $ not <$> tag (current isConverting) clickG

                            let gifE = ffilter id $ updated isConverting
                        -- TODO button to clip start and stop
                        trimEv <- tile flex $ textButton buttonCfg "🔴 trimVideo"
                        pure (startE, stopE, gifE, trimEv)

                recEv <- performEvent $ ffor startE $ \_ -> liftIO $ do
                    startRecording videosDir
                processState <- holdDyn Nothing $ Just <$> recEv
                performEvent_ $ ffor (tag (current processState) stopE) $ \maybePh ->
                    liftIO $ Data.Foldable.forM_ maybePh stopRecording

                performEvent_ $ ffor gifE $ \_ -> liftIO $ do
                    convertToGif $ "/home/shane/" </> videosDir <> "/20241228_232222.mp4"

                performEvent_ $ ffor trimEv $ \_ -> liftIO $ do
                    trimVideo $ "/home/shane/" </> videosDir <> "/20241228_232222.mp4"
                -- Timer logic using button events
                isRunning <- toggle False $ leftmost [startE, stopE]

                -- Get tick events for when we're running
                tick <- tickLossyFromPostBuildTime 1

                -- Track accumulated time
                accumTime <-
                    foldDyn ($) 0 $
                        mergeWith
                            (.)
                            [ const 0 <$ startE -- Reset to 0
                            , (+ 1) <$ gate (current isRunning) tick -- Only add time when running
                            ]

                -- Timer display
                grout (fixed 3) $ boxTitle (constant singleBoxStyle) (constant "Timer") $ col $ do
                    row $ do
                        grout flex $ displayTimer accumTime

                -- Recent recordings list
                _ <- grout (fixed 10) $ boxTitle (constant singleBoxStyle) (constant "Recent Recordings") $ col $ do
                    -- Get initial files and set up watcher
                    pb <- getPostBuild
                    homePath <- performEvent $ pb $> liftIO getHomeDirectory
                    let videosPath = (</> videosDir) <$> homePath

                    -- Wait for the initial path before setting up watcher
                    watchPath <- headE videosPath

                    -- Watch for file system changes
                    fsEvents <-
                        watchDir
                            FS.defaultConfig -- Using defaultConfig from System.FSNotify
                            watchPath
                            (const True)

                    -- Combine initial load with file system events
                    let refreshTrigger =
                            leftmost
                                [ pb -- Initial load
                                , void fsEvents -- Trigger on any file system event
                                ]

                    -- Update file listing on any trigger
                    videoFiles <-
                        holdDyn [T.pack "Loading..."]
                            =<< performEvent (refreshTrigger $> liftIO (getVideoFiles videosDir))

                    -- Main recordings list with interactive elements

                    -- Display the files
                    tile flex $ scrollableText def $ T.unlines <$> videoFiles

                grout (fixed 5) $ boxTitle (constant singleBoxStyle) (constant "Recent Recordings") $ col $ do
                    grout (fixed 1) $ text "Settings"
                    grout (fixed 1) $ row $ do
                        grout (fixed 15) $ text "Output Format:"
                        tile flex $ text "MP4"
                    grout (fixed 1) $ row $ do
                        grout (fixed 15) $ text "Save Location:"
                        tile flex $ text "~/Videos/"

                -- Help text
                grout (fixed 2) $
                    text "Tab: Navigate | Enter/Space: Select | Ctrl+C: Exit"

            grout flex blank -- Right margin
  where
    -- Perform recording actions

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
    let formatMyTime seconds =
            let total = floor seconds :: Integer
                hours = total `div` 3600
                mins = (total `mod` 3600) `div` 60
                secs = total `mod` 60
             in T.pack $ show hours <> ":" <> padZero mins <> ":" <> padZero secs
        padZero n = if n < 10 then "0" <> show n else show n

    text $ formatMyTime <$> current accumTime

getVideoFiles :: [Char] -> IO [T.Text]
getVideoFiles vidDir = do
    home <- getHomeDirectory
    let videosDir = home </> vidDir
    dirExists <- doesDirectoryExist videosDir
    if dirExists
        then do
            files <- listDirectory videosDir
            if null files
                then return [T.pack "No recordings yet..."]
                else do
                    -- Get modification times and create (time, file) pairs
                    filesWithTimes <- forM files $ \file -> do
                        modTime <- getModificationTime (videosDir </> file)
                        return (modTime, T.pack file)
                    -- Sort by time (in reverse order) and extract just the filenames
                    return $ map snd $ sortBy (comparing (Down . fst)) filesWithTimes
        else return [T.pack "Videos directory not found"]
