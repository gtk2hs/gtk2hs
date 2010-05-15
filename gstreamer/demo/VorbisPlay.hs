import System.Exit
import Data.Maybe
import qualified Media.Streaming.GStreamer as Gst
import qualified System.Glib as G
import qualified System.Glib.MainLoop as G
import qualified System.Glib.Properties as G
import qualified System.Glib.GError as G
import qualified System.Glib.Signals as G
import Text.Printf
import Control.Monad
import System.IO
import System

mkElement action =
    do element <- action
       case element of
         Just element' ->
             return element'
         Nothing -> 
             do hPutStrLn stderr "could not create all GStreamer elements\n"
                exitFailure

main =
    do args <- getArgs
       when (length args /= 1) $
            do hPutStrLn stderr "Usage: vorbis-play <Ogg/Vorbis filename>\n"
               exitFailure
       
       Gst.init
       
       mainLoop <- G.mainLoopNew Nothing True
       
       pipeline <- Gst.pipelineNew "audio-player"
       source <- mkElement $ Gst.elementFactoryMake "filesrc" $ Just "file-source"
       parser <- mkElement $ Gst.elementFactoryMake "oggdemux" $ Just "ogg-parser"
       decoder <- mkElement $ Gst.elementFactoryMake "vorbisdec" $ Just "vorbis-decoder"
       conv <- mkElement $ Gst.elementFactoryMake "audioconvert" $ Just "convert"
       sink <- mkElement $ Gst.elementFactoryMake "alsasink" $ Just "alsa-output"
       
       let elements = [source, parser, decoder, conv, sink]
       
       G.objectSetPropertyString "location" source (head args)
       
       bus <- Gst.pipelineGetBus (Gst.castToPipeline pipeline)
       Gst.busAddWatch bus G.priorityDefault $ \bus message ->
           do case Gst.messageType message of
                Gst.MessageEOS ->
                    do putStrLn "end of stream"
                       G.mainLoopQuit mainLoop
                Gst.MessageError ->
                    let G.GError _ _ msg = fst $ fromJust $ Gst.messageParseError message
                        messageStr = "Error: " ++ msg
                    in do hPutStrLn stderr messageStr
                          G.mainLoopQuit mainLoop
                _ -> return ()
              return True
       
       mapM_ (Gst.binAdd $ Gst.castToBin pipeline) elements
       
       Gst.elementLink source parser
       Gst.elementLink decoder conv
       Gst.elementLink conv sink
       
       G.on parser Gst.elementPadAdded $ \pad ->
           do sinkPad <- Gst.elementGetStaticPad decoder "sink"
              Gst.padLink pad $ fromJust sinkPad
              return ()
       
       flip G.timeoutAdd 100 $ do
         position <- Gst.elementQueryPosition pipeline Gst.FormatTime
         duration <- Gst.elementQueryDuration pipeline Gst.FormatTime
         case position of
           Just (_, position') ->
               case duration of
                 Just (_, duration') -> do
                   printf "%10d / %10d\r" 
                     (fromIntegral (position' `div` Gst.second)::Integer)
                     (fromIntegral (duration' `div` Gst.second)::Integer)
                 Nothing -> do
                   putStr "no information\r"
           Nothing -> do
             putStr "no information\r"
         hFlush stdout
         return True
       
       Gst.elementSetState pipeline Gst.StatePlaying
       
       G.mainLoopRun mainLoop
       
       Gst.elementSetState pipeline Gst.StateNull
       
       return ()
