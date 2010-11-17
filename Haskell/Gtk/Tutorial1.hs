module Gtk.Tutorial1 where

import Control.Monad.Trans
import Graphics.UI.Gtk

just doIt = liftIO doIt >> return True

main = do
    initGUI
    
    window <- windowNew
    on window deleteEvent $ just $ do
        putStrLn "delete event occurred"
        return True
    on window destroyEvent (just mainQuit)
    containerSetBorderWidth window 10
    
    button <- buttonNewWithLabel "Hello World!"
    on button buttonActivated (liftIO $ putStrLn "Hello World")
    after button buttonActivated (liftIO $ widgetDestroy window)
    
    containerAdd window button
    widgetShow button
    widgetShow window
    
    mainGUI
