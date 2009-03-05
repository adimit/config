-- yi.hs
-- Aleksandar Dimitrov

import Yi
import Yi.Keymap.Vim (keymap)
import qualified Yi.Mode.Haskell as Haskell
import Yi.Style
import Yi.Hoogle (hoogle, hoogleSearch)
import Yi.Style.Library
import Yi.Prelude
import Prelude ()

defaultVimUiTheme :: Theme
defaultVimUiTheme = defaultLightTheme  `override` \super self ->
        super { selectedStyle = modelineFocusStyle self }

myConfigUI :: UIConfig
myConfigUI = (configUI defaultConfig)  
             { configTheme = defaultVimUiTheme
             , configWindowFill = '~'
             }

haskellModeKeys = [ ctrlCh 'c' ?>> char 'g' ?>>! Haskell.ghciLoadBuffer
                  , ctrlCh 'c' ?>> char 'l' ?>>! Haskell.ghciGet
                  , ctrlCh 'c' ?>> char 'h' ?>>! hoogle
                  , ctrlCh 'c' ?>> char 's' ?>>! hoogleSearch
                  ]

myHaskellMode = Haskell.cleverMode { modeKeymap = (choice haskellModeKeys <||) }

main :: IO ()
main = yi $ defaultConfig 
        { configUI = myConfigUI
        , defaultKm = keymap
        , modeTable = AnyMode myHaskellMode : modeTable defaultConfig
        }
