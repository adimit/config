import Yi
import Yi.Misc (adjIndent)
import qualified Yi.Keymap.Vim as V
import qualified Yi.Mode.Haskell as H
import Yi.Prelude
import Yi.Hoogle (hoogle,hoogleSearch)
import Yi.Keymap.Keys
import Data.Monoid

bestHaskellMode = H.cleverMode { modeKeymap = (choice haskellModeKeys <||) }


haskellModeKeys = [ ctrlCh 'c' ?>> char 'g' ?>>! H.ghciLoadBuffer
                  , ctrlCh 'c' ?>> char 'l' ?>>! H.ghciGet
                  , ctrlCh 'c' ?>> char 'h' ?>>! hoogle
                  , ctrlCh 'c' ?>> char 's' ?>>! hoogleSearch
                  ]


main :: IO ()
main = yi $ defaultConfig
        { configKillringAccumulate = False
        , modeTable = AnyMode bestHaskellMode : modeTable defaultConfig
        , configUI = (configUI defaultConfig)
          { configWindowFill = '~'
          , configTheme = defaultLightTheme `override` \super' _ -> super'
            { selectedStyle = Endo $ \a -> a {foreground = black, background = white} }
          }
        , defaultKm = V.mkKeymap aleks'vimKeymap
	-- , defaultKm = E.keymap
        }

aleks'vimKeymap :: Proto V.ModeMap
aleks'vimKeymap = V.defKeymap `override` \super' self -> super'
                  { V.v_top_level = (deprioritize >> V.v_top_level super')
                          <|> (char 'o' ?>> V.beginIns self $
                              do moveToEol
                                 insertB '\n'
                                 adjIndent IncreaseOnly)
                  , V.v_ins_char  = (deprioritize >> V.v_ins_char super')
                          <|> ( spec KEnter ?>>!
                              do insertB '\n'
                                 adjIndent IncreaseOnly)
                  }
