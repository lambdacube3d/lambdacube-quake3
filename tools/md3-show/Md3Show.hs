import Control.Monad
import System.Environment
import Text.Show.Pretty
import GameEngine.Loader.MD3

main = getArgs >>= mapM_ (loadMD3 >=> pPrint)
