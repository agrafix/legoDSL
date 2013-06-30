import ExampleCode
import NXT.Core

import Control.Monad ((=<<), (>>=))

main =
    tree >>= (mkProg "lineFollow.nxc")
