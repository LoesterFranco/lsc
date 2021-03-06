{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module LSC.DEF
  ( module Language.DEF.Builder
  , module Language.DEF.Parser
  , fromDEF, toDEF
  ) where

import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Default
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Map (fromList, fromListWith, lookup, insert, assocs, withoutKeys)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Data.Vector as V
import Prelude hiding (lookup)

import Language.DEF.Builder
import Language.DEF.Parser (parseDEF)
import Language.DEF.Syntax as DEF

import LSC.Types as Rect



fromDEF :: DEF -> NetGraph
fromDEF (DEF options area rs ts cs ps ns _) = def &~ do
    identifier .= identifierFrom options
    supercell .= supercellFrom area ts rs ps
    gates .= (uncurry (set wires) <$> V.zip paths nodes)
    nets .= edges
    where

      paths = V.accum (\ m (k, v) -> insert k v m) (V.replicate (length nodes) mempty)
          [ (i, (x ^. identifier, e ^. identifier))
          | e <- toList edges
          , (i, xs) <- views contacts assocs e
          , i >= 0
          , x <- xs
          ]

      nodes = set number `imap` V.fromList (fromComponent <$> cs)

      edges = fromList [ (n ^. identifier, n) | n <- fromNet gateNumber <$> ns ]

      gateNumber y = maybe (-1) id $ lookup y $ fromList [ (x, i) | (i, Component x _ _) <- zip [0..] cs ]



fromNet :: (Ident -> Number) -> DEF.Net -> Rect.Net
fromNet n (DEF.Net i cs _) = Rect.Net i
    mempty
    (fromListWith (++) [ (g, [def & identifier .~ p]) | (g, p) <- either (-1, ) (first n) <$> cs ])


fromLayer :: LayerName -> Rect.Layer
fromLayer "metal1" = Metal1
fromLayer "metal2" = Metal2
fromLayer "metal3" = Metal3
fromLayer "metal4" = Metal4
fromLayer "metal5" = Metal5
fromLayer "metal6" = Metal6
fromLayer "metal7" = Metal7
fromLayer "metal8" = Metal8
fromLayer "metal9" = Metal9
fromLayer "metal10" = Metal10
fromLayer _ = AnyLayer



supercellFrom :: DieArea -> [DEF.Track] -> [DEF.Row] -> [DEF.Pin] -> AbstractCell
supercellFrom (DieArea (x1, y1) (x2, y2)) ts rs ps = def &~ do
    geometry .= [Rect (ceiling x1) (ceiling y1) (ceiling x2) (ceiling y2)]
    tracks .= fmap fromTrack ts
    rows .= set number `imap` V.fromList (fmap fromRow rs)
    pins .= fromList [ (p ^. identifier, p) | p <- fmap fromPin ps ] 


fromPin :: DEF.Pin -> Rect.Pin
fromPin (DEF.Pin p _ d layer placed) = def &~ do
    identifier .= p
    dir .= fmap fromDirection d
    geometry .= join
      [ fromPlaced f
        <&> l +~ x1
        <&> b +~ y1
        <&> r +~ x2
        <&> t +~ y2
        <&> z .~ [fromLayer q]
      | (Layer q (x1, y1) (x2, y2), f) <- maybeToList $ (,) <$> layer <*> placed
      ]


fromDirection :: DEF.Direction -> Dir
fromDirection DEF.Input  = In
fromDirection DEF.Output = Out
fromDirection DEF.InputOutput = InOut


fromTrack :: DEF.Track -> Either Rect.Track Rect.Track
fromTrack (DEF.Track "X" a ss c d)
    = Right
    $ Rect.Track (ceiling a) (fromIntegral ss) (ceiling c) [fromLayer d] 
fromTrack (DEF.Track   _ a ss c d)
    = Left
    $ Rect.Track (ceiling a) (fromIntegral ss) (ceiling c) [fromLayer d]


fromRow :: DEF.Row -> Rect.Row
fromRow (DEF.Row _ i x y o ss _ w _)
    = Rect.Row (-1) i x y (fromOrientation o) ss w



fromComponent :: DEF.Component -> Gate
fromComponent (Component _ j placed@(Just (Fixed _ _))) = def &~ do
    identifier .= j
    geometry .= maybe mempty fromPlaced placed
    fixed .= True
fromComponent (Component _ j placed) = def &~ do
    identifier .= j
    geometry .= maybe mempty fromPlaced placed



fromPlaced :: Placed -> Path
fromPlaced (Placed (x, y) ori) =
  [ Layered (ceiling x) (ceiling y) (ceiling x) (ceiling y)
    [Metal2, Metal3] (fromOrientation ori)
  ]
fromPlaced (Fixed (x, y) ori) =
  [ Layered (ceiling x) (ceiling y) (ceiling x) (ceiling y)
    [Metal2, Metal3] (fromOrientation ori)
  ]
fromPlaced _ = mempty



fromOrientation :: Identifier -> Orientation
fromOrientation "N" = N
fromOrientation "S" = S
fromOrientation "W" = W
fromOrientation "E" = E
fromOrientation "FN" = FN
fromOrientation "FS" = FS
fromOrientation "FW" = FW
fromOrientation "FE" = FE
fromOrientation _ = error "undefined orientation"



identifierFrom :: [Option] -> Identifier
identifierFrom (Design i : _) = i
identifierFrom (_ : xs) = identifierFrom xs
identifierFrom _ = "top"



toDEF :: Double -> NetGraph -> DEF
toDEF scale top = DEF
  (filter units (defaultOptions $ Just $ top ^. identifier) ++ [Units $ DistanceList $ ceiling scale])
  (dieArea $ listToMaybe $ top ^. supercell . geometry)
  (toList $ top ^. supercell . rows <&> toRow)
  (toList $ top ^. supercell . tracks <&> toTrack)
  (toList $ set number `imap` view gates top <&> toComponent)
  (toList $ top ^. supercell . pins <&> toPin)
  (toList $ withoutKeys (top ^. nets) power <&> toNet top)
  mempty

  where

    units (Units _) = False
    units _ = True



toRow :: Rect.Row -> DEF.Row
toRow x = DEF.Row
    (enumeratedRow x)
    (view identifier x)
    (view l x) (view b x)
    (toOrientation $ view orientation x)
    (view cardinality x) 1
    (view granularity x) 0



toTrack :: Either Rect.Track Rect.Track -> DEF.Track
toTrack (Right (Rect.Track a ss c d))
    = DEF.Track "X" (fromIntegral a) (fromIntegral ss) (fromIntegral c) (last $ toLayer <$> AnyLayer : d)
toTrack (Left (Rect.Track a ss c d))
    = DEF.Track "Y" (fromIntegral a) (fromIntegral ss) (fromIntegral c) (last $ toLayer <$> AnyLayer : d)



dieArea :: Maybe (Rect.Component l Integer) -> DieArea
dieArea (Just p) = DieArea
    (fromIntegral $ p^.l, fromIntegral $ p^.b)
    (fromIntegral $ p^.r, fromIntegral $ p^.t)
dieArea _ = DieArea (0, 0) (0, 0)



power :: Set Identifier
power = Set.fromList ["gnd", "vdd"]


enumeratedGate :: Gate -> Identifier
enumeratedGate g = view identifier g <> "_" <> pack (views number show g)

enumeratedRow :: Rect.Row -> Identifier
enumeratedRow x = "ROW_" <> pack (views number show x)


toComponent :: Gate -> DEF.Component
toComponent g = Component (enumeratedGate g) (g ^. identifier) (listToMaybe $ g ^. geometry <&> place)
  where
    place x | g ^. fixed = Fixed (fromIntegral $ x^.l, fromIntegral $ x^.b) "N"
    place x = Placed (fromIntegral $ x^.l, fromIntegral $ x^.b) "N"



toNet :: NetGraph -> Rect.Net -> DEF.Net
toNet top n = DEF.Net (n ^. identifier)

    [ maybe (Left (p ^. identifier)) (Right . (, p ^. identifier) . enumeratedGate)
    $ top ^. gates ^? ix i
    | (i, ps) <- n ^. contacts . to assocs
    , p <- ps
    ]

    Nothing



toPin :: Rect.Pin -> DEF.Pin
toPin pin = DEF.Pin (pin ^. identifier)
    (Just $ pin ^. identifier)
    (pin ^. dir <&> toDirection)
    (listToMaybe [ Layer (toLayer $ last $ AnyLayer : p^.z) (0, 0)
                         (fromIntegral $ width p, fromIntegral $ height p) | p <- pin ^. geometry ])
    (listToMaybe [ Fixed (fromIntegral $ p^.l, fromIntegral $ p^.b)
                         (toOrientation $ p ^. orientation) | p <- pin ^. geometry ])


toOrientation :: Orientation -> Identifier
toOrientation N = "N"
toOrientation S = "S"
toOrientation W = "W"
toOrientation E = "E"
toOrientation FN = "FN"
toOrientation FS = "FS"
toOrientation FW = "FW"
toOrientation FE = "FE"


toLayer :: Rect.Layer -> LayerName
toLayer Metal1 = "metal1"
toLayer Metal2 = "metal2"
toLayer Metal3 = "metal3"
toLayer Metal4 = "metal4"
toLayer Metal5 = "metal5"
toLayer Metal6 = "metal6"
toLayer Metal7 = "metal7"
toLayer Metal8 = "metal8"
toLayer Metal9 = "metal9"
toLayer Metal10 = "metal10"
toLayer AnyLayer = "UNKNOWN"


toDirection :: Dir -> DEF.Direction
toDirection In  = DEF.Input
toDirection Out = DEF.Output
toDirection InOut = DEF.InputOutput

