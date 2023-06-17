module Utils.Output where

import Prettyprinter
import Prettyprinter.Render.String

output :: forall ann. Doc ann -> String
output doc = renderString (layoutPretty defaultLayoutOptions doc)

prettyWhen :: forall ann. Bool -> Doc ann -> Doc ann
prettyWhen True d = d
prettyWhen False _ = mempty

prettyUnless :: forall ann. Bool -> Doc ann -> Doc ann
prettyUnless b = prettyWhen (not b)

(<×>) :: Doc ann -> Doc ann -> Doc ann
x <×> y = x <> "\t" <> y
infixr 6 <×> -- like <>
