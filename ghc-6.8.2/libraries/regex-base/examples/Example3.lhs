> {-# OPTIONS_GHC -fglasgow-exts #-}

> import Text.Regex.Base

> import qualified Text.Regex.PCRE as R
> import qualified Text.Regex.PosixRE as S
> import qualified Text.Regex.Parsec as F

Choose which library to use depending on presence of PCRE library.

> (=~) :: (RegexMaker R.Regex R.CompOption R.ExecOption a,RegexContext R.Regex b t
>         ,RegexMaker F.Regex F.CompOption F.ExecOption a,RegexContext F.Regex b t
>         ,RegexMaker S.Regex S.CompOption S.ExecOption a,RegexContext S.Regex b t)
>      => b -> a -> t
> (=~) = case R.getVersion of
>          Just _ -> (R.=~)
>          Nothing -> case S.getVersion of
>                       Just _ -> (S.=~)
>                       Nothing -> (F.=~)

> main = print ("abc" =~ "(.)c" :: Bool)