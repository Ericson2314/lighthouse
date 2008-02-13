module Graphics.UI.ObjectIO.Process.IOState where
data GUI ps a

noLS :: (ps -> GUI ps ps) -> (ls, ps) -> GUI ps (ls, ps)
noLS1 :: (x -> ps -> GUI ps ps) -> x -> (ls, ps) -> GUI ps (ls, ps)
