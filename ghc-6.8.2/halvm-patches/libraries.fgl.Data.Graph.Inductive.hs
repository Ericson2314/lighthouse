*** ghc-pristine/libraries/fgl/Data/Graph/Inductive.hs	2007-11-01 20:03:28.000000000 -0700
--- ghc-xen/libraries/fgl/Data/Graph/Inductive.hs	2007-11-14 18:20:44.000000000 -0800
***************
*** 14,22 ****
      module Data.Graph.Inductive.Monad.IOArray,
      module Data.Graph.Inductive.Query,
      module Data.Graph.Inductive.Graphviz,
!     module Data.Graph.Inductive.NodeMap,
      -- * Version Information
!     version
  ) where
  
  import Data.Graph.Inductive.Graph
--- 14,25 ----
      module Data.Graph.Inductive.Monad.IOArray,
      module Data.Graph.Inductive.Query,
      module Data.Graph.Inductive.Graphviz,
!     module Data.Graph.Inductive.NodeMap
! 
! #ifndef xen_HOST_OS
      -- * Version Information
!     ,version
! #endif
  ) where
  
  import Data.Graph.Inductive.Graph
***************
*** 28,33 ****
--- 31,38 ----
  import Data.Graph.Inductive.Graphviz
  import Data.Graph.Inductive.NodeMap
  
+ #ifndef xen_HOST_OS
  -- | Version info
  version :: IO ()
  version = putStrLn "\nFGL - Functional Graph Library, April 2007"
+ #endif
