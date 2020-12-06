module spec Data.Vector.Grow.Unboxed where

import Data.Vector

measure vlen    :: forall a. (U.Vector a) -> Int

invariant       {v: U.Vector a | 0 <= vlen v }

(U.!)           :: forall a. x:(U.Vector a) -> vec:{v:Nat | v < vlen x } -> a

U.unsafeIndex :: forall a. x:(U.Vector a) -> vec:{v:Nat | v < vlen x } -> a

U.fromList  :: forall a. x:[a] -> {v: U.Vector a  | vlen v = len x }

U.length    :: forall a. x:(U.Vector a) -> {v : Nat | v = vlen x }

U.replicate :: n:Nat -> a -> {v:U.Vector a | vlen v = n}

U.imap :: (Nat -> a -> b) -> x:(U.Vector a) -> {y:U.Vector b | vlen y = vlen x }

U.map :: (a -> b) -> x:(U.Vector a) -> {y:U.Vector b | vlen y = vlen x }

U.head :: forall a. {xs: U.Vector a | vlen xs > 0} -> a


measure gvlen :: GrowVector s a -> Int
measure gvcap :: GrowVector s a -> Int

invariant {v: GrowVector s a | 0 <= gvlen v }
invariant {v: GrowVector s a | 0 <= gvcap v }
invariant {v: GrowVector s a | gvlen v <= gvcap v }

type GrowVectorN s a N = {v:GrowVector s a | gvlen v == N}
type GrowVectorC s a C = {v:GrowVector s a | gvcap v == C}
type GrowVectorNC s a N C = {v:GrowVector s a | gvlen v == N && gvcap v == C}

assume capacity :: (Unbox a, PrimMonad m) => x : GrowVector (PrimState m) a -> m {v:Int | v = gvcap x}

assume length :: (Unbox a, PrimMonad m) => x : GrowVector (PrimState m) a -> m {v:Int | v = gvlen x}

null :: (Unbox a, PrimMonad m) => x : GrowVector (PrimState m) a -> m { v:Bool | v <=> gvlen x = 0}

new :: (Unbox a, PrimMonad m) => c:Nat -> m {v:(GrowVector (PrimState m) a) | gvlen v = 0 && gvcap v = c}

assume newSized :: (Unbox a, PrimMonad m) => l:Nat -> {c:Nat | c >= l} -> m { v:(GrowVector (PrimState m) a) | l = gvlen v && c = gvcap v }

assume slice :: (Unbox a, PrimMonad m)
  => i:Nat
  -> n:Nat
  -> {x:(GrowVector (PrimState m) a) | i < gvlen x && i + n <= gvlen x }
  -> m {y:(GrowVector (PrimState m) a) | gvlen y = n && gvcap y = n }

assume thaw :: (Unbox a, PrimMonad m)
  => v : U.Vector a
  -> m {gv:(GrowVector (PrimState m) a) | vlen v == gvlen gv && gvcap gv == 0 }

freeze :: (Unbox a, PrimMonad m)
  => gv : GrowVector (PrimState m) a
  -> m {v:(U.Vector a) | vlen v == gvlen gv }

assume ensure :: (Unbox a, PrimMonad m)
  => x : GrowVector (PrimState m) a
  -> c : Nat
  -> m {r:() | gvcap x = c}

assume ensureAppend :: (Unbox a, PrimMonad m)
  => x : GrowVector (PrimState m) a
  -> n : Nat
  -> m {r:() | gvcap x >= gvlen x + n }

read :: (Unbox a, PrimMonad m)
  => x : GrowVector (PrimState m) a
  -> {i:Nat | i < gvlen x}
  -> m a

unsafeRead :: (Unbox a, PrimMonad m)
  => x : GrowVector (PrimState m) a
  -> {i:Nat | i < gvlen x}
  -> m a

write :: (Unbox a, PrimMonad m)
  => x : GrowVector (PrimState m) a
  -> {i:Nat | i < gvlen x}
  -> a
  -> m ()

unsafeWrite :: (Unbox a, PrimMonad m)
  => x : GrowVector (PrimState m) a
  -> {i:Nat | i < gvlen x}
  -> a
  -> m ()
