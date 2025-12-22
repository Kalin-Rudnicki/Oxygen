package oxygen.meta.k0

type Const[A] = [_] =>> A
type Id[A] = A
type IdBounded[Bound] = [A <: Bound] =>> A
