package oxygen.meta

private[meta] object InternalHelpers {

  object Stage1 {
    type Expr[A] = scala.quoted.Expr[A]
    type Type[A] = scala.quoted.Type[A]
    type Quotes = scala.quoted.Quotes
    type Term = oxygen.quoted.Term
  }

  object Stage2 {
    type Expr[A] = scala.quoted.Expr[A]
    type Type[A] = scala.quoted.Type[A]
    type Quotes = scala.quoted.Quotes
    type Term = oxygen.quoted.Term
  }

}
