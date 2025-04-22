package oxygen.core

import oxygen.core.collection.NonEmptyList

type EitherNel[A, B] = Either[NonEmptyList[A], B]

type RightProjection[Left] = [Right] =>> Either[Left, Right]
type LeftProjection[Right] = [Left] =>> Either[Left, Right]
