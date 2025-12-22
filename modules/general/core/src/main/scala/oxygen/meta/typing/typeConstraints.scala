package oxygen.meta.typing

type <:<[A, B] = TypeRelationship.SubType[A, B]
type !<:<[A, B] = TypeRelationship.NotSubType[A, B]

type >:>[A, B] = TypeRelationship.SuperType[A, B]
type !>:>[A, B] = TypeRelationship.NotSuperType[A, B]

type <<:<[A, B] = TypeRelationship.StrictSubType[A, B]
type !<<:<[A, B] = TypeRelationship.NotStrictSubType[A, B]

type >>:>[A, B] = TypeRelationship.StrictSuperType[A, B]
type !>>:>[A, B] = TypeRelationship.NotStrictSuperType[A, B]

type =:=[A, B] = TypeRelationship.Equals[A, B]
type =!=[A, B] = TypeRelationship.NotEquals[A, B]

type >:<[A, B] = TypeRelationship.Disjoint[A, B]
type >!<[A, B] = TypeRelationship.NotDisjoint[A, B]
