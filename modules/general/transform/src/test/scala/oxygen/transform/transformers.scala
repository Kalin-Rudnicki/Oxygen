package oxygen.transform

object transformers {

  given Transform[domain.Email, String] = _.email
  given Transform[domain.Password, String] = _.password

  given Transform[domain.Person, api.Person] = Transform.derived
  given Transform[domain.SumExample, api.SumExample] = Transform.derived

}
