package oxygen.http

import oxygen.predef.test.*
import oxygen.zio.instances.chunkSeqOps
import scala.annotation.experimental

@experimental
object UserApiContract extends Contract[UserApi] {

  private def generateCreateRequest(
      first: Specified[String] = Specified.WasNotSpecified,
      last: Specified[String] = Specified.WasNotSpecified,
      age: Specified[Int] = Specified.WasNotSpecified,
  ): UIO[CreateUser] =
    for {
      first <- first.orGen { RandomGen.lowerCaseString() }
      last <- last.orGen { RandomGen.lowerCaseString() }
      age <- age.orGen { Random.nextIntBetween(18, 35) }
    } yield CreateUser(first, last, age)

  private def generateCreatedUser(
      first: Specified[String] = Specified.WasNotSpecified,
      last: Specified[String] = Specified.WasNotSpecified,
      age: Specified[Int] = Specified.WasNotSpecified,
  ): URIO[UserApi, User] =
    for {
      req <- generateCreateRequest(first = first, last = last, age = age)
      user <- ZIO.serviceWithZIO[UserApi](_.createUser(req))
    } yield user

  override def testSpec: TestSpec =
    suite("UserApiContract")(
      test("test 1") {
        for {
          api <- ZIO.service[UserApi]

          garbageId <- Random.nextUUID
          create1 <- generateCreateRequest()
          create2 <- generateCreateRequest()
          create3 <- generateCreateRequest()

          _ <- ZIO.logDebug("step 1")
          garbageGet <- api.userById(garbageId).exit
          _ <- ZIO.logDebug("step 2")
          _ <- api.userById(garbageId).exit
          _ <- ZIO.logDebug("step 3")

          user1 <- api.createUser(create1)
          get1 <- api.userById(user1.id)
          all1 <- api.allUsers().map(_.into[Set])

          _ <- ZIO.logDebug("step 4")

          user2 <- api.createUser(create2)
          user3 <- api.createUser(create3)
          get2 <- api.userById(user1.id)
          all2 <- api.allUsers().map(_.into[Set])

          _ <- ZIO.logDebug("step 5")

        } yield assert(garbageGet)(fails(equalTo(ApiError.NoSuchUser(garbageId)))) &&
          assertTrue(
            get1.first == create1.first,
            get1.last == create1.last,
            get1.age == create1.age,
            get1 == user1,
            all1 == Set(user1),
            get2 == user1,
            all2 == Set(user1, user2, user3),
          )
      },
      test("test 2") {
        for {
          api <- ZIO.service[UserApi]

          u_f1_l1 <- generateCreatedUser(first = "F1", last = "L1")
          u_f2_l1 <- generateCreatedUser(first = "F2", last = "L1")
          u_fr_l1 <- generateCreatedUser(last = "L1")
          u_f1_l2 <- generateCreatedUser(first = "F1", last = "L2")
          u_f2_l2 <- generateCreatedUser(first = "F2", last = "L2")
          u_fr_l2 <- generateCreatedUser(last = "L2")
          u_f1_lr <- generateCreatedUser(first = "F1")
          u_f2_lr <- generateCreatedUser(first = "F2")
          u_fr_lr <- generateCreatedUser()

          search_a_a <- api.userSearch()
          search_f1_a <- api.userSearch(firstName = "F1".some)
          search_f2_a <- api.userSearch(firstName = "F2".some)
          search_a_l1 <- api.userSearch(lastName = "L1".some)
          search_a_l2 <- api.userSearch(lastName = "L2".some)
          search_f1_l1 <- api.userSearch(firstName = "F1".some, lastName = "L1".some)
          search_f2_l2 <- api.userSearch(firstName = "F2".some, lastName = "L2".some)
        } yield assertTrue(
          search_a_a == Set(u_f1_l1, u_f2_l1, u_fr_l1, u_f1_l2, u_f2_l2, u_fr_l2, u_f1_lr, u_f2_lr, u_fr_lr),
          search_f1_a == Set(u_f1_l1, u_f1_l2, u_f1_lr),
          search_f2_a == Set(u_f2_l1, u_f2_l2, u_f2_lr),
          search_a_l1 == Set(u_f1_l1, u_f2_l1, u_fr_l1),
          search_a_l2 == Set(u_f1_l2, u_f2_l2, u_fr_l2),
          search_f1_l1 == Set(u_f1_l1),
          search_f2_l2 == Set(u_f2_l2),
        )
      },
      test("test 3") {
        for {
          api <- ZIO.service[UserApi]

          create1 <- generateCreateRequest()
          user1 <- api.createUser(create1)

          events <- api.userEvents(user1.id, 5.some).toEventStream.runCollect

        } yield assertTrue(
          events.length == 6,
          events.head.data.message == "Welcome to the stream!",
        )
      },
    )

}
