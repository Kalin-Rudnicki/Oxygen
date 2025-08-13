package oxygen.http

import oxygen.predef.test.*
import oxygen.zio.instances.chunkSeqOps
import scala.annotation.experimental

@experimental
object UserApiContract extends Contract[UserApi] {

  private val genCreate: UIO[CreateUser] =
    for {
      first <- RandomGen.lowerCaseString()
      last <- RandomGen.lowerCaseString()
      age <- Random.nextIntBetween(18, 35)
    } yield CreateUser(first, last, age)

  override def testSpec: TestSpec =
    suite("UserApiContract")(
      test("test 1") {
        for {
          api <- ZIO.service[UserApi]

          garbageId <- Random.nextUUID
          create1 <- genCreate
          create2 <- genCreate
          create3 <- genCreate

          _ <- ZIO.logDebug("step 1")

          garbageGet <- api.userById(garbageId).exit
          _ <- api.userById(garbageId).exit

          _ <- ZIO.logDebug("step 2")

          user1 <- api.createUser(create1)
          get1 <- api.userById(user1.id)
          all1 <- api.allUsers().map(_.into[Set])

          _ <- ZIO.logDebug("step 3")

          user2 <- api.createUser(create2)
          user3 <- api.createUser(create3)
          get2 <- api.userById(user1.id)
          all2 <- api.allUsers().map(_.into[Set])

          _ <- ZIO.logDebug("step 4")

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
    )

}
