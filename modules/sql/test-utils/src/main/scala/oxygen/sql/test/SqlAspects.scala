package oxygen.sql.test

import oxygen.predef.test.*
import oxygen.sql.*
import zio.test.{TestFailure, TestSuccess}

object SqlAspects {

  /**
    * Runs each test within a database transaction that is rolled back at the end, regardless of success or failure.
    * This is very useful when you have multiple tests which are touching the same tables.
    *
    * [test-1] Tests that look like:
    * 1. INSERT INTO table_x
    * 2. SELECT * FROM table_x where id=?
    * generally don't suffer from this issue
    *
    * [test-2] But when you have tests that look like:
    * 1. (INSERT INTO table_x) * 10
    * 2. SELECT * FROM table_x -- select all
    * You will run into this issue, because if 'test-1' runs before 'test-2', then the select all will return 11 results instead of 10.
    * This problem is even worse if you are not using test containers, and have a local db spun up for all your tests to connect to.
    * In that case, not only do you have to worry about tests running in parallel, or the ordering of tests,
    * but you also have to worry about persistence BETWEEN tests runs.
    *
    * Note: if your spec has multiple tests doing a LARGE amount of db operations and need this level of isolation,
    *       you might be better off using a non-shared test container layer.
    *       The extra couple seconds it takes to spin up additional test containers is probably going to be better than having multiple HUGE parallel transactions.
    *       (especially if you are looking to test performance)
    */
  val isolateTestsInRollbackTransaction: TestAspect.PerTest.AtLeastR[Database] =
    new TestAspect.PerTest.AtLeastR[Database] {
      override def perTest[R <: Database, E](test: ZIO[R, TestFailure[E], TestSuccess])(using trace: Trace): ZIO[R, TestFailure[E], TestSuccess] =
        test @@ Atomically.RollbackDB.atomically
    }

}
