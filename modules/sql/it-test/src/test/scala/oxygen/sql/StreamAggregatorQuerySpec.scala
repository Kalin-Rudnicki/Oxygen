package oxygen.sql

import oxygen.predef.test.*
import oxygen.sql.migration.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.persistence.MigrationRepo
import oxygen.sql.query.*
import oxygen.sql.query.dsl.Q.*
import oxygen.sql.schema.*
import oxygen.sql.test.SqlAspects
import scala.annotation.experimental
import zio.*

@experimental
object StreamAggregatorQuerySpec extends OxygenSpec[Database] {

  // override def defaultLogLevel: LogLevel = LogLevel.Trace

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      tables
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private object tables {

    @schemaName("ex") @tableName("user")
    final case class User(
        @primaryKey id: Int,
        first: String,
        last: String,
    )
    object User extends TableCompanion[User, Int](TableRepr.derived[User])

    @schemaName("ex") @tableName("task")
    final case class Task(
        @primaryKey id: Int,
        @references[User] userId: Int,
        task: String,
    )
    object Task extends TableCompanion[Task, Int](TableRepr.derived[Task])

    @schemaName("ex") @tableName("sub_task")
    final case class SubTask(
        @primaryKey id: Int,
        @references[Task] taskId: Int,
        subTask: String,
    )
    object SubTask extends TableCompanion[SubTask, Int](TableRepr.derived[SubTask])

    @schemaName("ex") @tableName("sub_task_comment")
    final case class SubTaskComment(
        @primaryKey id: Int,
        @references[SubTask] subTaskId: Int,
        comment: String,
    )
    object SubTaskComment extends TableCompanion[SubTaskComment, Int](TableRepr.derived[SubTaskComment])

    @schemaName("ex") @tableName("note")
    final case class Note(
        @primaryKey id: Int,
        @references[User] userId: Int,
        note: String,
    )
    object Note extends TableCompanion[Note, Int](TableRepr.derived[Note])

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      queries
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  @scala.annotation.nowarn // FIX-PRE-MERGE (KR) :
  private object queries {
    import oxygen.sql.query.dsl.*
    import oxygen.sql.query.dsl.T.*
    import tables.*

    @compile
    val user_task: QueryO[(User, List[Task])] =
      for {
        u <- select[User]
        _ <- orderBy(u.id.asc)
      } yield (
        u,
        agg.many[List] {
          for {
            t <- select[Task]
            _ <- where if t.userId == u.id
            _ <- orderBy(t.id.asc)
          } yield t
        },
      )

    // FIX-PRE-MERGE (KR) :
    // @compile
    def user_task__limit: QueryIO[Option[Int], (User, List[Task])] =
      for {
        lim <- input.optional[Int]
        u <- select[User]
        _ <- orderBy(u.id.asc)
        _ <- limit(lim)
      } yield (
        u,
        agg.many[List] {
          for {
            t <- select[Task]
            _ <- where if t.userId == u.id
            _ <- orderBy(t.id.asc)
          } yield t
        },
      )

    // FIX-PRE-MERGE (KR) :
    // @compile
    val user_task_subTask: QueryO[(User, List[(Task, List[SubTask])])] =
      for {
        u <- select[User]
        _ <- orderBy(u.id.asc)
      } yield (
        u,
        agg.many[List] {
          for {
            t <- select[Task]
            _ <- where if t.userId == u.id
            _ <- orderBy(t.id.asc)
          } yield (
            t,
            agg.many[List] {
              for {
                st <- select[SubTask]
                _ <- where if st.taskId == t.id
                _ <- orderBy(st.id.asc)
              } yield st
            },
          )
        },
      )

    // FIX-PRE-MERGE (KR) :
    // @compile
    def user_task_subTask__limit: QueryIO[Option[Int], (User, List[(Task, List[SubTask])])] =
      for {
        lim <- input.optional[Int]
        u <- select[User]
        _ <- orderBy(u.id.asc)
        _ <- limit(lim)
      } yield (
        u,
        agg.many[List] {
          for {
            t <- select[Task]
            _ <- where if t.userId == u.id
            _ <- orderBy(t.id.asc)
          } yield (
            t,
            agg.many[List] {
              for {
                st <- select[SubTask]
                _ <- where if st.taskId == t.id
                _ <- orderBy(st.id.asc)
              } yield st
            },
          )
        },
      )

    // FIX-PRE-MERGE (KR) :
    // @compile
    def user_task_note: QueryO[(User, List[Task], List[Note])] =
      for {
        u <- select[User]
        _ <- orderBy(u.id.asc)
      } yield (
        u,
        agg.many[List] {
          for {
            t <- select[Task]
            _ <- where if t.userId == u.id
            _ <- orderBy(t.id.asc)
          } yield t
        },
        agg.many[List] {
          for {
            n <- select[Note]
            _ <- where if n.userId == u.id
            _ <- orderBy(n.id.asc)
          } yield n
        },
      )

    // FIX-PRE-MERGE (KR) :
    // @compile
    def user_task_note__limit: QueryIO[Option[Int], (User, List[Task], List[Note])] =
      for {
        lim <- input.optional[Int]
        u <- select[User]
        _ <- orderBy(u.id.asc)
        _ <- limit(lim)
      } yield (
        u,
        agg.many[List] {
          for {
            t <- select[Task]
            _ <- where if t.userId == u.id
            _ <- orderBy(t.id.asc)
          } yield t
        },
        agg.many[List] {
          for {
            n <- select[Note]
            _ <- where if n.userId == u.id
            _ <- orderBy(n.id.asc)
          } yield n
        },
      )

    // FIX-PRE-MERGE (KR) :
    @compile
    val user_task_subTask_note: QueryO[(User, List[(Task, List[SubTask])], List[Note])] =
      for {
        u <- select[User]
        _ <- orderBy(u.id.asc)
      } yield (
        u,
        agg.many[List] {
          for {
            t <- select[Task]
            _ <- where if t.userId == u.id
            _ <- orderBy(t.id.asc)
          } yield (
            t,
            agg.many[List] {
              for {
                st <- select[SubTask]
                _ <- where if st.taskId == t.id
                _ <- orderBy(st.id.asc)
              } yield st
            },
          )
        },
        agg.many[List] {
          for {
            n <- select[Note]
            _ <- where if n.userId == u.id
            _ <- orderBy(n.id.asc)
          } yield n
        },
      )

    // FIX-PRE-MERGE (KR) :
    @compile
    val user_task_subTask_note__by_id: QueryIO[Int, (User, List[(Task, List[SubTask])], List[Note])] =
      for {
        userIdFilter <- input[Int]
        u <- select[User]
        _ <- where if u.id == userIdFilter
        _ <- orderBy(u.id.asc)
      } yield (
        u,
        agg.many[List] {
          for {
            t <- select[Task]
            _ <- where if t.userId == u.id
            _ <- orderBy(t.id.asc)
          } yield (
            t,
            agg.many[List] {
              for {
                st <- select[SubTask]
                _ <- where if st.taskId == t.id
                _ <- orderBy(st.id.asc)
              } yield st
            },
          )
        },
        agg.many[List] {
          for {
            n <- select[Note]
            _ <- where if n.userId == u.id
            _ <- orderBy(n.id.asc)
          } yield n
        },
      )

    // FIX-PRE-MERGE (KR) :
    // @compile
    def user_task_subTask_note__limit: QueryIO[Option[Int], (User, List[(Task, List[SubTask])], List[Note])] =
      for {
        lim <- input.optional[Int]
        u <- select[User]
        _ <- orderBy(u.id.asc)
        _ <- limit(lim)
      } yield (
        u,
        agg.many[List] {
          for {
            t <- select[Task]
            _ <- where if t.userId == u.id
            _ <- orderBy(t.id.asc)
          } yield (
            t,
            agg.many[List] {
              for {
                st <- select[SubTask]
                _ <- where if st.taskId == t.id
                _ <- orderBy(st.id.asc)
              } yield st
            },
          )
        },
        agg.many[List] {
          for {
            n <- select[Note]
            _ <- where if n.userId == u.id
            _ <- orderBy(n.id.asc)
          } yield n
        },
      )

    // FIX-PRE-MERGE (KR) :
    // @compile
    def user_task_subTask_subTaskComment: QueryO[(User, List[(Task, List[(SubTask, List[SubTaskComment])])])] =
      for {
        u <- select[User]
        _ <- orderBy(u.id.asc)
      } yield (
        u,
        agg.many[List] {
          for {
            t <- select[Task]
            _ <- where if t.userId == u.id
            _ <- orderBy(t.id.asc)
          } yield (
            t,
            agg.many[List] {
              for {
                st <- select[SubTask]
                _ <- where if st.taskId == t.id
                _ <- orderBy(st.id.asc)
              } yield (
                st,
                agg.many[List] {
                  for {
                    stc <- select[SubTaskComment]
                    _ <- where if stc.subTaskId == st.id
                    _ <- orderBy(stc.id.asc)
                  } yield stc
                },
              )
            },
          )
        },
      )

    // FIX-PRE-MERGE (KR) :
    // @compile
    def user_task_subTask_subTaskComment__limit: QueryIO[Option[Int], (User, List[(Task, List[(SubTask, List[SubTaskComment])])])] =
      for {
        lim <- input.optional[Int]
        u <- select[User]
        _ <- orderBy(u.id.asc)
        _ <- limit(lim)
      } yield (
        u,
        agg.many[List] {
          for {
            t <- select[Task]
            _ <- where if t.userId == u.id
            _ <- orderBy(t.id.asc)
          } yield (
            t,
            agg.many[List] {
              for {
                st <- select[SubTask]
                _ <- where if st.taskId == t.id
                _ <- orderBy(st.id.asc)
              } yield (
                st,
                agg.many[List] {
                  for {
                    stc <- select[SubTaskComment]
                    _ <- where if stc.subTaskId == st.id
                    _ <- orderBy(stc.id.asc)
                  } yield stc
                },
              )
            },
          )
        },
      )

    // FIX-PRE-MERGE (KR) :
    // @compile
    def user_task_subTask_subTaskComment_note: QueryO[(User, List[(Task, List[(SubTask, List[SubTaskComment])])], List[Note])] =
      for {
        u <- select[User]
        _ <- orderBy(u.id.asc)
      } yield (
        u,
        agg.many[List] {
          for {
            t <- select[Task]
            _ <- where if t.userId == u.id
            _ <- orderBy(t.id.asc)
          } yield (
            t,
            agg.many[List] {
              for {
                st <- select[SubTask]
                _ <- where if st.taskId == t.id
                _ <- orderBy(st.id.asc)
              } yield (
                st,
                agg.many[List] {
                  for {
                    stc <- select[SubTaskComment]
                    _ <- where if stc.subTaskId == st.id
                    _ <- orderBy(stc.id.asc)
                  } yield stc
                },
              )
            },
          )
        },
        agg.many[List] {
          for {
            n <- select[Note]
            _ <- where if n.userId == u.id
            _ <- orderBy(n.id.asc)
          } yield n
        },
      )

    // FIX-PRE-MERGE (KR) :
    // @compile
    val user_task_subTask_subTaskComment_note__limit: QueryIO[Option[Int], (User, List[(Task, List[(SubTask, List[SubTaskComment])])], List[Note])] =
      for {
        lim <- input.optional[Int]
        u <- select[User]
        _ <- orderBy(u.id.asc)
        _ <- limit(lim)
      } yield (
        u,
        agg.many[List] {
          for {
            t <- select[Task]
            _ <- where if t.userId == u.id
            _ <- orderBy(t.id.asc)
          } yield (
            t,
            agg.many[List] {
              for {
                st <- select[SubTask]
                _ <- where if st.taskId == t.id
                _ <- orderBy(st.id.asc)
              } yield (
                st,
                agg.many[List] {
                  for {
                    stc <- select[SubTaskComment]
                    _ <- where if stc.subTaskId == st.id
                    _ <- orderBy(stc.id.asc)
                  } yield stc
                },
              )
            },
          )
        },
        agg.many[List] {
          for {
            n <- select[Note]
            _ <- where if n.userId == u.id
            _ <- orderBy(n.id.asc)
          } yield n
        },
      )

    // FIX-PRE-MERGE (KR) :
    // @compile
    def user_lastNote: QueryO[(User, Option[Note])] =
      for {
        u <- select[User]
        _ <- orderBy(u.id.asc)
      } yield (
        u,
        agg.optional {
          for {
            n <- select[Note]
            _ <- where if n.userId == u.id
            _ <- orderBy(n.id.desc)
            _ <- limit(const(1))
          } yield n
        },
      )

    // FIX-PRE-MERGE (KR) :
    // @compile
    val user_lastNote__limit: QueryIO[Option[Int], (User, Option[Note])] =
      for {
        lim <- input.optional[Int]
        u <- select[User]
        _ <- orderBy(u.id.asc)
        _ <- limit(lim)
      } yield (
        u,
        agg.optional {
          for {
            n <- select[Note]
            _ <- where if n.userId == u.id
            _ <- orderBy(n.id.desc)
            _ <- limit(const(1))
          } yield n
        },
      )

    // FIX-PRE-MERGE (KR) :
    import agg.types.*
    @compile
    // val removeMeOverkill: QueryIO[Option[Int], (User, List[(Task, List[(SubTask, List[SubTaskComment])])], List[Note])] =
    val removeMeOverkill: QueryIO[Option[Int], AppZS[User][AppKS[Task][S2[SubTask, SubTaskComment], Note], Note][List]] =
      for {
        lim <- input.optional[Int]
        u <- select[User]
        _ <- orderBy(u.id.asc)
        _ <- limit(lim)
      } yield (
        u,
        agg.many[List] {
          for {
            t <- select[Task]
            _ <- where if t.userId == u.id
            _ <- orderBy(t.id.asc)
          } yield (
            t,
            agg.many[List] {
              for {
                st <- select[SubTask]
                _ <- where if st.taskId == t.id
                _ <- orderBy(st.id.asc)
              } yield (
                st,
                agg.many[List] {
                  for {
                    stc <- select[SubTaskComment]
                    _ <- where if stc.subTaskId == st.id
                    _ <- orderBy(stc.id.asc)
                  } yield stc
                },
              )
            },
            agg.many[List] {
              for {
                n <- select[Note]
                _ <- where if n.userId == t.id
                _ <- orderBy(n.id.asc)
              } yield n
            },
          )
        },
        agg.many[List] {
          for {
            n <- select[Note]
            _ <- where if n.userId == u.id
            _ <- orderBy(n.id.asc)
          } yield n
        },
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Data
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private object testData {
    import tables.*

    object users {
      val _1: User = User(1, "F1", "L1")
      val _2: User = User(2, "F2", "L1")
      val _3: User = User(3, "F3", "L2")
      val _4: User = User(4, "F4", "L2")

      val all: Seq[User] = Seq(_1, _2, _3, _4)
    }

    object tasks {
      val _1_1: Task = Task(11, users._1.id, "T 1 1")
      val _1_2: Task = Task(12, users._1.id, "T 1 2")
      val _1_3: Task = Task(13, users._1.id, "T 1 3")
      val _2_1: Task = Task(21, users._2.id, "T 2 1")
      val _2_2: Task = Task(22, users._2.id, "T 2 2")
      val _3_1: Task = Task(31, users._3.id, "T 3 1")

      val all: Seq[Task] = Seq(_1_1, _1_2, _1_3, _2_1, _2_2, _3_1)
    }

    object subTasks {
      val _1_3_1: SubTask = SubTask(131, tasks._1_3.id, "ST 1 3 1")
      val _1_3_2: SubTask = SubTask(132, tasks._1_3.id, "ST 1 3 2")
      val _2_2_1: SubTask = SubTask(221, tasks._2_2.id, "ST 2 2 1")
      val _2_2_2: SubTask = SubTask(222, tasks._2_2.id, "ST 2 2 2")
      val _3_1_1: SubTask = SubTask(311, tasks._3_1.id, "ST 3 1 1")

      val all: Seq[SubTask] = Seq(_1_3_1, _1_3_2, _2_2_1, _2_2_2, _3_1_1)
    }

    object subTaskComments {
      val _1_3_1_1: SubTaskComment = SubTaskComment(1311, subTasks._1_3_1.id, "STC 1 3 1 1")
      val _1_3_1_2: SubTaskComment = SubTaskComment(1312, subTasks._1_3_1.id, "STC 1 3 1 2")

      val all: Seq[SubTaskComment] = Seq(_1_3_1_1, _1_3_1_2)
    }

    object notes {
      val _1_1: Note = Note(10001, users._1.id, "N 1 1")
      val _1_2: Note = Note(10002, users._1.id, "N 1 2")
      val _1_3: Note = Note(10003, users._1.id, "N 1 3")
      val _1_4: Note = Note(10004, users._1.id, "N 1 4")
      val _4_1: Note = Note(40001, users._4.id, "N 4 1")
      val _4_2: Note = Note(40002, users._4.id, "N 4 2")

      val all: Seq[Note] = Seq(_1_1, _1_2, _1_3, _1_4, _4_1, _4_2)
    }

    object full {
      import oxygen.sql.query.dsl.Q.agg.types.*

      type Grouped = AppKS[User][S3[Task, SubTask, SubTaskComment], Note][List]

      val _1: Grouped =
        (
          users._1,
          List(
            (
              tasks._1_1,
              Nil,
            ),
            (
              tasks._1_2,
              Nil,
            ),
            (
              tasks._1_3,
              List(
                (
                  subTasks._1_3_1,
                  List(
                    subTaskComments._1_3_1_1,
                    subTaskComments._1_3_1_2,
                  ),
                ),
                (
                  subTasks._1_3_2,
                  Nil,
                ),
              ),
            ),
          ),
          List(
            notes._1_1,
            notes._1_2,
            notes._1_3,
            notes._1_4,
          ),
        )

      val _2: Grouped =
        (
          users._2,
          List(
            (
              tasks._2_1,
              Nil,
            ),
            (
              tasks._2_2,
              List(
                (
                  subTasks._2_2_1,
                  Nil,
                ),
                (
                  subTasks._2_2_2,
                  Nil,
                ),
              ),
            ),
          ),
          Nil,
        )

      val _3: Grouped =
        (
          users._3,
          List(
            (
              tasks._3_1,
              List(
                (
                  subTasks._3_1_1,
                  Nil,
                ),
              ),
            ),
          ),
          Nil,
        )

      val _4: Grouped =
        (
          users._4,
          Nil,
          List(
            notes._4_1,
            notes._4_2,
          ),
        )

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Spec
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def seedTestData: RIO[Database, Unit] =
    tables.User.insert.batched(testData.users.all).unit *>
      tables.Task.insert.batched(testData.tasks.all).unit *>
      tables.SubTask.insert.batched(testData.subTasks.all).unit *>
      tables.SubTaskComment.insert.batched(testData.subTaskComments.all).unit *>
      tables.Note.insert.batched(testData.notes.all).unit

  override def testSpec: TestSpec =
    suite("StreamAggregatorQuerySpec")(
      test("user_task") {
        for {
          _ <- seedTestData

          res1 <- queries.user_task.execute().to[List]
          res2 <- queries.user_task__limit.execute(None).to[List]
          res3 <- queries.user_task__limit.execute(4.some).to[List]
          res4 <- queries.user_task__limit.execute(2.some).to[List]
          res5 <- queries.user_task__limit.execute(1.some).to[List]

          convert = (g1: testData.full.Grouped) => (g1._1, g1._2.map(_._1))
          exp1 = convert(testData.full._1)
          exp2 = convert(testData.full._2)
          exp3 = convert(testData.full._3)
          exp4 = convert(testData.full._4)

        } yield assertTrue(
          res1 == List(exp1, exp2, exp3, exp4),
          res2 == List(exp1, exp2, exp3, exp4),
          res3 == List(exp1, exp2, exp3, exp4),
          res4 == List(exp1, exp2),
          res5 == List(exp1),
        )
      },
      test("user_task_subTask") {
        for {
          _ <- seedTestData

          res1 <- queries.user_task_subTask.execute().to[List]
          res2 <- queries.user_task_subTask__limit.execute(None).to[List]
          res3 <- queries.user_task_subTask__limit.execute(4.some).to[List]
          res4 <- queries.user_task_subTask__limit.execute(2.some).to[List]
          res5 <- queries.user_task_subTask__limit.execute(1.some).to[List]

          convert = (g1: testData.full.Grouped) => (g1._1, g1._2.map(g2 => (g2._1, g2._2.map(_._1))))
          exp1 = convert(testData.full._1)
          exp2 = convert(testData.full._2)
          exp3 = convert(testData.full._3)
          exp4 = convert(testData.full._4)

        } yield assertTrue(
          res1 == List(exp1, exp2, exp3, exp4),
          res2 == List(exp1, exp2, exp3, exp4),
          res3 == List(exp1, exp2, exp3, exp4),
          res4 == List(exp1, exp2),
          res5 == List(exp1),
        )
      },
      test("user_task_subTask_subTaskComment") {
        for {
          _ <- seedTestData

          res1 <- queries.user_task_subTask_subTaskComment.execute().to[List]
          res2 <- queries.user_task_subTask_subTaskComment__limit.execute(None).to[List]
          res3 <- queries.user_task_subTask_subTaskComment__limit.execute(4.some).to[List]
          res4 <- queries.user_task_subTask_subTaskComment__limit.execute(2.some).to[List]
          res5 <- queries.user_task_subTask_subTaskComment__limit.execute(1.some).to[List]

          convert = (g1: testData.full.Grouped) => (g1._1, g1._2)
          exp1 = convert(testData.full._1)
          exp2 = convert(testData.full._2)
          exp3 = convert(testData.full._3)
          exp4 = convert(testData.full._4)

        } yield assertTrue(
          res1 == List(exp1, exp2, exp3, exp4),
          res2 == List(exp1, exp2, exp3, exp4),
          res3 == List(exp1, exp2, exp3, exp4),
          res4 == List(exp1, exp2),
          res5 == List(exp1),
        )
      },
      test("user_task_note") {
        for {
          _ <- seedTestData

          res1 <- queries.user_task_note.execute().to[List]
          res2 <- queries.user_task_note__limit.execute(None).to[List]
          res3 <- queries.user_task_note__limit.execute(4.some).to[List]
          res4 <- queries.user_task_note__limit.execute(2.some).to[List]
          res5 <- queries.user_task_note__limit.execute(1.some).to[List]

          convert = (g1: testData.full.Grouped) => (g1._1, g1._2.map(_._1), g1._3)
          exp1 = convert(testData.full._1)
          exp2 = convert(testData.full._2)
          exp3 = convert(testData.full._3)
          exp4 = convert(testData.full._4)

        } yield assertTrue(
          res1 == List(exp1, exp2, exp3, exp4),
          res2 == List(exp1, exp2, exp3, exp4),
          res3 == List(exp1, exp2, exp3, exp4),
          res4 == List(exp1, exp2),
          res5 == List(exp1),
        )
      },
      test("user_task_subTask_note") {
        for {
          _ <- seedTestData

          res1 <- queries.user_task_subTask_note.execute().to[List]
          res2 <- queries.user_task_subTask_note__limit.execute(None).to[List]
          res3 <- queries.user_task_subTask_note__limit.execute(4.some).to[List]
          res4 <- queries.user_task_subTask_note__limit.execute(2.some).to[List]
          res5 <- queries.user_task_subTask_note__limit.execute(1.some).to[List]

          convert = (g1: testData.full.Grouped) => (g1._1, g1._2.map(g2 => (g2._1, g2._2.map(_._1))), g1._3)
          exp1 = convert(testData.full._1)
          exp2 = convert(testData.full._2)
          exp3 = convert(testData.full._3)
          exp4 = convert(testData.full._4)

        } yield assertTrue(
          res1 == List(exp1, exp2, exp3, exp4),
          res2 == List(exp1, exp2, exp3, exp4),
          res3 == List(exp1, exp2, exp3, exp4),
          res4 == List(exp1, exp2),
          res5 == List(exp1),
        )
      },
      test("user_task_subTask_subTaskComment_note") {
        for {
          _ <- seedTestData

          res1 <- queries.user_task_subTask_subTaskComment_note.execute().to[List]
          res2 <- queries.user_task_subTask_subTaskComment_note__limit.execute(None).to[List]
          res3 <- queries.user_task_subTask_subTaskComment_note__limit.execute(4.some).to[List]
          res4 <- queries.user_task_subTask_subTaskComment_note__limit.execute(2.some).to[List]
          res5 <- queries.user_task_subTask_subTaskComment_note__limit.execute(1.some).to[List]

          convert = (g1: testData.full.Grouped) => (g1._1, g1._2, g1._3)
          exp1 = convert(testData.full._1)
          exp2 = convert(testData.full._2)
          exp3 = convert(testData.full._3)
          exp4 = convert(testData.full._4)

        } yield assertTrue(
          res1 == List(exp1, exp2, exp3, exp4),
          res2 == List(exp1, exp2, exp3, exp4),
          res3 == List(exp1, exp2, exp3, exp4),
          res4 == List(exp1, exp2),
          res5 == List(exp1),
        )
      },
      test("user_lastNote") {
        for {
          _ <- seedTestData

          res1 <- queries.user_lastNote.execute().to[List]
          res2 <- queries.user_lastNote__limit.execute(None).to[List]
          res3 <- queries.user_lastNote__limit.execute(4.some).to[List]
          res4 <- queries.user_lastNote__limit.execute(2.some).to[List]
          res5 <- queries.user_lastNote__limit.execute(1.some).to[List]

          convert = (g1: testData.full.Grouped) => (g1._1, g1._3.lastOption)
          exp1 = convert(testData.full._1)
          exp2 = convert(testData.full._2)
          exp3 = convert(testData.full._3)
          exp4 = convert(testData.full._4)

        } yield assertTrue(
          res1 == List(exp1, exp2, exp3, exp4),
          res2 == List(exp1, exp2, exp3, exp4),
          res3 == List(exp1, exp2, exp3, exp4),
          res4 == List(exp1, exp2),
          res5 == List(exp1),
        )
      },
    )

  override def testAspects: Chunk[StreamAggregatorQuerySpec.TestSpecAspect] = Chunk(TestAspect.nondeterministic, TestAspect.withLiveClock, SqlAspects.isolateTestsInRollbackTransaction)

  override def layerProvider: LayerProvider[R] =
    LayerProvider.provideShared[Env](
      Helpers.testContainerLayer,
      Helpers.databaseLayer,
      MigrationService.layer,
      MigrationConfig.defaultLayer,
      MigrationService.migrateLayer(
        Migrations(
          PlannedMigration.auto(1)(
            tables.User.tableRepr,
            tables.Task.tableRepr,
            tables.SubTask.tableRepr,
            tables.SubTaskComment.tableRepr,
            tables.Note.tableRepr,
          ),
        ),
      ),
      Atomically.LiveDB.layer,
      MigrationRepo.layer,
    )

}
