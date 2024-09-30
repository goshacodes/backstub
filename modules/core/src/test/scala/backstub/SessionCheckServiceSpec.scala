package backstub

import backstub.*

class SessionCheckServiceSpec extends munit.FunSuite with Stubs:
  override def afterEach(context: AfterEach) =
    resetStubs()

  val ExistingSessionId = "session-id-exists"
  val NonExistingSessionId = "session-id-not-exists"
  val SessionWithDifferentUserId = "session-with-different-user-id"

  val ExistingUserId = "user-id-exists"
  val NonExistingUserId = "user-id-not-exists"
  val OtherExistingUserId = "other-existing-user-id"

  val existingSession = Session(ExistingSessionId, ExistingUserId, None)
  val existingUser = User(ExistingUserId, "Peter", 22)

  val sessionCheckService =
    SessionCheckService.Impl(
      sessionService = stub[SessionService]:
        Expect[SessionService]
          .method(_.findSession).returns:
            case ExistingSessionId => Some(existingSession)
            case NonExistingSessionId => None
            case SessionWithDifferentUserId => Some(existingSession.copy(userId = OtherExistingUserId)),

      userService = stub[UserService]:
        Expect[UserService]
          .method(_.find).returns:
            case ExistingUserId => Some(existingUser)
            case NonExistingUserId => None
    )

  test("session not found"):
    val result = sessionCheckService.getSessionAndValidate(NonExistingSessionId, ExistingUserId)
    val expectedResult = Left(SessionNotFound(NonExistingSessionId))
    assert(result == expectedResult)

  test("user not found"):
    val result = sessionCheckService.getSessionAndValidate(ExistingSessionId, NonExistingUserId)
    val expectedResult = Left(UserNotFound(NonExistingUserId))
    assert(result == expectedResult)

  test("user not match"):
    val result = sessionCheckService.getSessionAndValidate(SessionWithDifferentUserId, ExistingUserId)
    val expectedResult = Left(UserNotMatch(ExistingUserId, OtherExistingUserId))
    assert(result == expectedResult)


  test("pass"):
    val result = sessionCheckService.getSessionAndValidate(ExistingSessionId, ExistingUserId)
    val expectedResult = Right(existingSession)
    assert(result == expectedResult)










