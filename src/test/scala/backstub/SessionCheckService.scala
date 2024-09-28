package backstub

trait SessionCheckService:
  def getSessionAndValidate(
    sessionId: String,
    userId: String
  ): Either[UserNotFound | SessionNotFound | UserNotMatch, Session]

object SessionCheckService:
  class Impl(
      sessionService: SessionService,
      userService: UserService
  ) extends SessionCheckService:
    
    override def getSessionAndValidate(
      sessionId: String, 
      userId: String
    ): Either[UserNotFound | SessionNotFound | UserNotMatch, Session] =
      for
        session <- sessionService.findSession(sessionId).toRight(SessionNotFound(sessionId))
        user <- userService.find(userId).toRight(UserNotFound(userId))
        _ <- Either.cond(session.userId == user.id, (), UserNotMatch(user.id, session.userId))
      yield session
      



trait SessionService:
  def findSession(sessionId: String): Option[Session]

trait UserService:
  def find(userId: String): Option[User]

case class Session(id: String, userId: String, deviceId: Option[String])

case class User(id: String, name: String, age: Int)

case class UserNotFound(userId: String)
case class SessionNotFound(sessionId: String)
case class UserNotMatch(userId: String, actualUserId: String)