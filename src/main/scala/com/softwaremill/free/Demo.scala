package com.softwaremill.free

import java.util.UUID

import cats.free.Free
import cats.implicits._
import cats.{Monad, ~>}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.higherKinds

object Original {

  trait UserRepository {
    def findUser(id: UUID): Future[Option[User]]

    def updateUser(u: User): Future[Unit]
  }

  class LoyaltyPoints(ur: UserRepository) {
    def addPoints(userId: UUID,
                  pointsToAdd: Int): Future[Either[String, Unit]] = {
      ur.findUser(userId).flatMap {
        case None => Future.successful(Left("User not found"))
        case Some(user) =>
          val updated =
            user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
          ur.updateUser(updated).map(_ => Right(()))
      }
    }
  }

}

object UsingFree {

  sealed trait UserRepositoryAlg[T]
  case class FindUser(id: UUID) extends UserRepositoryAlg[Option[User]]
  case class UpdateUser(u: User) extends UserRepositoryAlg[Unit]

  type UserRepository[T] = Free[UserRepositoryAlg, T]

  def findUser(id: UUID): UserRepository[Option[User]] =
    Free.liftF(FindUser(id))
  def updateUser(u: User): UserRepository[Unit] = Free.liftF(UpdateUser(u))

  def addPoints(userId: UUID,
                pointsToAdd: Int): UserRepository[Either[String, Unit]] = {
    findUser(userId).flatMap {
      case None => Free.pure(Left("User not found"))
      case Some(user) =>
        val updated =
          user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
        updateUser(updated).map(_ => Right(()))
    }
  }
  val futureInterpreter = new (UserRepositoryAlg ~> Future) {
    override def apply[A](fa: UserRepositoryAlg[A]): Future[A] = fa match {
      case FindUser(id)  => Future.successful(None) //go to database
      case UpdateUser(u) => Future.successful(())
    }
  }
  var r = addPoints(UUID.randomUUID(), 10).foldMap(futureInterpreter)
}

object UsingTagless {
  trait UserRepositoryAlg[F[_]] {
    def findUser(id: UUID): F[Option[User]]

    def updateUser(u: User): F[Unit]
  }

  class LoyaltyPoints[F[_]: Monad](ur: UserRepositoryAlg[F]) {
    def addPoints(userId: UUID, pointsToAdd: Int): F[Either[String, Unit]] = {
      ur.findUser(userId).flatMap {
        case None => implicitly[Monad[F]].pure(Left("User not found"))
        case Some(user) =>
          val updated =
            user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
          ur.updateUser(updated).map(_ => Right(()))
      }
    }
  }
  trait FutureInterpreter extends UserRepositoryAlg[Future] {
    override def findUser(id: UUID): Future[Option[User]] = {
      //go to db
      Future.successful(None)
    }

    override def updateUser(u: User): Future[Unit] = Future.successful(())

  }

  new LoyaltyPoints(new FutureInterpreter {}).addPoints(UUID.randomUUID(), 10)
}
