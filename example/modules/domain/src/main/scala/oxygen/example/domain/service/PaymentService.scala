package oxygen.example.domain.service

import oxygen.example.domain.model.error.*
import oxygen.example.domain.model.user.*
import oxygen.example.domain.repo.UserRepo
import oxygen.payments.stripe.model.CreateCustomerRequest
import oxygen.payments.stripe.service.StripeService
import oxygen.predef.core.*
import oxygen.stripe.model.StripeCustomerId
import zio.*

final class PaymentService(userRepo: UserRepo, stripeService: StripeService) {

  def ensureStripeCustomer(user: FullUser): IO[DomainError, (FullUser, StripeCustomerId)] =
    user.stripeCustomerId match {
      case Some(stripeCustomerId) => ZIO.succeed { (user, stripeCustomerId) }
      case None                   =>
        for {
          stripeCustomerId <- genCustomerId(user)
          updatedUser = user.copy(stripeCustomerId = stripeCustomerId.some)
          _ <- userRepo.update(updatedUser)
        } yield (updatedUser, stripeCustomerId)
    }

  /////// Helpers ///////////////////////////////////////////////////////////////

  private def genCustomerId(user: FullUser): IO[DomainError, StripeCustomerId] =
    for {
      _ <- ZIO.logInfo(s"Generating a stripe customer for ${user.show}")
      genCustomer <- stripeService.createCustomer(CreateCustomerRequest(user.fullName.some, user.email.some)).orDie // TODO (KR) :
    } yield genCustomer.id

}
object PaymentService {

  val layer: URLayer[UserRepo & StripeService, PaymentService] =
    ZLayer.fromFunction { PaymentService.apply }

}
