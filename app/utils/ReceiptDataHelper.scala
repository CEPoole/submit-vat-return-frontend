/*
 * Copyright 2020 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package utils

import javax.inject.{Inject, Singleton}
import models.auth.User
import models.errors.{HttpError, UnknownError}
import models.nrs.{Declaration, _}
import models.{CustomerDetails, SubmitVatReturnModel}
import play.api.i18n.{Lang, MessagesApi}
import play.api.{Logger, Play}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.views.helpers.MoneyPounds

import scala.concurrent.ExecutionContext

@Singleton
class ReceiptDataHelper @Inject()(implicit val messages: MessagesApi) {

  def extractReceiptData(submitModel: SubmitVatReturnModel, customerDetails: Either[HttpError, CustomerDetails])
                        (implicit user: User[_], hc: HeaderCarrier, ec: ExecutionContext): Either[HttpError, ReceiptData] = {

    val language: Language = user.cookies.get(Play.langCookieName) match {
      case Some(cookieValue) => Language.fromString(cookieValue.value)
      case None => EN
    }
    
    extractDeclaration(submitModel, customerDetails, Lang.apply(language.languageCode)) match {
      case Right(declaration) =>
        Right(ReceiptData(
          language,
          extractAnswers(submitModel, Lang.apply(language.languageCode)),
          declaration
        ))
      case Left(error) => Left(error)
    }
  }

  private def extractAnswers(submitModel: SubmitVatReturnModel, lang: Lang)(implicit user: User[_]): Seq[Answers] = {
    val boxSixSearchKey = if (submitModel.flatRateScheme) "boxSixFlatRate" else "boxSixNoFlatRate"

    val answerSeq = Seq(
      ("box1", messages("confirm_submission.boxOneDescription")(lang), submitModel.box1),
      ("box2", messages("confirm_submission.boxTwoDescription")(lang), submitModel.box2),
      ("box3", messages("confirm_submission.boxThreeDescription")(lang), submitModel.box3),
      ("box4", messages("confirm_submission.boxFourDescription")(lang), submitModel.box4),
      ("box5", messages("confirm_submission.boxFiveDescription")(lang), submitModel.box5),
      ("box6", messages(s"confirm_submission.$boxSixSearchKey")(lang), submitModel.box6),
      ("box7", messages("confirm_submission.boxSevenDescription")(lang), submitModel.box7),
      ("box8", messages("confirm_submission.boxEightDescription")(lang), submitModel.box8),
      ("box9", messages("confirm_submission.boxNineDescription")(lang), submitModel.box9)
    ).map { case (questionId, question, answer) =>
      Answer(questionId, question, Some("£" + MoneyPounds(answer, 2).quantity))
    }

    Seq(Answers(
      messages("confirmation_view.title"),
      answerSeq
    ))
  }

  private def extractDeclaration(submitModel: SubmitVatReturnModel, customerDetails: Either[HttpError, CustomerDetails], lang: Lang)
                                (implicit user: User[_], hc: HeaderCarrier, ec: ExecutionContext): Either[HttpError, Declaration] = {

    val declarationAgentOrNonAgent = if (user.isAgent) "agentDeclaration" else "nonAgentDeclaration"

    customerDetails match {
      case Right(model) => model.clientName match {
        case Some(name) => Right(
          Declaration(
            messages(s"confirm_submission.$declarationAgentOrNonAgent")(lang),
            name,
            None,
            declarationConsent = true
          )
        )
        case None =>
          Logger.warn("[ReceiptDataHelper][extractDeclaration] Client name missing")
          Left(UnknownError)
      }
      case Left(error) =>
        Logger.debug("[ReceiptDataHelper][extractDeclaration] Failed to retrieve customer details from vat-subscription\n" + error.message)
        Logger.warn("[ReceiptDataHelper][extractDeclaration] Failed to retrieve customer details from vat-subscription")
        Left(error)
    }
  }
}
