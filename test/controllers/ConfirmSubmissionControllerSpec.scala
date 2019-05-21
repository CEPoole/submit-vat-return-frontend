/*
 * Copyright 2019 HM Revenue & Customs
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

package controllers

import java.time.LocalDate

import base.BaseSpec
import common.{MandationStatuses, SessionKeys}
import mocks.{MockAuth, MockMandationPredicate}
import models.{NineBoxModel, VatObligation, VatObligations}
import play.api.http.Status
import play.api.libs.json.Json
import play.api.mvc.Result
import play.api.test.Helpers._

import scala.concurrent.Future

class ConfirmSubmissionControllerSpec extends BaseSpec with MockAuth with MockMandationPredicate {

  object TestConfirmSubmissionController extends ConfirmSubmissionController(
    messagesApi,
    mockMandationStatusPredicate,
    errorHandler,
    mockAuthPredicate,
    mockAppConfig
  )

  "ConfirmSubmissionController .show" when {

    "user is authorised" should {

      val obsModel: String = Json.stringify(Json.toJson(VatObligations(Seq(VatObligation(
        LocalDate.now(),
        LocalDate.now(),
        LocalDate.now(),
        "18AA"
      )))))

      val nbModel: String = Json.stringify(Json.toJson(
        NineBoxModel(
          1000,
          1000,
          1000,
          1000,
          1000,
          1000,
          1000,
          1000,
          1000
        )
      ))

      lazy val result: Future[Result] = TestConfirmSubmissionController.show(
        frs = false,
        obsModel,
        nbModel,
        None,
        "18AA")(fakeRequest.withSession(SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB))

      "return 200" in {
        mockAuthorise(mtdVatAuthorisedResponse)
        status(result) shouldBe Status.OK
      }

      "return HTML" in {
        contentType(result) shouldBe Some("text/html")
      }
    }
  }

  "ConfirmSubmissionController .submit" when {
    "user is authorised" should {

      lazy val result: Future[Result] = TestConfirmSubmissionController.submit("18AA")(fakeRequest)

      "return 303" in {
        status(result) shouldBe Status.SEE_OTHER
      }

      s"redirect url should be ${controllers.routes.ConfirmationController.show()}" in {
        redirectLocation(result) shouldBe Some(controllers.routes.ConfirmationController.show().url)
      }
    }
  }
}
