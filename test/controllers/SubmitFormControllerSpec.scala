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
import assets.CustomerDetailsTestAssets._
import connectors.httpParsers.ResponseHttpParsers.HttpGetResult
import mocks.MockAuth
import mocks.service.{MockDateService, MockVatObligationsService, MockVatSubscriptionService}
import mocks.MockMandationPredicate
import play.api.mvc.AnyContentAsFormUrlEncoded
import models._
import models.auth.User
import models.errors.UnexpectedJsonFormat
import org.jsoup.Jsoup
import play.api.http.Status
import play.api.libs.json.Json
import play.api.mvc.{AnyContentAsEmpty, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._

import scala.concurrent.Future

class SubmitFormControllerSpec extends BaseSpec
  with MockVatSubscriptionService
  with MockVatObligationsService
  with MockAuth
  with MockMandationPredicate
  with MockDateService {

  val vatSubscriptionResponse: Future[HttpGetResult[CustomerDetails]] = Future.successful(Right(customerDetailsWithFRS))

  object TestSubmitFormController extends SubmitFormController(
    messagesApi,
    mockVatSubscriptionService,
    mockVatObligationsService,
    mockMandationStatusPredicate,
    errorHandler,
    mockAuthPredicate,
    mockAppConfig,
    mockDateService
  )

  "SubmitFormController .show" when {

    "user is authorised" when {

      "there is a SubmitVatReturn model in session" when {

        val nineBoxModel: String = Json.stringify(Json.toJson(
          SubmitVatReturnModel(
            1000.00,
            1000.00,
            1000.00,
            1000.00,
            1000.00,
            1000.00,
            1000.00,
            1000.00,
            1000.00,
            flatRateScheme = true,
            LocalDate.now(),
            LocalDate.now(),
            LocalDate.now()
          )
        ))

        "a successful response is received from the service" should {

          lazy val requestWithSessionData = User[AnyContentAsEmpty.type]("123456789")(fakeRequest.withSession(
            SessionKeys.returnData -> nineBoxModel,
            SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB)
          )

          lazy val result: Future[Result] = {
            TestSubmitFormController.show("18AA")(requestWithSessionData)
          }

          "return 200" in {
            mockAuthorise(mtdVatAuthorisedResponse)
            setupVatSubscriptionService(vatSubscriptionResponse)
            status(result) shouldBe Status.OK
          }

          "return HTML" in {
            contentType(result) shouldBe Some("text/html")
          }

          "the user name should be displayed" in {
            Jsoup.parse(bodyOf(result)).select("#content > article > div > h2").text() shouldBe "ABC Solutions"
          }
        }

        "an unsuccessful response is received from the service" should {

          val vatSubscriptionFailureResponse: Future[HttpGetResult[CustomerDetails]] = Future.successful(Left(UnexpectedJsonFormat))

          lazy val requestWithSessionData = User[AnyContentAsEmpty.type]("123456789")(fakeRequest.withSession(
            SessionKeys.returnData -> nineBoxModel,
            SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB)
          )

          lazy val result: Future[Result] = {
            TestSubmitFormController.show("18AA")(requestWithSessionData)
          }

          "return 200" in {
            mockAuthorise(mtdVatAuthorisedResponse)
            setupVatSubscriptionService(vatSubscriptionFailureResponse)
            status(result) shouldBe Status.OK
          }

          "return HTML" in {
            contentType(result) shouldBe Some("text/html")
          }

          "the user name should not be displayed" in {
            Jsoup.parse(bodyOf(result)).select("#content > article > div > h2").text() shouldBe ""
          }
        }
      }

      "there is no view model in session" when {

        "a successful response is received from the service" when {

          "obligation end date is in the past" should {

            val obligations: VatObligations = VatObligations(Seq(
              VatObligation(
                LocalDate.parse("2019-01-12"),
                LocalDate.parse("2019-04-12"),
                LocalDate.parse("2019-05-12"),
                "18AA"
              )
            ))

            val vatObligationsResponse: Future[HttpGetResult[VatObligations]] = Future.successful(Right(obligations))

            lazy val result = {
              TestSubmitFormController.show("18AA")(fakeRequest.withSession(SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB))
            }

            "return 200" in {
              mockAuthorise(mtdVatAuthorisedResponse)
              setupVatSubscriptionService(vatSubscriptionResponse)
              setupVatObligationsService(vatObligationsResponse)
              mockDateHasPassed(response = true)
              status(result) shouldBe Status.OK
            }

            "return HTML" in {
              contentType(result) shouldBe Some("text/html")
            }
          }

          "obligation end date is in the future" should {

            val obligations: VatObligations = VatObligations(Seq(
              VatObligation(
                LocalDate.parse("2019-01-12"),
                LocalDate.parse("2019-04-12"),
                LocalDate.parse("2019-05-12"),
                "18AA"
              )
            ))

            val vatObligationsResponse: Future[HttpGetResult[VatObligations]] = Future.successful(Right(obligations))

            lazy val result = {
              TestSubmitFormController.show("18AA")(fakeRequest.withSession(SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB))
            }

            "return 400" in {
              mockAuthorise(mtdVatAuthorisedResponse)
              setupVatSubscriptionService(vatSubscriptionResponse)
              setupVatObligationsService(vatObligationsResponse)
              mockDateHasPassed(response = false)
              status(result) shouldBe Status.BAD_REQUEST
            }

            "render generic Bad Request page" in {
              Jsoup.parse(bodyOf(result)).title() shouldBe "Bad request - 400"
            }
          }
        }

        "an obligation doesn't match the provided period key" should {

          val obligations: VatObligations = VatObligations(Seq(
            VatObligation(
              LocalDate.parse("2019-01-12"),
              LocalDate.parse("2019-04-12"),
              LocalDate.parse("2019-05-12"),
              "17AA"
            )
          ))

          val vatObligationsResponse: Future[HttpGetResult[VatObligations]] = Future.successful(Right(obligations))

          lazy val result = {
            TestSubmitFormController.show("18AA")(fakeRequest.withSession(SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB))
          }

          "return a 303" in {
            mockAuthorise(mtdVatAuthorisedResponse)
            setupVatSubscriptionService(vatSubscriptionResponse)
            setupVatObligationsService(vatObligationsResponse)
            status(result) shouldBe Status.SEE_OTHER
          }

          s"redirect to ${mockAppConfig.returnDeadlinesUrl}" in {
            redirectLocation(result) shouldBe Some(mockAppConfig.returnDeadlinesUrl)
          }

        }

        "an error response is returned from the service" should {

          "return an internal server status" in {

            val vatSubscriptionErrorResponse: Future[HttpGetResult[CustomerDetails]] = Future.successful(Left(UnexpectedJsonFormat))
            val vatObligationsErrorResponse: Future[HttpGetResult[VatObligations]] = Future.successful(Left(UnexpectedJsonFormat))

            mockAuthorise(mtdVatAuthorisedResponse)
            setupVatSubscriptionService(vatSubscriptionErrorResponse)
            setupVatObligationsService(vatObligationsErrorResponse)

            lazy val result = TestSubmitFormController.show("18AA")(fakeRequest.withSession(SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB))
            status(result) shouldBe Status.INTERNAL_SERVER_ERROR

          }
        }
      }

    }

    authControllerChecks(TestSubmitFormController.show("18AA"), fakeRequest)
  }

  "SubmitFormController .submit" when {

      "redirect to next page" should {

        "successful" should {

          lazy val request = FakeRequest().withFormUrlEncodedBody(
            "box1" -> "1000.11",
            "box2" -> "1000",
            "box3" -> "2000.11",
            "box4" -> "1000",
            "box5" -> "1000.11",
            "box6" -> "1000",
            "box7" -> "1000.3",
            "box8" -> "1234567890123",
            "box9" -> "1234567890123.45",
            "flatRateScheme" -> "true",
            "start" -> "2019-01-01",
            "end" -> "2019-01-04",
            "due" -> "2019-01-05"
          ).withSession(
            SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB
          )

          lazy val result = {
            TestSubmitFormController.submit(periodKey = "93DH")(request)
          }
          "status is SEE_OTHER" in {
            mockAuthorise(mtdVatAuthorisedResponse)
            status(result) shouldBe SEE_OTHER
          }

          s"redirect to ${controllers.routes.ConfirmationController.show()}" in {
            redirectLocation(result).get.contains(controllers.routes.ConfirmSubmissionController.show("93DH").url) shouldBe true
          }
        }
      }

      "display a validation error" when {

        "there is a Submit Form view model in session" when {

          val sessionModel: String = Json.toJson(
            SubmitFormViewModel(
              hasFlatRateScheme = true,
              LocalDate.parse("2019-01-01"),
              LocalDate.parse("2019-01-04"),
              LocalDate.parse("2019-01-05"))
          ).toString()

          "an error occurs (unentered value in a box)" should {

            lazy val request = FakeRequest().withFormUrlEncodedBody(
              "box1" -> "1000",
              "box2" -> "1000",
              "box3" -> "2000",
              "box4" -> "1000",
              "box5" -> "3000",
              "box6" -> "1000",
              "box7" -> "1000",
              "box8" -> "1000",
              "box9" -> "",
              "flatRateScheme" -> "true",
              "start" -> "2019-01-01",
              "end" -> "2019-01-04",
              "due" -> "2019-01-05"
            ).withSession(
              SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB,
              SessionKeys.viewModel -> sessionModel
            )

            lazy val result = {
              TestSubmitFormController.submit(periodKey = "93DH")(request)
            }

            "status is BAD_REQUEST" in {
              mockAuthorise(mtdVatAuthorisedResponse)
              setupVatSubscriptionService(vatSubscriptionResponse)
              status(result) shouldBe BAD_REQUEST
            }

            "contains common header" in {
              contentAsString(result) should include("You have one or more errors")
            }

            "contains missing number error" in {
              contentAsString(result) should include("Enter a number")
            }
          }

          "an error occurs (too many numbers)" should {

            lazy val request = FakeRequest().withFormUrlEncodedBody(
              "box1" -> "1000",
              "box2" -> "1000",
              "box3" -> "2000",
              "box4" -> "1000",
              "box5" -> "3000",
              "box6" -> "1000",
              "box7" -> "12345.000",
              "box8" -> "12345.",
              "box9" -> "1234567890123456",
              "flatRateScheme" -> "true",
              "start" -> "2019-01-01",
              "end" -> "2019-01-04",
              "due" -> "2019-01-05"
            ).withSession(
              SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB,
              SessionKeys.viewModel -> sessionModel
            )

            lazy val result = {
              TestSubmitFormController.submit(periodKey = "93DH")(request)
            }

            "status is BAD_REQUEST" in {
              mockAuthorise(mtdVatAuthorisedResponse)
              setupVatSubscriptionService(vatSubscriptionResponse)
              status(result) shouldBe BAD_REQUEST
            }

            "contains common header" in {
              contentAsString(result) should include("You have one or more errors")
            }

            "contains the too many numbers error" in {
              contentAsString(result) should include("Enter a maximum of 13 digits for pounds." +
                "\nEnter a maximum of 2 decimal places for pence.\nYou can use a negative amount eg -13.2")
            }
          }

          "an error occurs (incorrect format)" when {

            lazy val request = FakeRequest().withFormUrlEncodedBody(
              "box1" -> "1000",
              "box2" -> "1000",
              "box3" -> "2000",
              "box4" -> "1000",
              "box5" -> "3000",
              "box6" -> "1000",
              "box7" -> "12345",
              "box8" -> "12345",
              "box9" -> "1234+][567,./;'#890123456",
              "flatRateScheme" -> "true",
              "start" -> "2019-01-01",
              "end" -> "2019-01-04",
              "due" -> "2019-01-05"
            ).withSession(
              SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB,
              SessionKeys.viewModel -> sessionModel
            )

            lazy val result = {
              TestSubmitFormController.submit(periodKey = "93DH")(request)
            }

            "status is BAD_REQUEST" in {
              mockAuthorise(mtdVatAuthorisedResponse)
              setupVatSubscriptionService(vatSubscriptionResponse)
              status(result) shouldBe BAD_REQUEST
            }

            "contains common header" in {
              contentAsString(result) should include("You have one or more errors")
            }

            "has the correct form error shown" in {
              contentAsString(result) should include("Enter a number in the correct format")
            }
          }

          "an error occurs (negative number)" when {
            lazy val request = FakeRequest().withFormUrlEncodedBody(
              "box1" -> "1000",
              "box2" -> "1000",
              "box3" -> "2000",
              "box4" -> "1000",
              "box5" -> "-3000",
              "box6" -> "1000",
              "box7" -> "1000",
              "box8" -> "1000",
              "box9" -> "1000",
              "flatRateScheme" -> "true",
              "start" -> "2019-01-01",
              "end" -> "2019-01-04",
              "due" -> "2019-01-05"
            ).withSession(
              SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB,
              SessionKeys.viewModel -> sessionModel
            )

            lazy val result = {
              TestSubmitFormController.submit(periodKey = "93DH")(request)
            }

            "status is BAD_REQUEST" in {
              mockAuthorise(mtdVatAuthorisedResponse)
              setupVatSubscriptionService(vatSubscriptionResponse)
              status(result) shouldBe BAD_REQUEST
            }

            "contains common header" in {
              contentAsString(result) should include("You have one or more errors")
            }

            "contains negative number error" in {
              contentAsString(result) should include("Enter a maximum of 13 digits for pounds." +
                "\nEnter a maximum of 2 decimal places for pence.\nDo not use a negative amount eg -13.2")
            }
          }

          "an error occurs (incorrect box additions)" when {

            lazy val request = FakeRequest().withFormUrlEncodedBody(
              "box1" -> "1000",
              "box2" -> "1000",
              "box3" -> "12000",
              "box4" -> "1000",
              "box5" -> "121000",
              "box6" -> "1000",
              "box7" -> "1000",
              "box8" -> "1000",
              "box9" -> "1000",
              "flatRateScheme" -> "true",
              "start" -> "2019-01-01",
              "end" -> "2019-01-04",
              "due" -> "2019-01-05"
            ).withSession(
              SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB,
              SessionKeys.viewModel -> sessionModel
            )

            lazy val result = {
              TestSubmitFormController.submit(periodKey = "93DH")(request)
            }

            "status is BAD_REQUEST" in {
              mockAuthorise(mtdVatAuthorisedResponse)
              setupVatSubscriptionService(vatSubscriptionResponse)
              status(result) shouldBe BAD_REQUEST
            }

            "contains common header" in {
              contentAsString(result) should include("You have one or more errors")
            }

            "contains box 3 error" in {
              contentAsString(result) should include("Add the number from box 1 to the number from box 2 and write it here")
            }

            "contains box 5 error" in {
              contentAsString(result) should include("Subtract the number in box 4 away from the number in box 3 and write it here")
            }
          }

          "display box 3 error if box 1 or box 2 are not numbers" when {

            def request(input: Boolean): FakeRequest[AnyContentAsFormUrlEncoded] = FakeRequest().withFormUrlEncodedBody(
              "box1" -> (if (input) "e" else "1000"),
              "box2" -> (if (!input) "e" else "1000"),
              "box3" -> "1000",
              "box4" -> "1000",
              "box5" -> "1000",
              "box6" -> "1000",
              "box7" -> "1000",
              "box8" -> "1000",
              "box9" -> "1000",
              "flatRateScheme" -> "true",
              "start" -> "2019-01-01",
              "end" -> "2019-01-04",
              "due" -> "2019-01-05"
            ).withSession(
              SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB,
              SessionKeys.viewModel -> sessionModel
            )

            lazy val resultBox1Error = {
              TestSubmitFormController.submit(periodKey = "93DH")(request(true))
            }

            lazy val resultBox2Error = {
              TestSubmitFormController.submit(periodKey = "93DH")(request(false))
            }

            "status is BAD_REQUEST for box1Error" in {
              mockAuthorise(mtdVatAuthorisedResponse)
              setupVatSubscriptionService(vatSubscriptionResponse)
              status(resultBox1Error) shouldBe BAD_REQUEST
            }

            "status is BAD_REQUEST for box2Error" in {
              mockAuthorise(mtdVatAuthorisedResponse)
              setupVatSubscriptionService(vatSubscriptionResponse)
              status(resultBox2Error) shouldBe BAD_REQUEST
            }

            "error is displayed for box1Error" in {
              contentAsString(resultBox1Error) should include("Add the number from box 1 to the number from box 2 and write it here")
            }

            "error is displayed for box2Error" in {
              contentAsString(resultBox1Error) should include("Add the number from box 1 to the number from box 2 and write it here")
            }

            "display box 5 error if box 3 or box 4 are not numbers" when {

              def request(input: Boolean): FakeRequest[AnyContentAsFormUrlEncoded] = FakeRequest().withFormUrlEncodedBody(
                "box1" -> "1000",
                "box2" -> "1000",
                "box3" -> (if (input) "e" else "1000"),
                "box4" -> (if (!input) "e" else "1000"),
                "box5" -> "1000",
                "box6" -> "1000",
                "box7" -> "1000",
                "box8" -> "1000",
                "box9" -> "1000",
                "flatRateScheme" -> "true",
                "start" -> "2019-01-01",
                "end" -> "2019-01-04",
                "due" -> "2019-01-05"
              ).withSession(
                SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB,
                SessionKeys.viewModel -> sessionModel
              )

              lazy val resultBox3Error = {
                TestSubmitFormController.submit(periodKey = "93DH")(request(true))
              }
              lazy val resultBox4Error = {
                TestSubmitFormController.submit(periodKey = "93DH")(request(false))
              }

              "status is BAD_REQUEST for box3Error" in {
                mockAuthorise(mtdVatAuthorisedResponse)
                setupVatSubscriptionService(vatSubscriptionResponse)
                status(resultBox3Error) shouldBe BAD_REQUEST
              }

              "status is BAD_REQUEST for box4Error" in {
                mockAuthorise(mtdVatAuthorisedResponse)
                setupVatSubscriptionService(vatSubscriptionResponse)
                status(resultBox4Error) shouldBe BAD_REQUEST
              }

              "error is displayed for box3Error" in {
                contentAsString(resultBox3Error) should include("Subtract the number in box 4 away from the number in box 3 and write it here")
              }

              "error is displayed for box4Error" in {
                contentAsString(resultBox4Error) should include("Subtract the number in box 4 away from the number in box 3 and write it here")
              }
            }

            "an error occurs (box3 empty)" when {
              lazy val request = FakeRequest().withFormUrlEncodedBody(
                "box1" -> "1000",
                "box2" -> "1000",
                "box3" -> "",
                "box4" -> "1000",
                "box5" -> "1000",
                "box6" -> "1000",
                "box7" -> "1000",
                "box8" -> "1000",
                "box9" -> "1000",
                "flatRateScheme" -> "true",
                "start" -> "2019-01-01",
                "end" -> "2019-01-04",
                "due" -> "2019-01-05").withSession(
                SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB,
                SessionKeys.viewModel -> sessionModel
              )

              lazy val result = {
                TestSubmitFormController.submit(periodKey = "93DH")(request)
              }

              "status is BAD_REQUEST" in {
                mockAuthorise(mtdVatAuthorisedResponse)
                setupVatSubscriptionService(vatSubscriptionResponse)
                status(result) shouldBe BAD_REQUEST
              }

              "error is displayed" in {
                contentAsString(result) should include("Enter a number")
              }
            }

            "an error occurs (box3 invalid format)" when {
              lazy val request = FakeRequest().withFormUrlEncodedBody(
                "box1" -> "1000",
                "box2" -> "1000",
                "box3" -> "e",
                "box4" -> "1000",
                "box5" -> "1000",
                "box6" -> "1000",
                "box7" -> "1000",
                "box8" -> "1000",
                "box9" -> "1000",
                "flatRateScheme" -> "true",
                "start" -> "2019-01-01",
                "end" -> "2019-01-04",
                "due" -> "2019-01-05"
              ).withSession(
                SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB,
                SessionKeys.viewModel -> sessionModel
              )

              lazy val result = {
                TestSubmitFormController.submit(periodKey = "93DH")(request)
              }

              "status is BAD_REQUEST" in {
                mockAuthorise(mtdVatAuthorisedResponse)
                setupVatSubscriptionService(vatSubscriptionResponse)
                status(result) shouldBe BAD_REQUEST
              }

              "error is displayed" in {
                contentAsString(result) should include("Enter a number in the correct format")
              }
            }

            "an error occurs (box3 incorrect amount of numbers)" when {

              lazy val request1 = FakeRequest().withFormUrlEncodedBody(
                "box1" -> "1000",
                "box2" -> "1000",
                "box3" -> "1234567890098765",
                "box4" -> "1000",
                "box5" -> "1000",
                "box6" -> "1000",
                "box7" -> "1000",
                "box8" -> "1000",
                "box9" -> "1000",
                "flatRateScheme" -> "true",
                "start" -> "2019-01-01",
                "end" -> "2019-01-04",
                "due" -> "2019-01-05"
              ).withSession(
                SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB,
                SessionKeys.viewModel -> sessionModel
              )

              lazy val request2 = FakeRequest().withFormUrlEncodedBody(
                "box1" -> "1000",
                "box2" -> "1000",
                "box3" -> "1234.3456789",
                "box4" -> "1000",
                "box5" -> "1000",
                "box6" -> "1000",
                "box7" -> "1000",
                "box8" -> "1000",
                "box9" -> "1000",
                "flatRateScheme" -> "true",
                "start" -> "2019-01-01",
                "end" -> "2019-01-04",
                "due" -> "2019-01-05"
              ).withSession(
                SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB,
                SessionKeys.viewModel -> sessionModel
              )

              lazy val result1 = {
                TestSubmitFormController.submit(periodKey = "93DH")(request1)
              }

              lazy val result2 = {
                TestSubmitFormController.submit(periodKey = "93DH")(request2)
              }

              "status is BAD_REQUEST for too many non decimal digits" in {
                mockAuthorise(mtdVatAuthorisedResponse)
                setupVatSubscriptionService(vatSubscriptionResponse)
                status(result1) shouldBe BAD_REQUEST
              }

              "status is BAD_REQUEST for too many decimal digits" in {
                mockAuthorise(mtdVatAuthorisedResponse)
                setupVatSubscriptionService(vatSubscriptionResponse)
                status(result2) shouldBe BAD_REQUEST
              }

              "error is displayed for too many non decimal digits" in {
                contentAsString(result1) should include("Enter a maximum of 13 digits for pounds." +
                  "\nEnter a maximum of 2 decimal places for pence.\nYou can use a negative amount eg -13.2")
              }

              "error is displayed for too many decimal digits" in {
                contentAsString(result2) should include("Enter a maximum of 13 digits for pounds." +
                  "\nEnter a maximum of 2 decimal places for pence.\nYou can use a negative amount eg -13.2")
              }
            }

            "an error occurs (box5 empty)" when {
              lazy val request = FakeRequest().withFormUrlEncodedBody(
                "box1" -> "1000",
                "box2" -> "1000",
                "box3" -> "12000",
                "box4" -> "1000",
                "box5" -> "",
                "box6" -> "1000",
                "box7" -> "1000",
                "box8" -> "1000",
                "box9" -> "1000",
                "flatRateScheme" -> "true",
                "start" -> "2019-01-01",
                "end" -> "2019-01-04",
                "due" -> "2019-01-05"
              ).withSession(
                SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB,
                SessionKeys.viewModel -> sessionModel
              )

              lazy val result = {
                TestSubmitFormController.submit(periodKey = "93DH")(request)
              }

              "status is BAD_REQUEST" in {
                mockAuthorise(mtdVatAuthorisedResponse)
                setupVatSubscriptionService(vatSubscriptionResponse)
                status(result) shouldBe BAD_REQUEST
              }

              "error is displayed" in {
                contentAsString(result) should include("Enter a number")
              }
            }

            "an error occurs (box5 invalid format)" when {

              lazy val request = FakeRequest().withFormUrlEncodedBody(
                "box1" -> "1000",
                "box2" -> "1000",
                "box3" -> "12000",
                "box4" -> "1000",
                "box5" -> "e",
                "box6" -> "1000",
                "box7" -> "1000",
                "box8" -> "1000",
                "box9" -> "1000",
                "flatRateScheme" -> "true",
                "start" -> "2019-01-01",
                "end" -> "2019-01-04",
                "due" -> "2019-01-05").withSession(
                SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB,
                SessionKeys.viewModel -> sessionModel
              )

              lazy val result = {
                TestSubmitFormController.submit(periodKey = "93DH")(request)
              }

              "status is BAD_REQUEST" in {
                mockAuthorise(mtdVatAuthorisedResponse)
                setupVatSubscriptionService(vatSubscriptionResponse)
                status(result) shouldBe BAD_REQUEST
              }

              "error is displayed" in {
                contentAsString(result) should include("Enter a number in the correct format")
              }
            }

            "an error occurs (box5 incorrect amount of numbers)" when {

              lazy val request1 = FakeRequest().withFormUrlEncodedBody(
                "box1" -> "1000",
                "box2" -> "1000",
                "box3" -> "2000",
                "box4" -> "1000",
                "box5" -> "1000123456789012345",
                "box6" -> "1000",
                "box7" -> "1000",
                "box8" -> "1000",
                "box9" -> "1000",
                "flatRateScheme" -> "true",
                "start" -> "2019-01-01",
                "end" -> "2019-01-04",
                "due" -> "2019-01-05"
              ).withSession(
                SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB,
                SessionKeys.viewModel -> sessionModel
              )

              lazy val request2 = FakeRequest().withFormUrlEncodedBody(
                "box1" -> "1000",
                "box2" -> "1000",
                "box3" -> "2000",
                "box4" -> "1000",
                "box5" -> "1000.1234567",
                "box6" -> "1000",
                "box7" -> "1000",
                "box8" -> "1000",
                "box9" -> "1000",
                "flatRateScheme" -> "true",
                "start" -> "2019-01-01",
                "end" -> "2019-01-04",
                "due" -> "2019-01-05"
              ).withSession(
                SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB,
                SessionKeys.viewModel -> sessionModel
              )

              lazy val result1 = {
                TestSubmitFormController.submit(periodKey = "93DH")(request1)
              }

              lazy val result2 = {
                TestSubmitFormController.submit(periodKey = "93DH")(request2)
              }

              "status is BAD_REQUEST for too many non decimal digits" in {
                mockAuthorise(mtdVatAuthorisedResponse)
                setupVatSubscriptionService(vatSubscriptionResponse)
                status(result1) shouldBe BAD_REQUEST
              }

              "status is BAD_REQUEST for too many decimal digits" in {
                mockAuthorise(mtdVatAuthorisedResponse)
                setupVatSubscriptionService(vatSubscriptionResponse)
                status(result2) shouldBe BAD_REQUEST
              }

              "error is displayed for too many non decimal digits" in {
                contentAsString(result1) should include("Enter a maximum of 13 digits for pounds." +
                  "\nEnter a maximum of 2 decimal places for pence.\nDo not use a negative amount eg -13.2")
              }

              "error is displayed for too many decimal digits" in {
                contentAsString(result2) should include("Enter a maximum of 13 digits for pounds." +
                  "\nEnter a maximum of 2 decimal places for pence.\nDo not use a negative amount eg -13.2")
              }
            }
          }

          "an unsuccessful response is received from the service" should {

            val vatSubscriptionFailureResponse: Future[HttpGetResult[CustomerDetails]] = Future.successful(Left(UnexpectedJsonFormat))

            lazy val request = FakeRequest().withFormUrlEncodedBody(
              "box1" -> "1000",
              "box2" -> "1000",
              "box3" -> "2000",
              "box4" -> "1000",
              "box5" -> "3000",
              "box6" -> "1000",
              "box7" -> "1000",
              "box8" -> "1000",
              "box9" -> "",
              "flatRateScheme" -> "true",
              "start" -> "2019-01-01",
              "end" -> "2019-01-04",
              "due" -> "2019-01-05"
            ).withSession(
              SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB,
              SessionKeys.viewModel -> sessionModel
            )

            lazy val result: Future[Result] = {
              TestSubmitFormController.submit("18AA")(request)
            }

            "return BAD_REQUEST" in {
              mockAuthorise(mtdVatAuthorisedResponse)
              setupVatSubscriptionService(vatSubscriptionFailureResponse)
              status(result) shouldBe Status.BAD_REQUEST
            }

            "return HTML" in {
              contentType(result) shouldBe Some("text/html")
            }

            "the user name should not be displayed" in {
              Jsoup.parse(bodyOf(result)).select("#content > article > div > h2").text() shouldBe ""
            }
          }

        }

        "there is no model in session" should {

          lazy val requestWithError = FakeRequest().withFormUrlEncodedBody(
            "box1" -> "1000.11",
            "box2" -> "1000",
            "box3" -> "2000.11",
            "box4" -> "1000",
            "box5" -> "1000.11",
            "box6" -> "1000",
            "box7" -> "1000.3",
            "box8" -> "1234567890123",
            "box9" -> "",
            "flatRateScheme" -> "true",
            "start" -> "2019-01-01",
            "end" -> "2019-01-04",
            "due" -> "2019-01-05"
          ).withSession(
            SessionKeys.mandationStatus -> MandationStatuses.nonMTDfB
          )

          "a successful response is received from the service" should {

            val obligations: VatObligations = VatObligations(Seq(
              VatObligation(
                LocalDate.parse("2019-01-12"),
                LocalDate.parse("2019-04-12"),
                LocalDate.parse("2019-05-12"),
                "18AA"
              )
            ))

            val vatObligationsResponse: Future[HttpGetResult[VatObligations]] = Future.successful(Right(obligations))

            lazy val result = {
              TestSubmitFormController.submit("18AA")(requestWithError)
            }

            "return 200" in {
              mockAuthorise(mtdVatAuthorisedResponse)
              setupVatSubscriptionService(vatSubscriptionResponse)
              setupVatObligationsService(vatObligationsResponse)
              status(result) shouldBe Status.OK
            }

            "return HTML" in {
              contentType(result) shouldBe Some("text/html")
            }
          }

          "an obligation doesn't match the provided period key" should {

            val obligations: VatObligations = VatObligations(Seq(
              VatObligation(
                LocalDate.parse("2019-01-12"),
                LocalDate.parse("2019-04-12"),
                LocalDate.parse("2019-05-12"),
                "17AA"
              )
            ))

            val vatObligationsResponse: Future[HttpGetResult[VatObligations]] = Future.successful(Right(obligations))

            lazy val result = {
              TestSubmitFormController.submit("18AA")(requestWithError)
            }

            "return a 303" in {
              mockAuthorise(mtdVatAuthorisedResponse)
              setupVatSubscriptionService(vatSubscriptionResponse)
              setupVatObligationsService(vatObligationsResponse)
              status(result) shouldBe Status.SEE_OTHER
            }

            s"redirect to ${mockAppConfig.returnDeadlinesUrl}" in {
              redirectLocation(result) shouldBe Some(mockAppConfig.returnDeadlinesUrl)
            }

          }

          "an error response is returned from the service" should {

            "return an internal server status" in {

              val vatSubscriptionErrorResponse: Future[HttpGetResult[CustomerDetails]] = Future.successful(Left(UnexpectedJsonFormat))
              val vatObligationsErrorResponse: Future[HttpGetResult[VatObligations]] = Future.successful(Left(UnexpectedJsonFormat))

              mockAuthorise(mtdVatAuthorisedResponse)
              setupVatSubscriptionService(vatSubscriptionErrorResponse)
              setupVatObligationsService(vatObligationsErrorResponse)

              lazy val result = TestSubmitFormController.submit("18AA")(requestWithError)
              status(result) shouldBe Status.INTERNAL_SERVER_ERROR

            }
          }
        }

      }

    authControllerChecks(TestSubmitFormController.submit(periodKey = "93DH"), fakeRequest)
  }
}
