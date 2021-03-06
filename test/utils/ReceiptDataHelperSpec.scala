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

import java.time.LocalDate

import base.BaseSpec
import models.auth.User
import models.errors.{BadRequestError, UnknownError}
import models.nrs._
import models.{CustomerDetails, SubmitVatReturnModel}
import play.api.mvc.{AnyContentAsEmpty, Cookie}


class ReceiptDataHelperSpec extends BaseSpec {

  val service = new ReceiptDataHelper()

  val dateToUse: LocalDate = LocalDate.now()

  def createReturnModel(flatRateScheme: Boolean): SubmitVatReturnModel =
    SubmitVatReturnModel(
      10.00,
      25.55,
      33.00,
      74.00,
      95.06,
      1006,
      1006.66,
      889.9,
      900.0,
      flatRateScheme,
      dateToUse,
      dateToUse,
      dateToUse
    )

  def expectedAnswers(frs: Boolean, language: Language): Seq[Answers] = {
    def ifEnglishElse(ifEnglish: String, ifWelsh: String): String = if (language == EN) ifEnglish else ifWelsh

    val box1Expected = ifEnglishElse(
      "VAT you charged on sales and other supplies",
      "TAW a godwyd gennych ar werthiannau a chyflenwadau eraill"
    )

    val box2Expected = ifEnglishElse(
      "VAT you owe on goods purchased from EC countries and brought into the UK",
      "TAW sydd arnoch ar nwyddau a brynwyd o wledydd y GE ac y daethpwyd â nhw i mewn i’r DU"
    )

    val box3Expected = ifEnglishElse(
      "VAT you owe before deductions (this is the total of box 1 and 2)",
      "TAW sydd arnoch cyn didyniadau (dyma gyfanswm blwch 1 a 2)"
    )

    val box4Expected = ifEnglishElse(
      "VAT you have claimed back",
      "TAW yr ydych wedi’i hawlio’n ôl"
    )

    val box5Expected = ifEnglishElse(
      "Net VAT you owe HMRC or HMRC owes you (this is the difference between box 3 and 4)",
      "TAW net sydd arnoch i CThEM neu sydd ar CThEM i chi (dyma’r gwahaniaeth rhwng Blychau 3 a 4)"
    )

    val box6Expected = (frs, language) match {
      case (true, EN) => "Total value of sales and other supplies, including VAT"
      case (false, EN) => "Total value of sales and other supplies, excluding VAT"
      case (true, CY) => "Cyfanswm gwerth gwerthiannau a chyflenwadau eraill, gan gynnwys TAW"
      case (_, _) => "Cyfanswm gwerth y gwerthiannau a chyflenwadau eraill, ac eithrio TAW"
    }

    val box7Expected = ifEnglishElse(
      "Total value of purchases and other expenses, excluding VAT",
      "Cyfanswm gwerth y pryniannau a threuliau eraill, ac eithrio TAW"
    )

    val box8Expected = ifEnglishElse(
      "Total value of supplied goods to EC countries and related costs (excluding VAT)",
      "Cyfanswm gwerth y nwyddau a gyflenwyd i wledydd y GE a chostau perthynol (ac eithrio TAW)"
    )

    val box9Expected = ifEnglishElse(
      "Total value of goods purchased from EC countries and brought into the UK, as well as any related costs (excluding VAT)",
      "Cyfanswm gwerth y nwyddau a brynwyd o wledydd y GE ac y daethpwyd â nhw i mewn i’r DU, yn ogystal ag unrhyw gostau perthynol (ac eithrio TAW)"
    )

    Seq(Answers(
      "VAT Return submission complete",
      Seq(
        ("box1", box1Expected, Some("£10.00"), None),
        ("box2", box2Expected, Some("£25.55"), None),
        ("box3", box3Expected, Some("£33.00"), None),
        ("box4", box4Expected, Some("£74.00"), None),
        ("box5", box5Expected, Some("£95.06"), None),
        ("box6", box6Expected, Some("£1,006.00"), None),
        ("box7", box7Expected, Some("£1,006.66"), None),
        ("box8", box8Expected, Some("£889.90"), None),
        ("box9", box9Expected, Some("£900.00"), None)
      ).map((Answer.apply _).tupled(_))))
  }

  def expectedDeclaration(isAgent: Boolean, language: Language): Declaration = {
    val declarationText: String = (isAgent, language) match {
      case (true, EN) => "I confirm that my client has received a copy of the information contained in this return and approved " +
        "the information as being correct and complete to the best of their knowledge and belief."
      case (false, EN) => "By submitting this return, you are making a legal declaration that the information is " +
        "correct and complete to the best of your knowledge and belief. A false declaration can result in prosecution."
      case (true, CY) => "Rwy’n cadarnhau bod fy nghleient wedi cael copi o’r wybodaeth sydd ar y Ffurflen TAW hon, ac " +
        "wedi cytuno bod yr wybodaeth yn gywir ac yn gyflawn hyd eithaf ei wybodaeth a’i gred."
      case (false, CY) => "Drwy gyflwyno’r Ffurflen TAW hon, rydych yn gwneud datganiad cyfreithiol bod yr wybodaeth yn gywir " +
        "ac yn gyflawn hyd eithaf eich gwybodaeth a’ch cred. Gall datganiad ffug arwain at erlyniad."
    }

    Declaration(declarationText, "Test Name", None, declarationConsent = true)
  }

  val errorResponse = Left(BadRequestError("Bad Request", "There has been a bad request"))

  private def successResponse(frs: Boolean, noName: Boolean = false) = {
    Right(CustomerDetails(
      if(noName) None else Some("Test"), if(noName) None else Some("Name"), None, None, frs
    ))
  }

  def userWithCookie(cookieString: String): User[AnyContentAsEmpty.type] = User[AnyContentAsEmpty.type](vrn)(fakeRequest
    .withCookies(Cookie(
      "PLAY_LANG", cookieString, None, "/", None, secure = false, httpOnly = true
    )))

  def agentUserWithCookie(cookieString: String): User[AnyContentAsEmpty.type] = User[AnyContentAsEmpty.type](vrn, Some(arn))(fakeRequestWithClientsVRN
    .withCookies(Cookie(
      "PLAY_LANG", cookieString, None, "/", None, secure = false, httpOnly = true
    )))

  "extractReceiptData" should {
    "return a receipt" when {
      "the user is an agent, with frs" in {
        val userToUse = agentUserWithCookie(EN.languageCode)

        val result = await(service.extractReceiptData(createReturnModel(true), successResponse(frs = true))(userToUse, hc, ec))

        result shouldBe Right(
          ReceiptData(EN, expectedAnswers(frs = true, EN), expectedDeclaration(isAgent = true, EN))
        )
      }
      "the user is an individual, without frs" in {
        val userToUse = userWithCookie(CY.languageCode)

        val result = await(service.extractReceiptData(createReturnModel(false), successResponse(frs = false))(userToUse, hc, ec))

        result shouldBe Right(
          ReceiptData(CY, expectedAnswers(frs = false, CY), expectedDeclaration(isAgent = false, CY))
        )
      }
      "the user has no language cookie" in {
        val result = await(service.extractReceiptData(createReturnModel(false), successResponse(frs = false))(user, hc, ec))

        result shouldBe Right(
          ReceiptData(EN, expectedAnswers(frs = false, EN), expectedDeclaration(isAgent = false, EN))
        )
      }
    }
    "return an error" when {

      val implicitUser: User[AnyContentAsEmpty.type] = userWithCookie(EN.languageCode)

      "there is an error from vat subscription" in {
        val expectedResult = BadRequestError("Bad Request", "There has been a bad request")

        val result = await(service.extractReceiptData(createReturnModel(true), errorResponse)(user = implicitUser, hc, ec))

        result shouldBe Left(expectedResult)
      }

      "the user has no name" in {
        val expectedResult = UnknownError

        val result = await(service.extractReceiptData(createReturnModel(true), successResponse(frs = true, noName = true))(user = implicitUser, hc, ec))

        result shouldBe Left(expectedResult)
      }
    }
  }
}
