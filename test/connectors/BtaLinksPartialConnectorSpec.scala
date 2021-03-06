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

package connectors

import play.api.http.Status
import play.twirl.api.Html
import uk.gov.hmrc.http.{HeaderCarrier, HttpReads}
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}
import config.VatHeaderCarrierForPartialsConverter
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import uk.gov.hmrc.play.partials.HtmlPartial
import uk.gov.hmrc.play.partials.HtmlPartial.{Failure, Success}
import views.templates.TemplateBaseSpec

class BtaLinksPartialConnectorSpec extends TemplateBaseSpec {

  val header: VatHeaderCarrierForPartialsConverter = injector.instanceOf[VatHeaderCarrierForPartialsConverter]
  implicit val ec: ExecutionContext = injector.instanceOf[ExecutionContext]
  val validHtml = Html("<nav>BTA lINK</nav>")

  private trait Test {
    implicit lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()
    val result :Future[HtmlPartial] = Future.successful(Success(None,validHtml))
    val httpClient: HttpClient = mock[HttpClient]
    lazy val connector: BtaLinksPartialConnector = {

      (httpClient.GET[HtmlPartial](_: String)(_: HttpReads[HtmlPartial],_: HeaderCarrier,_: ExecutionContext))
        .stubs(*,*,*,*)
        .returns(result)
      new BtaLinksPartialConnector(httpClient, header)(messagesApi, mockAppConfig)
    }
  }

  "BtaLinksPartialConnector" should {
    "generate the correct url" in new Test {
      connector.btaUrl shouldBe mockAppConfig.btaPartialUrl
    }
  }

  "getBtaLinksPartial" when{
    "a connectionExceptionsAsHtmlPartialFailure error is returned" should {
      "return the fall back partial" in new Test{
        override val result: Future[Failure] = Future.successful(Failure(Some(Status.GATEWAY_TIMEOUT)))
        await(connector.getBtaLinksPartial()) shouldBe views.html.templates.btaNavigationLinks()
      }
    }

    "an unexpected Exception is returned" should {
      "return the fall back partial" in new Test{
        override val result: Future[Failure] = Future.successful(Failure(Some(Status.INTERNAL_SERVER_ERROR)))
        await(connector.getBtaLinksPartial()) shouldBe views.html.templates.btaNavigationLinks()
      }
    }

    "a successful response is returned" should {
      "return the Bta partial" in new Test{
        await(connector.getBtaLinksPartial()) shouldBe validHtml
      }
    }
  }
}