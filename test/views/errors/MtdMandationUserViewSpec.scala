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

package views.errors

import models.auth.User
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.mvc.AnyContentAsEmpty
import views.ViewBaseSpec
import assets.messages.MtdMandationMessages
import auth.AuthKeys
import play.api.test.FakeRequest

class MtdMandationUserViewSpec extends ViewBaseSpec {

  "Rendering the mtd mandation user view" when {

    object Selectors {
      val serviceName = ".header__menu__proposition-name"
      val pageHeading = "#content article h1"
      val instructions = "#content article p"
      val instructionsLink = "#content article a"
    }

    "the user is a non agent" should {

      lazy val view = views.html.errors.mtd_mandated_user()(fakeRequest, mockAppConfig, messages, user)
      lazy implicit val document: Document = Jsoup.parse(view.body)

      "have the correct document title" in {
        document.title shouldBe MtdMandationMessages.title
      }

      "have a the correct page heading" in {
        elementText(Selectors.pageHeading) shouldBe MtdMandationMessages.heading
      }

      "have the correct instructions on the page" in {
        elementText(Selectors.instructions) shouldBe MtdMandationMessages.nonAgentParagraph
      }

      s"have the correct link text of ${MtdMandationMessages.nonAgentLinkText}" in {
        elementText(Selectors.instructionsLink) shouldBe MtdMandationMessages.nonAgentLinkText
      }

      "have a link to the return deadlines page" in {
        element(Selectors.instructionsLink).attr("href") shouldBe "/return-deadlines"
      }
    }

    "the user is an agent" should {

      lazy val fakeRequestWithClientsVRN: FakeRequest[AnyContentAsEmpty.type] =
        FakeRequest().withSession(AuthKeys.agentSessionVrn -> "999999999")

      lazy val agentUser: User[AnyContentAsEmpty.type] = User[AnyContentAsEmpty.type]("999999999", Some("XAIT012345678"))(fakeRequestWithClientsVRN)
      lazy val view = views.html.errors.mtd_mandated_user()(fakeRequestWithClientsVRN, mockAppConfig, messages, user = agentUser)
      lazy implicit val document: Document = Jsoup.parse(view.body)

      "have the correct instructions on the page" in {
        elementText(Selectors.instructions) shouldBe MtdMandationMessages.agentParagraph
      }

      s"have the correct link text of ${MtdMandationMessages.agentLinkText}" in {
        elementText(Selectors.instructionsLink) shouldBe MtdMandationMessages.agentLinkText
      }

      "have a link to the agent action page" in {
        element(Selectors.instructionsLink).attr("href") shouldBe "/agent-action"
      }
    }
  }
}
