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

package config.filters

import mocks.MockConfig
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.{Application, Configuration}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.Results.Ok
import play.api.mvc.{Action, Call}
import play.api.test.FakeRequest
import play.api.test.Helpers._

class WhitelistFilterSpec extends PlaySpec with GuiceOneServerPerSuite {

  lazy implicit val config: Configuration = app.configuration

  lazy val mockConfig: MockConfig = new MockConfig()

  override implicit lazy val app: Application = {
    new GuiceApplicationBuilder()
      .configure(Configuration(
        "whitelist.enabled" -> true
      ))
      .routes({
        case ("GET", "/submission-confirmation") => Action(Ok("success"))
        case _ => Action(Ok("failure"))
      })
      .build()
  }

  "Whitelist filter" when {

    "supplied with a non whitelisted IP" should {

      lazy val fakeRequest = FakeRequest("GET", "/submission-confirmation").withHeaders(
        "True-Client-IP" -> "127.0.0.2"
      )

      Call(fakeRequest.method, fakeRequest.uri)

      lazy val Some(result) = route(app, fakeRequest)

      "return status of 303" in {
        status(result) mustBe 303
      }

      "redirect to shutter page" in {
        redirectLocation(result) mustBe Some(mockConfig.shutterPage)
      }
    }

    "supplied with a whitelisted IP" should {

      lazy val fakeRequest = FakeRequest("GET", "/submission-confirmation").withHeaders(
        "True-Client-IP" -> "127.0.0.1"
      )

      lazy val Some(result) = route(app, fakeRequest)

      "return status of 200" in {
        status(result) mustBe 200
      }

      "return success" in {
        contentAsString(result) mustBe "success"
      }
    }
  }
}
