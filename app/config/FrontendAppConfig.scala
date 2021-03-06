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

package config

import java.util.Base64

import javax.inject.{Inject, Singleton}
import play.api.{Configuration, Environment}
import play.api.Mode.Mode
import uk.gov.hmrc.play.config.ServicesConfig
import common.ConfigKeys
import config.features.Features
import play.api.i18n.Lang
import play.api.mvc.Call
import uk.gov.hmrc.play.binders.ContinueUrl

trait AppConfig extends ServicesConfig {
  val reportAProblemPartialUrl: String
  val reportAProblemNonJSUrl: String
  val betaFeedbackUrl: String
  val betaFeedbackUnauthenticatedUrl: String
  val whitelistedIps: Seq[String]
  val whitelistEnabled: Boolean
  val whitelistExcludedPaths: Seq[Call]
  val shutterPage: String
  val signInUrl: String
  val timeoutPeriod: Int
  val timeoutCountdown: Int
  val agentClientLookupStartUrl: String => String
  val agentClientUnauthorisedUrl: String => String
  val govUkGuidanceMtdVat: String
  val govUkGuidanceAgentServices: String
  val vatSummaryUrl: String
  val manageClientUrl: String
  val changeClientUrl: String
  val returnDeadlinesUrl: String
  val viewSubmittedReturnUrl: String
  def signOutUrl(identifier: String): String
  val unauthorisedSignOutUrl: String
  def exitSurveyUrl(identifier: String): String
  val features: Features
  val staticDateValue: String
  def submitReturnUrl(vrn: String): String
  val submitNrsUrl: String
  val agentActionUrl: String
  val accessibilityLinkUrl: String
  def feedbackUrl(redirectUrl: String): String
  def routeToSwitchLanguage: String => Call
  def languageMap: Map[String, Lang]
  val btaBaseUrl: String
  val btaHomeUrl: String
  val btaMessagesUrl: String
  val btaManageAccountUrl: String
  val btaHelpAndContactUrl: String
  val btaPartialUrl: String
}

@Singleton
class FrontendAppConfig @Inject()(val runModeConfiguration: Configuration, environment: Environment) extends AppConfig {
  override protected def mode: Mode = environment.mode

  lazy val appName: String = getString(ConfigKeys.appName)

  private val contactHost = getString(ConfigKeys.contactFrontendService)
  private val contactFormServiceIdentifier = "VATC"

  lazy val reportAProblemPartialUrl = s"$contactHost/contact/problem_reports_ajax?service=$contactFormServiceIdentifier"
  lazy val reportAProblemNonJSUrl = s"$contactHost/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier"
  override lazy val betaFeedbackUrl = s"$contactHost/contact/beta-feedback"
  override lazy val betaFeedbackUnauthenticatedUrl = s"$contactHost/contact/beta-feedback-unauthenticated"

  // Gov.uk guidance
  override lazy val govUkGuidanceMtdVat: String = getString(ConfigKeys.govUkGuidanceMtdVat)
  override lazy val govUkGuidanceAgentServices: String = getString(ConfigKeys.govUkGuidanceAgentServices)

  //Language config
  override def routeToSwitchLanguage: String => Call = (lang: String) => controllers.routes.LanguageController.switchToLanguage(lang)
  override def languageMap: Map[String, Lang] = Map(
    "english" -> Lang("en"),
    "cymraeg" -> Lang("cy")
  )

  // Whitelist config
  private def whitelistConfig(key: String): Seq[String] = Some(new String(Base64.getDecoder
    .decode(getString(key)), "UTF-8"))
    .map(_.split(",")).getOrElse(Array.empty).toSeq

  override lazy val whitelistEnabled: Boolean = getBoolean(ConfigKeys.whitelistEnabled)
  override lazy val whitelistedIps: Seq[String] = whitelistConfig(ConfigKeys.whitelistedIps)
  override lazy val whitelistExcludedPaths: Seq[Call] = whitelistConfig(ConfigKeys.whitelistExcludedPaths) map
    (path => Call("GET", path))
  override val shutterPage: String = getString(ConfigKeys.whitelistShutterPage)

  // Sign-in
  private lazy val signInBaseUrl: String = getString(ConfigKeys.signInBaseUrl)
  private lazy val signInContinueBaseUrl: String = getString(ConfigKeys.signInContinueBaseUrl)
  private lazy val signInContinueUrl: String = signInContinueBaseUrl + getString(ConfigKeys.signInContinueUrl)
  private lazy val signInOrigin = getString(ConfigKeys.appName)
  override lazy val signInUrl: String = s"$signInBaseUrl?continue=$signInContinueUrl&origin=$signInOrigin"

  //Sign-out
  private lazy val feedbackSurveyBaseUrl =getString(ConfigKeys.feedbackSurveyHost) + getString(ConfigKeys.feedbackSurveyUrl)
  override def exitSurveyUrl(identifier: String): String = s"$feedbackSurveyBaseUrl/$identifier"

  //Session timeout countdown
  override lazy val timeoutCountdown: Int = getInt(ConfigKeys.timeoutCountdown)
  override lazy val timeoutPeriod: Int = getInt(ConfigKeys.timeoutPeriod)

  private lazy val governmentGatewayHost: String = getString(ConfigKeys.governmentGatewayHost)

  override lazy val unauthorisedSignOutUrl: String = s"$governmentGatewayHost/gg/sign-out?continue=$signInContinueUrl"
  override def signOutUrl(identifier: String): String =
    s"$governmentGatewayHost/gg/sign-out?continue=${exitSurveyUrl(identifier)}"

  override lazy val vatSummaryUrl: String = getString(ConfigKeys.vatSummaryHost) + getString(ConfigKeys.vatSummaryUrl)
  override lazy val returnDeadlinesUrl: String = getString(ConfigKeys.viewVatReturnsHost) + getString(ConfigKeys.returnDeadlinesUrl)

  override lazy val viewSubmittedReturnUrl: String = getString(ConfigKeys.viewVatReturnsHost) + getString(ConfigKeys.submittedReturnsUrl)

  // Agent Client Lookup
  private lazy val platformHost = getString(ConfigKeys.platformHost)
  private lazy val agentClientLookupRedirectUrl: String => String = uri => ContinueUrl(platformHost + uri).encodedUrl
  private lazy val agentClientLookupHost = getString(ConfigKeys.vatAgentClientLookupFrontendHost)
  override lazy val agentClientLookupStartUrl: String => String = uri =>
    agentClientLookupHost +
    getString(ConfigKeys.vatAgentClientLookupFrontendStartUrl) +
    s"?redirectUrl=${agentClientLookupRedirectUrl(uri)}"
  override lazy val agentClientUnauthorisedUrl: String => String = uri =>
    agentClientLookupHost +
    getString(ConfigKeys.vatAgentClientLookupFrontendUnauthorisedUrl) +
    s"?redirectUrl=${agentClientLookupRedirectUrl(uri)}"

  override lazy val manageClientUrl: String =
    getString(ConfigKeys.vatAgentClientLookupFrontendNonStubHost) + getString(ConfigKeys.manageClientUrl)
  override lazy val changeClientUrl: String =
    getString(ConfigKeys.vatAgentClientLookupFrontendHost) + getString(ConfigKeys.vatAgentClientLookupFrontendStartUrl)
  override lazy val agentActionUrl: String = agentClientLookupHost + getString(ConfigKeys.vatAgentClientLookupFrontendAgentActionUrl)

  private lazy val vatReturnsHost: String = baseUrl(ConfigKeys.vatReturnsBase)
  override def submitReturnUrl(vrn: String): String = s"$vatReturnsHost/${getString(ConfigKeys.submitReturnUrl)}/$vrn"
  override lazy val submitNrsUrl: String = s"$vatReturnsHost/${getString(ConfigKeys.submitNrsUrl)}"

  override val features = new Features(runModeConfiguration)
  override lazy val staticDateValue: String = getString(ConfigKeys.staticDateValue)

  override def feedbackUrl(redirectUrl: String): String = {s"$contactHost/contact/beta-feedback?service=$contactFormServiceIdentifier" +
    s"&backUrl=${ContinueUrl(platformHost + redirectUrl).encodedUrl}"}

  override val accessibilityLinkUrl: String = getString(ConfigKeys.vatSummaryHost) + getString(ConfigKeys.vatSummaryAccessibilityUrl)


  private lazy val helpAndContactFrontendUrl: String = getString(ConfigKeys.helpAndContactFrontendBase)

  override lazy val btaBaseUrl: String = getString(ConfigKeys.businessTaxAccountHost)
  override lazy val btaHomeUrl: String = btaBaseUrl + getString(ConfigKeys.businessTaxAccountUrl)
  override lazy val btaMessagesUrl: String = btaHomeUrl + getString(ConfigKeys.businessTaxAccountMessagesUrl)
  override lazy val btaManageAccountUrl: String = btaHomeUrl + getString(ConfigKeys.businessTaxAccountManageAccountUrl)
  override lazy val btaHelpAndContactUrl: String = helpAndContactFrontendUrl + getString(ConfigKeys.helpAndContactHelpUrl)

  private lazy val btaMicroserviceUrl: String = baseUrl(ConfigKeys.businessTaxAccount)
  override lazy val btaPartialUrl: String = btaMicroserviceUrl + getString(ConfigKeys.businessTaxAccountPartialUrl)

}
