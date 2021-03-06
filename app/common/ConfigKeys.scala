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

package common

object ConfigKeys {

  val googleAnalyticsToken: String = "google-analytics.token"
  val googleAnalyticsHost: String = "google-analytics.host"

  val contactFrontendService: String = "contact-frontend.host"

  val vatObligationsHost: String = "microservice.services.vat-obligations.host"
  val vatObligationPort: String = "microservice.services.vat-obligations.port"
  val vatObligationsContextUrl: String = "vat-obligations.contextUrl"

  val whitelistEnabled: String = "whitelist.enabled"
  val whitelistedIps: String = "whitelist.allowedIps"
  val whitelistExcludedPaths: String = "whitelist.excludedPaths"
  val whitelistShutterPage: String = "whitelist.shutter-page-url"

  val vatSubscriptionHost: String = "microservice.services.vat-subscription.host"
  val vatSubscriptionPort: String = "microservice.services.vat-subscription.port"

  val signInBaseUrl: String = "signIn.url"
  val signInContinueBaseUrl: String = "signIn.continueBaseUrl"
  val signInContinueUrl: String = "signIn.continueUrl"
  val timeoutPeriod: String = "timeout.period"
  val timeoutCountdown: String = "timeout.countDown"
  val appName: String = "appName"
  val vatAgentClientLookupFrontendHost: String = "vat-agent-client-lookup-frontend.host"
  val vatAgentClientLookupFrontendNonStubHost: String = "vat-agent-client-lookup-frontend.nonStubHost"
  val vatAgentClientLookupFrontendStartUrl: String = "vat-agent-client-lookup-frontend.startUrl"
  val vatAgentClientLookupFrontendUnauthorisedUrl: String = "vat-agent-client-lookup-frontend.unauthorisedUrl"
  val vatAgentClientLookupFrontendAgentActionUrl: String = "vat-agent-client-lookup-frontend.agentActionUrl"
  val platformHost: String = "platform.host"
  val vatSummaryHost: String = "vat-summary-frontend.host"
  val vatSummaryUrl: String = "vat-summary-frontend.url"
  val vatSummaryAccessibilityUrl: String = "vat-summary-frontend.accessibilityUrl"
  val viewVatReturnsHost: String = "view-vat-returns-frontend.host"
  val returnDeadlinesUrl: String = "view-vat-returns-frontend.url"
  val submittedReturnsUrl: String = "view-vat-returns-frontend.submittedReturnsUrl"
  val feedbackSurveyHost: String = "feedback-frontend.host"
  val feedbackSurveyUrl: String  = "feedback-frontend.url"
  val governmentGatewayHost: String = "government-gateway.host"
  val govUkGuidanceMtdVat: String = "gov-uk.guidance.mtdVat.url"
  val govUkGuidanceAgentServices: String = "gov-uk.guidance.agentServices.url"
  val vatReturnsBase: String = "vat-returns"
  val submitReturnUrl: String = s"microservice.services.$vatReturnsBase.returnUrl"
  val submitNrsUrl: String = s"microservice.services.$vatReturnsBase.nrsUrl"
  val manageClientUrl: String = "vat-agent-client-lookup-frontend.whatToDoUrl"

  val languageToggleFeature: String = "features.languageToggle.enabled"
  val staticDateEnabledFeature: String = "features.staticDate.enabled"
  val viewVatReturnEnabled: String = "features.viewVatReturn.enabled"
  val staticDateValue: String = "date-service.staticDate.value"

  val businessTaxAccount: String = "business-tax-account"
  val businessTaxAccountHost: String = "business-tax-account.host"
  val businessTaxAccountUrl: String = "business-tax-account.homeUrl"
  val businessTaxAccountMessagesUrl: String = "business-tax-account.messagesUrl"
  val businessTaxAccountManageAccountUrl: String = "business-tax-account.manageAccountUrl"

  val businessTaxAccountPartialUrl: String = "business-tax-account.partialUrl"

  val helpAndContactFrontendBase: String = "help-and-contact-frontend.host"
  val helpAndContactHelpUrl: String = "help-and-contact-frontend.helpUrl"
}
