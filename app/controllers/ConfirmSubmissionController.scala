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

import config.{AppConfig, ErrorHandler}
import controllers.predicates.{AuthPredicate, MandationStatusPredicate}
import javax.inject.Inject
import models._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.Json
import play.api.mvc._
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.Future

class ConfirmSubmissionController @Inject()(val messagesApi: MessagesApi,
                                            val mandationStatusCheck: MandationStatusPredicate,
                                            val errorHandler: ErrorHandler,
                                            authPredicate: AuthPredicate,
                                            implicit val appConfig: AppConfig) extends FrontendController with I18nSupport {

  def show(frs: Boolean, obs: String, nbm: String, name: Option[String], periodKey: String): Action[AnyContent] = Action.async { implicit request =>
    val viewModel = ConfirmSubmissionViewModel(
      Json.parse(obs).as[VatObligations].obligations.head, frs, Json.parse(nbm).as[NineBoxModel], name
    )
    Future.successful(Ok(views.html.confirm_submission(viewModel)))
  }

  def submit(periodKey: String): Action[AnyContent] = Action.async { implicit request =>
    Future.successful(Redirect(controllers.routes.ConfirmationController.show().url))
  }
}