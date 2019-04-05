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

package models

import play.api.libs.json.{Json, OFormat}
import play.api.http.Status._

case class ErrorModel(code: String, httpStatus: Int, message: String)

object FailedToParseCustomerDetails extends
  ErrorModel("PARSING_ERROR", INTERNAL_SERVER_ERROR, "There was an error parsing the Json returned from vat-subscription")
object FailedToRetrieveCustomerDetails extends
  ErrorModel("DOWNSTREAM_ERROR", INTERNAL_SERVER_ERROR, "Downstream error returned when retrieving CustomerDetails")

object ErrorModel {
  implicit val formats: OFormat[ErrorModel] = Json.format[ErrorModel]
}