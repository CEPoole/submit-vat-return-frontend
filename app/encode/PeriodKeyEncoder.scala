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

package encode

import java.net.URLEncoder
import models.SubmitVatFilters
import models.SubmitVatFilters._
import play.api.mvc.QueryStringBindable

object PeriodKeyEncoder {

  implicit def periodKeyEncoder(implicit stringBinder: QueryStringBindable[String]): QueryStringBindable[SubmitVatFilters] = {
    new QueryStringBindable[SubmitVatFilters] {
      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, SubmitVatFilters]] = {
        val boundPeriodKey = bindPeriodKey(periodKeyValue, params)

        boundPeriodKey match {
          case Right(Some(periodKey)) => Some(Right(SubmitVatFilters(periodKey)))
          case Right(None) => None
          case Left(error) => Some(Left(error))
        }
      }

      override def unbind(key: String, params: SubmitVatFilters): String = params.toSeqQueryParams.map {
        case (paramKey, paramValue) => s"$paramKey=${URLEncoder.encode(paramValue, "utf-8")}"
      }.mkString("&")

      private[encode] def bindPeriodKey(key: String, params: Map[String, Seq[String]]) = params.get(key) match {
        case Some(values) =>
          values.head match {
            case data if data.matches("^([0-9]{2}[A-Z0-9]{2})$|^(#[0-9]{3})$") => Right(Some(values.head))
            case _ => Left(s"Failed to bind '$key=${values.head}'.")
          }
        case _ => Right(None)
      }
    }
  }

}
