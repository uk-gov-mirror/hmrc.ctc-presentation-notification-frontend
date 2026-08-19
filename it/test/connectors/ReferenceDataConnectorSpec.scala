/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.data.NonEmptySet
import com.github.tomakehurst.wiremock.client.WireMock.*
import connectors.ReferenceDataConnector.NoReferenceDataFoundException
import itbase.{ItSpecBase, WireMockServerHandler}
import models.LocationOfGoodsIdentification
import models.reference.*
import models.reference.TransportMode.{BorderMode, InlandMode}
import models.reference.transport.border.active.Identification
import models.reference.transport.transportMeans.TransportMeansIdentification
import org.scalacheck.Gen
import org.scalatest.{Assertion, EitherValues}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.cache.AsyncCacheApi
import play.api.inject.guice.GuiceApplicationBuilder

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ReferenceDataConnectorSpec extends ItSpecBase with WireMockServerHandler with ScalaCheckPropertyChecks with EitherValues {

  private val baseUrl = "customs-reference-data/test-only"

  private lazy val connector = app.injector.instanceOf[ReferenceDataConnector]

  override def guiceApplicationBuilder(): GuiceApplicationBuilder = super
    .guiceApplicationBuilder()
    .configure(
      conf = "microservice.services.customs-reference-data.port" -> server.port()
    )

  private lazy val asyncCacheApi: AsyncCacheApi = app.injector.instanceOf[AsyncCacheApi]

  override def beforeEach(): Unit = {
    super.beforeEach()
    asyncCacheApi.removeAll().futureValue
  }

  private val customsOfficesResponseJson: String =
    """
      |[
      |  {
      |    "referenceNumber": "GB1",
      |    "customsOfficeLsd" : {
      |      "customsOfficeUsualName" : "testName1"
      |    }
      |  },
      |  {
      |    "referenceNumber": "GB2",
      |    "customsOfficeLsd" : {
      |      "customsOfficeUsualName" : "testName2"
      |    }
      |  }
      |]
      |""".stripMargin

  private val countriesResponseJson: String =
    s"""
       |[
       |  {
       |    "key": "GB",
       |    "value": "United Kingdom"
       |  },
       |  {
       |    "key": "AD",
       |    "value": "Andorra"
       |  }
       |]
       |""".stripMargin

  private val countryResponseJson: String =
    s"""
       |[
       |    {
       |      "key": "GB",
       |      "value": "United Kingdom"
       |    }
       |]
       |""".stripMargin

  private val unLocodeResponseJson: String = """
      |[
      |    {
      |      "key": "UN1",
      |      "value": "testName1"
      |    }
      |]
      |""".stripMargin

  private val nationalitiesResponseJson: String =
    """
      |[
      |    {
      |      "key":"AR",
      |      "value":"Argentina"
      |    },
      |    {
      |      "key":"AU",
      |      "value":"Australia"
      |    }
      |]
      |""".stripMargin

  private val nationalityResponseJson: String =
    """
      |[
      |    {
      |      "key":"AR",
      |      "value":"Argentina"
      |    }
      |]
      |""".stripMargin

  private val locationTypesResponseJson: String =
    """
      |[
      |  {
      |    "key": "A",
      |    "value": "Designated location"
      |  },
      |  {
      |    "key": "B",
      |    "value": "Authorised place"
      |   }
      |]
      |""".stripMargin

  private val locationTypeResponseJson: String =
    """
      |[
      |  {
      |    "key": "A",
      |    "value": "Designated location"
      |  }
      |]
      |""".stripMargin

  private val locationOfGoodsIdentificationResponseJson: String =
    """
      |[
      |    {
      |      "key":"T",
      |      "value":"Postal code"
      |    },
      |    {
      |      "key":"X",
      |      "value":"EORI number"
      |    }
      |]
      |""".stripMargin

  private val meansOfTransportIdentificationTypesActiveResponseJson: String =
    """
      |[
      |    {
      |      "key":"10",
      |      "value":"IMO Ship Identification Number"
      |    },
      |    {
      |      "key":"11",
      |      "value":"Name of the sea-going vessel"
      |    }
      |]
      |""".stripMargin

  private val meansOfTransportIdentificationTypeActiveResponseJson: String =
    """
      |[
      |    {
      |      "key":"10",
      |      "value":"IMO Ship Identification Number"
      |    }
      |]
      |""".stripMargin

  private val meansOfTransportIdentificationTypesResponseJson: String =
    """
      |[
      |    {
      |      "key":"10",
      |      "value":"IMO Ship Identification Number"
      |    },
      |    {
      |      "key":"11",
      |      "value":"Name of the sea-going vessel"
      |    }
      |]
      |""".stripMargin

  private val meansOfTransportIdentificationTypeResponseJson: String =
    """
      |[
      |    {
      |      "key":"10",
      |      "value":"IMO Ship Identification Number"
      |    }
      |]
      |""".stripMargin

  private val emptyResponseJson: String =
    """
      |[]
      |""".stripMargin

  "Reference Data" - {

    "getTypesOfLocation" - {
      val url = s"/$baseUrl/lists/TypeOfLocation"

      "must return Seq of security types when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(locationTypesResponseJson))
        )

        val expectedResult = NonEmptySet.of(
          LocationType("A", "Designated location"),
          LocationType("B", "Authorised place")
        )

        connector.getTypesOfLocation().futureValue.value mustEqual expectedResult

      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, connector.getTypesOfLocation())
      }

      "must handle client and server errors for control types" in {
        checkErrorResponse(url, connector.getTypesOfLocation())
      }
    }

    "getTypeOfLocation" - {
      val locationType = "A"

      def url(locationType: String) = s"/$baseUrl/lists/TypeOfLocation?keys=$locationType"
      "must return Seq of security types when successful" in {
        server.stubFor(
          get(urlEqualTo(url(locationType)))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(locationTypeResponseJson))
        )

        val expectedResult = LocationType("A", "Designated location")

        connector.getTypeOfLocation(locationType).futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url(locationType), connector.getTypeOfLocation(locationType))
      }

      "must handle client and server errors for control types" in {
        checkErrorResponse(url(locationType), connector.getTypeOfLocation(locationType))
      }
    }

    "getCustomsOfficesOfTransitForCountry" - {

      def url(countryId: String) = s"/$baseUrl/lists/CustomsOffices?countryCodes=$countryId&roles=TRA"

      "must return a successful future response with a sequence of CustomsOffices" in {

        val countryId = "GB"

        server.stubFor(
          get(urlEqualTo(url(countryId)))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(customsOfficesResponseJson))
        )

        val expectedResult = NonEmptySet.of(
          CustomsOffice("GB1", "testName1", None),
          CustomsOffice("GB2", "testName2", None)
        )

        connector.getCustomsOfficesOfTransitForCountry(CountryCode(countryId)).futureValue.value mustEqual expectedResult

      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        val countryId = "AR"
        checkNoReferenceDataFoundResponse(url(countryId), connector.getCustomsOfficesOfTransitForCountry(CountryCode(countryId)))
      }

      "must handle client and server errors for control types" in {
        val countryId = "GB"
        checkErrorResponse(url(countryId), connector.getCustomsOfficesOfTransitForCountry(CountryCode(countryId)))
      }
    }

    "getCustomsOfficeForId" - {

      def url(officeId: String) = s"/$baseUrl/lists/CustomsOffices?referenceNumbers=$officeId"

      "must return a successful future response with a sequence of CustomsOffices" in {

        val id = "GB1"

        server.stubFor(
          get(urlEqualTo(url(id)))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(customsOfficesResponseJson))
        )

        val expectedResult = CustomsOffice("GB1", "testName1", None)

        connector.getCustomsOfficeForId(id).futureValue.value mustEqual expectedResult

      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        val id = "GB3"
        checkNoReferenceDataFoundResponse(url(id), connector.getCustomsOfficeForId(id))
      }

      "must return an exception when an error response is returned" in {
        val id = "GB1"
        checkErrorResponse(url(id), connector.getCustomsOfficeForId(id))
      }
    }

    "getCustomsOfficesForIds" - {

      val ids = Seq("GB1", "GB2")

      val url = s"/$baseUrl/lists/CustomsOffices?referenceNumbers=GB1&referenceNumbers=GB2"

      "must return a successful future response with a sequence of CustomsOffices" in {

        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(customsOfficesResponseJson))
        )

        val expectedResult = NonEmptySet.of(
          CustomsOffice("GB1", "testName1", None),
          CustomsOffice("GB2", "testName2", None)
        )

        connector.getCustomsOfficesForIds(ids).futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, connector.getCustomsOfficesForIds(ids))
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getCustomsOfficesForIds(ids))
      }
    }

    "getCustomsOfficesOfDestinationForCountry" - {

      def url(countryId: String) = s"/$baseUrl/lists/CustomsOffices?countryCodes=$countryId&roles=DES"

      "must return a successful future response with a sequence of CustomsOffices" in {

        val countryId = "GB"

        server.stubFor(
          get(urlEqualTo(url(countryId)))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(customsOfficesResponseJson))
        )

        val expectedResult = NonEmptySet.of(
          CustomsOffice("GB1", "testName1", None),
          CustomsOffice("GB2", "testName2", None)
        )

        connector.getCustomsOfficesOfDestinationForCountry(CountryCode(countryId)).futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        val countryId = "AR"
        checkNoReferenceDataFoundResponse(
          url(countryId),
          connector.getCustomsOfficesOfDestinationForCountry(CountryCode(countryId))
        )
      }

      "must return an exception when an error response is returned" in {
        val countryId = "GB"
        checkErrorResponse(url(countryId), connector.getCustomsOfficesOfDestinationForCountry(CountryCode(countryId)))
      }
    }

    "getNationalities" - {
      val url: String = s"/$baseUrl/lists/Nationality"

      "must return Seq of Country when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(nationalitiesResponseJson))
        )

        val expectedResult = NonEmptySet.of(
          Nationality("AR", "Argentina"),
          Nationality("AU", "Australia")
        )

        connector.getNationalities().futureValue.value mustEqual expectedResult

      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, connector.getNationalities())
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getNationalities())
      }
    }

    "getNationality" - {
      val code = "AR"

      def url(code: String): String = s"/$baseUrl/lists/Nationality?keys=$code"
      "must return a Nationality when successful" in {
        server.stubFor(
          get(urlEqualTo(url(code)))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(nationalityResponseJson))
        )

        val expectedResult = Nationality("AR", "Argentina")

        connector.getNationality(code).futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url(code), connector.getNationality(code))
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url(code), connector.getNationality(code))
      }
    }

    "getTransportModeCodes" - {
      val url: String = s"/$baseUrl/lists/TransportModeCode"

      "when inland modes" - {

        "must return Seq of inland modes when successful" in {
          val responseJson: String =
            """
                |[
                |    {
                |      "key": "1",
                |      "value": "Maritime Transport"
                |    },
                |    {
                |      "key": "2",
                |      "value": "Rail Transport"
                |    }
                |]
                |""".stripMargin

          server.stubFor(
            get(urlEqualTo(url))
              .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
              .willReturn(okJson(responseJson))
          )

          val expectedResult = NonEmptySet.of(
            InlandMode("1", "Maritime Transport"),
            InlandMode("2", "Rail Transport")
          )

          connector.getInlandModes().futureValue.value mustEqual expectedResult
        }

        "must throw a NoReferenceDataFoundException for an empty response" in {
          checkNoReferenceDataFoundResponse(url, connector.getInlandModes())
        }

        "must return an exception when an error response is returned" in {
          checkErrorResponse(url, connector.getInlandModes())
        }

      }

      "when border modes" - {

        "must return Seq of border modes when successful" in {
          val responseJson: String =
            """
                |[
                |    {
                |      "key": "1",
                |      "value": "Maritime Transport"
                |    },
                |    {
                |      "key": "1",
                |      "value": "Maritime Transport"
                |    },
                |    {
                |      "key": "2",
                |      "value": "Rail Transport"
                |    }
                |]
                |""".stripMargin

          server.stubFor(
            get(urlEqualTo(url))
              .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
              .willReturn(okJson(responseJson))
          )

          val expectedResult = NonEmptySet.of(
            BorderMode("1", "Maritime Transport"),
            BorderMode("2", "Rail Transport")
          )

          connector.getBorderModes().futureValue.value mustEqual expectedResult
        }

        "must throw a NoReferenceDataFoundException for an empty response" in {
          checkNoReferenceDataFoundResponse(url, connector.getBorderModes())
        }

        "must return an exception when an error response is returned" in {
          checkErrorResponse(url, connector.getBorderModes())
        }
      }
    }

    "getInlandModeCode" - {
      val inlandModeCode = "1"

      def url(code: String): String = s"/$baseUrl/lists/TransportModeCode?keys=$code"
      "must return inland mode when successful" in {
        val responseJson: String =
          """
                |[
                |    {
                |      "key": "1",
                |      "value": "Maritime Transport"
                |    }
                |]
                |""".stripMargin

        server.stubFor(
          get(urlEqualTo(url(inlandModeCode)))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(responseJson))
        )

        val expectedResult = InlandMode("1", "Maritime Transport")

        connector.getInlandMode(inlandModeCode).futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url(inlandModeCode), connector.getInlandMode(inlandModeCode))
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url(inlandModeCode), connector.getInlandMode(inlandModeCode))
      }

    }

    "getBorderModeCode" - {
      val borderModeCode = "1"

      def url(code: String): String = s"/$baseUrl/lists/TransportModeCode?keys=$code"
      "must return inland mode when successful" in {
        val responseJson: String =
          """
                |[
                |    {
                |      "key": "1",
                |      "value": "Maritime Transport"
                |    }
                |]
                |""".stripMargin

        server.stubFor(
          get(urlEqualTo(url(borderModeCode)))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(responseJson))
        )

        val expectedResult = BorderMode("1", "Maritime Transport")

        connector.getBorderMode(borderModeCode).futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url(borderModeCode), connector.getBorderMode(borderModeCode))
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url(borderModeCode), connector.getBorderMode(borderModeCode))
      }
    }

    "getQualifierOfTheIdentifications" - {
      val url: String = s"/$baseUrl/lists/QualifierOfTheIdentification"

      "must return Seq of Identification qualifiers when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(locationOfGoodsIdentificationResponseJson))
        )

        val expectedResult = NonEmptySet.of(
          LocationOfGoodsIdentification("T", "Postal code"),
          LocationOfGoodsIdentification("X", "EORI number")
        )

        connector.getQualifierOfTheIdentifications().futureValue.value mustEqual expectedResult

      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, connector.getQualifierOfTheIdentifications())
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getQualifierOfTheIdentifications())
      }
    }

    "getCustomsOfficesOfExitForCountry" - {

      def url(countryId: String) = s"/$baseUrl/lists/CustomsOffices?countryCodes=$countryId&roles=EXT"

      "must return a successful future response with a sequence of CustomsOffices" in {

        val countryId = "GB"

        server.stubFor(
          get(urlEqualTo(url(countryId)))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(customsOfficesResponseJson))
        )

        val expectedResult = NonEmptySet.of(
          CustomsOffice("GB1", "testName1", None),
          CustomsOffice("GB2", "testName2", None)
        )

        connector.getCustomsOfficesOfExitForCountry(CountryCode(countryId)).futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        val countryId = "AR"
        checkNoReferenceDataFoundResponse(url(countryId), connector.getCustomsOfficesOfExitForCountry(CountryCode(countryId)))
      }

      "must return an exception when an error response is returned" in {
        val countryId = "GB"
        checkErrorResponse(url(countryId), connector.getCustomsOfficesOfExitForCountry(CountryCode(countryId)))
      }
    }

    "getCustomsOfficesOfDepartureForCountry" - {
      def url(countryId: String) = s"/$baseUrl/lists/CustomsOffices?countryCodes=$countryId&roles=DEP"

      "must return a successful future response with a sequence of CustomsOffices" in {

        val countryId = "GB"

        server.stubFor(
          get(urlEqualTo(url(countryId)))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(customsOfficesResponseJson))
        )

        val expectedResult = NonEmptySet.of(
          CustomsOffice("GB1", "testName1", None),
          CustomsOffice("GB2", "testName2", None)
        )

        connector.getCustomsOfficesOfDepartureForCountry(countryId).futureValue.value mustEqual expectedResult

      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        val countryId = "AR"
        checkNoReferenceDataFoundResponse(url(countryId), connector.getCustomsOfficesOfDepartureForCountry(countryId))
      }

      "must return an exception when an error response is returned" in {
        val countryId = "GB"
        checkErrorResponse(url(countryId), connector.getCustomsOfficesOfDepartureForCountry(countryId))
      }
    }

    "getCountries for full list" - {
      val url = s"/$baseUrl/lists/CountryCodesFullList"

      "must return Seq of Country when successful" in {

        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(countriesResponseJson))
        )

        val expectedResult = NonEmptySet.of(
          Country(CountryCode("GB"), "United Kingdom"),
          Country(CountryCode("AD"), "Andorra")
        )
        connector.getCountries("CountryCodesFullList").futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, connector.getCountries("CountryCodesFullList"))
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getCountries("CountryCodesFullList"))
      }
    }

    "getCountriesWithoutZipCountry" - {

      def url(countryId: String) = s"/$baseUrl/lists/CountryWithoutZip?keys=$countryId"
      "must return Seq of Country when successful" in {
        val countryId = "GB"
        server.stubFor(
          get(urlEqualTo(url(countryId)))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(countryResponseJson))
        )

        val expectedResult = CountryCode(countryId)

        connector.getCountriesWithoutZipCountry(countryId).futureValue.value mustEqual expectedResult

      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        val countryId = "AD"
        checkNoReferenceDataFoundResponse(url(countryId), connector.getCountriesWithoutZipCountry(countryId))
      }

      "must return an exception when an error response is returned" in {
        val countryId = "AD"
        checkErrorResponse(url(countryId), connector.getCountriesWithoutZipCountry(countryId))
      }
    }

    "getUnLocode" - {
      val code = "UN1"

      val url = s"/$baseUrl/lists/UnLocodeExtended?keys=UN1"

      "must return a Seq of UN/LOCODES when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(unLocodeResponseJson))
        )

        val expectedResult = UnLocode("UN1", "testName1")

        connector.getUnLocode(code).futureValue.value mustEqual expectedResult

      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, connector.getUnLocode(code))

      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getUnLocode(code))
      }
    }

    "getMeansOfTransportIdentificationTypesActive" - {
      val url: String = s"/$baseUrl/lists/TypeOfIdentificationofMeansOfTransportActive"

      "must return Seq of Identification when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(meansOfTransportIdentificationTypesActiveResponseJson))
        )

        val expectedResult = NonEmptySet.of(
          Identification("10", "IMO Ship Identification Number"),
          Identification("11", "Name of the sea-going vessel")
        )

        connector.getMeansOfTransportIdentificationTypesActive().futureValue.value mustEqual expectedResult

      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, connector.getMeansOfTransportIdentificationTypesActive())
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getMeansOfTransportIdentificationTypesActive())
      }
    }

    "getMeansOfTransportIdentificationTypeActive" - {
      val code                      = "10"
      def url(code: String): String = s"/$baseUrl/lists/TypeOfIdentificationofMeansOfTransportActive?keys=$code"
      "must return Seq of Identification when successful" in {
        server.stubFor(
          get(urlEqualTo(url(code)))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(meansOfTransportIdentificationTypeActiveResponseJson))
        )

        val expectedResult = Identification("10", "IMO Ship Identification Number")

        connector.getMeansOfTransportIdentificationTypeActive(code).futureValue.value mustEqual expectedResult

      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url(code), connector.getMeansOfTransportIdentificationTypeActive(code))

      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url(code), connector.getMeansOfTransportIdentificationTypeActive(code))
      }
    }

    "getMeansOfTransportIdentificationTypes" - {
      val url: String = s"/$baseUrl/lists/TypeOfIdentificationOfMeansOfTransport"

      "must return Seq of Identification when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(meansOfTransportIdentificationTypesResponseJson))
        )

        val expectedResult = NonEmptySet.of(
          TransportMeansIdentification("10", "IMO Ship Identification Number"),
          TransportMeansIdentification("11", "Name of the sea-going vessel")
        )

        connector.getMeansOfTransportIdentificationTypes().futureValue.value mustEqual expectedResult

      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, connector.getMeansOfTransportIdentificationTypes())
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getMeansOfTransportIdentificationTypes())
      }

    }

    "getMeansOfTransportIdentificationType" - {
      val code = "10"

      def url(code: String): String = s"/$baseUrl/lists/TypeOfIdentificationOfMeansOfTransport?keys=$code"

      "must return Seq of Identification when successful" in {
        server.stubFor(
          get(urlEqualTo(url(code)))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(meansOfTransportIdentificationTypeResponseJson))
        )

        val expectedResult = TransportMeansIdentification("10", "IMO Ship Identification Number")

        connector.getMeansOfTransportIdentificationType(code).futureValue.value mustEqual expectedResult

      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url(code), connector.getMeansOfTransportIdentificationType(code))
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url(code), connector.getMeansOfTransportIdentificationType(code))

      }
    }
  }

  private def checkNoReferenceDataFoundResponse(url: String, result: => Future[Either[Exception, ?]]): Assertion = {
    server.stubFor(
      get(urlEqualTo(url))
        .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
        .willReturn(okJson(emptyResponseJson))
    )

    result.futureValue.left.value mustBe a[NoReferenceDataFoundException]
  }

  private def checkErrorResponse(url: String, result: => Future[Either[Exception, ?]]): Assertion = {
    val errorResponses: Gen[Int] = Gen.chooseNum(400: Int, 599: Int)

    forAll(errorResponses) {
      errorResponse =>
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(
              aResponse()
                .withStatus(errorResponse)
            )
        )

        result.futureValue.left.value mustBe an[Exception]
    }
  }
}
