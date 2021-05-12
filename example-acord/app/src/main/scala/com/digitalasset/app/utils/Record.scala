// Copyright (c) 2019-2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.app.utils

import java.time.LocalDate
import java.util.Collections
import java.util.stream.Collectors

import com.daml.ledger.javaapi.data.{Record => RecordData}
import com.daml.ledger.javaapi.data.{Variant, Date, Unit}

import scala.collection.JavaConverters._

object Record {

  implicit class RecordAccess(r: RecordData) {
    def get[T](label: String): T = {
      r
        .getFields.listIterator.asScala.toList.find(p => p.getLabel.get == label).get
        .getValue.asInstanceOf[T]
    }

    def getList[T](label: String): List[T] =
      r
        .getFields.listIterator.asScala.toList.find(p => p.getLabel.get == label).get
        .getValue.asList.get
        .getValues.stream.map[T](x => x.asInstanceOf[T]).collect(Collectors.toList())
        .asScala.toList

    def getOptional[T](label: String): Option[T] = {
      val variant = r
        .getFields.listIterator.asScala.toList.find(p => p.getLabel.get == label).get
        .getValue.asVariant.get

      variant.getConstructor match {
        case "None" => None
        case "Some" => Some(variant.getValue.asInstanceOf[T])
      }
    }
  }

  def toOptionalVariant(o: Option[LocalDate]): Variant = {
    o match {
      case Some(v) => new Variant("Some", new Date(v.toEpochDay.toInt))
      case None    => new Variant("None", Unit.getInstance())
    }
  }

  def unit = new RecordData(Collections.emptyList[RecordData.Field])
}
