// Copyright (c) 2026 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf.engine.script

import com.digitalasset.canton.logging.LoggerNameFromClass
import com.digitalasset.daml.lf.data.Ref.{Identifier, Location}
import com.digitalasset.daml.lf.engine.StackTrace
import com.digitalasset.daml.lf.engine.script.v2.ledgerinteraction.ScriptLedgerClient
import com.digitalasset.daml.lf.speedy.MachineLogger
import com.digitalasset.daml.lf.value.Value
import spray.json._

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets

/** Debug hooks of the Daml Script runtime.
  *
  * The runner and the ledger clients invoke these on the active
  * [[MachineLogger]] when it implements this trait. The default logger does
  * not, so the hooks cost a single pattern match unless `--debug-trace-file`
  * is set.
  *
  * This is the host-side half of the runtime debug contract used by
  * source-level debuggers (`dpm debug`). The interpreter-side half — a
  * per-source-location callback inside the Speedy evaluation loop (e.g.
  * `MachineLogger.onLocation`) — lives in the interpreter sources (Canton
  * repository) and is tracked as follow-up work; until then, step granularity
  * is: script questions, submissions, ledger events, and `trace`/`debug`
  * output, all of which carry source locations where available.
  */
private[lf] trait DebugTraceListener {
  def onScriptStart(script: Identifier): Unit
  def onScriptEnd(script: Identifier, result: Either[Throwable, Any]): Unit
  def onQuestion(name: String, version: Long, stackTrace: StackTrace): Unit
  def onSubmission(actAs: Seq[String], readAs: Seq[String], location: Option[Location]): Unit
  def onTransactionTree(tree: ScriptLedgerClient.TransactionTree): Unit
}

/** A [[MachineLogger]] that forwards to `underlying` and additionally appends
  * one JSON object per debug event to a JSONL sink.
  *
  * Emitted line shapes (unknown fields/events must be ignored by consumers):
  *
  * {{{
  * {"event":"script-start","script":"Module:name"}
  * {"event":"trace","message":"...","location":{LOC}}
  * {"event":"warning","message":"...","location":{LOC}}
  * {"event":"question","name":"Submit","version":1,"stackTrace":[{LOC},..]}
  * {"event":"submission","actAs":["p"],"readAs":[],"location":{LOC}}
  * {"event":"created","templateId":"pkg:Mod:Tpl","contractId":"..","argument":..}
  * {"event":"exercised","templateId":"pkg:Mod:Tpl","choice":"C","contractId":"..",
  *  "argument":..,"result":..}
  * {"event":"script-end","status":"success"}
  * {"event":"script-end","status":"error","error":"..","location":{LOC}}
  * }}}
  *
  * LOC = {"packageId","module","definition","startLine","startCol","endLine",
  * "endCol"} with 1-based positions, matching `daml-debug-info/v1` spans.
  * Runtime locations are 0-based (see `DA.Stack.SrcLoc`), hence the +1 below.
  */
private[lf] final class DebugTraceMachineLogger(
    underlying: ScriptMachineLogger,
    sink: PrintWriter,
) extends MachineLogger
    with DebugTraceListener {

  def trace(message: String, location: Option[Location])(implicit ln: LoggerNameFromClass): Unit = {
    underlying.trace(message, location)
    emit("trace", "message" -> JsString(message), "location" -> optLocationJson(location))
  }

  def warn(message: String, location: Option[Location])(implicit ln: LoggerNameFromClass): Unit = {
    underlying.warn(message, location)
    emit("warning", "message" -> JsString(message), "location" -> optLocationJson(location))
  }

  def traceIterator: Iterator[(String, Option[Location])] = underlying.traceIterator
  def warningIterator: Iterator[(String, Option[Location])] = underlying.warningIterator

  override def onScriptStart(script: Identifier): Unit =
    emit("script-start", "script" -> JsString(script.qualifiedName.toString))

  override def onScriptEnd(script: Identifier, result: Either[Throwable, Any]): Unit =
    result match {
      case Right(_) =>
        emit(
          "script-end",
          "script" -> JsString(script.qualifiedName.toString),
          "status" -> JsString("success"),
        )
      case Left(err) =>
        val location = err match {
          case Script.FailedCmd(_, stackTrace, _) => stackTrace.topFrame
          case _ => None
        }
        val message = err match {
          case Script.FailedCmd(description, _, cause) =>
            s"Command $description failed: ${Option(cause.getMessage).getOrElse(cause.toString)}"
          case _ => Option(err.getMessage).getOrElse(err.toString)
        }
        emit(
          "script-end",
          "script" -> JsString(script.qualifiedName.toString),
          "status" -> JsString("error"),
          "error" -> JsString(message),
          "location" -> optLocationJson(location),
        )
    }

  override def onQuestion(name: String, version: Long, stackTrace: StackTrace): Unit =
    emit(
      "question",
      "name" -> JsString(name),
      "version" -> JsNumber(version),
      "stackTrace" -> JsArray(stackTrace.frames.map(locationJson): _*),
    )

  override def onSubmission(
      actAs: Seq[String],
      readAs: Seq[String],
      location: Option[Location],
  ): Unit =
    emit(
      "submission",
      "actAs" -> JsArray(actAs.map(JsString(_)): _*),
      "readAs" -> JsArray(readAs.map(JsString(_)): _*),
      "location" -> optLocationJson(location),
    )

  override def onTransactionTree(tree: ScriptLedgerClient.TransactionTree): Unit =
    tree.rootEvents.foreach(emitTreeEvent)

  private def emitTreeEvent(event: ScriptLedgerClient.TreeEvent): Unit =
    event match {
      case created: ScriptLedgerClient.Created =>
        emit(
          "created",
          "templateId" -> JsString(created.templateId.toString),
          "contractId" -> JsString(created.contractId.coid),
          "argument" -> valueJson(created.argument),
        )
      case exercised: ScriptLedgerClient.Exercised =>
        emit(
          "exercised",
          "templateId" -> JsString(exercised.templateId.toString),
          "interfaceId" -> exercised.interfaceId.fold[JsValue](JsNull)(id =>
            JsString(id.toString)
          ),
          "choice" -> JsString(exercised.choice),
          "contractId" -> JsString(exercised.contractId.coid),
          "argument" -> valueJson(exercised.argument),
          "result" -> valueJson(exercised.result),
        )
        exercised.childEvents.foreach(emitTreeEvent)
    }

  private def valueJson(value: Value): JsValue =
    // The enriched values in tree events are ordinary LF values; fall back to
    // their rendering if a value resists the API JSON encoding.
    try LfValueCodec.apiValueToJsValue(value)
    catch { case scala.util.control.NonFatal(_) => JsString(value.toString) }

  private def optLocationJson(location: Option[Location]): JsValue =
    location.fold[JsValue](JsNull)(locationJson)

  private def locationJson(loc: Location): JsValue =
    JsObject(
      "packageId" -> JsString(loc.packageId),
      "module" -> JsString(loc.module.dottedName),
      "definition" -> JsString(loc.definition),
      "startLine" -> JsNumber(loc.start._1 + 1),
      "startCol" -> JsNumber(loc.start._2 + 1),
      "endLine" -> JsNumber(loc.end._1 + 1),
      "endCol" -> JsNumber(loc.end._2 + 1),
    )

  private def emit(event: String, fields: (String, JsValue)*): Unit = {
    val line = JsObject(Map("event" -> (JsString(event): JsValue)) ++ fields).compactPrint
    sink.synchronized {
      sink.println(line)
      sink.flush()
    }
  }

  def close(): Unit = sink.synchronized(sink.close())
}

private[lf] object DebugTraceMachineLogger {
  def apply(file: File): DebugTraceMachineLogger = {
    val parent = file.getParentFile
    if (parent != null) {
      val _ = parent.mkdirs()
    }
    new DebugTraceMachineLogger(
      ScriptMachineLogger(),
      new PrintWriter(file, StandardCharsets.UTF_8.name()),
    )
  }
}
