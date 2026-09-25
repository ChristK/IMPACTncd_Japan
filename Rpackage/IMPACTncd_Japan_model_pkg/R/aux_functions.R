## IMPACTncdJapan is an implementation of the IMPACTncd framework, developed by Chris
## Kypridemos with contributions from Peter Crowther (Melandra Ltd), Maria
## Guzman-Castillo, Amandine Robert, and Piotr Bandosz. This work has been
## funded by NIHR  HTA Project: 16/165/01 - IMPACTncdJapan: Health Outcomes
## Research Simulation Environment.  The views expressed are those of the
## authors and not necessarily those of the NHS, the NIHR or the Department of
## Health.
##
## Copyright (C) 2018-2020 University of Liverpool, Chris Kypridemos
##
## IMPACTncdJapan is free software; you can redistribute it and/or modify it under
## the terms of the GNU General Public License as published by the Free Software
## Foundation; either version 3 of the License, or (at your option) any later
## version. This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
## details. You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/> or write
## to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
## Boston, MA 02110-1301 USA.


#' @export
proportional_reduction <- function(exposure, change, weights, penalty = 1){
	c <- (weighted.mean(exposure, weights) - (weighted.mean(exposure, weights) + change)) / weighted.mean(exposure^penalty, weights)
	return(exposure - c * exposure^penalty)
		# exposure: eg. SBP_curr_xps, 
		# chage: -5 mmHg, 
		# penalty: determines how sharply the penalty increases with x. penalty
		# When penalty = 1, the SBP is simply "reduced at the same rate in proportion to the original SBP,
		# When penalty > 1, the distribution is reduced “more” for larger SBP values and “less” for smaller values.
}




# The user provides:
#   •	threshold: A numeric value for exposure.
# •	target_prop: The target proportion (0-1) of values exceeding the threshold after applying reduction.
# •	weights: The weights associated with the exposures.
# •	penalty: A penalty parameter, defaulted to 1 (or any desired numeric value).
# 2.	The function calculates the necessary change by using an optimization method (e.g., uniroot) to find the reduction that achieves the desired proportion.
# 3.	After determining the optimal change, apply it to exposures using the proportional logic you previously defined.

#' @export
proportional_reduction_threshold <- function(exposure, threshold, target_prop, weights, penalty = 1) {

  # Function to compute proportion above threshold given a specific change
  calc_prop_above <- function(change){
    adj_exposure <- proportional_reduction(exposure, change, weights, penalty)
    sum(weights[adj_exposure > threshold]) / sum(weights)
  }

  # Find the change required to achieve the target proportion above threshold
  optimal_change <- uniroot(
    f = function(change) calc_prop_above(change) - target_prop,
    interval = c(-max(exposure)*10, max(exposure)*10),
    extendInt = "yes", tol = .Machine$double.eps, maxiter = 1000, trace = 0
  )$root

  # Apply the optimal change
  proportional_reduction(exposure, optimal_change, weights, penalty)
}







#' @export
inflate <- function(x, percentage_rate, year, baseline_year) {
  x * (1 + percentage_rate / 100) ^ (year - baseline_year)
}
# inflate(1000, 3, 2011:2020, 2013)

#' @export
deflate <- function(x, percentage_rate, year, baseline_year) {
  x * (1 - percentage_rate / 100) ^ (year - baseline_year)
}



# helper func for gamlss::fitDistr
#' @export
distr_best_fit <-
  function(dt,
           var,
           wt,
           distr_family,
           distr_extra = NULL,
           pred = FALSE,
           seed = NULL,
           trace = TRUE) {
    if (pred) {
      print("Selection based on minimum prediction global deviance")
      if (!is.null(seed))
        set.seed(seed)
      lns <- sample(nrow(dt), round(nrow(dt) * 0.8))
      dt_trn   <- dt[lns,] # train dataset
      dt_crv   <- dt[!lns,]  # cross-validation dataset
      marg_distr <- gamlss::fitDistPred(
        dt_trn[[var]],
        type = distr_family,
        weights = dt_trn[[wt]],
        extra = distr_extra,
        try.gamlss = TRUE,
        trace = trace,
        newdata = dt_crv[[var]]
      )
    } else {
      print("Selection based on BIC")
      marg_distr <-
        gamlss::fitDist(
          dt[[var]],
          log(nrow(dt)),
          type = distr_family,
          weights = dt[[wt]],
          extra = distr_extra,
          try.gamlss = TRUE,
          trace = trace
        )
    }
    marg_distr
  }


# Atomic cache writes -----------------------------------------------------
# During a multicore run several parallel workers can compute and write the
# SAME cache path concurrently (see Disease$gen_parf_files / gen_parf, where
# the parf .fst path and the synthpop .qs path are deterministic functions of
# the disease and its inputs, hence identical across workers/iterations).
# Writing straight to the final path lets another worker read a half-written
# file and fail with "It seems the file header was damaged or incomplete".
# These helpers write to a per-process temporary in the SAME directory and
# then rename it into place. POSIX rename atomically replaces the destination
# (the content is deterministic, so all workers write identical bytes); on
# Windows rename fails when the destination already exists, so the first writer
# wins and later writers simply discard their temp. Either way the final path
# only ever appears via an atomic rename of an already-complete file, so a
# reader sees either no file or a complete one -- never a partial one.

# Same-directory temp path, unique per process so forked/PSOCK workers never
# collide. Kept next to `path` to guarantee rename stays on one filesystem.
atomic_tmp_path <- function(path) {
  paste0(path, ".tmp", Sys.getpid())
}

atomic_write_fst <- function(x, path, compress = 100L) {
  tmp <- atomic_tmp_path(path)
  on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
  write_fst(x, tmp, compress)
  suppressWarnings(file.rename(tmp, path))
  invisible(path)
}

atomic_qs_save <- function(x, path, nthreads = 1L) {
  tmp <- atomic_tmp_path(path)
  on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
  qs_save(x, tmp, nthreads = nthreads)
  suppressWarnings(file.rename(tmp, path))
  invisible(path)
}


# ---------------------------------------------------------------------------
# Encoding-robust sourcing of scenario scripts
# ---------------------------------------------------------------------------

# Count how many '(' and '{' remain open at the end of each line, ignoring
# anything inside strings ('...', "...", `...`) and comments (# to end of
# line). This is a diagnostic heuristic (raw strings like r"(...)" are not
# handled), only used to explain parse errors, never to alter evaluation.
scan_bracket_balance <- function(lines) {
  open_paren <- integer(length(lines))
  open_brace <- integer(length(lines))
  paren <- 0L
  brace <- 0L
  in_str <- ""  # one of "", "'", '"', "`"
  for (i in seq_along(lines)) {
    chars <- strsplit(lines[[i]], "", fixed = TRUE)[[1L]]
    j <- 1L
    n <- length(chars)
    while (j <= n) {
      ch <- chars[[j]]
      if (nzchar(in_str)) {
        if (ch == "\\" && in_str != "`") {
          j <- j + 1L # skip escaped character inside a quoted string
        } else if (ch == in_str) {
          in_str <- ""
        }
      } else if (ch == "#") {
        break # comment: rest of the line is inert, whatever language it is in
      } else if (ch == "'" || ch == '"' || ch == "`") {
        in_str <- ch
      } else if (ch == "(") {
        paren <- paren + 1L
      } else if (ch == ")") {
        paren <- paren - 1L
      } else if (ch == "{") {
        brace <- brace + 1L
      } else if (ch == "}") {
        brace <- brace - 1L
      }
      j <- j + 1L
    }
    # unterminated strings legitimately span lines; carry state over
    open_paren[[i]] <- paren
    open_brace[[i]] <- brace
  }
  list(paren = open_paren, brace = open_brace)
}

# Assert that a text file is valid UTF-8 before a UTF-8-only reader touches it.
#
# fread(yaml = TRUE) and yaml::read_yaml decode their headers under the running
# machine's locale and fail with a cryptic libyaml/iconv error (or, worse,
# silently truncate) when handed a Shift-JIS/CP932 file saved by a Japanese
# Windows editor. This turns that into an actionable message naming the file
# and the offending lines.
#
# Unlike source_scenario_script(), this NEVER transcodes: these are DATA files
# (relative-risk tables, the sim-design YAML), and silently guessing the
# encoding of data risks turning a mis-decoded byte into a wrong number with no
# error. For data we validate and refuse; the user re-saves as UTF-8.
assert_file_utf8 <- function(path, what = "input file") {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    return(invisible(TRUE))
  }
  sz <- file.size(path)
  if (is.na(sz) || sz == 0) {
    return(invisible(TRUE))
  }
  raw_bytes <- readBin(path, what = "raw", n = sz)
  # A UTF-8 BOM is valid UTF-8; strip it before validating so its bytes are not
  # mistaken for the file being non-UTF-8.
  if (length(raw_bytes) >= 3L &&
      identical(raw_bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) {
    raw_bytes <- raw_bytes[-(1:3)]
  }
  has_nul <- any(raw_bytes == as.raw(0L))
  bad <- integer(0)
  if (!has_nul) {
    txt <- rawToChar(raw_bytes)
    if (validUTF8(txt)) {
      return(invisible(TRUE))
    }
    # Locate the offending lines at the BYTE level: strsplit() on a string that
    # holds invalid bytes is locale-dependent (it may warn and refuse to split),
    # so split on the newline byte (0x0A) directly and validate each chunk.
    nl <- which(raw_bytes == as.raw(0x0A))
    starts <- c(1L, nl + 1L)
    ends <- c(nl - 1L, length(raw_bytes))
    line_ok <- vapply(seq_along(starts), function(k) {
      if (starts[k] > ends[k]) {
        return(TRUE) # empty line
      }
      validUTF8(rawToChar(raw_bytes[starts[k]:ends[k]]))
    }, logical(1L))
    bad <- which(!line_ok)
  }
  stop(
    what, " '", basename(path), "' is not valid UTF-8",
    if (has_nul) {
      " (it contains NUL bytes; it looks like UTF-16/UTF-32 or a binary file)"
    } else if (length(bad)) {
      paste0(
        ". Lines with undecodable bytes: ",
        paste(utils::head(bad, 10L), collapse = ", "),
        " (a Shift-JIS/CP932 save from a Japanese Windows editor is the usual ",
        "cause)"
      )
    } else {
      ""
    },
    ". Re-save the file as UTF-8 and retry.",
    call. = FALSE
  )
}

#' Source an R scenario script robustly across encodings
#'
#' A drop-in replacement for \code{\link[base]{source}} intended for scenario
#' scripts written by international collaborators. Comments (and strings) in any
#' language are safe: the file is read as raw bytes, normalised to UTF-8
#' (accepting UTF-8 with or without a byte-order mark, and falling back to
#' CP932/Shift-JIS -- the Japanese Windows default -- then Latin-1), and parsed
#' with an explicit UTF-8 declaration, so the result does not depend on the
#' locale of the machine that runs it or the machine that saved it.
#'
#' @details
#' On a syntax error the function additionally reports how many brackets are
#' still open at the offending line. This pinpoints the most common
#' scenario-script mistake: a closing brace placed too early inside the function
#' passed to \code{Simulation$update_primary_prevention_scn()}, which ends the
#' scenario function prematurely so that the statements after it are parsed as
#' stray call arguments. R then fails with a misleading \dQuote{unexpected
#' symbol} pointing at whatever (often non-ASCII) comment precedes the next
#' statement, which makes the encoding look like the culprit when it is not.
#'
#' A non-UTF-8 file is transcoded from CP932 (Shift-JIS) or Latin-1 with a
#' message. A file that decodes as none of these, or that contains NUL bytes
#' (for example UTF-16 without a byte-order mark), stops with an explanatory
#' error instead of being silently evaluated as garbage.
#'
#' @param path Path to the R script.
#' @param envir Environment in which to evaluate the script. Defaults to the
#'   caller's environment, like \code{source()}.
#' @param chdir If \code{TRUE}, temporarily change the working directory to the
#'   script's directory while evaluating, like \code{source(chdir = TRUE)}.
#' @return Invisibly, the value of the last evaluated expression. Top-level
#'   visible values are not auto-printed (the same default as \code{source()}).
#' @seealso \code{\link[base]{source}}
#' @examples
#' \dontrun{
#' # Run a scenario script that may contain Japanese comments and may have been
#' # saved as UTF-8 or CP932, without depending on the session locale:
#' source_scenario_script("./inputs/scenarios/LDL_statin.R")
#' }
#' @export
source_scenario_script <- function(path, envir = parent.frame(), chdir = FALSE) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    stop("source_scenario_script(): file not found: ", path)
  }

  raw_bytes <- readBin(path, what = "raw", n = file.size(path))

  # Byte-order marks: strip UTF-8 BOM; transcode UTF-16 outright.
  txt <- NULL
  if (length(raw_bytes) >= 3L &&
      identical(raw_bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) {
    raw_bytes <- raw_bytes[-(1:3)]
  } else if (length(raw_bytes) >= 2L &&
             (identical(raw_bytes[1:2], as.raw(c(0xFE, 0xFF))) ||
              identical(raw_bytes[1:2], as.raw(c(0xFF, 0xFE))))) {
    txt <- iconv(list(raw_bytes), from = "UTF-16", to = "UTF-8")
    # iconv() returns NA on undecodable input; without this guard txt stays
    # non-NULL-but-NA, the branch below is skipped, and parse() silently
    # yields expression(NA) so the whole script "runs" as NA.
    if (length(txt) != 1L || is.na(txt)) {
      stop(
        "source_scenario_script(): '", basename(path),
        "' has a UTF-16 byte-order mark but could not be decoded as UTF-16. ",
        "Re-save the file as UTF-8.",
        call. = FALSE
      )
    }
  }

  if (is.null(txt)) {
    # rawToChar() errors on embedded NULs (e.g. UTF-16 text with no BOM, or a
    # binary file handed in by mistake). Catch that up front with an
    # actionable message rather than an opaque "embedded nul in string".
    if (any(raw_bytes == as.raw(0L))) {
      stop(
        "source_scenario_script(): '", basename(path),
        "' contains NUL bytes and is not a UTF-8 text script (it looks like ",
        "UTF-16/UTF-32 without a byte-order mark, or a binary file). ",
        "Re-save it as UTF-8.",
        call. = FALSE
      )
    }
    txt <- rawToChar(raw_bytes)
    if (validUTF8(txt)) {
      Encoding(txt) <- "UTF-8"
    } else {
      # Not valid UTF-8: try the Japanese Windows default first because it
      # can *fail* (and therefore actually discriminates); Latin-1 accepts
      # any byte sequence so it must come last.
      txt <- iconv(list(raw_bytes), from = "CP932", to = "UTF-8")
      if (!is.na(txt)) {
        message(
          "source_scenario_script(): '", basename(path),
          "' is not UTF-8; interpreted as CP932 (Shift-JIS) and converted."
        )
      } else {
        txt <- iconv(list(raw_bytes), from = "latin1", to = "UTF-8")
        message(
          "source_scenario_script(): '", basename(path),
          "' is not UTF-8; interpreted as Latin-1 and converted."
        )
      }
      if (is.na(txt)) {
        bad <- which(!validUTF8(strsplit(rawToChar(raw_bytes), "\n",
                                         fixed = TRUE)[[1L]]))
        stop(
          "source_scenario_script(): cannot determine the encoding of '",
          path, "'. Lines with undecodable bytes: ",
          paste(utils::head(bad, 10L), collapse = ", "),
          ". Re-save the file as UTF-8."
        )
      }
    }
  }

  lines <- strsplit(txt, "\r\n|\r|\n")[[1L]]

  exprs <- tryCatch(
    parse(text = lines, keep.source = TRUE,
          srcfile = srcfilecopy(path, lines, isFile = TRUE),
          encoding = "UTF-8"),
    error = function(e) {
      msg <- conditionMessage(e)
      extra <- character(0)
      # Classify the error from STRUCTURE (bracket balance, byte validity),
      # not from the wording of `msg`: R localises parser messages, so a
      # Japanese-locale session reports e.g. "予期しない
      # シンボル" instead of "unexpected symbol", and any
      # grepl() on the English text would silently miss on exactly the
      # machines this helper exists to serve.
      m <- regmatches(msg, regexec("(?::|^)(\\d+):(\\d+)", msg))[[1L]]
      if (length(m) == 3L) {
        err_line <- as.integer(m[[2L]])
        bal <- scan_bracket_balance(lines)
        before <- max(err_line - 1L, 1L)
        n_paren <- bal$paren[[before]]
        n_brace <- bal$brace[[before]]
        extra <- c(extra, sprintf(
          "At line %d there are %d unclosed '(' and %d unclosed '{'.",
          err_line, n_paren, n_brace
        ))
        if (n_paren > 0L && n_brace == 0L) {
          # inside an unclosed call but no open function body: the classic
          # prematurely-closed scenario function. The suspect line is where
          # the brace count last fell back to zero while a call was open.
          prev_brace <- c(0L, bal$brace[-length(bal$brace)])
          closed_at <- which(bal$brace == 0L & prev_brace > 0L & bal$paren > 0L)
          closed_at <- closed_at[closed_at < err_line]
          suspect <- if (length(closed_at)) max(closed_at) else NA_integer_
          extra <- c(extra, paste0(
            "This pattern usually means a '}' closed the scenario function ",
            "passed to update_*_prevention_scn() too early",
            if (!is.na(suspect)) {
              sprintf(" (the '}' at line %d)", suspect)
            } else {
              ""
            },
            ", so the following statements are parsed as stray call ",
            "arguments. Move that '}' to just before the closing ')' of the ",
            "update_*_prevention_scn() call."
          ))
        }
        bad <- which(!validUTF8(lines))
        if (length(bad)) {
          extra <- c(extra, paste0(
            "The file mixes text encodings. Lines with undecodable bytes: ",
            paste(utils::head(bad, 10L), collapse = ", "),
            ". Re-save the whole file as UTF-8."
          ))
        } else {
          extra <- c(extra, paste0(
            "Note: the file decoded cleanly, so non-ASCII characters in ",
            "comments are NOT the cause; comments in any language are fine."
          ))
        }
      }
      stop(
        "source_scenario_script(): syntax error in '", path, "':\n",
        msg,
        if (length(extra)) paste0("\n", paste(extra, collapse = "\n")),
        call. = FALSE
      )
    }
  )

  if (isTRUE(chdir)) {
    owd <- setwd(dirname(normalizePath(path)))
    on.exit(setwd(owd), add = TRUE)
  }

  last <- NULL
  for (i in seq_along(exprs)) {
    last <- eval(exprs[[i]], envir)
  }
  invisible(last)
}
