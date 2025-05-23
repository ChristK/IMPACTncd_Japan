% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Exposure_class.R
\name{Exposure}
\alias{Exposure}
\title{R6 Class representing an exposure}
\description{
An exposure object has the RR between an exposure and a disease.
}
\details{
To be completed...
}
\examples{

## ------------------------------------------------
## Method `Exposure$new`
## ------------------------------------------------

af_stroke$read_xps_prm("./inputs/RR/af_stroke.csvy", design)

## ------------------------------------------------
## Method `Exposure$gen_stochastic_effect`
## ------------------------------------------------

af_stroke <- Exposure$new("af", "stroke")
af_stroke$read_xps_prm("./inputs/RR/af_stroke.csvy", design)
af_stroke$gen_stochastic_effect(design, TRUE)
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{The name of the exposure.}

\item{\code{outcome}}{The name of the outcome the exposure influences.}

\item{\code{lag}}{Disease lag.}

\item{\code{distribution}}{The distribution to be used for Monte Carlo.
Currently lognormal and normal are supported.}

\item{\code{source}}{Citation info for the effect size.}

\item{\code{notes}}{Any notes regarding the exposure -> outcome relation.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Exposure-new}{\code{Exposure$new()}}
\item \href{#method-Exposure-gen_stochastic_effect}{\code{Exposure$gen_stochastic_effect()}}
\item \href{#method-Exposure-del_stochastic_effect}{\code{Exposure$del_stochastic_effect()}}
\item \href{#method-Exposure-get_rr}{\code{Exposure$get_rr()}}
\item \href{#method-Exposure-clear_cache}{\code{Exposure$clear_cache()}}
\item \href{#method-Exposure-xps_to_rr}{\code{Exposure$xps_to_rr()}}
\item \href{#method-Exposure-validate_rr}{\code{Exposure$validate_rr()}}
\item \href{#method-Exposure-get_input_rr}{\code{Exposure$get_input_rr()}}
\item \href{#method-Exposure-get_metadata}{\code{Exposure$get_metadata()}}
\item \href{#method-Exposure-get_seed}{\code{Exposure$get_seed()}}
\item \href{#method-Exposure-write_xps_tmplte_file}{\code{Exposure$write_xps_tmplte_file()}}
\item \href{#method-Exposure-convert_from_old_format}{\code{Exposure$convert_from_old_format()}}
\item \href{#method-Exposure-get_lag}{\code{Exposure$get_lag()}}
\item \href{#method-Exposure-get_ideal_xps_lvl}{\code{Exposure$get_ideal_xps_lvl()}}
\item \href{#method-Exposure-get_name}{\code{Exposure$get_name()}}
\item \href{#method-Exposure-print}{\code{Exposure$print()}}
\item \href{#method-Exposure-clone}{\code{Exposure$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-new"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-new}{}}}
\subsection{Method \code{new()}}{
Reads exposure parameter from file and creates a new exposure object..
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$new(sRelativeRiskByPopulationSubsetForExposureFilePath, design)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sRelativeRiskByPopulationSubsetForExposureFilePath}}{string, path to .csvy file detailing relative risk (RR) by population subset (age, sex, maybe decile 'Index of Multiple Deprivation' DIMD) for a specific exposure. File header may contain other exposure parameters.}

\item{\code{design}}{A design object with the simulation parameters.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An `Exposure` object.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{af_stroke$read_xps_prm("./inputs/RR/af_stroke.csvy", design)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-gen_stochastic_effect"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-gen_stochastic_effect}{}}}
\subsection{Method \code{gen_stochastic_effect()}}{
Generates and write to disk the stochastic effect.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$gen_stochastic_effect(
  design_ = design,
  overwrite = FALSE,
  smooth,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{design_}}{A design object with the simulation parameters.}

\item{\code{overwrite}}{If TRUE overwrite the files. Else if files exist they
are not regenerated.}

\item{\code{smooth}}{If true applies loess smoothing to the input relative risks}

\item{\code{...}}{Further arguments to be passed to `loess`, usually span =
0.7, degree = 1}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An `Exposure` object.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{af_stroke <- Exposure$new("af", "stroke")
af_stroke$read_xps_prm("./inputs/RR/af_stroke.csvy", design)
af_stroke$gen_stochastic_effect(design, TRUE)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-del_stochastic_effect"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-del_stochastic_effect}{}}}
\subsection{Method \code{del_stochastic_effect()}}{
Deletes the stochastic effect file and index from disk.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$del_stochastic_effect(invert = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{invert}}{If TRUE keeps the  file with the current checksum and deletes
all other files for this exposure~outcome.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The invisible self for chaining.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-get_rr"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-get_rr}{}}}
\subsection{Method \code{get_rr()}}{
Get relative risks from disk.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$get_rr(mc, design_ = design, drop = FALSE, plot_rr = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{mc}}{An integer that signifies the Monte Carlo iteration.}

\item{\code{design_}}{A design object with the simulation parameters.}

\item{\code{drop}}{If `TRUE` returns a scalar numeric of the RR if the RR is
common for all age groups and both sexes. Otherwise, a data.table.}

\item{\code{plot_rr}}{If TRUE, plots the relative risk}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A data.table with the stochastic relative risks, if stochastic
  = TRUE or mc > 0; else, the deterministic relative risks.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-clear_cache"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-clear_cache}{}}}
\subsection{Method \code{clear_cache()}}{
Clear the cache for get_rr.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$clear_cache()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The `Exposure` object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-xps_to_rr"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-xps_to_rr}{}}}
\subsection{Method \code{xps_to_rr()}}{
Apply the RR in a new column in sp$pop based on the
  exposure level. I the case of smok_quit_yrs it modifies the risk of
  smok_cig or packyrs and no new column is created.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$xps_to_rr(
  sp_,
  design_,
  checkNAs = design_$sim_prm$logs,
  forPARF = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sp_}}{A synthetic population.}

\item{\code{design_}}{A design object with the simulation parameters.}

\item{\code{checkNAs}}{If `TRUE`, prints the table of NAs before they get
overwritten with 1. Note that for some exposures, NAs are expected
for certain levels of exposure (i.e. for active days).}

\item{\code{forPARF}}{Set TRUE when applied on the specialised forPARF
SynthPop}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The invisible self for chaining.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-validate_rr"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-validate_rr}{}}}
\subsection{Method \code{validate_rr()}}{
Get relative risks from disk.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$validate_rr()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A plot with the input and stochastic relative risks.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-get_input_rr"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-get_input_rr}{}}}
\subsection{Method \code{get_input_rr()}}{
Get original input relative risks by age and sex.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$get_input_rr()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A copied data.table with the original relative risks.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-get_metadata"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-get_metadata}{}}}
\subsection{Method \code{get_metadata()}}{
Get original metadata.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$get_metadata()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list with the original metadata.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-get_seed"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-get_seed}{}}}
\subsection{Method \code{get_seed()}}{
Get seed for RNG.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$get_seed()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A seed for the RNG that is produced by the digest of exposure
  name and outcome.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-write_xps_tmplte_file"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-write_xps_tmplte_file}{}}}
\subsection{Method \code{write_xps_tmplte_file()}}{
Writes a template exposure file to disk.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$write_xps_tmplte_file(file_path = "./inputs/RR/template.csvy")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{file_path}}{Path including file name and .csvy extension to write the
file with placeholder exposure parameters.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The `Exposure` object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-convert_from_old_format"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-convert_from_old_format}{}}}
\subsection{Method \code{convert_from_old_format()}}{
Convert the old format .csv to the new format .csvy.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$convert_from_old_format(
  old_file,
  metadata,
  file_path,
  estimates = NULL,
  second_estimate_is_p = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{old_file}}{Path to the old format .csv file with the RR.}

\item{\code{metadata}}{List with the metadata information.}

\item{\code{file_path}}{Path including file name and .csvy extension to write the
new file.}

\item{\code{estimates}}{Only used when old_file is missing. A vector of length
2 with the 1st element being the point estimate and the 2nd one of
the CI.}

\item{\code{second_estimate_is_p}}{If `TRUE` and the `estimates` not `NULL` then the
second element of `estimates` is interpreted like a p-value for ratio
rather than a CI.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The `Exposure` object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-get_lag"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-get_lag}{}}}
\subsection{Method \code{get_lag()}}{
Get exposure lag.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$get_lag(mc_)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{mc_}}{A vector of Monte Carlo iterations. If missing or 0 return median (= mean).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An integer vector with exposure~disease lag.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-get_ideal_xps_lvl"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-get_ideal_xps_lvl}{}}}
\subsection{Method \code{get_ideal_xps_lvl()}}{
Get ideal exposure level.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$get_ideal_xps_lvl(mc_)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{mc_}}{A vector of Monte Carlo iterations. If missing or 0 return
the user input lag.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An integer vector with exposure~disease lag.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-get_name"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-get_name}{}}}
\subsection{Method \code{get_name()}}{
Get name of the object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$get_name()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
An string.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-print"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-print}{}}}
\subsection{Method \code{print()}}{
Print the simulation parameters.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$print()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The `Exposure` object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Exposure-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Exposure-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Exposure$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
