\name{aggression}
\docType{data}
\alias{aggression}
\title{Verbal aggression data}
\description{Item response data regarding verbal agression from 316 persons and 24 items. Participants were instructed to imagine four frustrating scenarios in which either another or oneself is to blame. For each scenario, they responded "yes", "perhaps", or "no" regarding whether they would react by cursing, scolding, and shouting. They also responded whether they would want to engage in those three behaviors, resulting in a total six items per scenario. An example item is, "A bus fails to stop for me. I would want to curse."
}
\usage{aggression}
\format{A long-form data.frame (one row per item response) with the following columns:
	\describe{
  \item{person}{Integer person identifier.}
  \item{item}{Integer item identifier.}
  \item{poly}{Original, polytomous response. 0 indicates "no", 1 "perhaps", and 3 "yes".}
  \item{dich}{Dichotomized response. 0 indicates "no" and 1 indicates "perhaps" or "yes".}
  \item{description}{Brief description of the item.}
  \item{anger}{Trait anger score for a person.}
  \item{male}{Indicator for whether person is male.}
  \item{do}{Indicator for whether item concerns actually doing the behavior instead of wanting to do it.}
  \item{other}{Indicator for whether item concerns another person being to blame instead of self to blame.}
  \item{scold}{Indicator for whether item concerns scolding behavior instead of cursing or shouting.}
  \item{shout}{Indicator for whether item concerns shouting behavior instead of cursing or scolding.}
  }
}
\source{Vansteelandt, K. (2000). Formal models for contextualized personality psychology. Unpublished doctoral dissertation. K. U. Leuven, Belgium.}
\references{De Boeck, P. and Wilson, M. (2004) \emph{Explanatory Item Response Models}. New York: Springer.}
\keyword{datasets}
