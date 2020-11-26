\name{report}
\alias{report}
\title{A function to report on your analyses in written form.}
\usage{
report()
}
\description{
Use this function to report an a simple linear regression you performed. Currently only simple linear regression with 1 IV and 1 DV works reliably.
Format: report(MYMODEL)
}
\examples{
report(lm(cyl~displ, mpg))
}
