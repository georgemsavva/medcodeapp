\name{run_medcodeapp}
\alias{run_medcodeapp}
\title{Run the medcode app}
\usage{
run_medcodeapp()
}
\description{
Launches a shiny app to check and populate CPRD medcode lists.

The app will scan the uploaded excel workbook, allow you to select which sheets are to be used, and will then check these against the supplied medical code dictionary.

The `SnomedCTConceptId` codes present in each sheet will be compared to the dictionary, and any matching medcodes not already in the sheet (that is the medcode *and* the term are not currently included) will be added.  The resulting workbook will then be available for download, with the new entries highlighted in yellow.

There has been very little checking and testing with this version, and no error catching is implemented, so make sure the sheets you select are valid code lists.  At a minimum they need the columns SnomedCTConceptId, MedCodeId and Term (all case sensitive).

}
\examples{
run_medcodeapp()
}
