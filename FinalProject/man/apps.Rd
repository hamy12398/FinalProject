\name{apps_data}
\alias{apps_data}
\docType{data}
\title{
Apps on Google Play Store
}
\description{
The data set is collected from Google Play Store data to demonstrate the performance of new apps on the Google Play Store.
}
\usage{data("apps_data")}
\format{
  A data frame with 7729 observations on the following 13 variables.
  \describe{
    \item{Rating, Reviews, Size, Installs, Price}{numeric vriables}
    \item{App, Category, Type, Content Rating, Genres, Current Ver, Android Ver}{character variables}
    \item{Last Updated}{yy/mm/dd format}
  }
}
\details{
Google Play Store had a feature which certain apps that are promising, are
boosted to virally promote. The promotion will be conveyed as multiple ways
including recommendations sections (“Similar apps”, “You might also like”, “New
and updated games”). These will also get a boost in search results visibility. This
feature will help gain more attention to potential newer apps. The data set is collected from Google Play Store data to analyze the potential new apps.
}
\source{
https://www.kaggle.com/lava18/google-play-store-apps
}
\examples{
data(apps_data)
## maybe str(apps_data) ; plot(apps_data) ...
}
\keyword{datasets}
