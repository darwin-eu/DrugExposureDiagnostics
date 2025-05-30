% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shinyApp.R
\name{ShinyApp}
\alias{ShinyApp}
\title{DrugExposureDiagnostics ShinyApp}
\description{
DrugExposureDiagnostics shiny app that shows tables and plots
}
\details{
The module consists of the following:
\describe{
\item{"dataPlotPanel"}{Table and a plot (bar or box) for each check.}
\item{"metaDataPanel"}{Table containing the metadata.}
}
}
\section{Super class}{
\code{\link[DrugExposureDiagnostics:ShinyModule]{DrugExposureDiagnostics::ShinyModule}} -> \code{ShinyApp}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ShinyApp-new}{\code{ShinyApp$new()}}
\item \href{#method-ShinyApp-clone}{\code{ShinyApp$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="DrugExposureDiagnostics" data-topic="ShinyModule" data-id="UI"><a href='../../DrugExposureDiagnostics/html/ShinyModule.html#method-ShinyModule-UI'><code>DrugExposureDiagnostics::ShinyModule$UI()</code></a></span></li>
<li><span class="pkg-link" data-pkg="DrugExposureDiagnostics" data-topic="ShinyModule" data-id="server"><a href='../../DrugExposureDiagnostics/html/ShinyModule.html#method-ShinyModule-server'><code>DrugExposureDiagnostics::ShinyModule$server()</code></a></span></li>
<li><span class="pkg-link" data-pkg="DrugExposureDiagnostics" data-topic="ShinyModule" data-id="validate"><a href='../../DrugExposureDiagnostics/html/ShinyModule.html#method-ShinyModule-validate'><code>DrugExposureDiagnostics::ShinyModule$validate()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShinyApp-new"></a>}}
\if{latex}{\out{\hypertarget{method-ShinyApp-new}{}}}
\subsection{Method \code{new()}}{
Initializer method
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ShinyApp$new(resultList, database_id = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{resultList}}{(\code{list}) List containing the output of the checks}

\item{\code{database_id}}{(\code{character}) Database identifier (optional)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
(\code{invisible(self)})
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShinyApp-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ShinyApp-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ShinyApp$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
